#include "clang-service/Messaging.h"
#include "clang-service/XPC/XPC.h"
#include "clang-service/XPC/Service.h"

#include "llvm/Support/ManagedStatic.h"

namespace XPC {

using ClangService::Value;
using llvm::StringRef;

namespace {
ClangService::CSUID MsgUID{"msg"};
}

static llvm::ManagedStatic<Service> GlobalXPCService;

Service &Service::get() {
  return *GlobalXPCService;
}

void Service::serve() {
  log("Starting loop...");
  xpc_main(handleConnection);
}

void Service::handleConnection(xpc_connection_t Peer) {
  Service &Service = get();

  // Maintain a positive refcount to keep the connection alive.
  xpc_transaction_begin();

  xpc_connection_set_event_handler(Peer, ^(xpc_object_t Event) {
    Service.handleClientActivity(Peer, Event);
  });

  // The connection starts off suspended, so resume it explicitly.
  xpc_connection_resume(Peer);
}

static Value extractRequestMessage(Value Request) {
  Value &Msg = Request.getDict()[MsgUID];
  if (Msg.isArray() && Msg.getArray().size())
    return std::move(Msg.getArray()[0]);
  return {};
}

// FIXME: Add support for request cancellations (to deal with IndexerCallbacks
// in a better way).
void Service::handleClientActivity(xpc_connection_t Peer, xpc_object_t Event) {
  if (Event == XPC_ERROR_CONNECTION_INVALID ||
      Event == XPC_ERROR_CONNECTION_INTERRUPTED) {
    log("Invalid/interrupted connection; cancelling peer.");
    xpc_connection_cancel(Peer);
    return;
  }

  xpc_type_t Type = xpc_get_type(Event);

  if (Type != XPC_TYPE_DICTIONARY)
    log(describe(Event).get());

  Value Request = XPC::deserialize(Event);
  Value RequestMsg = extractRequestMessage(std::move(Request));

  Value Response = handle(std::move(RequestMsg));

  ClangService::ErrorKind EK = getResponseErrorKind(Response);

  if (EK != ClangService::ErrorKind::NoError) {
    log("Error:");
    log(getResponseErrorString(Response));

    if (EK == ClangService::ErrorKind::Fatal) {
      log("Shutting down; error was fatal.");
      xpc_transaction_end();
      xpc_connection_cancel(Peer);
      return;
    }
  }

  Ref Reply{xpc_dictionary_create_reply(Event)};
  if (!Reply.get()) {
    log("Dropping client; no way to respond to it.");
    xpc_connection_cancel(Peer);
    return;
  }

  xpc_dictionary_set_value(Reply.get(), ClangService::KeyMsgResponse,
                           XPC::serialize(std::move(Response)).get());
  xpc_connection_send_message(Peer, Reply.get());
}

} // end namespace XPC
