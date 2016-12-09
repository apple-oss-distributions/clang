#include "clang-service/Messaging.h"
#include "clang-service/XPC/XPC.h"
#include "clang-service/XPC/Client.h"

#include "llvm/Support/raw_ostream.h"

#include <cassert>

namespace XPC {

using ClangService::Message;
using ClangService::Value;
using llvm::StringRef;

namespace {
ClangService::CSUID MsgUID{"msg"};
}

Client::Client() : State(Offline) {
  Q = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
  Conn = xpc_connection_create("com.apple.ClangXPC", Q);
  if (!Conn) {
    log("Failed to connect to the service");
    return;
  }

  xpc_connection_set_event_handler(Conn, ^(xpc_object_t Event) {
    handleEvent(Event);
  });

  // The connection starts off suspended, so resume it explicitly and update
  // the connection status.
  xpc_connection_resume(Conn);
  ConnRefCount = 1;
  State = Online;
}

Client::~Client() {
  dropConnectionRef();
  State = Offline;
}

Value Client::getWrappedRequest(Value Request) {
  return Value::dict({{MsgUID, Value::array({std::move(Request)})}});
}

Value Client::decodeReply(xpc_object_t Reply) {
  xpc_type_t Type = xpc_get_type(Reply);

  if (Type != XPC_TYPE_DICTIONARY) {
    log("Response type is not a dictionary!");
    return {};
  }

  auto RespXO = xpc_dictionary_get_value(Reply, ClangService::KeyMsgResponse);
  return deserialize(RespXO);
}

Value Client::request(Value Request) {
  if (State != Online) {
    log("Failed to create request, connection not online");
    return {};
  }

  Ref WrappedMsg = serialize(getWrappedRequest(std::move(Request)));

  return decodeReply(
      xpc_connection_send_message_with_reply_sync(Conn, WrappedMsg.get()));
}

void Client::request(Value Request, std::function<void (Value)> Handler) {
  if (State != Online) {
    log("Failed to create request, connection not online");
    return;
  }

  Ref WrappedMsg = serialize(getWrappedRequest(std::move(Request)));

  xpc_connection_send_message_with_reply(Conn, WrappedMsg.get(), Q,
                                         ^(xpc_object_t Reply) {
    Handler(decodeReply(Reply));
  });
}

void Client::bumpConnectionRef() {
  ++ConnRefCount;
  xpc_retain(Conn);
}

void Client::dropConnectionRef() {
  --ConnRefCount;
  xpc_release(Conn);
}

void Client::handleEvent(xpc_object_t Event) {
  if (State != Online) {
    log("Cannot handle event, service not online");
    return;
  }

  xpc_type_t Type = xpc_get_type(Event);

  if (Type != XPC_TYPE_DICTIONARY) {
    handleError(Event);
    return;
  }

  xpc_object_t Contents =
      xpc_dictionary_get_value(Event, ClangService::KeyInternalMsg);
  if (!Contents) {
    log("Received unexpected message!", /*Fatal=*/true);
    return;
  }

  // FIXME: We need to figure out these message kinds. UIDSynchronization in
  // particular has other benefits noted in the code for XPC serialization.
#if 0
  auto MessageType = (Message)xpc_array_get_uint64(Contents, 0);
  switch (MessageType) {
  case Message::Initialization:
  case Message::Notification:
  case Message::UIDSynchronization:
  case Message::TraceMessage:
  default:
    log("Received message with invalid type!", /*Fatal=*/true);
  }
#endif
}

void Client::handleError(xpc_object_t Event) {
  if (Event == XPC_ERROR_CONNECTION_INVALID) {
    log("Shutting down the connection!", /*Fatal=*/true);
  } else if (Event == XPC_ERROR_CONNECTION_INTERRUPTED) {
    log("The service crashed. Attempting to respawn it...");

    // Retain the connection until it's re-established or dead.
    bumpConnectionRef();

    handleServiceCrash();
  } else {
    log("Unexpected error or invalid event:");
    log(describe(Event).get());
  }
}

// FIXME: We need tests for failure recovery, pings, etc.
void Client::handleServiceCrash() {
  State = Interrupted;
  xpc_object_t Ping = xpc_dictionary_create(nullptr, nullptr, 0);
  xpc_dictionary_set_bool(Ping, "ping", true);

  xpc_connection_send_message_with_reply(Conn, Ping, Q, ^(xpc_object_t Reply) {
    xpc_type_t Type = xpc_get_type(Reply);

    if (Type != XPC_TYPE_ERROR) {
      // Looks like it's back! Drop the 'keep-alive' ref on the connection.
      dropConnectionRef();
      return;
    }

    if (Reply == XPC_ERROR_CONNECTION_INVALID) {
      dropConnectionRef();
      log("Can't respawn the service, shutting down!", /*Fatal=*/true);
    } else {
      // Try respawning again.
      handleServiceCrash();
    }
  });
}

void Client::log(const char *Msg, bool Fatal) {
  llvm::errs() << "[ClangService] " << Msg << '\n';
  llvm::errs().flush();
  if (Fatal)
    State = Offline;
}

} // end namespace XPC
