#ifndef LLVM_CLANG_CLANGSERVICE_XPC_CLIENT_H
#define LLVM_CLANG_CLANGSERVICE_XPC_CLIENT_H

#include "clang-service/Client.h"

#include <xpc/xpc.h>

namespace XPC {

class Client : public ClangService::Client {
public:
  Client();

  virtual ~Client();

  virtual ClangService::Value request(ClangService::Value Request) override;

  virtual void
  request(ClangService::Value Request,
          std::function<void(ClangService::Value)> Handler) override;

private:
  enum Status {
    Online,
    Interrupted,
    Offline
  };

  Status State;
  dispatch_queue_t Q;
  xpc_connection_t Conn;
  unsigned ConnRefCount;

  void bumpConnectionRef();

  void dropConnectionRef();

  void handleEvent(xpc_object_t Event);

  void handleError(xpc_object_t Event);

  void handleServiceCrash();

  ClangService::Value getWrappedRequest(ClangService::Value Request);

  ClangService::Value decodeReply(xpc_object_t Reply);

  void log(const char *Msg, bool Fatal = false);
};

} // end namespace XPC

#endif // LLVM_CLANG_CLANGSERVICE_XPC_CLIENT_H
