#ifndef LLVM_CLANG_CLANGSERVICE_XPC_SERVICE_H
#define LLVM_CLANG_CLANGSERVICE_XPC_SERVICE_H

#include "clang-service/Service.h"

#include <xpc/xpc.h>

namespace XPC {

class Service : public ClangService::Service {
public:
  virtual ~Service() {}

  static Service &get();

  virtual void serve() override;

private:
  static void handleConnection(xpc_connection_t Peer);

  void handleClientActivity(xpc_connection_t Peer, xpc_object_t Event);
};

} // end namespace XPC

#endif // LLVM_CLANG_CLANGSERVICE_XPC_SERVICE_H
