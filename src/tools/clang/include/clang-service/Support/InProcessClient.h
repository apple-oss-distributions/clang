#ifndef LLVM_CLANG_CLANGSERVICE_SUPPORT_INPROCESSCLIENT_H
#define LLVM_CLANG_CLANGSERVICE_SUPPORT_INPROCESSCLIENT_H

#include "clang-service/Client.h"
#include "clang-service/Service.h"

namespace ClangService {

class InProcessClient : public Client {
  /// InProcessClient calls into the clang service directly, without using any
  /// inter-process communication.

  Service CS;

public:
  Value request(Value Request) override {
    return CS.handle(std::move(Request)).clone();
  }

  void request(Value Request, std::function<void(Value)> Handler) override {
    Handler(request(std::move(Request)));
  }
};

} // end namespace ClangService

#endif // LLVM_CLANG_CLANGSERVICE_SUPPORT_INPROCESSCLIENT_H
