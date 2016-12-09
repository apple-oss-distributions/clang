#ifndef LLVM_CLANG_CLANGSERVICE_CLIENT_H
#define LLVM_CLANG_CLANGSERVICE_CLIENT_H

#include "clang-service/Support/Value.h"

#include <functional>

namespace ClangService {

class Client {
public:
  virtual ~Client() {}

  virtual Value request(Value Request) = 0;

  virtual void request(Value Request, std::function<void (Value)> Handler) = 0;
};

} // end namespace ClangService

#endif // LLVM_CLANG_CLANGSERVICE_CLIENT_H
