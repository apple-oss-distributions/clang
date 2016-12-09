#ifndef LLVM_CLANG_CLANGSERVICE_SERVICE_H
#define LLVM_CLANG_CLANGSERVICE_SERVICE_H

#include "clang-service/Support/Value.h"

#include "llvm/ADT/StringRef.h"

namespace ClangService {

struct ServiceState;

class Service {
public:
  Service();

  virtual ~Service();

  virtual void serve();

  Value handle(Value Request);

private:
  ServiceState *State;

protected:
  void log(Value &V);

  void log(llvm::StringRef Msg);
};

} // end namespace ClangService

#endif // LLVM_CLANG_CLANGSERVICE_SERVICE_H
