#ifndef LLVM_CLANG_CLANGSERVICE_XPC_XPC_H
#define LLVM_CLANG_CLANGSERVICE_XPC_XPC_H

#include "clang-service/Support/OwnedString.h"
#include "clang-service/Support/Value.h"

#include <utility>
#include <cassert>

#include <xpc/xpc.h>

namespace XPC {

class Ref {
  /// Ref is a RAII wrapper for retained XPC objects.

  xpc_object_t Obj;

public:
  Ref() : Obj(nullptr) {}

  explicit Ref(xpc_object_t XO) : Obj(XO) {}

  ~Ref() {
    if (Obj) {
      xpc_release(Obj);
      Obj = nullptr;
    }
  }

  Ref(const Ref &) = delete;

  Ref &operator=(const Ref &XR) = delete;

  Ref(Ref &&XR) : Obj(nullptr) { operator=(std::move(XR)); }

  Ref &operator=(Ref &&XR) {
    assert(!Obj && "XPC object leaked");
    Obj = XR.Obj;
    XR.Obj = nullptr;
    return *this;
  }

  xpc_object_t get() const { return Obj; }
};

class Retain : public Ref {
  /// Retain is a RAII wrapper for non-retained XPC objects.

public:
  explicit Retain(xpc_object_t XO) : Ref(XO) {
    if (XO)
      xpc_retain(XO);
  }
};

class OwnedString : public ClangService::OwnedString {
  /// Create an OwnedString by retaining an XPC string.
 
  Retain Obj;

public:
  explicit OwnedString(xpc_object_t XO)
      : ClangService::OwnedString(
            {xpc_string_get_string_ptr(XO), xpc_string_get_length(XO)}),
        Obj(XO) {
    assert(xpc_get_type(XO) == XPC_TYPE_STRING);
  }

  virtual ~OwnedString() {}
};

class OwnedData : public ClangService::OwnedString {
  /// Create an OwnedString by retaining an XPC data object.

  Retain Obj;

public:
  explicit OwnedData(xpc_object_t XO)
      : ClangService::OwnedString(
            {reinterpret_cast<const char *>(xpc_data_get_bytes_ptr(XO)),
             xpc_data_get_length(XO)}),
        Obj(XO) {
    assert(xpc_get_type(XO) == XPC_TYPE_DATA);
  }

  virtual ~OwnedData() {}
};

Ref serialize(ClangService::Value Obj);

ClangService::Value deserialize(Ref Obj);

ClangService::Value deserialize(xpc_object_t Obj);

ClangService::CUniqueStr describe(xpc_object_t Obj);

} // end namespace XPC

#endif // LLVM_CLANG_CLANGSERVICE_XPC_XPC_H
