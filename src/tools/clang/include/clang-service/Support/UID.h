#ifndef LLVM_CLANG_CLANGSERVICE_SUPPORT_UID_H
#define LLVM_CLANG_CLANGSERVICE_SUPPORT_UID_H

#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/StringRef.h"

#include <atomic>

namespace ClangService {
class CSUID;
}

namespace llvm {
template <> struct DenseMapInfo<ClangService::CSUID>;
}

namespace ClangService {

class CSUID {
  /// CSUID is an efficient, taggable resource identifier.

public:
  CSUID() : UID(nullptr) {}

  CSUID(void *Raw) : UID(Raw) {}

  CSUID(llvm::StringRef S);

  llvm::StringRef getName() const;

  const char *c_str() const { return getName().data(); }

  void *getTag() const;

  void setTag(void *Tag) const;

  uint64_t getAsU64() const;

  static CSUID getFromU64(uint64_t ID);

  bool isValid() const { return UID; }

  static bool isValidIdentifier(llvm::StringRef S);

  friend bool operator==(const CSUID &LHS, const CSUID &RHS) {
    return LHS.UID == RHS.UID;
  }

  friend bool operator!=(const CSUID &LHS, const CSUID &RHS) {
    return !operator==(LHS, RHS);
  }

  void *getRawStorage() const { return UID; }

private:
  void* UID;
};

class LazyCSUID {
  /// LazyCSUID is like CSUID, but it defers initialization to its first use.

  const char *S;
  std::atomic<CSUID> UID;

public:
  LazyCSUID(const char *S = nullptr) : S(S), UID(CSUID()) {}

  LazyCSUID &operator=(const LazyCSUID &Other) {
    S = Other.S;
    return *this;
  }

  CSUID get() {
    if (!UID.load().isValid())
      UID = CSUID(S);
    return UID;
  }

  operator CSUID() { return get(); }

  llvm::StringRef getName() { return get().getName(); }

  const char *c_str() { return get().getName().data(); }
};

} // end namespace ClangService

namespace llvm {

using ClangService::CSUID;

template<> struct DenseMapInfo<CSUID> {
  static inline CSUID getEmptyKey() {
    return {llvm::DenseMapInfo<void *>::getEmptyKey()};
  }

  static inline CSUID getTombstoneKey() {
    return {llvm::DenseMapInfo<void *>::getTombstoneKey()};
  }

  static unsigned getHashValue(const CSUID &Val) {
    return llvm::DenseMapInfo<void *>::getHashValue(Val.getRawStorage());
  }

  static bool isEqual(const CSUID &LHS, const CSUID &RHS) {
    return LHS.getRawStorage() == RHS.getRawStorage();
  }
};

} // end namespace llvm

#endif // LLVM_CLANG_CLANGSERVICE_SUPPORT_UID_H
