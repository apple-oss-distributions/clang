#ifndef LLVM_CLANG_CLANGSERVICE_SUPPORT_OWNEDSTRING_H
#define LLVM_CLANG_CLANGSERVICE_SUPPORT_OWNEDSTRING_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

namespace ClangService {

class OwnedString {
public:
  /// OwnedString guarantees that a pointer to stringy data is alive.
  ///
  /// Note: Using this container directly is unsafe. It's meant to be derived,
  /// so that the owner of the stringy data can be specified explicitly.

  explicit OwnedString(llvm::StringRef S) : Str(S) {}

  virtual ~OwnedString() {}

  OwnedString(const OwnedString &) = delete;

  OwnedString &operator=(const OwnedString &) = delete;

  OwnedString(OwnedString &&) = delete;

  OwnedString &operator=(OwnedString &&) = delete;

  llvm::StringRef get() const { return Str; }

  char *data() const { return const_cast<char *>(get().data()); }

  unsigned size() const { return get().size(); }

protected:
  llvm::StringRef Str;
};

class InlineOwnedString : public OwnedString {
  /// An InlineOwnedString is formed by allocating space for stringy data along
  /// with space for the OwnedString base.

  InlineOwnedString(llvm::StringRef S)
      : OwnedString({getInlineData(), S.size()}) {
    memcpy((void *)getInlineData(), (const void *)S.data(), S.size());
    const_cast<char *>(getInlineData())[S.size()] = '\0';
  }

  InlineOwnedString(unsigned Length) : OwnedString({getInlineData(), Length}) {
    auto *Buf = const_cast<char *>(getInlineData());
    Buf[Length + 1] = '\0';
  }

  const char *getInlineData() const {
    return (const char *)(this) + sizeof(OwnedString);
  }

public:
  static std::unique_ptr<InlineOwnedString> create(llvm::StringRef S) {
    auto *Buf = ::operator new(sizeof(OwnedString) + S.size() + 1);
    new (Buf) InlineOwnedString(S);
    auto *OS = reinterpret_cast<InlineOwnedString *>(Buf);
    return std::unique_ptr<InlineOwnedString>(OS);
  }

  static std::unique_ptr<InlineOwnedString> create(unsigned Length) {
    auto *Buf = ::operator new(sizeof(OwnedString) + Length + 1);
    new (Buf) InlineOwnedString(Length);
    auto *OS = reinterpret_cast<InlineOwnedString*>(Buf);
    return std::unique_ptr<InlineOwnedString>(OS);
  }

  virtual ~InlineOwnedString() {}
};

class raw_inlinestring_ostream : public llvm::raw_ostream {
  /// raw_inlinestring_ostream is a raw_ostream backed by an InlineOwnedString.

  InlineOwnedString &IOS;

  /// See raw_ostream::write_impl.
  void write_impl(const char *Ptr, size_t Size) override {
    assert(current_pos() + Size <= IOS.size());
    memcpy((void *)(IOS.data() + current_pos()), (const void *)(Ptr), Size);
  }

  /// Return the current position within the stream, not counting the bytes
  /// currently in the buffer.
  uint64_t current_pos() const override { return GetNumBytesInBuffer(); }

public:
  raw_inlinestring_ostream(InlineOwnedString &IOS) : IOS(IOS) {
    SetBuffer(IOS.data(), IOS.size());
  }

  virtual ~raw_inlinestring_ostream() { flush(); }
};

struct CFree {
  void operator()(void *P) { free(P); }
};

using CUniqueStr = std::unique_ptr<char, CFree>;

} // end namespace ClangService

#endif // LLVM_CLANG_CLANGSERVICE_SUPPORT_OWNEDSTRING_H
