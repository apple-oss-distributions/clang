#ifndef LLVM_CLANG_CLANGSERVICE_SUPPORT_VALUE_H
#define LLVM_CLANG_CLANGSERVICE_SUPPORT_VALUE_H

#include "clang-service/Support/OwnedString.h"
#include "clang-service/Support/UID.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/STLExtras.h"

#include <cstdint>
#include <initializer_list>
#include <memory>
#include <utility>

namespace llvm {
class raw_ostream;
}

namespace ClangService {

class Value {
  /// Value represents a structured message.

public:
  enum class VariantKind : unsigned {
    NullKind,
    BoolKind,
    Int64Kind,
    UIDKind,
    StringKind,
    OwnedStringKind,
    DataKind,
    OwnedDataKind,
    ArrayKind,
    DictKind
  };

  using Array = llvm::SmallVector<Value, 8>;
  using Dict = llvm::SmallDenseMap<CSUID, Value>;

private:
  union VariantData {
    bool B;
    int64_t I;
    CSUID U;
    const char *S;
    std::unique_ptr<OwnedString> O;
    std::unique_ptr<Array> A;
    std::unique_ptr<Dict> D;

    VariantData() {
      memset((void *)this, 0, sizeof(VariantData));
    }

    ~VariantData() {}
  };

  VariantData Val;
  unsigned DataLen;
  VariantKind Kind;

  void ingest(Value &&V) {
    Kind = V.Kind;
    V.Kind = VariantKind::NullKind;

    switch (Kind) {
    case VariantKind::NullKind:
      break;

    case VariantKind::BoolKind:
      Val.B = V.Val.B;
      break;

    case VariantKind::Int64Kind:
      Val.I = V.Val.I;
      break;

    case VariantKind::UIDKind:
      Val.U = V.Val.U;
      break;

    case VariantKind::StringKind:
      Val.S = V.Val.S;
      break;

    case VariantKind::OwnedStringKind:
      new (&Val.O) std::unique_ptr<OwnedString>(std::move(V.Val.O));
      break;

    case VariantKind::DataKind:
      Val.S = V.Val.S;
      DataLen = V.DataLen;
      break;

    case VariantKind::OwnedDataKind:
      new (&Val.O) std::unique_ptr<OwnedString>(std::move(V.Val.O));
      DataLen = V.DataLen;
      break;

    case VariantKind::ArrayKind:
      new (&Val.A) std::unique_ptr<Array>(std::move(V.Val.A));
      break;

    case VariantKind::DictKind:
      new (&Val.D) std::unique_ptr<Dict>(std::move(V.Val.D));
      break;
    }
  }

public:
  Value() : Kind(VariantKind::NullKind) {}

  Value(bool B) : Kind(VariantKind::BoolKind) { Val.B = B; }

  Value(int64_t I) : Kind(VariantKind::Int64Kind) { Val.I = I; }

  Value(CSUID U) : Kind(VariantKind::UIDKind) { Val.U = U; }

  Value(const char *S) : Value(S, 0, VariantKind::StringKind) {}

  Value(std::unique_ptr<Array> A) : Kind(VariantKind::ArrayKind) {
    new (&Val.A) std::unique_ptr<Array>(std::move(A));
  }

  Value(std::unique_ptr<Dict> D) : Kind(VariantKind::DictKind) {
    new (&Val.D) std::unique_ptr<Dict>(std::move(D));
  }

  Value(const char *S, unsigned DataLen, VariantKind Kind)
      : DataLen(DataLen), Kind(Kind) {
    Val.S = S;
  }

  Value(std::unique_ptr<OwnedString> O, unsigned DataLen, VariantKind Kind)
      : DataLen(DataLen), Kind(Kind) {
    new (&Val.O) std::unique_ptr<OwnedString>(std::move(O));
  }

  Value(const Value &) = delete;

  Value &operator=(const Value &) = delete;

  Value(Value &&V) { ingest(std::move(V)); }

  Value &operator=(Value &&V) {
    this->~Value();
    ingest(std::move(V));
    return *this;
  }

  ~Value() {
    switch (getKind()) {
    case VariantKind::OwnedStringKind:
    case VariantKind::OwnedDataKind:
      Val.O.~unique_ptr<OwnedString>();
      break;

    case VariantKind::ArrayKind:
      Val.A.~unique_ptr<Array>();
      break;

    case VariantKind::DictKind:
      Val.D.~unique_ptr<Dict>();
      break;

    default:
      return;
    }
  }

  static Value uid(CSUID U) { return {U}; }

  static Value uid(LazyCSUID &LU) { return {LU.get()}; }

  static Value string(std::unique_ptr<OwnedString> O) {
    return {std::move(O), 0, VariantKind::OwnedStringKind};
  }

  static Value data(const char *S, unsigned Len) {
    return {S, Len, VariantKind::DataKind};
  }

  static Value data(std::unique_ptr<OwnedString> O) {
    unsigned Size = O->size();
    return {std::move(O), Size, VariantKind::OwnedDataKind};
  }

  static Value array(std::initializer_list<Value> Arr) {
    auto A = llvm::make_unique<Array>();
    A->reserve(Arr.size());
    for (auto &Elt : Arr)
      A->push_back(std::move(const_cast<Value &>(Elt)));
    return {std::move(A)};
  }

  static Value dict(std::initializer_list<std::pair<CSUID, Value>> Mappings) {
    auto D = llvm::make_unique<Dict>();
    D->grow(Mappings.size());
    for (auto &Pair : Mappings)
      if (!Pair.second.isNull())
        D->operator[](Pair.first) = std::move(const_cast<Value &>(Pair.second));
    return {std::move(D)};
  }

  VariantKind getKind() const { return Kind; }

  bool isNull() const { return getKind() == VariantKind::NullKind; }

  bool isBool() const { return getKind() == VariantKind::BoolKind; }

  bool isInt64() const { return getKind() == VariantKind::Int64Kind; }

  bool isUID() const { return getKind() == VariantKind::UIDKind; }

  bool isStringLike() const {
    return Kind == VariantKind::StringKind ||
           Kind == VariantKind::OwnedStringKind;
  }

  bool isDataLike() const {
    return Kind == VariantKind::DataKind || Kind == VariantKind::OwnedDataKind;
  }

  bool isArray() const { return getKind() == VariantKind::ArrayKind; }

  bool isDict() const { return getKind() == VariantKind::DictKind; }

  bool getBool() const {
    assert(isBool());
    return Val.B;
  }

  int64_t getInt64() const {
    assert(isInt64());
    return Val.I;
  }

  CSUID getCSUID() const {
    assert(isUID());
    return Val.U;
  }

  const char *getString() const {
    assert(isStringLike());
    return getStringyData();
  }

  llvm::StringRef getStringRef() const {
    assert(isStringLike());
    if (getKind() == VariantKind::StringKind)
      return llvm::StringRef(Val.S);
    return Val.O->get();
  }

  const char *getData() const {
    assert(isDataLike());
    return getStringyData();
  }

  unsigned getDataLen() const {
    assert(isDataLike());
    return DataLen;
  }

  llvm::StringRef getDataRef() const {
    assert(isDataLike());
    if (getKind() == VariantKind::DataKind)
      return {Val.S, DataLen};
    return Val.O->get();
  }

  Array &getArray() const {
    assert(isArray());
    return *Val.A.get();
  }

  Dict &getDict() const {
    assert(isDict());
    return *Val.D.get();
  }

  Value clone() const;

  void dump(llvm::raw_ostream &OS, bool AddNewline = true) const;

  void structure(llvm::raw_ostream &OS, bool AddNewline = true) const;

  friend bool operator!=(const Value &LHS, const Value &RHS) {
    return !(LHS == RHS);
  }

  friend bool operator==(const Value &LHS, const Value &RHS);

private:
  const char *getStringyData() const {
    if (getKind() == VariantKind::StringKind ||
        getKind() == VariantKind::DataKind)
      return Val.S;
    else
      return Val.O->get().data();
  }
};

}; // end namespace ClangService

#endif // LLVM_CLANG_CLANGSERVICE_SUPPORT_VALUE_H
