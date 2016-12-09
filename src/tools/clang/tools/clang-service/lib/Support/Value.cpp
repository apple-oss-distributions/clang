#include "clang-service/Messaging.h"
#include "clang-service/Support/Value.h"
#include "clang-service/Support/ValueUtils.h"

#include "llvm/Support/raw_ostream.h"

#include <algorithm>

namespace ClangService {

namespace {
#define REGISTER_UID(Name, UIDStr) LazyCSUID Name{UIDStr};
#include "clang-service/ProtocolUIDs.inc"
}

Value Value::clone() const {
  switch (getKind()) {
  case Value::VariantKind::NullKind:
    return {};

  case Value::VariantKind::BoolKind:
    return {getBool()};

  case Value::VariantKind::Int64Kind:
    return {getInt64()};

  case Value::VariantKind::UIDKind:
    return {getCSUID()};

  case Value::VariantKind::StringKind:
    return {getString()};

  case Value::VariantKind::OwnedStringKind:
    return Value::string(InlineOwnedString::create(getStringRef()));

  case Value::VariantKind::DataKind:
    return Value::data(getData(), getDataLen());

  case Value::VariantKind::OwnedDataKind:
    return Value::data(InlineOwnedString::create(getDataRef()));

  case Value::VariantKind::ArrayKind: {
    auto A = Value::array({});
    auto &Arr = A.getArray();
    for (auto &Elt : getArray())
      Arr.push_back(Elt.clone());
    return A;
  }

  case Value::VariantKind::DictKind: {
    auto D = Value::dict({});
    auto &Mappings = D.getDict();
    for (auto &Pair : getDict())
      Mappings[Pair.first] = Pair.second.clone();
    return D;
  }
  }
}

void Value::dump(llvm::raw_ostream &OS, bool AddNewline) const {
  switch (getKind()) {
  case Value::VariantKind::NullKind:
    OS << "nil";
    break;

  case Value::VariantKind::BoolKind:
    OS << (getBool() ? "true" : "false");
    break;

  case Value::VariantKind::Int64Kind:
    OS << getInt64();
    break;

  case Value::VariantKind::UIDKind:
    OS << "CSUID(" << getCSUID().getAsU64() << ")";
    break;

  case Value::VariantKind::StringKind:
  case Value::VariantKind::OwnedStringKind: {
    auto SR = getStringRef();
    OS << "String(" << SR.size() << ", " << SR << ")";
    break;
  }

  case Value::VariantKind::DataKind:
  case Value::VariantKind::OwnedDataKind: {
    auto DR = getDataRef();
    OS << "Data(" << DR.size() << ", " << DR << ")";
    break;
  }

  case Value::VariantKind::ArrayKind: {
    OS << "[ ";
    auto &Arr = getArray();
    for (auto &Elt : Arr) {
      Elt.dump(OS, false);
      OS << " ";
    }
    OS << "]";
    break;
  }

  case Value::VariantKind::DictKind: {
    OS << "{ ";
    auto &Mappings = getDict();
    for (auto &Pair : Mappings) {
      OS << Pair.first.getName() << ":";
      Pair.second.dump(OS, false);
      OS << " ";
    }
    OS << "}";
    break;
  }
  }

  if (AddNewline)
    OS << "\n";
}

void Value::structure(llvm::raw_ostream &OS, bool AddNewline) const {
  switch (getKind()) {
  case Value::VariantKind::NullKind:
    OS << "nil";
    break;

  case Value::VariantKind::BoolKind:
    OS << "bool";
    break;

  case Value::VariantKind::Int64Kind:
    OS << "int64";
    break;

  case Value::VariantKind::UIDKind:
    OS << "CSUID";
    break;

  case Value::VariantKind::StringKind:
    OS << "String";
    break;

  case Value::VariantKind::OwnedStringKind:
    OS << "OwnedStringKind";
    break;

  case Value::VariantKind::DataKind:
    OS << "DataKind";
    break;

  case Value::VariantKind::OwnedDataKind:
    OS << "OwnedDataKind";
    break;

  case Value::VariantKind::ArrayKind: {
    OS << "[ ";
    auto &Arr = getArray();
    for (auto &Elt : Arr) {
      Elt.structure(OS, false);
      OS << " ";
    }
    OS << "]";
    break;
  }

  case Value::VariantKind::DictKind: {
    OS << "{ ";
    auto &Mappings = getDict();
    for (auto &Pair : Mappings) {
      Pair.second.structure(OS, false);
      OS << " ";
    }
    OS << "}";
    break;
  }
  }

  if (AddNewline)
    OS << "\n";
}

bool operator==(const Value &LHS, const Value &RHS) {
  if (LHS.isStringLike() != RHS.isStringLike())
    return false;

  if (LHS.isDataLike() != RHS.isDataLike())
    return false;

  if (!LHS.isStringLike() && !LHS.isDataLike())
    if (LHS.getKind() != RHS.getKind())
      return false;

  switch (LHS.getKind()) {
  case Value::VariantKind::NullKind:
    return true;

  case Value::VariantKind::BoolKind:
    return LHS.getBool() == RHS.getBool();

  case Value::VariantKind::Int64Kind:
    return LHS.getInt64() == RHS.getInt64();

  case Value::VariantKind::UIDKind:
    return LHS.getCSUID() == RHS.getCSUID();

  case Value::VariantKind::StringKind:
  case Value::VariantKind::OwnedStringKind:
    return LHS.getStringRef().equals(RHS.getStringRef());

  case Value::VariantKind::DataKind:
  case Value::VariantKind::OwnedDataKind:
    return LHS.getDataRef().equals(RHS.getDataRef());

  case Value::VariantKind::ArrayKind: {
    auto &LA = LHS.getArray();
    auto &RA = RHS.getArray();
    return LA.size() == RA.size() &&
           std::equal(LA.begin(), LA.end(), RA.begin());
  }

  case Value::VariantKind::DictKind: {
    auto &LD = LHS.getDict();
    auto &RD = RHS.getDict();
    if (LD.size() != RD.size())
      return false;
    for (auto &Pair : LD)
      if (*LD.find(Pair.first) != *RD.find(Pair.first))
        return false;
    return true;
  }
  }
}

ErrorKind getResponseErrorKind(Value &Response) {
  if (!Response.isDataLike())
    return ErrorKind::NoError;

  auto EK = ErrorKind(Response.getData()[0]);
  if (EK == ErrorKind::RequestInvalid) return EK;
  if (EK == ErrorKind::RequestFailed) return EK;
  if (EK == ErrorKind::RequestInterrupted) return EK;
  if (EK == ErrorKind::RequestCancelled) return EK;
  if (EK == ErrorKind::Fatal) return EK;
  return ErrorKind::NoError;
}

llvm::StringRef getResponseErrorString(Value &Response) {
  if (getResponseErrorKind(Response) == ErrorKind::NoError)
    return "NoError";
  return Response.getDataRef().substr(1);
}

Value toStringArray(llvm::ArrayRef<const char *> Arr) {
  auto A = llvm::make_unique<Value::Array>();
  A->reserve(Arr.size());
  for (auto *Elt : Arr)
    A->emplace_back(Elt);
  return {std::move(A)};
}

bool toStringArray(Value &V, llvm::SmallVectorImpl<const char *> &Out) {
  assert(V.isArray());
  for (Value &Elt : V.getArray()) {
    if (!Elt.isStringLike())
      return false;

    Out.push_back(Elt.getString());
  }
  return true;
}

Value toUnsavedFilesArray(llvm::ArrayRef<CXUnsavedFile *> Arr) {
  auto A = llvm::make_unique<Value::Array>();
  A->reserve(Arr.size());
  for (auto *Elt : Arr) {
    A->push_back(
        Value::dict({{KeyName, Elt->Filename},
                     {KeyBuffer, Value::data(Elt->Contents, Elt->Length)}}));
  }
  return {std::move(A)};
}

bool toUnsavedFilesArray(Value &V, llvm::SmallVectorImpl<CXUnsavedFile> &Out) {
  assert(V.isArray());
  for (Value &Elt : V.getArray()) {
    if (!Elt.isDict())
      return false;

    Value::Dict &UFD = Elt.getDict();
    Value &ClientKeyName = UFD[KeyName];
    Value &ClientKeyBuffer = UFD[KeyBuffer];
    if (!ClientKeyName.isStringLike() || !ClientKeyBuffer.isDataLike())
      return false;

    CXUnsavedFile UF;
    UF.Filename = ClientKeyName.getString();
    UF.Contents = ClientKeyBuffer.getData();
    UF.Length = ClientKeyBuffer.getDataLen();
    Out.push_back(UF);
  }
  return true;
}

} // end namespace ClangService
