#include "clang-service/C-API.h"
#include "clang-service/Support/InProcessClient.h"
#include "clang-service/Support/Value.h"
#include "clang-service/XPC/Client.h"

using ClangService::CSUID;
using ClangService::Value;

namespace {

/// Take ownership of \p value by copying it into \p V, and setting \p value
/// to null.
void takeOwnership(Value *V, CXSValue *value) {
  memcpy((void *)V, (const void *)value, sizeof(Value));
  new (value) Value();
}

/// Perform a shallow clone of \p V without taking ownership of it. The clone
/// is only valid during the lifetime of \p V, and must not be freed.
CXSValue cloneWithoutOwnership(Value &V) {
  CXSValue value;
  memcpy((void *)&value, (const void *)&V, sizeof(Value));
  return value;
}

/// Escape the ownership of \p V into a CXSValue.
CXSValue escapeOwnership(Value V) {
  CXSValue value;
  memcpy((void *)&value, (const void *)&V, sizeof(Value));
  new (&V) Value();
  return value;
}

}

extern "C" {

CXSUID CXSUID_create(const char *uid) { return CSUID(uid).getRawStorage(); }

const char *CXSUID_get_name(CXSUID cxs_uid) { return CSUID(cxs_uid).c_str(); }

CXSValue CXSValue_null_create(void) {
  CXSValue V;
  new (&V) Value();
  return V;
}

CXSValue CXSValue_bool_create(bool value) {
  CXSValue V;
  new (&V) Value(value);
  return V;
}

bool CXSValue_bool_get_value(CXSValue xbool) {
  auto *V = (Value *)&xbool;
  return V->getBool();
}

CXSValue CXSValue_int64_create(int64_t value) {
  CXSValue V;
  new (&V) Value(value);
  return V;
}

int64_t CXSValue_int64_get_value(CXSValue xint) {
  auto *V = (Value *)&xint;
  return V->getInt64();
}

CXSValue CXSValue_uid_create(CXSUID cxs_uid) {
  CXSValue V;
  new (&V) Value(CSUID(cxs_uid));
  return V;
}

CXSUID CXSValue_uid_get_value(CXSValue xuid) {
  auto *V = (Value *)&xuid;
  return V->getCSUID().getRawStorage();
}

CXSValue CXSValue_string_create(const char *string, bool no_copy) {
  CXSValue V;
  if (no_copy) {
    new (&V) Value(string, 0, Value::VariantKind::StringKind);
  } else {
    auto IOS = ClangService::InlineOwnedString::create({string});
    new (&V) Value(std::move(IOS), 0, Value::VariantKind::OwnedStringKind);
  }
  return V;
}

size_t CXSValue_string_get_length(CXSValue xstring) {
  auto *V = (Value *)&xstring;
  return V->getStringRef().size();
}

const char *CXSValue_string_get_string_ptr(CXSValue xstring) {
  auto *V = (Value *)&xstring;
  return V->getString();
}

CXSValue CXSValue_data_create(char *bytes, size_t length, bool no_copy) {
  CXSValue V;
  if (no_copy) {
    new (&V) Value(bytes, length, Value::VariantKind::DataKind);
  } else {
    auto IOS = ClangService::InlineOwnedString::create({bytes, length});
    new (&V) Value(std::move(IOS), length, Value::VariantKind::OwnedDataKind);
  }
  return V;
}

size_t CXSValue_data_get_length(CXSValue xdata) {
  auto *V = (Value *)&xdata;
  return V->getDataLen();
}

const char *CXSValue_data_get_bytes_ptr(CXSValue xdata) {
  auto *V = (Value *)&xdata;
  return V->getDataRef().data();
}

CXSValue CXSValue_array_create() {
  CXSValue V;
  new (&V) Value(llvm::make_unique<Value::Array>());
  return V;
}

void CXSValue_array_append_value(CXSValue array, CXSValue *value) {
  Value V;
  takeOwnership(&V, value);
  auto *AV = (Value *)&array;
  AV->getArray().push_back(std::move(V));
}

CXSValue CXSValue_array_get_value(CXSValue array, size_t index) {
  auto *AV = (Value *)&array;
  return cloneWithoutOwnership(AV->getArray()[index]);
}

size_t CXSValue_array_get_count(CXSValue array) {
  auto *AV = (Value *)&array;
  return AV->getArray().size();
}

CXSValue CXSValue_dictionary_create() {
  CXSValue V;
  new (&V) Value(llvm::make_unique<Value::Dict>());
  return V;
}

void CXSValue_dictionary_set_value(CXSValue dictionary, CXSUID key,
                                   CXSValue *value) {
  Value V;
  takeOwnership(&V, value);
  auto *DV = (Value *)&dictionary;
  DV->getDict()[CSUID(key)] = std::move(V);
}

CXSValue CXSValue_dictionary_get_value(CXSValue dictionary, CXSUID key) {
  auto *DV = (Value *)&dictionary;
  return cloneWithoutOwnership(DV->getDict()[CSUID(key)]);
}

void CXSValue_dictionary_apply(CXSValue dictionary,
                               CXSValue_dictionary_applier_t applier) {
  auto *DV = (Value *)&dictionary;
  for (auto &Pair : DV->getDict()) {
    CXSUID key = Pair.first.getRawStorage();
    CXSValue value = cloneWithoutOwnership(Pair.second);
    if (!applier(key, value))
      return;
  }
}

size_t CXSValue_dictionary_get_count(CXSValue dictionary) {
  auto *DV = (Value *)&dictionary;
  return DV->getDict().size();
}

enum CXSValue_Kind CXSValue_get_kind(CXSValue cxs_value) {
  auto *V = (Value *)&cxs_value;
  switch (V->getKind()) {
  case Value::VariantKind::NullKind:
    return CXSValue_Null;
  case Value::VariantKind::BoolKind:
    return CXSValue_Bool;
  case Value::VariantKind::Int64Kind:
    return CXSValue_Int64;
  case Value::VariantKind::UIDKind:
    return CXSValue_UID;
  case Value::VariantKind::StringKind:
  case Value::VariantKind::OwnedStringKind:
    return CXSValue_String;
  case Value::VariantKind::DataKind:
  case Value::VariantKind::OwnedDataKind:
    return CXSValue_Data;
  case Value::VariantKind::ArrayKind:
    return CXSValue_Array;
  case Value::VariantKind::DictKind:
    return CXSValue_Dict;
  }
}

void CXSValue_free(CXSValue *cxs_value) {
  Value V;
  takeOwnership(&V, cxs_value);
}

CXSClient CXSClient_create(enum CXSClient_Kind client_kind) {
  switch (client_kind) {
  case CXSClient_InProcess:
    return (CXSClient) new ClangService::InProcessClient();
  case CXSClient_XPC:
    return (CXSClient) new XPC::Client();
  }
}

CXSValue CXSClient_request(CXSClient client, CXSValue *request) {
  Value V;
  takeOwnership(&V, request);
  auto *C = (ClangService::Client *)client;
  Value Resp = C->request(std::move(V));
  return escapeOwnership(std::move(Resp));
}

void CXSClient_request_async(CXSClient client, CXSValue *request,
                             CXSClient_handler_t handler) {
  Value V;
  takeOwnership(&V, request);
  auto *C = (ClangService::Client *)client;
  C->request(std::move(V), [handler](Value Resp) {
    handler(escapeOwnership(std::move(Resp)));
  });
}

void CXSClient_free(CXSClient client) {
  delete (ClangService::Client *)client;
}

} // end extern "C"
