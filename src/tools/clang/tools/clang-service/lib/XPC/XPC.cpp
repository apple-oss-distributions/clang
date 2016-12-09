#include "clang-service/XPC/XPC.h"

namespace XPC {

using ClangService::CSUID;
using ClangService::Value;
using llvm::StringRef;

// FIXME: Change the way we serialize arrays to allow for a more efficient
// encoding of UID -> Value dictionaries.

Ref serialize(Value Obj) {
  auto sinkValue = [](Value V) -> xpc_object_t {
    switch (V.getKind()) {
    case Value::VariantKind::NullKind:
      return xpc_null_create();

    case Value::VariantKind::BoolKind:
      return xpc_bool_create(V.getBool());

    case Value::VariantKind::Int64Kind:
      return xpc_int64_create(V.getInt64());

    case Value::VariantKind::UIDKind:
      return xpc_uint64_create(V.getCSUID().getAsU64());

    case Value::VariantKind::StringKind:
    case Value::VariantKind::OwnedStringKind: {
      return xpc_string_create(V.getString());
    }

    case Value::VariantKind::DataKind:
    case Value::VariantKind::OwnedDataKind: {
      return xpc_data_create(reinterpret_cast<const void *>(V.getData()),
                             V.getDataLen());
    }

    case Value::VariantKind::ArrayKind: {
      xpc_object_t Arr = xpc_array_create(nullptr, 0);
      for (auto &Elt : V.getArray()) {
        Ref Obj = serialize(std::move(Elt));
        xpc_array_append_value(Arr, Obj.get());
      }
      return Arr;
    }

    case Value::VariantKind::DictKind: {
      xpc_object_t Mappings = xpc_dictionary_create(nullptr, nullptr, 0);
      for (auto &Pair : V.getDict()) {
        const char *KeyName = Pair.first.c_str();
        Ref Obj = serialize(std::move(Pair.second));
        xpc_dictionary_set_value(Mappings, KeyName, Obj.get());
      }
      return Mappings;
    }
    }
  };

  return Ref(sinkValue(std::move(Obj)));
}

Value deserialize(xpc_object_t Obj) {
  assert(Obj && "Cannot deserialize nullptr");

  xpc_type_t Type = xpc_get_type(Obj);

  if (Type == XPC_TYPE_NULL) {
    return {};

  } else if (Type == XPC_TYPE_BOOL) {
    return {xpc_bool_get_value(Obj)};

  } else if (Type == XPC_TYPE_INT64) {
    return {xpc_int64_get_value(Obj)};

  } else if (Type == XPC_TYPE_UINT64) {
    return {CSUID::getFromU64(xpc_uint64_get_value(Obj))};

  } else if (Type == XPC_TYPE_STRING) {
    auto OS = llvm::make_unique<OwnedString>(Obj);
    return Value::string(std::move(OS));

  } else if (Type == XPC_TYPE_DATA) {
    auto OD = llvm::make_unique<OwnedData>(Obj);
    return Value::data(std::move(OD));

  } else if (Type == XPC_TYPE_ARRAY) {
    auto V = Value::array({});
    Value::Array &Arr = V.getArray();
    size_t Len = xpc_array_get_count(Obj);
    Arr.reserve(Len);
    for (size_t I = 0; I < Len; ++I)
      Arr.push_back(deserialize(xpc_array_get_value(Obj, I)));
    return V;

  } else if (Type == XPC_TYPE_DICTIONARY) {
    auto V = Value::dict({});
    Value::Dict &D = V.getDict();
    D.grow(xpc_dictionary_get_count(Obj));
    xpc_dictionary_apply(Obj, ^(const char *Key, xpc_object_t Val) {
      if (!CSUID::isValidIdentifier(Key))
        return false;
      CSUID KeyID{Key};
      D[KeyID] = deserialize(Val);
      return true;
    });
    return V;
  }

  assert(!"Could not deserialize XPC object");
  return {};
}

Value deserialize(Ref Obj) {
  return deserialize(Obj.get());
}

ClangService::CUniqueStr describe(xpc_object_t Obj) {
  return ClangService::CUniqueStr(xpc_copy_description(Obj));
}

} // end namespace XPC
