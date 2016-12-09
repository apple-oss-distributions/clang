//===--- IndexDataStoreSymbolUtils.h - Utilities for indexstore symbols ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_INDEX_INDEXDATASTORESYMBOLUTILS_H
#define LLVM_CLANG_INDEX_INDEXDATASTORESYMBOLUTILS_H

#include "indexstore/indexstore.h"
#include "clang/Index/IndexSymbol.h"
#include "llvm/Support/ErrorHandling.h"

namespace clang {
namespace index {

/// Map an indexstore_symbol_kind_t to a SymbolKind, handling unknown values.
static inline SymbolKind getSymbolKind(indexstore_symbol_kind_t K) {
  switch ((uint64_t)K) {
  default:
  case INDEXSTORE_SYMBOL_KIND_UNKNOWN:
    return SymbolKind::Unknown;
  case INDEXSTORE_SYMBOL_KIND_MODULE:
    return SymbolKind::Module;
  case INDEXSTORE_SYMBOL_KIND_NAMESPACE:
    return SymbolKind::Namespace;
  case INDEXSTORE_SYMBOL_KIND_NAMESPACEALIAS:
    return SymbolKind::NamespaceAlias;
  case INDEXSTORE_SYMBOL_KIND_MACRO:
    return SymbolKind::Macro;
  case INDEXSTORE_SYMBOL_KIND_ENUM:
    return SymbolKind::Enum;
  case INDEXSTORE_SYMBOL_KIND_STRUCT:
    return SymbolKind::Struct;
  case INDEXSTORE_SYMBOL_KIND_CLASS:
    return SymbolKind::Class;
  case INDEXSTORE_SYMBOL_KIND_PROTOCOL:
    return SymbolKind::Protocol;
  case INDEXSTORE_SYMBOL_KIND_EXTENSION:
    return SymbolKind::Extension;
  case INDEXSTORE_SYMBOL_KIND_UNION:
    return SymbolKind::Union;
  case INDEXSTORE_SYMBOL_KIND_TYPEALIAS:
    return SymbolKind::TypeAlias;
  case INDEXSTORE_SYMBOL_KIND_FUNCTION:
    return SymbolKind::Function;
  case INDEXSTORE_SYMBOL_KIND_VARIABLE:
    return SymbolKind::Variable;
  case INDEXSTORE_SYMBOL_KIND_FIELD:
    return SymbolKind::Field;
  case INDEXSTORE_SYMBOL_KIND_ENUMCONSTANT:
    return SymbolKind::EnumConstant;
  case INDEXSTORE_SYMBOL_KIND_INSTANCEMETHOD:
    return SymbolKind::InstanceMethod;
  case INDEXSTORE_SYMBOL_KIND_CLASSMETHOD:
    return SymbolKind::ClassMethod;
  case INDEXSTORE_SYMBOL_KIND_STATICMETHOD:
    return SymbolKind::StaticMethod;
  case INDEXSTORE_SYMBOL_KIND_INSTANCEPROPERTY:
    return SymbolKind::InstanceProperty;
  case INDEXSTORE_SYMBOL_KIND_CLASSPROPERTY:
    return SymbolKind::ClassProperty;
  case INDEXSTORE_SYMBOL_KIND_STATICPROPERTY:
    return SymbolKind::StaticProperty;
  case INDEXSTORE_SYMBOL_KIND_CONSTRUCTOR:
    return SymbolKind::Constructor;
  case INDEXSTORE_SYMBOL_KIND_DESTRUCTOR:
    return SymbolKind::Destructor;
  case INDEXSTORE_SYMBOL_KIND_CONVERSIONFUNCTION:
    return SymbolKind::ConversionFunction;
  case INDEXSTORE_SYMBOL_KIND_PREFIXOPERATOR:
    return SymbolKind::PrefixOperator;
  case INDEXSTORE_SYMBOL_KIND_POSTFIXOPERATOR:
    return SymbolKind::PostfixOperator;
  case INDEXSTORE_SYMBOL_KIND_INFIXOPERATOR:
    return SymbolKind::InfixOperator;
  case INDEXSTORE_SYMBOL_KIND_ACCESSOR:
    return SymbolKind::Accessor;
  case INDEXSTORE_SYMBOL_KIND_SUBSCRIPT:
    return SymbolKind::Subscript;
  case INDEXSTORE_SYMBOL_KIND_ASSOCIATEDTYPE:
    return SymbolKind::AssociatedType;
  case INDEXSTORE_SYMBOL_KIND_GENERICTYPEPARAM:
    return SymbolKind::GenericTypeParam;
  }
}

/// Map an indexstore_symbol_language_t to a SymbolLanguage, handling unknown
/// values.
static inline SymbolLanguage getSymbolLanguage(indexstore_symbol_language_t L) {
  switch ((uint64_t)L) {
  default: // FIXME: add an unknown language?
  case INDEXSTORE_SYMBOL_LANG_C:
    return SymbolLanguage::C;
  case INDEXSTORE_SYMBOL_LANG_OBJC:
    return SymbolLanguage::ObjC;
  case INDEXSTORE_SYMBOL_LANG_CXX:
    return SymbolLanguage::CXX;
  case INDEXSTORE_SYMBOL_LANG_SWIFT:
    return SymbolLanguage::Swift;
  }
}

/// Map an indexstore representation to a SymbolSubKindSet, handling
/// unknown values.
static inline SymbolSubKindSet getSymbolSubKinds(uint64_t SubKinds) {
  // FIXME: currently these enums must be kept in sync.
  return (uint64_t)SubKinds;
}

/// Map an indexstore representation to a SymbolRoleSet, handling unknown
/// values.
static inline SymbolRoleSet getSymbolRoles(uint64_t Roles) {
  // FIXME: currently these enums must be kept in sync.
  return (uint64_t)Roles;
}

/// Map a SymbolLanguage to a indexstore_symbol_language_t.
static inline indexstore_symbol_kind_t getIndexStoreKind(SymbolKind K) {
  switch (K) {
  case SymbolKind::Unknown:
    return INDEXSTORE_SYMBOL_KIND_UNKNOWN;
  case SymbolKind::Module:
    return INDEXSTORE_SYMBOL_KIND_MODULE;
  case SymbolKind::Namespace:
    return INDEXSTORE_SYMBOL_KIND_NAMESPACE;
  case SymbolKind::NamespaceAlias:
    return INDEXSTORE_SYMBOL_KIND_NAMESPACEALIAS;
  case SymbolKind::Macro:
    return INDEXSTORE_SYMBOL_KIND_MACRO;
  case SymbolKind::Enum:
    return INDEXSTORE_SYMBOL_KIND_ENUM;
  case SymbolKind::Struct:
    return INDEXSTORE_SYMBOL_KIND_STRUCT;
  case SymbolKind::Class:
    return INDEXSTORE_SYMBOL_KIND_CLASS;
  case SymbolKind::Protocol:
    return INDEXSTORE_SYMBOL_KIND_PROTOCOL;
  case SymbolKind::Extension:
    return INDEXSTORE_SYMBOL_KIND_EXTENSION;
  case SymbolKind::Union:
    return INDEXSTORE_SYMBOL_KIND_UNION;
  case SymbolKind::TypeAlias:
    return INDEXSTORE_SYMBOL_KIND_TYPEALIAS;
  case SymbolKind::Function:
    return INDEXSTORE_SYMBOL_KIND_FUNCTION;
  case SymbolKind::Variable:
    return INDEXSTORE_SYMBOL_KIND_VARIABLE;
  case SymbolKind::Field:
    return INDEXSTORE_SYMBOL_KIND_FIELD;
  case SymbolKind::EnumConstant:
    return INDEXSTORE_SYMBOL_KIND_ENUMCONSTANT;
  case SymbolKind::InstanceMethod:
    return INDEXSTORE_SYMBOL_KIND_INSTANCEMETHOD;
  case SymbolKind::ClassMethod:
    return INDEXSTORE_SYMBOL_KIND_CLASSMETHOD;
  case SymbolKind::StaticMethod:
    return INDEXSTORE_SYMBOL_KIND_STATICMETHOD;
  case SymbolKind::InstanceProperty:
    return INDEXSTORE_SYMBOL_KIND_INSTANCEPROPERTY;
  case SymbolKind::ClassProperty:
    return INDEXSTORE_SYMBOL_KIND_CLASSPROPERTY;
  case SymbolKind::StaticProperty:
    return INDEXSTORE_SYMBOL_KIND_STATICPROPERTY;
  case SymbolKind::Constructor:
    return INDEXSTORE_SYMBOL_KIND_CONSTRUCTOR;
  case SymbolKind::Destructor:
    return INDEXSTORE_SYMBOL_KIND_DESTRUCTOR;
  case SymbolKind::ConversionFunction:
    return INDEXSTORE_SYMBOL_KIND_CONVERSIONFUNCTION;
  case SymbolKind::PrefixOperator:
    return INDEXSTORE_SYMBOL_KIND_PREFIXOPERATOR;
  case SymbolKind::PostfixOperator:
    return INDEXSTORE_SYMBOL_KIND_POSTFIXOPERATOR;
  case SymbolKind::InfixOperator:
    return INDEXSTORE_SYMBOL_KIND_INFIXOPERATOR;
  case SymbolKind::Accessor:
    return INDEXSTORE_SYMBOL_KIND_ACCESSOR;
  case SymbolKind::Subscript:
    return INDEXSTORE_SYMBOL_KIND_SUBSCRIPT;
  case SymbolKind::AssociatedType:
    return INDEXSTORE_SYMBOL_KIND_ASSOCIATEDTYPE;
  case SymbolKind::GenericTypeParam:
    return INDEXSTORE_SYMBOL_KIND_GENERICTYPEPARAM;
  }
  llvm_unreachable("unexpected symbol kind");
}

/// Map a SymbolLanguage to a indexstore_symbol_language_t.
static inline indexstore_symbol_language_t getIndexStoreLang(SymbolLanguage L) {
  switch (L) {
  case SymbolLanguage::C:
    return INDEXSTORE_SYMBOL_LANG_C;
  case SymbolLanguage::ObjC:
    return INDEXSTORE_SYMBOL_LANG_OBJC;
  case SymbolLanguage::CXX:
    return INDEXSTORE_SYMBOL_LANG_CXX;
  case SymbolLanguage::Swift:
    return INDEXSTORE_SYMBOL_LANG_SWIFT;
  }
  llvm_unreachable("unexpected symbol language");
}

/// Map a SymbolSubKindSet to its indexstore representation.
static inline uint64_t getIndexStoreSubKinds(SymbolSubKindSet SubKinds) {
  // FIXME: currently these enums must be kept in sync.
  return (uint64_t)SubKinds;
}

/// Map a SymbolRoleSet to its indexstore representation.
static inline uint64_t getIndexStoreRoles(SymbolRoleSet Roles) {
  // FIXME: currently these enums must be kept in sync.
  return (uint64_t)Roles;
}

} // end namespace index
} // end namespace clang

#endif // LLVM_CLANG_INDEX_INDEXDATASTORESYMBOLUTILS_H
