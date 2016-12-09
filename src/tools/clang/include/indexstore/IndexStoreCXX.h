//===--- IndexStoreCXX.h - C++ wrapper for the Index Store C API. ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Header-only C++ wrapper for the Index Store C API.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_INDEXSTORE_INDEXSTORECXX_H
#define LLVM_CLANG_INDEXSTORE_INDEXSTORECXX_H

#include "indexstore/indexstore.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/TimeValue.h"

namespace indexstore {
  using llvm::ArrayRef;
  using llvm::Optional;
  using llvm::StringRef;

static inline StringRef stringFromIndexStoreStringRef(indexstore_string_ref_t str) {
  return StringRef(str.data, str.length);
}

class IndexRecordSymbol {
  indexstore_symbol_t obj;
  friend class IndexRecordReader;

public:
  IndexRecordSymbol(indexstore_symbol_t obj) : obj(obj) {}

  indexstore_symbol_language_t getLanguage() {
    return indexstore_symbol_get_language(obj);
  }
  indexstore_symbol_kind_t getKind() { return indexstore_symbol_get_kind(obj); }
  uint64_t getSubKinds() {
    return indexstore_symbol_get_sub_kinds(obj);
  }
  uint64_t getRoles() { return indexstore_symbol_get_roles(obj); }
  uint64_t getRelatedRoles() { return indexstore_symbol_get_related_roles(obj); }
  StringRef getName() { return stringFromIndexStoreStringRef(indexstore_symbol_get_name(obj)); }
  StringRef getUSR() { return stringFromIndexStoreStringRef(indexstore_symbol_get_usr(obj)); }
  StringRef getCodegenName() { return stringFromIndexStoreStringRef(indexstore_symbol_get_codegen_name(obj)); }
};

class IndexSymbolRelation {
  indexstore_symbol_relation_t obj;

public:
  IndexSymbolRelation(indexstore_symbol_relation_t obj) : obj(obj) {}

  uint64_t getRoles() { return indexstore_symbol_relation_get_roles(obj); }
  IndexRecordSymbol getSymbol() { return indexstore_symbol_relation_get_symbol(obj); }
};

class IndexRecordOccurrence {
  indexstore_occurrence_t obj;

public:
  IndexRecordOccurrence(indexstore_occurrence_t obj) : obj(obj) {}

  IndexRecordSymbol getSymbol() { return indexstore_occurrence_get_symbol(obj); }
  uint64_t getRoles() { return indexstore_occurrence_get_roles(obj); }

  bool foreachRelation(llvm::function_ref<bool(IndexSymbolRelation)> receiver) {
#if INDEXSTORE_HAS_BLOCKS
    return indexstore_occurrence_relations_apply(obj, ^bool(indexstore_symbol_relation_t sym_rel) {
      return receiver(sym_rel);
    });
#else
    return false;
#endif
  }

  std::pair<unsigned, unsigned> getLineCol() {
    unsigned line, col;
    indexstore_occurrence_get_line_col(obj, &line, &col);
    return std::make_pair(line, col);
  }
};

class IndexStore;
typedef std::shared_ptr<IndexStore> IndexStoreRef;

class IndexStore {
  indexstore_t obj;
  friend class IndexRecordReader;
  friend class IndexUnitReader;

public:
  IndexStore(StringRef path, std::string &error) {
    llvm::SmallString<64> buf = path;
    indexstore_error_t c_err = nullptr;
    obj = indexstore_store_create(buf.c_str(), &c_err);
    if (c_err) {
      error = indexstore_error_get_description(c_err);
      indexstore_error_dispose(c_err);
    }
  }

  IndexStore(IndexStore &&other) : obj(other.obj) {
    other.obj = nullptr;
  }

  ~IndexStore() {
    indexstore_store_dispose(obj);
  }

  static IndexStoreRef create(StringRef path, std::string &error) {
    auto storeRef = std::make_shared<IndexStore>(path, error);
    if (storeRef->isInvalid())
      return nullptr;
    return storeRef;
  }

  bool isValid() const { return obj; }
  bool isInvalid() const { return !isValid(); }
  explicit operator bool() const { return isValid(); }

  bool foreachUnit(bool sorted, llvm::function_ref<bool(StringRef unitName)> receiver) {
#if INDEXSTORE_HAS_BLOCKS
    return indexstore_store_units_apply(obj, sorted, ^bool(indexstore_string_ref_t unit_name) {
      return receiver(stringFromIndexStoreStringRef(unit_name));
    });
#else
    return false;
#endif
  }

  enum class UnitEventKind {
    Added,
    Removed,
    Modified,
    DirectoryDeleted,
  };
  struct UnitEvent {
    UnitEventKind Kind;
    StringRef UnitName;
  };
  typedef std::function<void(ArrayRef<UnitEvent> Events)> UnitEventHandler;

  bool setUnitEventHandler(UnitEventHandler handler, std::string &error) {
#if INDEXSTORE_HAS_BLOCKS
    if (!handler) {
      indexstore_error_t c_err = nullptr;
      bool ret = indexstore_store_set_unit_event_handler(obj, nullptr, &c_err);
      if (c_err) {
        error = indexstore_error_get_description(c_err);
        indexstore_error_dispose(c_err);
      }
      return ret;
    }

    indexstore_error_t c_err = nullptr;
    bool ret = indexstore_store_set_unit_event_handler(obj, ^(indexstore_unit_event_t *events, size_t count) {
      llvm::SmallVector<UnitEvent, 16> unitEvts;
      unitEvts.reserve(count);
      for (size_t i = 0; i != count; ++i) {
        indexstore_unit_event_t c_evt = events[i];
        UnitEventKind K;
        switch (c_evt.kind) {
        case INDEXSTORE_UNIT_EVENT_ADDED: K = UnitEventKind::Added; break;
        case INDEXSTORE_UNIT_EVENT_REMOVED: K = UnitEventKind::Removed; break;
        case INDEXSTORE_UNIT_EVENT_MODIFIED: K = UnitEventKind::Modified; break;
        case INDEXSTORE_UNIT_EVENT_DIRECTORY_DELETED: K = UnitEventKind::DirectoryDeleted; break;
        }
        unitEvts.push_back(UnitEvent{K, stringFromIndexStoreStringRef(c_evt.unit_name)});
      }
      handler(unitEvts);
    }, &c_err);

    if (c_err) {
      error = indexstore_error_get_description(c_err);
      indexstore_error_dispose(c_err);
    }
    return ret;
#else
    error = "unit event handler requires blocks";
    return true;
#endif
  }

  void discardUnit(StringRef UnitName) {
    llvm::SmallString<64> buf = UnitName;
    indexstore_store_discard_unit(obj, buf.c_str());
  }

  void getUnitNameFromOutputPath(StringRef outputPath, llvm::SmallVectorImpl<char> &nameBuf) {
    llvm::SmallString<256> buf = outputPath;
    size_t nameLen = indexstore_store_get_unit_name_from_output_path(obj, buf.c_str(), nameBuf.data(), nameBuf.size());
    if (nameLen+1 > nameBuf.size()) {
      nameBuf.resize(nameLen+1);
      indexstore_store_get_unit_name_from_output_path(obj, buf.c_str(), nameBuf.data(), nameBuf.size());
    }
  }

  llvm::Optional<llvm::sys::TimeValue> getUnitModificationTime(StringRef unitName, std::string &error) {
    llvm::SmallString<64> buf = unitName;
    int64_t seconds, nanoseconds;
    indexstore_error_t c_err = nullptr;
    bool err = indexstore_store_get_unit_modification_time(obj, buf.c_str(),
      &seconds, &nanoseconds, &c_err);
    if (err && c_err) {
      error = indexstore_error_get_description(c_err);
      indexstore_error_dispose(c_err);
      return llvm::None;
    }
    return llvm::sys::TimeValue(seconds, nanoseconds);
  }

  void purgeStaleRecords(ArrayRef<StringRef> activeRecords) {
    llvm::SmallVector<indexstore_string_ref_t, 16> names;
    names.reserve(activeRecords.size());
    for (StringRef rec : activeRecords) {
      names.push_back(indexstore_string_ref_t{rec.data(), rec.size()});
    }
    indexstore_store_purge_stale_records(obj, names.data(), names.size());
  }
};

class IndexRecordReader {
  indexstore_record_reader_t obj;

public:
  IndexRecordReader(IndexStore &store, StringRef recordName, std::string &error) {
    llvm::SmallString<64> buf = recordName;
    indexstore_error_t c_err = nullptr;
    obj = indexstore_record_reader_create(store.obj, buf.c_str(), &c_err);
    if (c_err) {
      error = indexstore_error_get_description(c_err);
      indexstore_error_dispose(c_err);
    }
  }

  IndexRecordReader(IndexRecordReader &&other) : obj(other.obj) {
    other.obj = nullptr;
  }

  ~IndexRecordReader() {
    indexstore_record_reader_dispose(obj);
  }

  bool isValid() const { return obj; }
  bool isInvalid() const { return !isValid(); }
  explicit operator bool() const { return isValid(); }

  /// Goes through and passes record decls, after filtering using a \c Checker
  /// function.
  ///
  /// Resulting decls can be used as filter for \c foreachOccurrence. This
  /// allows allocating memory only for the record decls that the caller is
  /// interested in.
  bool searchSymbols(llvm::function_ref<bool(IndexRecordSymbol, bool &stop)> filter,
                     llvm::function_ref<void(IndexRecordSymbol)> receiver) {
#if INDEXSTORE_HAS_BLOCKS
    return indexstore_record_reader_search_symbols(obj, ^bool(indexstore_symbol_t symbol, bool *stop) {
      return filter(symbol, *stop);
    }, ^(indexstore_symbol_t symbol) {
      receiver(symbol);
    });
#else
    return false;
#endif
  }

  bool foreachSymbol(bool noCache, llvm::function_ref<bool(IndexRecordSymbol)> receiver) {
#if INDEXSTORE_HAS_BLOCKS
    return indexstore_record_reader_symbols_apply(obj, noCache, ^bool(indexstore_symbol_t sym) {
      return receiver(sym);
    });
#else
    return false;
#endif
  }

  /// \param DeclsFilter if non-empty indicates the list of decls that we want
  /// to get occurrences for. An empty array indicates that we want occurrences
  /// for all decls.
  /// \param RelatedDeclsFilter Same as \c DeclsFilter but for related decls.
  bool foreachOccurrence(ArrayRef<IndexRecordSymbol> symbolsFilter,
                         ArrayRef<IndexRecordSymbol> relatedSymbolsFilter,
              llvm::function_ref<bool(IndexRecordOccurrence)> receiver) {
#if INDEXSTORE_HAS_BLOCKS
    llvm::SmallVector<indexstore_symbol_t, 16> c_symbolsFilter;
    c_symbolsFilter.reserve(symbolsFilter.size());
    for (IndexRecordSymbol sym : symbolsFilter) {
      c_symbolsFilter.push_back(sym.obj);
    }
    llvm::SmallVector<indexstore_symbol_t, 16> c_relatedSymbolsFilter;
    c_relatedSymbolsFilter.reserve(relatedSymbolsFilter.size());
    for (IndexRecordSymbol sym : relatedSymbolsFilter) {
      c_relatedSymbolsFilter.push_back(sym.obj);
    }
    return indexstore_record_reader_occurrences_of_symbols_apply(obj,
                                c_symbolsFilter.data(), c_symbolsFilter.size(),
                                c_relatedSymbolsFilter.data(),
                                c_relatedSymbolsFilter.size(),
                                ^bool(indexstore_occurrence_t occur) {
                                  return receiver(occur);
                                });
#else
    return false;
#endif
  }

  bool foreachOccurrence(
              llvm::function_ref<bool(IndexRecordOccurrence)> receiver) {
#if INDEXSTORE_HAS_BLOCKS
    return indexstore_record_reader_occurrences_apply(obj, ^bool(indexstore_occurrence_t occur) {
      return receiver(occur);
    });
#else
    return false;
#endif
  }
};

class IndexUnitDependency {
  indexstore_unit_dependency_t obj;
  friend class IndexUnitReader;

public:
  IndexUnitDependency(indexstore_unit_dependency_t obj) : obj(obj) {}

  enum class DependencyKind {
    Unit,
    Record,
    File,
  };
  DependencyKind getKind() {
    switch (indexstore_unit_dependency_get_kind(obj)) {
    case INDEXSTORE_UNIT_DEPENDENCY_UNIT: return DependencyKind::Unit;
    case INDEXSTORE_UNIT_DEPENDENCY_RECORD: return DependencyKind::Record;
    case INDEXSTORE_UNIT_DEPENDENCY_FILE: return DependencyKind::File;
    }
  }
  bool isSystem() { return indexstore_unit_dependency_is_system(obj); }
  StringRef getName() { return stringFromIndexStoreStringRef(indexstore_unit_dependency_get_name(obj)); }
  StringRef getFilePath() { return stringFromIndexStoreStringRef(indexstore_unit_dependency_get_filepath(obj)); }
};

class IndexUnitReader {
  indexstore_unit_reader_t obj;

public:
  IndexUnitReader(IndexStore &store, StringRef unitName, std::string &error) {
    llvm::SmallString<64> buf = unitName;
    indexstore_error_t c_err = nullptr;
    obj = indexstore_unit_reader_create(store.obj, buf.c_str(), &c_err);
    if (c_err) {
      error = indexstore_error_get_description(c_err);
      indexstore_error_dispose(c_err);
    }
  }

  IndexUnitReader(IndexUnitReader &&other) : obj(other.obj) {
    other.obj = nullptr;
  }

  ~IndexUnitReader() {
    indexstore_unit_reader_dispose(obj);
  }

  bool isValid() const { return obj; }
  bool isInvalid() const { return !isValid(); }
  explicit operator bool() const { return isValid(); }

  StringRef getProviderIdentifier() {
    return stringFromIndexStoreStringRef(indexstore_unit_reader_get_provider_identifier(obj));
  }
  StringRef getProviderVersion() {
    return stringFromIndexStoreStringRef(indexstore_unit_reader_get_provider_version(obj));
  }

  llvm::sys::TimeValue getModificationTime() {
    int64_t seconds, nanoseconds;
    indexstore_unit_reader_get_modification_time(obj, &seconds, &nanoseconds);
    return llvm::sys::TimeValue(seconds, nanoseconds);
  }

  bool isSystemUnit() { return indexstore_unit_reader_is_system_unit(obj); }
  bool hasMainFile() { return indexstore_unit_reader_has_main_file(obj); }

  StringRef getMainFilePath() {
    return stringFromIndexStoreStringRef(indexstore_unit_reader_get_main_file(obj));
  }
  StringRef getWorkingDirectory() {
    return stringFromIndexStoreStringRef(indexstore_unit_reader_get_working_dir(obj));
  }
  StringRef getOutputFile() {
    return stringFromIndexStoreStringRef(indexstore_unit_reader_get_output_file(obj));
  }
  StringRef getSysrootPath() {
    return stringFromIndexStoreStringRef(indexstore_unit_reader_get_sysroot_path(obj));
  }
  StringRef getTarget() {
    return stringFromIndexStoreStringRef(indexstore_unit_reader_get_target(obj));
  }

  bool foreachDependency(llvm::function_ref<bool(IndexUnitDependency)> receiver) {
#if INDEXSTORE_HAS_BLOCKS
    return indexstore_unit_reader_dependencies_apply(obj, ^bool(indexstore_unit_dependency_t dep) {
      return receiver(dep);
    });
#else
    return false;
#endif
  }
};

} // namespace indexstore

#endif
