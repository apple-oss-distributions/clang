//===--- IndexDataStore.h - Index data store info -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_INDEX_INDEXDATASTORE_H
#define LLVM_CLANG_INDEX_INDEXDATASTORE_H

#include "clang/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/STLExtras.h"
#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace clang {
namespace index {

class IndexDataStore {
public:
  ~IndexDataStore();

  static std::unique_ptr<IndexDataStore>
    create(StringRef IndexStorePath, std::string &Error);

  StringRef getFilePath() const;
  bool foreachUnitName(bool sorted,
                       llvm::function_ref<bool(StringRef unitName)> receiver);

  static unsigned getFormatVersion();

  enum class UnitEventKind {
    Added,
    Removed,
    Modified,
    /// The directory got deleted. No more events will follow.
    DirectoryDeleted,
  };
  struct UnitEvent {
    UnitEventKind Kind;
    StringRef UnitName;
  };
  typedef std::function<void(ArrayRef<UnitEvent> Events)> UnitEventHandler;

  /// \returns true if an error occurred.
  bool setUnitEventHandler(UnitEventHandler Handler, std::string &Error);

  void discardUnit(StringRef UnitName);

  void purgeStaleRecords(ArrayRef<StringRef> ActiveRecords);

private:
  IndexDataStore(void *Impl) : Impl(Impl) {}

  void *Impl; // An IndexDataStoreImpl.
};

} // namespace index
} // namespace clang

#endif
