//===--- IndexUnitReader.h - Index unit deserialization -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_INDEX_INDEXUNITREADER_H
#define LLVM_CLANG_INDEX_INDEXUNITREADER_H

#include "clang/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/STLExtras.h"

namespace llvm {
namespace sys {
  class TimeValue;
}
}

namespace clang {
namespace index {

class IndexUnitReader {
public:
  enum class DependencyKind {
    Unit,
    Record,
    File,
  };

  ~IndexUnitReader();

  static std::unique_ptr<IndexUnitReader>
    createWithUnitFilename(StringRef UnitFilename, StringRef StorePath,
                           std::string &Error);
  static std::unique_ptr<IndexUnitReader>
    createWithFilePath(StringRef FilePath, std::string &Error);

  static Optional<llvm::sys::TimeValue>
    getModificationTimeForUnit(StringRef UnitFilename, StringRef StorePath,
                               std::string &Error);

  StringRef getProviderIdentifier() const;
  StringRef getProviderVersion() const;

  llvm::sys::TimeValue getModificationTime() const;
  StringRef getWorkingDirectory() const;
  StringRef getOutputFile() const;
  StringRef getSysrootPath() const;
  StringRef getMainFilePath() const;
  StringRef getTarget() const;
  bool hasMainFile() const;
  bool isSystemUnit() const;

  struct DependencyInfo {
    DependencyKind Kind;
    bool IsSystem;
    StringRef UnitOrRecordName;
    StringRef FilePath;
  };
  /// Unit dependencies are provided ahead of record ones, record ones
  /// ahead of the file ones.
  bool foreachDependency(llvm::function_ref<bool(const DependencyInfo &Info)> Receiver);

private:
  IndexUnitReader(void *Impl) : Impl(Impl) {}

  void *Impl; // An IndexUnitReaderImpl.
};

} // namespace index
} // namespace clang

#endif
