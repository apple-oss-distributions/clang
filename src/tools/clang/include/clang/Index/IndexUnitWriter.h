//===--- IndexUnitWriter.h - Index unit serialization ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_INDEX_INDEXUNITWRITER_H
#define LLVM_CLANG_INDEX_INDEXUNITWRITER_H

#include "clang/Basic/LLVM.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallString.h"
#include <string>
#include <vector>

namespace llvm {
  class BitstreamWriter;
}

namespace clang {
  class FileEntry;

namespace index {

class IndexUnitWriter {
  SmallString<64> UnitsPath;
  std::string ProviderIdentifier;
  std::string ProviderVersion;
  std::string OutputFile;
  const FileEntry *MainFile;
  bool IsSystemUnit;
  std::string TargetTriple;
  std::string WorkDir;
  std::string SysrootPath;
  struct FileEntryData {
    const FileEntry *File;
    bool IsSystem;
  };
  std::vector<FileEntryData> Files;
  llvm::DenseMap<const FileEntry *, int> IndexByFile;
  llvm::DenseSet<const FileEntry *> SeenASTFiles;
  struct RecordOrUnitData {
    std::string Name;
    int FileIndex;
    bool IsSystem;
  };
  std::vector<RecordOrUnitData> Records;
  std::vector<RecordOrUnitData> ASTFileUnits;

public:
  /// \param MainFile the main file for a compiled source file. This should be
  /// null for PCH and module units.
  /// \param IsSystem true for system module units, false otherwise.
  IndexUnitWriter(StringRef StorePath,
                  StringRef ProviderIdentifier, StringRef ProviderVersion,
                  StringRef OutputFile,
                  const FileEntry *MainFile, bool IsSystem,
                  StringRef TargetTriple,
                  StringRef SysrootPath);
  ~IndexUnitWriter();

  int addFileDependency(const FileEntry *File, bool IsSystem);
  void addRecordFile(StringRef RecordFile, const FileEntry *File, bool IsSystem);
  void addASTFileDependency(const FileEntry *File, bool IsSystem);
  void addUnitDependency(StringRef UnitFile, const FileEntry *File, bool IsSystem);

  bool write(std::string &Error);

  static void getUnitNameForOutputFile(StringRef FilePath,
                                       SmallVectorImpl<char> &Str);
  static bool initIndexDirectory(StringRef StorePath, std::string &Error);

private:
  class PathStorage;
  void writeUnitInfo(llvm::BitstreamWriter &Stream, PathStorage &PathStore);
  void writeDependencies(llvm::BitstreamWriter &Stream, PathStorage &PathStore);
  void writePaths(llvm::BitstreamWriter &Stream, PathStorage &PathStore);
};

} // end namespace index
} // end namespace clang

#endif
