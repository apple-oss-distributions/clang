//===--- IndexUnitWriter.cpp - Index unit serialization -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "clang/Index/IndexUnitWriter.h"
#include "IndexDataStoreUtils.h"
#include "clang/Basic/FileManager.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::index;
using namespace clang::index::store;
using namespace llvm;

namespace {
struct BitPathComponent {
  size_t Offset = 0;
  size_t Size = 0;
};

struct DirBitPath {
  UnitFilePathPrefixKind PrefixKind = UNIT_PATH_PREFIX_NONE;
  BitPathComponent Dir;
};

struct FileBitPath {
  DirBitPath Dir;
  BitPathComponent Filename;
};
}

class IndexUnitWriter::PathStorage {
  std::string WorkDir;
  std::string SysrootPath;
  SmallString<512> PathsBuf;
  DenseMap<const DirectoryEntry *, DirBitPath> Dirs;
  DenseMap<const FileEntry *, FileBitPath> Files;

public:
  PathStorage(StringRef workDir, StringRef sysrootPath) {
    WorkDir = workDir;
    if (sysrootPath == "/")
      sysrootPath = StringRef();
    SysrootPath = sysrootPath;
  }

  StringRef getPathsBuffer() const { return PathsBuf.str(); }

  FileBitPath getBitPath(const FileEntry *FE) {
    if (!FE)
      return FileBitPath();
    auto pair = Files.insert(std::make_pair(FE, FileBitPath()));
    bool isNew = pair.second;
    auto &path = pair.first->second;

    if (isNew) {
      path.Dir = getDirBitPath(FE->getDir());
      StringRef filename = sys::path::filename(FE->getName());
      path.Filename.Offset = getPathOffset(filename);
      path.Filename.Size = filename.size();
    }
    return path;
  }

  size_t getPathOffset(StringRef Path) {
    if (Path.empty())
      return 0;
    size_t offset = PathsBuf.size();
    PathsBuf += Path;
    return offset;
  }
  
private:
  DirBitPath getDirBitPath(const DirectoryEntry *DE) {
    auto pair = Dirs.insert(std::make_pair(DE, DirBitPath()));
    bool isNew = pair.second;
    auto &dirPath = pair.first->second;

    if (isNew) {
      StringRef dirStr = DE->getName();
      if (!SysrootPath.empty() && dirStr.startswith(SysrootPath)) {
        dirPath.PrefixKind = UNIT_PATH_PREFIX_SYSROOT;
        dirStr = dirStr.drop_front(SysrootPath.size());
        while (!dirStr.empty() && dirStr[0] == '/')
          dirStr = dirStr.drop_front();
      } else if (!WorkDir.empty() && dirStr.startswith(WorkDir)) {
        dirPath.PrefixKind = UNIT_PATH_PREFIX_WORKDIR;
        dirStr = dirStr.drop_front(WorkDir.size());
        while (!dirStr.empty() && dirStr[0] == '/')
          dirStr = dirStr.drop_front();
      }
      dirPath.Dir.Offset = getPathOffset(dirStr);
      dirPath.Dir.Size = dirStr.size();
    }
    return dirPath;
  }
};

IndexUnitWriter::IndexUnitWriter(StringRef StorePath,
                                 StringRef ProviderIdentifier,
                                 StringRef ProviderVersion,
                                 StringRef OutputFile,
                                 const FileEntry *MainFile, bool IsSystem,
                                 StringRef TargetTriple,
                                 StringRef SysrootPath) {
  this->UnitsPath = StorePath;
  store::makeUnitSubDir(this->UnitsPath);
  this->ProviderIdentifier = ProviderIdentifier;
  this->ProviderVersion = ProviderVersion;
  this->OutputFile = OutputFile;
  this->MainFile = MainFile;
  this->IsSystemUnit = IsSystem;
  this->TargetTriple = TargetTriple;
  this->SysrootPath = SysrootPath;
}

IndexUnitWriter::~IndexUnitWriter() {}

int IndexUnitWriter::addFileDependency(const FileEntry *File, bool IsSystem) {
  assert(File);
  auto Pair = IndexByFile.insert(std::make_pair(File, Files.size()));
  bool WasInserted = Pair.second;
  if (WasInserted) {
    Files.push_back(FileEntryData{File, IsSystem});
  }
  return Pair.first->second;
}

void IndexUnitWriter::addRecordFile(StringRef RecordFile, const FileEntry *File,
                                    bool IsSystem) {
  int Dep = File ? addFileDependency(File, IsSystem) : -1;
  Records.push_back(RecordOrUnitData{RecordFile, Dep, IsSystem});
}

void IndexUnitWriter::addASTFileDependency(const FileEntry *File, bool IsSystem) {
  assert(File);
  if (!SeenASTFiles.insert(File).second)
    return;

  SmallString<64> UnitName;
  getUnitNameForOutputFile(File->getName(), UnitName);
  addUnitDependency(UnitName.str(), File, IsSystem);
}

void IndexUnitWriter::addUnitDependency(StringRef UnitFile,
                                        const FileEntry *File, bool IsSystem) {
  int Dep = File ? addFileDependency(File, IsSystem) : -1;
  ASTFileUnits.emplace_back(RecordOrUnitData{UnitFile, Dep, IsSystem});
}

void IndexUnitWriter::getUnitNameForOutputFile(StringRef FilePath,
                                               SmallVectorImpl<char> &Str) {
  StringRef Fname = sys::path::filename(FilePath);
  Str.append(Fname.begin(), Fname.end());
  Str.push_back('-');
  llvm::hash_code PathHashVal = llvm::hash_value(FilePath);
  llvm::APInt(64, PathHashVal).toString(Str, 36, /*Signed=*/false);
}

static void writeBlockInfo(BitstreamWriter &Stream) {
  RecordData Record;

  Stream.EnterBlockInfoBlock(3);
#define BLOCK(X) emitBlockID(X ## _ID, #X, Stream, Record)
#define RECORD(X) emitRecordID(X, #X, Stream, Record)

  BLOCK(UNIT_VERSION_BLOCK);
  RECORD(UNIT_VERSION);

  BLOCK(UNIT_INFO_BLOCK);
  RECORD(UNIT_INFO);

  BLOCK(UNIT_DEPENDENCIES_BLOCK);
  RECORD(UNIT_DEPENDENCY);

  BLOCK(UNIT_PATHS_BLOCK);
  RECORD(UNIT_PATHS);

#undef RECORD
#undef BLOCK
  Stream.ExitBlock();
}

static void writeVersionInfo(BitstreamWriter &Stream) {
  using namespace llvm::sys;

  Stream.EnterSubblock(UNIT_VERSION_BLOCK_ID, 3);

  BitCodeAbbrev *Abbrev = new BitCodeAbbrev();
  Abbrev->Add(BitCodeAbbrevOp(UNIT_VERSION));
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 6)); // Store format version
  unsigned AbbrevCode = Stream.EmitAbbrev(Abbrev);

  RecordData Record;
  Record.push_back(UNIT_VERSION);
  Record.push_back(STORE_FORMAT_VERSION);
  Stream.EmitRecordWithAbbrev(AbbrevCode, Record);

  Stream.ExitBlock();
}

bool IndexUnitWriter::write(std::string &Error) {
  using namespace llvm::sys;

  SmallString<128> CWDPath;
  std::error_code EC = sys::fs::current_path(CWDPath);
  if (EC) {
    llvm::raw_string_ostream Err(Error);
    Err << "failed to determine current working directory: " << EC.message();
    return true;
  }
  WorkDir = CWDPath.str();

  SmallString<512> Buffer;
  BitstreamWriter Stream(Buffer);
  Stream.Emit('I', 8);
  Stream.Emit('D', 8);
  Stream.Emit('X', 8);
  Stream.Emit('U', 8);

  PathStorage PathStore(WorkDir, SysrootPath);

  writeBlockInfo(Stream);
  writeVersionInfo(Stream);
  writeUnitInfo(Stream, PathStore);
  writeDependencies(Stream, PathStore);
  writePaths(Stream, PathStore);

  SmallString<256> UnitPath = StringRef(UnitsPath);
  UnitPath += '/';
  getUnitNameForOutputFile(OutputFile, UnitPath);

  SmallString<128> TempPath;
  TempPath = path::parent_path(UnitsPath);
  TempPath += '/';
  TempPath += path::filename(UnitPath);
  TempPath += "-%%%%%%%%";
  int TempFD;
  if (llvm::sys::fs::createUniqueFile(TempPath.str(), TempFD, TempPath)) {
    llvm::raw_string_ostream Err(Error);
    Err << "failed to create temporary file: " << TempPath;
    return true;
  }

  raw_fd_ostream OS(TempFD, /*shouldClose=*/true);
  OS.write(Buffer.data(), Buffer.size());
  OS.close();

  EC = fs::rename(/*from=*/TempPath.c_str(), /*to=*/UnitPath.c_str());
  if (EC) {
    llvm::raw_string_ostream Err(Error);
    Err << "failed to rename '" << TempPath << "' to '" << UnitPath << "': " << EC.message();
    return true;
  }

  return false;
}

static void addFilePathToRecord(const FileBitPath &path, RecordData &Record) {
  Record.push_back(path.Dir.PrefixKind);
  Record.push_back(path.Dir.Dir.Offset);
  Record.push_back(path.Dir.Dir.Size);
  Record.push_back(path.Filename.Offset);
  Record.push_back(path.Filename.Size);
}

void IndexUnitWriter::writeUnitInfo(llvm::BitstreamWriter &Stream,
                                    PathStorage &PathStore) {
  Stream.EnterSubblock(UNIT_INFO_BLOCK_ID, 3);

  BitCodeAbbrev *Abbrev = new BitCodeAbbrev();
  Abbrev->Add(BitCodeAbbrevOp(UNIT_INFO));
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Fixed, 1)); // IsSystemUnit
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 10)); // WorkDir offset
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // WorkDir size
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 10)); // OutputFile offset
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // OutputFile size
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 10)); // Sysroot offset
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // Sysroot size

  // For the main file we can use a FileBitPath reference to share path components.
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Fixed, UnitFilePathPrefixKindBitNum)); // Path prefix kind
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 10)); // DirPath offset
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // DirPath size
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 10)); // Filename offset
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 6)); // Filename size

  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 5)); // ProviderIdentifier size
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 5)); // ProviderVersion size
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Blob)); // ProviderIdentifier + ProviderVersion + target triple
  unsigned AbbrevCode = Stream.EmitAbbrev(Abbrev);

  RecordData Record;
  Record.push_back(UNIT_INFO);
  Record.push_back(IsSystemUnit);
  Record.push_back(PathStore.getPathOffset(WorkDir));
  Record.push_back(WorkDir.size());
  Record.push_back(PathStore.getPathOffset(OutputFile));
  Record.push_back(OutputFile.size());
  Record.push_back(PathStore.getPathOffset(SysrootPath));
  Record.push_back(SysrootPath.size());
  addFilePathToRecord(PathStore.getBitPath(MainFile), Record);
  Record.push_back(ProviderIdentifier.size());
  Record.push_back(ProviderVersion.size());
  SmallString<128> InfoStrings;
  InfoStrings += ProviderIdentifier;
  InfoStrings += ProviderVersion;
  InfoStrings += TargetTriple;
  Stream.EmitRecordWithBlob(AbbrevCode, Record, InfoStrings);

  Stream.ExitBlock();
}

void IndexUnitWriter::writeDependencies(llvm::BitstreamWriter &Stream,
                                        PathStorage &PathStore) {
  struct PathData {
    FileBitPath BitPath;
    bool IsSystem;
    bool UsedForRecordOrUnit;
  };
  SmallVector<PathData, 32> PathDataEntries;
  PathDataEntries.reserve(Files.size());
  for (auto &FileData : Files) {
    PathData Entry{PathStore.getBitPath(FileData.File), FileData.IsSystem, false};
    PathDataEntries.push_back(Entry);
  }

  Stream.EnterSubblock(UNIT_DEPENDENCIES_BLOCK_ID, 3);

  BitCodeAbbrev *Abbrev = new BitCodeAbbrev();
  Abbrev->Add(BitCodeAbbrevOp(UNIT_DEPENDENCY));
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Fixed, UnitDependencyKindBitNum)); // Dependency kind
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Fixed, 1)); // IsSystem
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Fixed, UnitFilePathPrefixKindBitNum)); // Path prefix kind
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 10)); // DirPath offset
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // DirPath size
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 10)); // Filename offset
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 6)); // Filename size
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Blob)); // Name
  unsigned AbbrevCode = Stream.EmitAbbrev(Abbrev);

  RecordData Record;

  auto addRecordOrUnitData = [&](UnitDependencyKind K, const RecordOrUnitData &Data) {
    Record.push_back(UNIT_DEPENDENCY);
    Record.push_back(K);
    Record.push_back(Data.IsSystem);
    if (Data.FileIndex != -1) {
      PathData &pathEntry = PathDataEntries[Data.FileIndex];
      addFilePathToRecord(pathEntry.BitPath, Record);
      pathEntry.UsedForRecordOrUnit = true;
    } else {
      addFilePathToRecord(FileBitPath(), Record);
    }
    Stream.EmitRecordWithBlob(AbbrevCode, Record, Data.Name);
  };

  for (auto &ASTData : ASTFileUnits) {
    Record.clear();
    addRecordOrUnitData(UNIT_DEPEND_KIND_UNIT, ASTData);
  }
  for (auto &recordData : Records) {
    Record.clear();
    addRecordOrUnitData(UNIT_DEPEND_KIND_RECORD, recordData);
  }
  for (auto &pathEntry : PathDataEntries) {
    if (pathEntry.UsedForRecordOrUnit)
      continue;
    Record.clear();
    Record.push_back(UNIT_DEPENDENCY);
    Record.push_back(UNIT_DEPEND_KIND_FILE);
    Record.push_back(pathEntry.IsSystem);
    addFilePathToRecord(pathEntry.BitPath, Record);
    Stream.EmitRecordWithBlob(AbbrevCode, Record, StringRef());
  }
  Stream.ExitBlock();
}

void IndexUnitWriter::writePaths(llvm::BitstreamWriter &Stream,
                                 PathStorage &PathStore) {
  Stream.EnterSubblock(UNIT_PATHS_BLOCK_ID, 3);

  BitCodeAbbrev *Abbrev = new BitCodeAbbrev();
  Abbrev->Add(BitCodeAbbrevOp(UNIT_PATHS));
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Blob)); // Paths buffer
  unsigned AbbrevCode = Stream.EmitAbbrev(Abbrev);

  RecordData Record;
  Record.push_back(UNIT_PATHS);
  Stream.EmitRecordWithBlob(AbbrevCode, Record, PathStore.getPathsBuffer());

  Stream.ExitBlock();
}

bool IndexUnitWriter::initIndexDirectory(StringRef StorePath,
                                         std::string &Error) {
  using namespace llvm::sys;
  SmallString<128> SubPath = StorePath;
  store::makeRecordSubDir(SubPath);
  std::error_code EC = fs::create_directories(SubPath);
  if (EC) {
    llvm::raw_string_ostream Err(Error);
    Err << "failed to create directory '" << SubPath << "': " << EC.message();
    return true;
  }

  SubPath = StorePath;
  store::makeUnitSubDir(SubPath);
  EC = fs::create_directory(SubPath);
  if (EC) {
    llvm::raw_string_ostream Err(Error);
    Err << "failed to create directory '" << SubPath << "': " << EC.message();
    return true;
  }

  return false;
}
