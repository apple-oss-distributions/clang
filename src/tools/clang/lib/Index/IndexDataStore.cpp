//===--- IndexDataStore.cpp - Index data store info -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "clang/Index/IndexDataStore.h"
#include "IndexDataStoreUtils.h"
#include "clang/Basic/DirectoryWatcher.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Mutex.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::index;
using namespace clang::index::store;
using namespace llvm;

static void makeSubDir(StringRef subdir, SmallVectorImpl<char> &StorePathBuf) {
  SmallString<10> VersionPath;
  raw_svector_ostream(VersionPath) << 'v' << STORE_FORMAT_VERSION;

  sys::path::append(StorePathBuf, VersionPath);
  sys::path::append(StorePathBuf, subdir);
}

void store::makeUnitSubDir(SmallVectorImpl<char> &StorePathBuf) {
  return makeSubDir("units", StorePathBuf);
}

void store::makeRecordSubDir(SmallVectorImpl<char> &StorePathBuf) {
  return makeSubDir("records", StorePathBuf);
}

//===----------------------------------------------------------------------===//
// IndexDataStore
//===----------------------------------------------------------------------===//

namespace {

class UnitEventHandlerData {
  mutable sys::Mutex Mtx;
  IndexDataStore::UnitEventHandler Handler;

public:
  void setHandler(IndexDataStore::UnitEventHandler handler) {
    sys::ScopedLock L(Mtx);
    Handler = std::move(handler);
  }
  IndexDataStore::UnitEventHandler getHandler() const {
    sys::ScopedLock L(Mtx);
    return Handler;
  }
};

class IndexDataStoreImpl {
  std::string FilePath;
  std::shared_ptr<UnitEventHandlerData> TheUnitEventHandlerData;
  std::unique_ptr<DirectoryWatcher> DirWatcher;

public:
  explicit IndexDataStoreImpl(StringRef indexStorePath)
    : FilePath(indexStorePath) {
    TheUnitEventHandlerData = std::make_shared<UnitEventHandlerData>();
  }

  StringRef getFilePath() const { return FilePath; }
  bool foreachUnitName(bool sorted,
                       llvm::function_ref<bool(StringRef unitName)> receiver);
  bool setUnitEventHandler(IndexDataStore::UnitEventHandler Handler,
                           std::string &Error);
  void discardUnit(StringRef UnitName);
  void purgeStaleRecords(ArrayRef<StringRef> ActiveRecords);
};

} // anonymous namespace

bool IndexDataStoreImpl::foreachUnitName(bool sorted,
                        llvm::function_ref<bool(StringRef unitName)> receiver) {
  SmallString<128> UnitPath;
  UnitPath = FilePath;
  makeUnitSubDir(UnitPath);

  std::vector<std::string> filenames;

  std::error_code EC;
  for (auto It = sys::fs::directory_iterator(UnitPath, EC),
           End = sys::fs::directory_iterator();
       !EC && It != End; It.increment(EC)) {
    StringRef unitName = sys::path::filename(It->path());
    if (!sorted) {
      if (!receiver(unitName))
        return false;
    } else {
      filenames.push_back(unitName);
    }
  }

  if (sorted) {
    llvm::array_pod_sort(filenames.begin(), filenames.end());
    for (auto &fname : filenames)
      if (!receiver(fname))
        return false;
  }
  return true;
}

bool IndexDataStoreImpl::setUnitEventHandler(IndexDataStore::UnitEventHandler handler,
                                             std::string &Error) {
  bool nullifying = handler == nullptr;
  TheUnitEventHandlerData->setHandler(std::move(handler));
  if (nullifying) {
    DirWatcher.reset();
    return false;
  }

  if (!DirWatcher) {
    SmallString<128> UnitPath;
    UnitPath = FilePath;
    makeUnitSubDir(UnitPath);

    auto localUnitEventHandlerData = TheUnitEventHandlerData;
    auto OnUnitsChange = [localUnitEventHandlerData](ArrayRef<DirectoryWatcher::Event> Events) {
      SmallVector<IndexDataStore::UnitEvent, 16> UnitEvents;
      UnitEvents.reserve(Events.size());
      for (const DirectoryWatcher::Event &evt : Events) {
        IndexDataStore::UnitEventKind K;
        StringRef UnitName = sys::path::filename(evt.Filename);
        switch (evt.Kind) {
        case DirectoryWatcher::EventKind::Added:
          K = IndexDataStore::UnitEventKind::Added; break;
        case DirectoryWatcher::EventKind::Removed:
          K = IndexDataStore::UnitEventKind::Removed; break;
        case DirectoryWatcher::EventKind::Modified:
          K = IndexDataStore::UnitEventKind::Modified; break;
        case DirectoryWatcher::EventKind::DirectoryDeleted:
          K = IndexDataStore::UnitEventKind::DirectoryDeleted;
          UnitName = StringRef();
          break;
        }
        UnitEvents.push_back(IndexDataStore::UnitEvent{K, UnitName});
      }

      if (auto handler = localUnitEventHandlerData->getHandler()) {
        handler(UnitEvents);
      }
    };

    DirWatcher = DirectoryWatcher::create(UnitPath.str(), OnUnitsChange,
                                        /*TrackFileModifications=*/true, Error);
    if (!DirWatcher)
      return true;
  }

  return false;
}

void IndexDataStoreImpl::discardUnit(StringRef UnitName) {
  SmallString<128> UnitPath;
  UnitPath = FilePath;
  makeUnitSubDir(UnitPath);
  sys::path::append(UnitPath, UnitName);
  sys::fs::remove(UnitPath);
}

void IndexDataStoreImpl::purgeStaleRecords(ArrayRef<StringRef> ActiveRecords) {
  using namespace llvm::sys;

  SmallString<128> RecordsPath;
  RecordsPath = FilePath;
  makeRecordSubDir(RecordsPath);

  struct DirEntry {
    fs::directory_entry File;
    bool IsActive = false;
    DirEntry() = default;
    DirEntry(fs::directory_entry File) : File(std::move(File)) {}
  };
  StringMap<DirEntry> RecordEntries;

  std::error_code EC;
  for (auto It = fs::directory_iterator(RecordsPath, EC),
            End = fs::directory_iterator();
         !EC && It != End; It.increment(EC)) {
    RecordEntries[path::filename(It->path())] = *It;
  }

  for (auto Rec : ActiveRecords) {
    auto It = RecordEntries.find(Rec);
    if (It != RecordEntries.end())
      It->second.IsActive = true;
  }

  TimeValue CurTime = TimeValue::now();

  for (auto &Pair : RecordEntries) {
    DirEntry &Entry = Pair.second;
    if (Entry.IsActive)
      continue;

    // There's a window where a new record file has showed up but it hasn't been
    // added as a clang symbol record yet; to be safe avoid deleting recent
    // files.
    fs::file_status Stat;
    EC = Entry.File.status(Stat);
    if (EC)
      continue;
    TimeValue Diff = CurTime - Stat.getLastModificationTime();
    if (Diff.seconds() < 10)
      continue;

    fs::remove(Entry.File.path());
  }
}


std::unique_ptr<IndexDataStore>
IndexDataStore::create(StringRef IndexStorePath, std::string &Error) {
  if (!sys::fs::exists(IndexStorePath)) {
    raw_string_ostream OS(Error);
    OS << "index store path does not exist: " << IndexStorePath;
    return nullptr;
  }

  return std::unique_ptr<IndexDataStore>(
    new IndexDataStore(new IndexDataStoreImpl(IndexStorePath)));
}

#define IMPL static_cast<IndexDataStoreImpl*>(Impl)

IndexDataStore::~IndexDataStore() {
  delete IMPL;
}

StringRef IndexDataStore::getFilePath() const {
  return IMPL->getFilePath();
}

bool IndexDataStore::foreachUnitName(bool sorted,
                     llvm::function_ref<bool(StringRef unitName)> receiver) {
  return IMPL->foreachUnitName(sorted, std::move(receiver));
}

unsigned IndexDataStore::getFormatVersion() {
  return STORE_FORMAT_VERSION;
}

bool IndexDataStore::setUnitEventHandler(UnitEventHandler Handler,
                                         std::string &Error) {
  return IMPL->setUnitEventHandler(std::move(Handler), Error);
}

void IndexDataStore::discardUnit(StringRef UnitName) {
  IMPL->discardUnit(UnitName);
}

void IndexDataStore::purgeStaleRecords(ArrayRef<StringRef> ActiveRecords) {
  IMPL->purgeStaleRecords(ActiveRecords);
}
