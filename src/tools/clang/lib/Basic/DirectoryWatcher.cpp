//===- DirectoryWatcher.cpp - Listens for directory file changes ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
/// \file
/// \brief Utility class for listening for file system changes in a directory.
//===----------------------------------------------------------------------===//

#include "clang/Basic/DirectoryWatcher.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/FileSystem.h"

#define HAVE_DISPATCH 0

#if defined(__has_include)
#if __has_include(<dispatch/dispatch.h>)

#include <dispatch/dispatch.h>
#undef HAVE_DISPATCH
#define HAVE_DISPATCH 1

#endif
#endif

using namespace clang;
using namespace llvm;

namespace {

struct DirData {
  bool TrackFileModifications;
  std::string Path;
  DirectoryWatcher::EventReceiver Receiver;
  bool InvokedOnce = false;

  struct FileInfo {
    bool WasRemoved = false;
    sys::fs::UniqueID FID;
  };
  llvm::StringMap<FileInfo> Filenames;

  void onDirChange() {
    using namespace llvm::sys;

    // Mark all filenames as removed.
    for (auto &Entry : Filenames) {
      Entry.getValue().WasRemoved = true;
    }

    SmallVector<DirectoryWatcher::Event, 8> Events;
  
    // FIXME: Use the file descriptor to scan the directory.
    std::error_code EC;
    for (auto It = fs::directory_iterator(Path, EC), End = fs::directory_iterator();
           !EC && It != End; It.increment(EC)) {
      auto Ret = Filenames.insert(std::make_pair(StringRef(It->path()), FileInfo()));
      bool IsNew = Ret.second;
      if (IsNew) {
        DirectoryWatcher::Event Event{DirectoryWatcher::EventKind::Added, It->path()};
        Events.push_back(std::move(Event));
      } else {
        Ret.first->getValue().WasRemoved = false;
      }

      if (TrackFileModifications) {
        sys::fs::UniqueID CurFID = getFID(Ret.first->first());
        if (!IsNew && Ret.first->getValue().FID != CurFID) {
          DirectoryWatcher::Event Event{DirectoryWatcher::EventKind::Modified, It->path()};
          Events.push_back(std::move(Event));
        }
        Ret.first->getValue().FID = CurFID;
      }
    }

    for (auto FIt = Filenames.begin(), FE = Filenames.end(); FIt != FE;) {
      StringRef Filename = FIt->getKey();
      bool Removed = FIt->getValue().WasRemoved;
      auto CurIt = FIt;
      ++FIt;
      if (Removed) {
        DirectoryWatcher::Event Event{DirectoryWatcher::EventKind::Removed, Filename};
        Events.push_back(std::move(Event));
        Filenames.erase(CurIt);
      }
    }

    if (!Events.empty() || !InvokedOnce) {
      Receiver(Events);
      InvokedOnce = true;
    }
  }

  void onDirDeleted() {
    DirectoryWatcher::Event DeletedEvt{DirectoryWatcher::EventKind::DirectoryDeleted, Path};
    Receiver(DeletedEvt);
  }

private:
  static sys::fs::UniqueID getFID(StringRef Filename) {
    sys::fs::UniqueID FID(0,0);
    sys::fs::getUniqueID(Filename, FID);
    return FID;
  }
};

}

struct DirectoryWatcher::Implementation {
#if HAVE_DISPATCH
  dispatch_queue_t queue;
  dispatch_source_t DirSource;

  ~Implementation() {
    dispatch_source_cancel(DirSource);
    dispatch_release(DirSource);
    dispatch_release(queue);
  }
#endif
};

DirectoryWatcher::DirectoryWatcher()
  : Impl(*new Implementation()) {}

DirectoryWatcher::~DirectoryWatcher() {
  delete &Impl;
}

std::unique_ptr<DirectoryWatcher> DirectoryWatcher::create(StringRef Path,
        EventReceiver Receiver,  bool TrackFileModifications,
        std::string &Error) {
#if HAVE_DISPATCH

  using namespace llvm::sys;

  if (!fs::exists(Path)) {
    std::error_code EC = fs::create_directories(Path);
    if (EC) {
      Error = EC.message();
      return nullptr;
    }
  }

  bool IsDir;
  std::error_code EC = fs::is_directory(Path, IsDir);
  if (EC) {
    Error = EC.message();
    return nullptr;
  }
  if (!IsDir) {
    Error = "path is not a directory: ";
    Error += Path;
    return nullptr;
  }

  int FD;
  EC = fs::openFileForRead(Path, FD);
  if (EC) {
    Error = EC.message();
    return nullptr;
  }

  std::unique_ptr<DirectoryWatcher> DirWatch;
  DirWatch.reset(new DirectoryWatcher());
  auto &Impl = DirWatch->Impl;
  auto Data = std::make_shared<DirData>();
  Data->TrackFileModifications = TrackFileModifications;
  Data->Path = Path;
  Data->Receiver = std::move(Receiver);
  Impl.queue = dispatch_queue_create("DirectoryWatcher", DISPATCH_QUEUE_SERIAL);

  dispatch_source_t source = dispatch_source_create(DISPATCH_SOURCE_TYPE_VNODE,
      FD, DISPATCH_VNODE_WRITE | DISPATCH_VNODE_DELETE, Impl.queue);
  Impl.DirSource = source;

  dispatch_source_set_cancel_handler(source, ^{
      close(FD);
  });

  dispatch_source_set_event_handler(source, ^{
      dispatch_source_vnode_flags_t flags = dispatch_source_get_data(source);
      if (flags & DISPATCH_VNODE_DELETE) {
        Data->onDirDeleted();
      }
      if (flags & DISPATCH_VNODE_WRITE) {
        Data->onDirChange();
      }
  });

  dispatch_resume(source);

  // Initial reading of the whole directory. It doesn't matter if the source
  // is triggered before we come here, it only means that we've already read
  // the directory and this will be a noop.
  dispatch_async(Impl.queue, ^{
    Data->onDirChange();
  });

  return DirWatch;
#else
  return nullptr;
#endif
}
