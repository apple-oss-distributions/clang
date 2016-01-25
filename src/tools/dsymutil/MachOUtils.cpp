//===-- MachOUtils.cpp - Utility class for accessing binaries -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This program is a utility that aims to be a dropin replacement for
// Darwin's dsymutil.
//
//===----------------------------------------------------------------------===//

#include "MachOUtils.h"
#include "llvm/Object/MachO.h"
#include "llvm/Object/MachOUniversal.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FileUtilities.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/Signals.h"

#define DEBUG_TYPE "dsymutil-macho-utils"

namespace llvm {
namespace dsymutil {

const char *MachOUtils::LipoExec = "lipo";

bool
MachOUtils::mergeThinFiles(StringRef Path,
                           ArrayRef<ArchAndFilename> ThinDwarfList,
                           StringRef FinalLinkedOut) {
  // No need to merge one file into a universal fat binary. First, try to move
  // it (rename) to the final location. If that fails because of cross-link
  // issues then copy and delete.
  if (ThinDwarfList.size() == 1) {
    StringRef From(ThinDwarfList[0].Path);
    // Nothing to do for stdout.
    if (From == "-")
      return true;
    if (sys::fs::rename(Twine(From), Twine(FinalLinkedOut))) {
      if (std::error_code EC =
          sys::fs::copy_file(Twine(From), Twine(FinalLinkedOut))) {
        errs() << "error: while copying " << From << " to " << FinalLinkedOut
               << ": " << EC.message() << "\n";
        return false;
      }
      sys::fs::remove(Twine(From));
    }
    return true;
  }

  SmallVector<const char *, 8> Args;
  Args.push_back(LipoExec);
  Args.push_back("-create");

  for (auto &Thin : ThinDwarfList)
    Args.push_back(Thin.Path.c_str());

  // Align segments to match dsymutil-classic alignment
  for (auto &Thin : ThinDwarfList) {
    Args.push_back("-segalign");
    Args.push_back(Thin.Arch.c_str());
    Args.push_back("20");
  }

  Args.push_back("-output");
  Args.push_back(FinalLinkedOut.data());
  Args.push_back(nullptr);

  // Run: lipo -create <input-slices> -output <fat>
  return runLipo(Path, Args);
}

bool MachOUtils::runLipo(StringRef ExecPath,
                         SmallVectorImpl<const char *> &Args) {
  auto Path = sys::findProgramByName(LipoExec,
                                     ArrayRef<StringRef>(ExecPath));
  if (!Path)
    Path = sys::findProgramByName(LipoExec);;

  if (!Path) {
    errs() << "Cannot find '" << LipoExec << "' in '"
           << ExecPath << "': " << Path.getError().message() << "\n";
    return false;
  }

  DEBUG(errs() << "\nAbout to run:\t" << *Path;
        for (unsigned i = 1, e = Args.size()-1; i != e; ++i)
          errs() << " " << Args[i];
        errs() << "\n";
        );

  std::string ErrMsg;
  int result = sys::ExecuteAndWait(*Path, Args.data(), nullptr, nullptr,
                            0, 0, &ErrMsg);

  DEBUG(
    if (result == 0)
      outs() << "Success!\n";
    else if (result > 0)
      outs() << "Exited with error code '" << result << "'\n";
    else if (result < 0) {
      if (result == -1)
        outs() << "Execute failed: " << ErrMsg << "\n";
      else
        outs() << "Crashed: " << ErrMsg << "\n";
    }
    if (result & 0x01000000)
      outs() << "Dumped core\n";
    );

  if (result) {
    errs() << "Error: " << ErrMsg << "\n";
    return false;
  }

  return true;
}

}
}
