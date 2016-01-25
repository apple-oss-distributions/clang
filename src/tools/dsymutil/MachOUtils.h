//===-- MachOUtils.h - Utility class for accessing binaries ------------===//
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

#ifndef LLVM_TOOLS_DSYMUTIL_MACHOUTILS_H
#define LLVM_TOOLS_DSYMUTIL_MACHOUTILS_H

#include <string>
#include <vector>
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"

namespace llvm {
namespace dsymutil {

class MachOUtils {
public:
  struct ArchAndFilename{
    std::string Arch, Path;
  };

  static bool mergeThinFiles(StringRef Path,
                             ArrayRef<ArchAndFilename> ThinDwarfList,
                             StringRef FinalLinkedOut);
private:
  static bool runLipo(StringRef Path,
                      SmallVectorImpl<const char *> &Args);
  static const char *LipoExec;
};

}
}

#endif
