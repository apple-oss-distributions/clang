//===--- ModuleDependencyListener.cpp - Collect module dependencies -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Collect the dependencies of a set of modules.
//
//===----------------------------------------------------------------------===//

#include "clang/Basic/VirtualFileSystem.h"
#include "clang/Frontend/Utils.h"
#include "clang/Serialization/ASTReader.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;

bool ModuleDependencyListener::visitInputFile(StringRef Filename, bool IsSystem,
                                              bool IsOverridden,
                                              bool IsExplicitModule) {
  return Collector.collectFile(Filename);
}

void ModuleDependencyListener::attachToASTReader(
    ASTReader &R, std::shared_ptr<ModuleDependencyCollector> Collector) {
  R.addListener(llvm::make_unique<ModuleDependencyListener>(*Collector));
}
