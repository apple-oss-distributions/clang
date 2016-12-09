//===--- IndexingAction.h - Frontend index action -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_INDEX_INDEXINGACTION_H
#define LLVM_CLANG_INDEX_INDEXINGACTION_H

#include "clang/Basic/LLVM.h"
#include <memory>
#include <string>

namespace clang {
  class ASTUnit;
  class FrontendAction;

namespace index {
  class IndexDataConsumer;

struct IndexingOptions {
  enum class SystemSymbolFilterKind {
    None,
    DeclarationsOnly,
    All,
  };

  SystemSymbolFilterKind SystemSymbolFilter
    = SystemSymbolFilterKind::DeclarationsOnly;
  bool IndexFunctionLocals = false;
};

struct RecordingOptions {
  std::string DataDirPath;
  bool RecordSymbolCodeGenName = false;
  bool RecordSystemDependencies = true;
};

/// \param WrappedAction another frontend action to wrap over or null.
std::unique_ptr<FrontendAction>
createIndexingAction(std::shared_ptr<IndexDataConsumer> DataConsumer,
                     IndexingOptions Opts,
                     std::unique_ptr<FrontendAction> WrappedAction);

void indexASTUnit(ASTUnit &Unit,
                  std::shared_ptr<IndexDataConsumer> DataConsumer,
                  IndexingOptions Opts);

/// \param WrappedAction another frontend action to wrap over or null.
std::unique_ptr<FrontendAction>
createIndexDataRecordingAction(IndexingOptions IndexOpts,
                               RecordingOptions RecordOpts,
                               std::unique_ptr<FrontendAction> WrappedAction);

} // namespace index
} // namespace clang

#endif
