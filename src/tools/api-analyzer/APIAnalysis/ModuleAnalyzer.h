//=- llvm/Analysis/APIUsageAnalysis.h - API Usage Analysis ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MODULEANALYZER_H
#define LLVM_MODULEANALYZER_H

#include "APIAnalysisImpl.h"
#include "llvm/Support/MemoryBuffer.h"

namespace llvm {
class Module;
}

int AnalyzeModule(llvm::StringRef &filePath,
                  APIAnalysisIntermediateResult &result,
                  const APIAnalysisOptions &options);

int AnalyzeModule(llvm::MemoryBufferRef &data,
                  APIAnalysisIntermediateResult &result,
                  const APIAnalysisOptions &options);

#endif
