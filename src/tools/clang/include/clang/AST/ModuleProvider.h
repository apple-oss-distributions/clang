//===--- AST/ModuleProvider.h - Emit and read .pcm files --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_MODULE_PROVIDER_H
#define LLVM_CLANG_AST_MODULE_PROVIDER_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/MemoryBuffer.h"
#include <memory>

namespace llvm {
  class raw_ostream;
  class BitstreamReader;
}

namespace clang {

class ASTConsumer;
class CodeGenOptions;
class DiagnosticsEngine;
class HeaderSearchOptions;
class LangOptions;
class PreprocessorOptions;
class TargetOptions;

struct ModuleBuffer {
  bool IsComplete;
  llvm::SmallVector<char, 0> Data;
};
  
class ModuleProvider {
public:
  virtual ~ModuleProvider();
  /// \brief Return an ASTconsumer that can be chained with a
  /// PCHGenerator that takes care of storing the physical
  /// representation of a module.
  virtual std::unique_ptr<ASTConsumer> CreateModuleContainerGenerator(
      DiagnosticsEngine &Diags, const std::string &ModuleName,
      const HeaderSearchOptions &HSO, const PreprocessorOptions &PPO,
      const CodeGenOptions &CGO, const TargetOptions &TO, const LangOptions &LO,
      llvm::raw_ostream *OS,
      std::shared_ptr<ModuleBuffer> Buffer) const = 0;

  /// \brief Initialize an llvm::BitstreamReader with the module
  /// inside the module container Buffer.
  virtual void UnwrapModuleContainer(llvm::MemoryBufferRef Buffer,
                                     llvm::BitstreamReader &StreamFile)
    const = 0;
};

/// \brief A simple pass-through module provider.
class SimpleModuleProvider : public ModuleProvider {
  /// \brief Return an ASTconsumer that can be chained with a
  /// PCHGenerator that writes the module to a flat file.
  std::unique_ptr<ASTConsumer> CreateModuleContainerGenerator(
      DiagnosticsEngine &Diags, const std::string &ModuleName,
      const HeaderSearchOptions &HSO, const PreprocessorOptions &PPO,
      const CodeGenOptions &CGO, const TargetOptions &TO, const LangOptions &LO,
      llvm::raw_ostream *OS,
      std::shared_ptr<ModuleBuffer> Buffer) const override;

  /// \brief Initialize an llvm::BitstreamReader with Buffer.
  void UnwrapModuleContainer(llvm::MemoryBufferRef Buffer,
                             llvm::BitstreamReader &StreamFile) const override;
};

}

#endif
