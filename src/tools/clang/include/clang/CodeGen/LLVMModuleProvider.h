//===-- CodeGen/LLVMModuleProvider.h - Emit and read .pcm files -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_CODEGEN_LLVM_MODULE_PROVIDER_H
#define LLVM_CLANG_CODEGEN_LLVM_MODULE_PROVIDER_H

#include "clang/AST/ModuleProvider.h"

namespace clang {

/// \brief A ModuleProvider that uses LLVM to wraps Clang modules inside a COFF,
/// ELF, or Mach-O container.
class LLVMModuleProvider
  : public ModuleProvider {
  /// \brief Return an ASTconsumer that uses LLVM to emit a module container
  /// that also contains full debug info for the module.
  std::unique_ptr<ASTConsumer>
    CreateModuleContainerGenerator(DiagnosticsEngine &Diags,
                                   const std::string &ModuleName,
                                   const HeaderSearchOptions &HSO,
                                   const PreprocessorOptions &PPO,
                                   const CodeGenOptions &CGO,
                                   const TargetOptions &TO,
                                   const LangOptions &LO,
                                   llvm::raw_ostream *OS,
                                   std::shared_ptr<ModuleBuffer> Buffer)
   const override;

  /// \brief Initialize an llvm::BitstreamReader with the module
  /// inside the module container Buffer.
  void UnwrapModuleContainer(llvm::MemoryBufferRef Buffer,
                             llvm::BitstreamReader &StreamFile) const override;


};

}


#endif
