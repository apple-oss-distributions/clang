//===--- ASTConsumer.cpp - Abstract interface for reading ASTs --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the ASTConsumer class.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "llvm/Bitcode/BitstreamReader.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclGroup.h"
#include "clang/AST/ModuleProvider.h"
#include "clang/Lex/ModuleLoader.h"

using namespace clang;

bool ASTConsumer::HandleTopLevelDecl(DeclGroupRef D) {
  return true;
}

void ASTConsumer::HandleInterestingDecl(DeclGroupRef D) {
  HandleTopLevelDecl(D);
}

void ASTConsumer::HandleTopLevelDeclInObjCContainer(DeclGroupRef D) {}

void ASTConsumer::HandleImplicitImportDecl(ImportDecl *D) {
  HandleTopLevelDecl(DeclGroupRef(D));
}

SharedModuleProvider::SharedModuleProvider(ModuleProvider *MP)
  : std::shared_ptr<ModuleProvider>(MP) {}

ModuleProvider::~ModuleProvider() {}

class ModuleContainerGenerator : public ASTConsumer {
  std::shared_ptr<ModuleBuffer> Buffer;
  raw_ostream *OS;
public:
  ModuleContainerGenerator(
      DiagnosticsEngine &diags, const std::string &ModuleName,
      const HeaderSearchOptions &HSO, const PreprocessorOptions &PPO,
      const CodeGenOptions &CGO, const TargetOptions &TO, const LangOptions &LO,
      raw_ostream *OS,
      std::shared_ptr<ModuleBuffer> Buffer) : Buffer(Buffer), OS(OS) {}

  virtual ~ModuleContainerGenerator() {}

  void HandleTranslationUnit(ASTContext &Ctx) override {
    auto &SerializedAST = Buffer->Data;
    OS->write(SerializedAST.data(), SerializedAST.size());
    OS->flush();
  }
};

std::unique_ptr<ASTConsumer> SimpleModuleProvider::CreateModuleContainerGenerator(
    DiagnosticsEngine &Diags, const std::string &ModuleName,
    const HeaderSearchOptions &HSO, const PreprocessorOptions &PPO,
    const CodeGenOptions &CGO, const TargetOptions &TO, const LangOptions &LO,
    llvm::raw_ostream *OS,
    std::shared_ptr<ModuleBuffer> Buffer) const {
 return llvm::make_unique<ModuleContainerGenerator>
   (Diags, ModuleName, HSO, PPO, CGO, TO, LO, OS, Buffer);
}

void SimpleModuleProvider::UnwrapModuleContainer(llvm::MemoryBufferRef Buffer,
                                                 llvm::BitstreamReader &StreamFile)
  const
{
  StreamFile.init((const unsigned char *)Buffer.getBufferStart(),
                  (const unsigned char *)Buffer.getBufferEnd());
}
