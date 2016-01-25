//===--- CodeGenModuleContainer.cpp - Emit .pcm files ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "clang/CodeGen/LLVMModuleProvider.h"
#include "CGDebugInfo.h"
#include "CodeGenModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/Expr.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/CodeGen/BackendUtil.h"
#include "clang/Frontend/CodeGenOptions.h"
#include "clang/Serialization/ASTWriter.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Bitcode/BitstreamReader.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Object/COFF.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/TargetRegistry.h"
#include <memory>
using namespace clang;

#define DEBUG_TYPE "modulecontainer"

namespace {
class ModuleContainerGenerator : public ASTConsumer {
  DiagnosticsEngine &Diags;
  const std::string ModuleName;
  ASTContext *Ctx;
  const HeaderSearchOptions &HeaderSearchOpts;
  const PreprocessorOptions &PreprocessorOpts;
  CodeGenOptions CodeGenOpts;
  const TargetOptions TargetOpts;
  const LangOptions LangOpts;
  std::unique_ptr<llvm::LLVMContext> VMContext;
  std::unique_ptr<llvm::Module> M;
  std::unique_ptr<CodeGen::CodeGenModule> Builder;
  raw_ostream *OS;
  std::shared_ptr<ModuleBuffer> Buffer;

  /// Visit every type and emit debug info for it.
  struct DebugTypeVisitor : public RecursiveASTVisitor<DebugTypeVisitor> {
    clang::CodeGen::CGDebugInfo &DI;
    ASTContext &Ctx;
    DebugTypeVisitor(clang::CodeGen::CGDebugInfo &DI, ASTContext &Ctx)
        : DI(DI), Ctx(Ctx) {}

    /// Determine whether this type can be represented in DWARF.
    static bool CanRepresent(const Type *Ty) {
      return !Ty->isDependentType() && !Ty->isUndeducedType();
    }

    bool VisitTypeDecl(TypeDecl *D) {
      QualType QualTy = Ctx.getTypeDeclType(D);
      if (!QualTy.isNull() && CanRepresent(QualTy.getTypePtr()))
        DI.getOrCreateStandaloneType(QualTy, D->getLocation());
      return true;
    }

    bool VisitValueDecl(ValueDecl *D) {
      QualType QualTy = D->getType();
      if (!QualTy.isNull() && CanRepresent(QualTy.getTypePtr()))
        DI.getOrCreateStandaloneType(QualTy, D->getLocation());
      return true;
    }

    bool VisitObjCInterfaceDecl(ObjCInterfaceDecl *D) {
      QualType QualTy(D->getTypeForDecl(), 0);
      if (!QualTy.isNull() && CanRepresent(QualTy.getTypePtr()))
        DI.getOrCreateStandaloneType(QualTy, D->getLocation());
      return true;
    }

    bool VisitFunctionDecl(FunctionDecl *D) {
      if (isa<CXXMethodDecl>(D))
        // Constructing the this argument mandates a CodeGenFunction.
        return true;
 
      SmallVector<QualType, 16> ArgTypes;
      for (auto i : D->params())
        ArgTypes.push_back(i->getType());
      QualType RetTy = D->getReturnType();
      QualType FnTy = Ctx.getFunctionType(RetTy, ArgTypes,
                                          FunctionProtoType::ExtProtoInfo());
      if (CanRepresent(FnTy.getTypePtr()))
        DI.EmitFunctionDecl(D, D->getLocation(), FnTy);
      return true;
    }

    bool VisitObjCMethodDecl(ObjCMethodDecl *D) {
      if (!D->getClassInterface())
        return true;

      bool selfIsPseudoStrong, selfIsConsumed;
      SmallVector<QualType, 16> ArgTypes;
      ArgTypes.push_back(D->getSelfType(Ctx, D->getClassInterface(),
                                        selfIsPseudoStrong, selfIsConsumed));
      ArgTypes.push_back(Ctx.getObjCSelType());
      for (auto i : D->params())
        ArgTypes.push_back(i->getType());
      QualType RetTy = D->getReturnType();
      QualType FnTy = Ctx.getFunctionType(RetTy, ArgTypes,
                                            FunctionProtoType::ExtProtoInfo());
      if (CanRepresent(FnTy.getTypePtr()))
        DI.EmitFunctionDecl(D, D->getLocation(), FnTy);
      return true;
    }
  };

public:
  ModuleContainerGenerator(
      DiagnosticsEngine &diags, const std::string &ModuleName,
      const HeaderSearchOptions &HSO, const PreprocessorOptions &PPO,
      const CodeGenOptions &CGO, const TargetOptions &TO, const LangOptions &LO,
      raw_ostream *OS,
      std::shared_ptr<ModuleBuffer> Buffer)
    : Diags(diags), ModuleName(ModuleName),
      HeaderSearchOpts(HSO), PreprocessorOpts(PPO),
      CodeGenOpts(CGO), TargetOpts(TO), LangOpts(LO), OS(OS),
      Buffer(Buffer) {}

  virtual ~ModuleContainerGenerator() {}

  void Initialize(ASTContext &Context) override {
    Ctx = &Context;
    VMContext.reset(new llvm::LLVMContext());
    M.reset(new llvm::Module(ModuleName, *VMContext));
    M->setDataLayout(Ctx->getTargetInfo().getTargetDescription());
    Builder.reset(new CodeGen::CodeGenModule(*Ctx, HeaderSearchOpts,
                                             PreprocessorOpts, CodeGenOpts,
                                             *M, M->getDataLayout(), Diags));
  }

  bool HandleTopLevelDecl(DeclGroupRef D) override {
    if (!Diags.hasErrorOccurred() &&
        (CodeGenOpts.getDebugInfo() > CodeGenOptions::NoDebugInfo)) {
      // Collect all the debug info.
      for (auto *I : D) {
        if (!I->isFromASTFile()) {
          DebugTypeVisitor DTV(*Builder->getModuleDebugInfo(), *Ctx);
          DTV.TraverseDecl(I);
        }
      }
    }
    return true;
  }

  void HandleTagDeclDefinition(TagDecl *D) override {
    if (Diags.hasErrorOccurred())
      return;
 
    Builder->UpdateCompletedType(D);
  }

  void HandleTagDeclRequiredDefinition(const TagDecl *D) override {
    if (Diags.hasErrorOccurred())
      return;

    if (CodeGen::CGDebugInfo *DI = Builder->getModuleDebugInfo())
      if (const RecordDecl *RD = dyn_cast<RecordDecl>(D))
        DI->completeRequiredType(RD);
  }

  /// Emit a container holding the serialized AST.
  void HandleTranslationUnit(ASTContext &Ctx) override {
    assert(M && VMContext && Builder);
    // Delete these on function exit.
    std::unique_ptr<llvm::LLVMContext> VMContext = std::move(this->VMContext);
    std::unique_ptr<llvm::Module> M = std::move(this->M);
    std::unique_ptr<CodeGen::CodeGenModule> Builder = std::move(this->Builder);

    if (Diags.hasErrorOccurred())
      return;

    M->setTargetTriple(Ctx.getTargetInfo().getTriple().getTriple());
    M->setDataLayout(Ctx.getTargetInfo().getTargetDescription());

    // Finalize the Builder.
    if (Builder)
      Builder->Release();

    // Initialize the backend if we haven't done so already.
    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllTargets();
    LLVMInitializeAllAsmPrinters();
    LLVMInitializeAllTargetMCs();

    // Ensure the target exists.
    std::string Error;
    auto Triple = Ctx.getTargetInfo().getTriple();
    if (!llvm::TargetRegistry::lookupTarget(Triple.getTriple(), Error))
      llvm::report_fatal_error(Error);

    // Emit the serialized Clang AST into its own section.
    assert(Buffer->IsComplete && "serialization did not complete");
    auto &SerializedAST = Buffer->Data;
    auto Size = SerializedAST.size();
    auto Int8Ty = llvm::Type::getInt8Ty(*VMContext);
    auto *Ty = llvm::ArrayType::get(Int8Ty, Size);
    auto *Data = llvm::ConstantDataArray::
      getString(*VMContext, StringRef(SerializedAST.data(), Size),
                /*AddNull=*/false);
    auto *ASTSym = new llvm::GlobalVariable(
        *M, Ty, /*constant*/ true, llvm::GlobalVariable::InternalLinkage, Data,
        "__clang_ast");
    ASTSym->setAlignment(8);
    if (Triple.isOSBinFormatMachO())
      // Include Mach-O segment name.
      ASTSym->setSection("__CLANG,__clangast");
    else if (Triple.isOSBinFormatCOFF())
      // Adhere to COFF eight-character limit.
      ASTSym->setSection("clangast");
    else
      ASTSym->setSection("__clangast");

    DEBUG(clang::EmitBackendOutput(Diags, CodeGenOpts, TargetOpts, LangOpts,
                                   Ctx.getTargetInfo().getTargetDescription(),
                                   M.get(), BackendAction::Backend_EmitLL,
                                   &llvm::dbgs()));

    // Use the LLVM backend to emit the pcm.
    clang::EmitBackendOutput(Diags, CodeGenOpts, TargetOpts, LangOpts,
                             Ctx.getTargetInfo().getTargetDescription(),
                             M.get(), BackendAction::Backend_EmitObj, OS);

    // Make sure the module container hits disk now.
    OS->flush();

    // Free up some memory, in case the process is kept alive.
    SerializedAST.clear();
  }
};
}

std::unique_ptr<ASTConsumer> LLVMModuleProvider::CreateModuleContainerGenerator(
    DiagnosticsEngine &Diags, const std::string &ModuleName,
    const HeaderSearchOptions &HSO, const PreprocessorOptions &PPO,
    const CodeGenOptions &CGO, const TargetOptions &TO, const LangOptions &LO,
    llvm::raw_ostream *OS,
    std::shared_ptr<ModuleBuffer> Buffer) const {
 return llvm::make_unique<ModuleContainerGenerator>
   (Diags, ModuleName, HSO, PPO, CGO, TO, LO, OS, Buffer);
}

void LLVMModuleProvider::UnwrapModuleContainer(
    llvm::MemoryBufferRef Buffer, llvm::BitstreamReader &StreamFile) const {
  if (auto OF = llvm::object::ObjectFile::createObjectFile(Buffer)) {
    auto *Obj = OF.get().get();
    bool IsCOFF = isa<llvm::object::COFFObjectFile>(Obj);
    // Find the clang AST section in the container.
    for (auto &Section : OF->get()->sections()) {
      StringRef Name;
      Section.getName(Name);
      if ((!IsCOFF && Name == "__clangast") || (IsCOFF && Name == "clangast")) {
        StringRef Buf;
        Section.getContents(Buf);
        StreamFile.init((const unsigned char *)Buf.begin(),
                        (const unsigned char *)Buf.end());
        return;
      }
    }
  }
  StreamFile.init((const unsigned char *)Buffer.getBufferStart(),
                  (const unsigned char *)Buffer.getBufferEnd());
  return;
}
