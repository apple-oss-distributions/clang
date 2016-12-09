//===- IndexingAction.cpp - Frontend index action -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "clang/Index/IndexingAction.h"
#include "clang/Index/IndexDataConsumer.h"
#include "FileIndexRecord.h"
#include "IndexingContext.h"
#include "ClangIndexRecordWriter.h"
#include "IndexDataStoreUtils.h"
#include "clang/Index/IndexUnitWriter.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/MultiplexConsumer.h"
#include "clang/Frontend/Utils.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Serialization/ASTReader.h"

using namespace clang;
using namespace clang::index;

void IndexDataConsumer::_anchor() {}

bool IndexDataConsumer::handleDeclOccurence(const Decl *D, SymbolRoleSet Roles,
                                            ArrayRef<SymbolRelation> Relations,
                                            FileID FID, unsigned Offset,
                                            ASTNodeInfo ASTNode) {
  return true;
}

bool IndexDataConsumer::handleMacroOccurence(const IdentifierInfo *Name,
                                             const MacroInfo *MI, SymbolRoleSet Roles,
                                             FileID FID, unsigned Offset) {
  return true;
}

bool IndexDataConsumer::handleModuleOccurence(const ImportDecl *ImportD,
                                              SymbolRoleSet Roles,
                                              FileID FID, unsigned Offset) {
  return true;
}

namespace {

class IndexASTConsumer : public ASTConsumer {
  IndexingContext &IndexCtx;

public:
  IndexASTConsumer(IndexingContext &IndexCtx)
    : IndexCtx(IndexCtx) {}

protected:
  void Initialize(ASTContext &Context) override {
    IndexCtx.setASTContext(Context);
    IndexCtx.getDataConsumer().initialize(Context);
  }

  bool HandleTopLevelDecl(DeclGroupRef DG) override {
    return IndexCtx.indexDeclGroupRef(DG);
  }

  void HandleInterestingDecl(DeclGroupRef DG) override {
    // Ignore deserialized decls.
  }

  void HandleTopLevelDeclInObjCContainer(DeclGroupRef DG) override {
    IndexCtx.indexDeclGroupRef(DG);
  }

  void HandleTranslationUnit(ASTContext &Ctx) override {
  }
};

class IndexActionBase {
protected:
  std::shared_ptr<IndexDataConsumer> DataConsumer;
  IndexingContext IndexCtx;

  IndexActionBase(std::shared_ptr<IndexDataConsumer> dataConsumer,
                  IndexingOptions Opts)
    : DataConsumer(std::move(dataConsumer)),
      IndexCtx(Opts, *DataConsumer) {}

  std::unique_ptr<IndexASTConsumer> createIndexASTConsumer(CompilerInstance &CI) {
    IndexCtx.setSysrootPath(CI.getHeaderSearchOpts().Sysroot);
    return llvm::make_unique<IndexASTConsumer>(IndexCtx);
  }

  void finish() {
    DataConsumer->finish();
  }
};

class IndexAction : public ASTFrontendAction, IndexActionBase {
public:
  IndexAction(std::shared_ptr<IndexDataConsumer> DataConsumer,
              IndexingOptions Opts)
    : IndexActionBase(std::move(DataConsumer), Opts) {}

protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) override {
    return createIndexASTConsumer(CI);
  }

  void EndSourceFileAction() override {
    FrontendAction::EndSourceFileAction();
    finish();
  }
};

class WrappingIndexAction : public WrapperFrontendAction, IndexActionBase {
  bool CreatedASTConsumer = false;

public:
  WrappingIndexAction(std::unique_ptr<FrontendAction> WrappedAction,
                      std::shared_ptr<IndexDataConsumer> DataConsumer,
                      IndexingOptions Opts)
    : WrapperFrontendAction(std::move(WrappedAction)),
      IndexActionBase(std::move(DataConsumer), Opts) {}

protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) override;
  void EndSourceFileAction() override;
};

} // anonymous namespace

void WrappingIndexAction::EndSourceFileAction() {
  // Invoke wrapped action's method.
  WrapperFrontendAction::EndSourceFileAction();
  if (CreatedASTConsumer)
    finish();
}

std::unique_ptr<ASTConsumer>
WrappingIndexAction::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
  auto OtherConsumer = WrapperFrontendAction::CreateASTConsumer(CI, InFile);
  if (!OtherConsumer)
    return nullptr;

  CreatedASTConsumer = true;
  std::vector<std::unique_ptr<ASTConsumer>> Consumers;
  Consumers.push_back(std::move(OtherConsumer));
  Consumers.push_back(createIndexASTConsumer(CI));
  return llvm::make_unique<MultiplexConsumer>(std::move(Consumers));
}

std::unique_ptr<FrontendAction>
index::createIndexingAction(std::shared_ptr<IndexDataConsumer> DataConsumer,
                            IndexingOptions Opts,
                            std::unique_ptr<FrontendAction> WrappedAction) {
  if (WrappedAction)
    return llvm::make_unique<WrappingIndexAction>(std::move(WrappedAction),
                                                  std::move(DataConsumer),
                                                  Opts);
  return llvm::make_unique<IndexAction>(std::move(DataConsumer), Opts);
}


static bool topLevelDeclVisitor(void *context, const Decl *D) {
  IndexingContext &IndexCtx = *static_cast<IndexingContext*>(context);
  return IndexCtx.indexTopLevelDecl(D);
}

static void indexTranslationUnit(ASTUnit &Unit, IndexingContext &IndexCtx) {
  Unit.visitLocalTopLevelDecls(&IndexCtx, topLevelDeclVisitor);
}

void index::indexASTUnit(ASTUnit &Unit,
                         std::shared_ptr<IndexDataConsumer> DataConsumer,
                         IndexingOptions Opts) {
  IndexingContext IndexCtx(Opts, *DataConsumer);
  IndexCtx.setASTContext(Unit.getASTContext());
  DataConsumer->initialize(Unit.getASTContext());
  indexTranslationUnit(Unit, IndexCtx);
}

//===----------------------------------------------------------------------===//
// Index Data Recording
//===----------------------------------------------------------------------===//

namespace {

class IndexDataRecorder : public IndexDataConsumer {
  typedef llvm::DenseMap<FileID, std::unique_ptr<FileIndexRecord>> RecordByFileTy;
  RecordByFileTy RecordByFile;

public:
  RecordByFileTy::const_iterator record_begin() const { return RecordByFile.begin(); }
  RecordByFileTy::const_iterator record_end() const { return RecordByFile.end(); }
  bool record_empty() const { return RecordByFile.empty(); }

private:
  bool handleDeclOccurence(const Decl *D, SymbolRoleSet Roles,
                           ArrayRef<SymbolRelation> Relations,
                           FileID FID, unsigned Offset,
                           ASTNodeInfo ASTNode) override {
    FileIndexRecord &Rec = getFileIndexRecord(FID);
    Rec.addDeclOccurence(Roles, Offset, D, Relations);
    return true;
  }

  FileIndexRecord &getFileIndexRecord(FileID FID) {
    auto &Entry = RecordByFile[FID];
    if (!Entry) {
      Entry.reset(new FileIndexRecord(FID));
    }
    return *Entry;
  }
};

class IndexDependencyCollector : public DependencyCollector {
  bool NeedsSystemDependencies;
  llvm::SetVector<const FileEntry *> Entries;
  llvm::BitVector IsSystemByUID;
  FileManager *FileMgr = nullptr;
  std::string SysrootPath;

public:
  explicit IndexDependencyCollector(bool NeedsSystemDependencies)
    : NeedsSystemDependencies(NeedsSystemDependencies) {}

  void setFileManager(FileManager *fileMgr) { FileMgr = fileMgr; }
  void setSysrootPath(StringRef sysroot) { SysrootPath = sysroot; }

  bool isSystemFile(const FileEntry *FE) {
    auto UID = FE->getUID();
    return IsSystemByUID.size() > UID && IsSystemByUID[UID];
  }

  ArrayRef<const FileEntry *> getEntries() const {
    return Entries.getArrayRef();
  }

private:
  bool needSystemDependencies() override { return NeedsSystemDependencies; }

  bool sawDependency(StringRef Filename, bool FromModule,
                     bool IsSystem, bool IsModuleFile, bool IsMissing) override {
    bool sawIt = DependencyCollector::sawDependency(Filename, FromModule,
                                                    IsSystem, IsModuleFile,
                                                    IsMissing);
    if (auto *FE = FileMgr->getFile(Filename)) {
      if (sawIt)
        Entries.insert(FE);
      // Record system-ness for all files that we pass through.
      if (IsSystemByUID.size() < FE->getUID()+1)
        IsSystemByUID.resize(FE->getUID()+1);
        IsSystemByUID[FE->getUID()] = IsSystem || isInSysroot(Filename);
    }
    return sawIt;
  }

  bool isInSysroot(StringRef Filename) {
    return !SysrootPath.empty() && Filename.startswith(SysrootPath);
  }
};

class IndexRecordActionBase {
protected:
  RecordingOptions RecordOpts;
  IndexDataRecorder Recorder;
  IndexingContext IndexCtx;
  IndexDependencyCollector DepCollector;

  IndexRecordActionBase(IndexingOptions IndexOpts, RecordingOptions recordOpts)
    : RecordOpts(std::move(recordOpts)),
      IndexCtx(IndexOpts, Recorder),
      DepCollector(RecordOpts.RecordSystemDependencies) {}

  std::unique_ptr<IndexASTConsumer>
  createIndexASTConsumer(CompilerInstance &CI) {
    IndexCtx.setSysrootPath(CI.getHeaderSearchOpts().Sysroot);

    Preprocessor &PP = CI.getPreprocessor();
    DepCollector.attachToPreprocessor(PP);
    DepCollector.setFileManager(&CI.getFileManager());
    DepCollector.setSysrootPath(IndexCtx.getSysrootPath());

    return llvm::make_unique<IndexASTConsumer>(IndexCtx);
  }

  void finish(CompilerInstance &CI);
};

class IndexRecordAction : public ASTFrontendAction, IndexRecordActionBase {
public:
  IndexRecordAction(IndexingOptions IndexOpts, RecordingOptions RecordOpts)
    : IndexRecordActionBase(std::move(IndexOpts), std::move(RecordOpts)) {}

protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) override {
    return createIndexASTConsumer(CI);
  }

  void EndSourceFileAction() override {
    FrontendAction::EndSourceFileAction();
    finish(getCompilerInstance());
  }
};

class WrappingIndexRecordAction : public WrapperFrontendAction, IndexRecordActionBase {
  bool CreatedASTConsumer = false;

public:
  WrappingIndexRecordAction(std::unique_ptr<FrontendAction> WrappedAction,
                            IndexingOptions IndexOpts,
                            RecordingOptions RecordOpts)
    : WrapperFrontendAction(std::move(WrappedAction)),
      IndexRecordActionBase(std::move(IndexOpts), std::move(RecordOpts)) {}

protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) override {
    auto OtherConsumer = WrapperFrontendAction::CreateASTConsumer(CI, InFile);
    if (!OtherConsumer)
      return nullptr;

    CreatedASTConsumer = true;
    std::vector<std::unique_ptr<ASTConsumer>> Consumers;
    Consumers.push_back(std::move(OtherConsumer));
    Consumers.push_back(createIndexASTConsumer(CI));
    return llvm::make_unique<MultiplexConsumer>(std::move(Consumers));
  }

  void EndSourceFileAction() override {
    // Invoke wrapped action's method.
    WrapperFrontendAction::EndSourceFileAction();
    if (CreatedASTConsumer)
      finish(getCompilerInstance());
  }
};

} // anonymous namespace

static std::string getClangVersion() {
  // Try picking the version from an Apple Clang tag.
  std::string RepositoryPath = getClangRepositoryPath();
  StringRef BuildNumber = StringRef(RepositoryPath);
  size_t DashOffset = BuildNumber.find('-');
  if (BuildNumber.startswith("clang") && DashOffset != StringRef::npos) {
    BuildNumber = BuildNumber.substr(DashOffset + 1);
    return BuildNumber;
  }
  // Fallback to the generic version.
  return CLANG_VERSION_STRING;
}

void IndexRecordActionBase::finish(CompilerInstance &CI) {
  SourceManager &SM = CI.getSourceManager();
  DiagnosticsEngine &Diag = CI.getDiagnostics();
  HeaderSearch &HS = CI.getPreprocessor().getHeaderSearchInfo();
  StringRef DataPath = RecordOpts.DataDirPath;

  std::string Error;
  if (IndexUnitWriter::initIndexDirectory(DataPath, Error)) {
    unsigned DiagID = Diag.getCustomDiagID(
        DiagnosticsEngine::Error, "failed creating index directory %0");
    Diag.Report(DiagID) << Error;
    return;
  }

  std::string OutputFile = CI.getFrontendOpts().OutputFile;
  if (OutputFile.empty()) {
    OutputFile = CI.getFrontendOpts().Inputs[0].getFile();
    OutputFile += ".o";
  }

  const FileEntry *RootFile = nullptr;
  bool IsSystemUnit = false;
  bool isModuleGeneration = !CI.getLangOpts().CurrentModule.empty();
  if (!isModuleGeneration &&
      CI.getFrontendOpts().ProgramAction != frontend::GeneratePCH) {
    RootFile = SM.getFileEntryForID(SM.getMainFileID());
  }
  if (isModuleGeneration) {
    if (auto *Mod = HS.lookupModule(CI.getLangOpts().CurrentModule,
                                    /*AllowSearch=*/false))
      IsSystemUnit = Mod->IsSystem;
  }

  IndexUnitWriter UnitWriter(DataPath,
                             "clang", getClangVersion(),
                             OutputFile,
                             RootFile, IsSystemUnit,
                             CI.getTargetOpts().Triple,
                             IndexCtx.getSysrootPath());

  FileManager &FileMgr = SM.getFileManager();
  for (auto *FE : DepCollector.getEntries()) {
    UnitWriter.addFileDependency(FE, DepCollector.isSystemFile(FE));
  }

  if (auto Reader = CI.getModuleManager()) {
    Reader->getModuleManager().visit([&](serialization::ModuleFile &Mod) -> bool {
      bool isSystemMod = false;
      if (Mod.isModule()) {
        if (auto *M = HS.lookupModule(Mod.ModuleName, /*AllowSearch=*/false))
          isSystemMod = M->IsSystem;
      }
      if (const FileEntry *FE = FileMgr.getFile(Mod.FileName))
        UnitWriter.addASTFileDependency(FE, isSystemMod);
      return true; // skip module dependencies.
    });
  }

  ClangIndexRecordWriter RecordWriter(CI.getASTContext(), RecordOpts);
  for (auto I = Recorder.record_begin(), E = Recorder.record_end(); I != E; ++I) {
    FileID FID = I->first;
    const FileIndexRecord &Rec = *I->second;
    const FileEntry *FE = SM.getFileEntryForID(FID);
    std::string RecordFile;
    std::string Error;

    if (RecordWriter.writeRecord(FE->getName(), Rec, Error, &RecordFile)) {
      unsigned DiagID = Diag.getCustomDiagID(DiagnosticsEngine::Error,
                                             "failed writing record '%0': %1");
      Diag.Report(DiagID) << RecordFile << Error;
      return;
    }
    UnitWriter.addRecordFile(RecordFile, FE, DepCollector.isSystemFile(FE));
  }

  if (UnitWriter.write(Error)) {
    unsigned DiagID = Diag.getCustomDiagID(DiagnosticsEngine::Error,
                                           "failed writing unit data: %0");
    Diag.Report(DiagID) << Error;
    return;
  }
}

std::unique_ptr<FrontendAction>
index::createIndexDataRecordingAction(IndexingOptions IndexOpts,
                                      RecordingOptions RecordOpts,
                                std::unique_ptr<FrontendAction> WrappedAction) {
  if (WrappedAction)
    return llvm::make_unique<WrappingIndexRecordAction>(std::move(WrappedAction),
                                                        std::move(IndexOpts),
                                                        std::move(RecordOpts));
  return llvm::make_unique<IndexRecordAction>(std::move(IndexOpts),
                                              std::move(RecordOpts));
}
