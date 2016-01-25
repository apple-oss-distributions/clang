//===--- XCTMT.cpp - XCTest Migrate Tool ----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "XCTMT.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Edit/Commit.h"
#include "clang/Edit/EditedSource.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/MacroArgs.h"
#include "clang/Lex/MacroInfo.h"
#include "clang/Lex/Token.h"
#include "clang/Serialization/ASTReader.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Path.h"

namespace {
struct TKPair {
  const char *SenTestStr;
  const char *XCTStr;
};
}

static const TKPair TKIncludes[] = {
  {"NSException_SenTestFailure.h", "NSException_XCTestAdditions.h"},
  {"NSInvocation_SenTesting.h",     nullptr},
  {"SenInterfaceTestCase.h",        nullptr},
  {"SenTest.h",                     "XCAbstractTest.h"},
  {"SenTestCase_Macros.h",          "XCTestAssertions.h"},
  {"SenTestCase.h",                 "XCTestCase.h"},
  {"SenTestCaseRun.h",              "XCTestCaseRun.h"},
  {"SenTestDefines.h",              "XCTestDefines.h"},
  {"SenTestDistributedNotifier.h",  "XCTestDistributedNotifier.h"},
  {"SenTestingKit.h",               "XCTest.h"},
  {"SenTestingUtilities.h",         nullptr},
  {"SenTestLog.h",                  "XCTestLog.h"},
  {"SenTestObserver.h",             "XCTestObserver.h"},
  {"SenTestProbe.h",                "XCTestProbe.h"},
  {"SenTestRun.h",                  "XCTestRun.h"},
  {"SenTestSuite.h",                "XCTestSuite.h"},
  {"SenTestSuiteRun.h",             "XCTestSuiteRun.h"}
};

static const TKPair TKMacros[] = {
  {"STAssertNil",                 "XCTAssertNil"},
  {"STAssertNotNil",              "XCTAssertNotNil"},
  {"STAssertTrue",                "XCTAssertTrue"},
  {"STAssertFalse",               "XCTAssertFalse"},
  {"STAssertEqualObjects",        "XCTAssertEqualObjects"},
  {"STAssertEquals",              "XCTAssertEqual"},
  {"STAssertEqualsWithAccuracy",  "XCTAssertEqualWithAccuracy"},
  {"STAssertThrows",              "XCTAssertThrows"},
  {"STAssertThrowsSpecific",      "XCTAssertThrowsSpecific"},
  {"STAssertThrowsSpecificNamed", "XCTAssertThrowsSpecificNamed"},
  {"STAssertNoThrow",             "XCTAssertNoThrow"},
  {"STAssertNoThrowSpecific",     "XCTAssertNoThrowSpecific"},
  {"STAssertNoThrowSpecificNamed","XCTAssertNoThrowSpecificNamed"},
  {"STFail",                      "XCTFail"},
  {"STAssertTrueNoThrow",         "XCTAssertTrue"},
  {"STAssertFalseNoThrow",        "XCTAssertFalse"},

  {"should",                      "XCTAssertTrue"},
  {"should1",                     "XCTAssertTrue"},
  {"shouldnt",                    "XCTAssertFalse"},
  {"shouldnt1",                   "XCTAssertFalse"},
  {"shouldBeEqual",               "XCTAssertEqual"},
  {"shouldBeEqual1",              "XCTAssertEqual"},
  {"shouldRaise",                 "XCTAssertThrows"},
  {"shouldRaise1",                "XCTAssertThrows"},
  {"shouldntRaise",               "XCTAssertNoThrow"},
  {"shouldntRaise1",              "XCTAssertNoThrow"},
  {"fail",                        "XCTFail"},
  {"fail1",                       "XCTFail"},
  {"shouldnoraise",               "XCTAssertNoThrow"},
  {"should1noraise",              "XCTAssertNoThrow"},
  {"shouldntnoraise",             "XCTAssertThrows"},
  {"shouldnt1noraise",            "XCTAssertThrows"},

  {"SENTEST_EXPORT",              "XCT_EXPORT"},
  {"SENTEST_IMPORT",              "XCT_IMPORT"},
  {"SENTEST_STATIC_INLINE",       nullptr},
  {"SENTEST_EXTERN_INLINE",       nullptr}
};

static const TKPair TKDecls[] = {
  {"SenTestFailureException",       "XCTestFailureException"},
  {"SenFailureTypeKey",             "XCTestFailureTypeKey"},
  {"SenConditionFailure",           "XCTestConditionFailure"},
  {"SenRaiseFailure",               "XCTestRaiseFailure"},
  {"SenEqualityFailure",            "XCTestEqualityFailure"},
  {"SenUnconditionalFailure",       "XCTestUnconditionalFailure"},
  {"SenTestConditionKey",           "XCTestConditionKey"},
  {"SenTestEqualityLeftKey",        "XCTestEqualityLeftKey"},
  {"SenTestEqualityRightKey",       "XCTestEqualityRightKey"},
  {"SenTestEqualityAccuracyKey",    "XCTestEqualityAccuracyKey"},
  {"SenTestFilenameKey",            "XCTestFilenameKey"},
  {"SenTestLineNumberKey",          "XCTestLineNumberKey"},
  {"SenTestDescriptionKey",         "XCTestDescriptionKey"},

  {"SenTestCaseDidStartNotification", "XCTestCaseDidStartNotification"},
  {"SenTestCaseDidStopNotification",  "XCTestCaseDidStopNotification"},
  {"SenTestCaseDidFailNotification",  "XCTestCaseDidFailNotification"},

  {"SenTest",                          "XCTest"},
  {"SenTestCase",                      "XCTestCase"},
  {"SenTestCaseRun",                   "XCTestCaseRun"},

  {"SenTestDistributedNotifier",       "XCTestDistributedNotifier"},
  {"SenTestNotificationIdentifierKey", "XCTestNotificationIdentifierKey"},
  {"SenTestLog",                       "XCTestLog"},
  {"SenTestObserver",                  "XCTestObserver"},
  {"SenTestObserverClassKey",          "XCTestObserverClassKey"},
  {"SenSelfTestMain",                  "XCTSelfTestMain"},
  {"SenTestProbe",                     "XCTestProbe"},
  {"SenTestedUnitPath",                "XCTestedUnitPath"},
  {"SenTestScopeKey",                  "XCTestScopeKey"},
  {"SenTestScopeAll",                  "XCTestScopeAll"},
  {"SenTestScopeNone",                 "XCTestScopeNone"},
  {"SenTestScopeSelf",                 "XCTestScopeSelf"},
  {"SenTestToolKey",                   "XCTestToolKey"},
  {"SenTestRun",                       "XCTestRun"},
  {"SenTestSuite",                     "XCTestSuite"},
  {"SenTestSuiteRun",                  "XCTestSuiteRun"},
  {"SenTestSuiteDidStartNotification", "XCTestSuiteDidStartNotification"},
  {"SenTestSuiteDidStopNotification",  "XCTestSuiteDidStopNotification"}
};

using namespace clang;
using namespace arcmt;

XCTMigrator::XCTMigrator(edit::EditedSource &Editor, ASTContext &Ctx)
  : Editor(Editor), Ctx(Ctx), SenKitDir(nullptr) {

  IdentifierTable &Ids = Ctx.Idents;
  for (unsigned i = 0, e = sizeof(TKIncludes)/sizeof(TKPair); i != e; ++i)
    IncludesMap[TKIncludes[i].SenTestStr] =
        TKIncludes[i].XCTStr ? TKIncludes[i].XCTStr : StringRef();
  for (unsigned i = 0, e = sizeof(TKMacros)/sizeof(TKPair); i != e; ++i)
    MacrosMap[&Ids.get(TKMacros[i].SenTestStr)] =
        TKMacros[i].XCTStr ? &Ids.get(TKMacros[i].XCTStr) : nullptr;
  for (unsigned i = 0, e = sizeof(TKDecls)/sizeof(TKPair); i != e; ++i)
    DeclsMap[&Ids.get(TKDecls[i].SenTestStr)] =
        TKDecls[i].XCTStr ? &Ids.get(TKDecls[i].XCTStr) : nullptr;

  STComposeStringName = DeclarationName(&Ids.get("STComposeString"));
}

void XCTMigrator::migrateRef(NamedDecl *D, SourceLocation Loc) {
  if (!D || Loc.isInvalid())
    return;

  llvm::DenseMap<IdentifierInfo *, IdentifierInfo *>::iterator DI =
      DeclsMap.find(D->getDeclName().getAsIdentifierInfo());
  if (DI == DeclsMap.end())
    return;
  if (!isFromSenTestInclude(D->getLocation()) || isFromSenTestInclude(Loc))
    return;

  assert(DI->second);
  edit::Commit commit(Editor);
  commit.replace(Loc, DI->second->getName());
  Editor.commit(commit);
}

void XCTMigrator::keepMacroArgInfo(const MacroArgs *Args,
                                   const MacroDirective *MD) {
  assert(Args && MD);
  const MacroInfo *MI = MD->getMacroInfo();
  if (!MI->isVariadic())
    return;
  unsigned NumArgs = MI->getNumArgs();
  if (NumArgs <= 1)
    return;

  // We'll keep track of the source range for the last (non-variadic) argument
  // passed to the SenTesting macro. This may be a 'nil' passed as format, which
  // we will check for and remove in migrateNilDescArg if this is the case.

  unsigned LastNonVariadicIdx = NumArgs - 2;
  const Token *AT = Args->getUnexpArgument(LastNonVariadicIdx);
  unsigned Len = Args->getArgLength(AT);
  if (Len == 0)
    return;
  const Token *NextAT = Args->getUnexpArgument(LastNonVariadicIdx+1);
  unsigned NextLen = Args->getArgLength(NextAT);
  if (NextLen != 0)
    return;

  SourceRange ArgRange(AT->getLocation(), AT[Len-1].getLocation());
  SourceLocation EOFLoc = AT[Len].getLocation();
  CharSourceRange RemoveRange;
  if (LastNonVariadicIdx == 0) {
      RemoveRange = CharSourceRange::getCharRange(ArgRange.getBegin(),
                                      Args->getUnexpArgument(1)->getLocation());
  } else {
    const Token *PrevAT = Args->getUnexpArgument(LastNonVariadicIdx-1);
    unsigned PrevLen = Args->getArgLength(PrevAT);
    SourceLocation PrevEOFLoc = PrevAT[PrevLen].getLocation();
    RemoveRange = CharSourceRange::getCharRange(PrevEOFLoc, EOFLoc);
  }

  XCMacroArgInfo Info = { ArgRange, RemoveRange };
  MacroArgsInfo.push_back(Info);
}

void XCTMigrator::migrateMacro(IdentifierInfo *Name, SourceRange Range,
                               SourceLocation DefLoc, const MacroArgs *Args,
                               const MacroDirective *MD) {
  SourceLocation Loc = Range.getBegin();
  if (!Name || Loc.isInvalid())
    return;

  llvm::DenseMap<IdentifierInfo *, IdentifierInfo *>::iterator MIt =
      MacrosMap.find(Name);
  if (MIt == MacrosMap.end())
    return;
  if (!isFromSenTestInclude(DefLoc) || isFromSenTestInclude(Loc))
    return;

  if (Args)
    keepMacroArgInfo(Args, MD);

  edit::Commit commit(Editor);
  if (MIt->second)
    commit.replace(Loc, MIt->second->getName());
  else
    commit.remove(Loc);
  Editor.commit(commit);
}

void XCTMigrator::migrateNilDescArg(SourceRange Range) {
  // We came upon a 'nil' passed to STComposeString, which is used by the
  // SenTesting macros, indicating that 'nil' was passed for format parameter.
  // If we can find the range in our recorded ranges for the last (non-variadic)
  // argument passed to a SenTesting macro, we'll remove it.

  SourceManager &SM = Ctx.getSourceManager();
  Range = SourceRange(SM.getFileLoc(Range.getBegin()),
                      SM.getFileLoc(Range.getEnd()));
  for (std::vector<XCMacroArgInfo>::iterator
         I = MacroArgsInfo.begin(), E = MacroArgsInfo.end(); I != E; ++I) {
    if (Range == I->ArgRange) {
      edit::Commit commit(Editor);
      commit.remove(I->RemoveRange);
      Editor.commit(commit);
      break;
    }
  }
}

void XCTMigrator::migrateInclude(llvm::StringRef Filename,
                                 CharSourceRange FilenameRange,
                                 SourceLocation HashLoc, bool isAngled) {
  StringRef Parent = "SenTestingKit/";
  if (!Filename.startswith(Parent))
    return;
  if (isFromSenTestInclude(HashLoc))
    return;

  edit::Commit commit(Editor);
  StringRef HeaderName = Filename.substr(Parent.size());
  llvm::StringMap<llvm::StringRef>::iterator I = IncludesMap.find(HeaderName);
  if (I == IncludesMap.end() || I->second.empty()) {
    commit.remove(CharSourceRange::getCharRange(HashLoc,FilenameRange.getEnd()));
  } else {
    SmallString<128> NewInclude;
    NewInclude.push_back(isAngled ? '<' : '"');
    NewInclude += "XCTest/";
    NewInclude += I->second;
    NewInclude.push_back(isAngled ? '>' : '"');
    commit.replace(FilenameRange, NewInclude.str());
  }
  Editor.commit(commit);
}

bool XCTMigrator::isSTComposeStringFunction(const FunctionDecl *FD) {
  if (!FD)
    return false;
  return FD->getDeclName() == STComposeStringName &&
         isFromSenTestInclude(FD->getLocation());
}

namespace {
/// brief Checks if the PCH file includes any of the SenTesting headers.
class XCTSenTestingPCHCheck : public ASTReaderListener {
public:
  llvm::StringSet<> IncludesSet;
  bool ContainsSenHeader;

  XCTSenTestingPCHCheck() {
    ContainsSenHeader = false;
    for (unsigned i = 0, e = sizeof(TKIncludes)/sizeof(TKPair); i != e; ++i)
      IncludesSet.insert(TKIncludes[i].SenTestStr);
  }

  bool needsInputFileVisitation() override { return true; }
  bool visitInputFile(StringRef Filename, bool isSystem, bool isOverride) override {
    if (isSystem)
      return false;
    StringRef Name = llvm::sys::path::filename(Filename);
    if (IncludesSet.count(Name)) {
      ContainsSenHeader = true;
      return false;
    }

    return true;
  }
};
}

void XCTMigrator::handleInvocation(CompilerInstance &CI) {
  PreprocessorOptions &PPOpts = CI.getPreprocessorOpts();
  if (PPOpts.ImplicitPCHInclude.empty())
    return;

  // Check if the PCH file includes any of the SenTesting headers, in which case
  // modify the invocation and have it include the prefix header directly, so
  // we can migrate it.
  // Including a SenTesting header in the PCH is uncommon, this allows to use a
  // simpler logic during migration (ignoring the PCH file), while enabling
  // migration when the PCH does include such a header.
  if (!CI.hasFileManager())
    CI.createFileManager();
  XCTSenTestingPCHCheck PCHCheck;
  ASTReader::readASTFileControlBlock(PPOpts.ImplicitPCHInclude,
                                     CI.getFileManager(),
                                     CI.getModuleProvider(), PCHCheck);
  if (PCHCheck.ContainsSenHeader) {
    std::string PrefixHeader =
        ASTReader::getOriginalSourceFile(PPOpts.ImplicitPCHInclude,
                                         CI.getFileManager(),
                                         CI.getModuleProvider(),
                                         CI.getDiagnostics());
    if (!PrefixHeader.empty())
      PPOpts.Includes.insert(PPOpts.Includes.begin(), PrefixHeader);
    PPOpts.ImplicitPCHInclude.clear();
  }
}

bool XCTMigrator::isFromSenTestInclude(SourceLocation Loc) {
  if (Loc.isInvalid())
    return false;
  SourceManager &SM = Ctx.getSourceManager();
  Loc = SM.getFileLoc(Loc);
  const FileEntry *FE = SM.getFileEntryForID(SM.getFileID(Loc));
  if (!FE)
    return false;
  if (SenKitDir)
    return SenKitDir == FE->getDir();

  llvm::sys::path::reverse_iterator PI =
      llvm::sys::path::rbegin(FE->getDir()->getName());
  llvm::sys::path::reverse_iterator PE =
      llvm::sys::path::rend(FE->getDir()->getName());
  if (PI == PE)
    return false;
  if (*PI != "Headers")
    return false;
  ++PI;
  if (PI == PE)
    return false;
  if (*PI == "SenTestingKit.framework") {
    SenKitDir = FE->getDir();
    return true;
  }
  return false;
}

namespace {
class XCTDeclVisitor : public RecursiveASTVisitor<XCTDeclVisitor> {
  XCTMigrator &XCTM;

public:
  XCTDeclVisitor(XCTMigrator &XCTM) : XCTM(XCTM) { }

  bool shouldVisitTemplateInstantiations() const { return false; }
  bool shouldWalkTypesOfTypeLocs() const { return false; }

  bool VisitObjCInterfaceDecl(ObjCInterfaceDecl *D) {
    return true;
  }

  bool VisitObjCImplementationDecl(ObjCImplementationDecl *D) {
    XCTM.migrateRef(D->getSuperClass(), D->getSuperClassLoc());
    return true;
  }

  bool VisitObjCCategoryDecl(ObjCCategoryDecl *D) {
    XCTM.migrateRef(D->getClassInterface(), D->getLocation());
    return true;
  }

  bool VisitObjCCategoryImplDecl(ObjCCategoryImplDecl *D) {
    XCTM.migrateRef(D->getClassInterface(), D->getLocation());
    return true;
  }

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    XCTM.migrateRef(E->getFoundDecl(), E->getExprLoc());
    return true;
  }

  bool VisitCallExpr(CallExpr *E) {
    if (FunctionDecl *FD = E->getDirectCallee()) {
      if (XCTM.isSTComposeStringFunction(FD)) {
        assert(E->getNumArgs() > 0);
        Expr *Arg = E->getArg(0);
        if (XCTM.getASTContext().isSentinelNullExpr(Arg))
          XCTM.migrateNilDescArg(Arg->getSourceRange());
      }
    }
    return true;
  }

  bool VisitObjCInterfaceTypeLoc(ObjCInterfaceTypeLoc TL) {
    XCTM.migrateRef(TL.getIFaceDecl(), TL.getNameLoc());
    return true;
  }
};
}

void XCTMigrator::visit(Decl *D) {
  XCTDeclVisitor(*this).TraverseDecl(D);
}
