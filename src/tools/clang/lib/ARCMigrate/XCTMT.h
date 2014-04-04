//===--- XCTMT.h - XCTest Migrate Tool ------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_ARCMIGRATE_XCTMT_H
#define LLVM_CLANG_LIB_ARCMIGRATE_XCTMT_H

#include "clang/AST/DeclarationName.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Allocator.h"
#include <vector>

namespace clang {
  class Decl;
  class NamedDecl;
  class FunctionDecl;
  class ASTContext;
  class SourceLocation;
  class SourceRange;
  class CharSourceRange;
  class DirectoryEntry;
  class IdentifierInfo;
  class MacroArgs;
  class MacroDirective;
  class CompilerInstance;

namespace edit {
  class EditedSource;
}

namespace arcmt {

class XCTMigrator {
  edit::EditedSource &Editor;
  ASTContext &Ctx;
  llvm::StringMap<llvm::StringRef> IncludesMap;
  llvm::DenseMap<IdentifierInfo *, IdentifierInfo *> MacrosMap;
  llvm::DenseMap<IdentifierInfo *, IdentifierInfo *> DeclsMap;

  struct XCMacroArgInfo {
    SourceRange ArgRange;
    CharSourceRange RemoveRange;
  };
  // Keeps track of the source ranges for the last (non-variadic) argument
  // passed to SenTesting macros.
  std::vector<XCMacroArgInfo> MacroArgsInfo;

  const DirectoryEntry *SenKitDir;
  DeclarationName STComposeStringName;

public:
  XCTMigrator(edit::EditedSource &Editor, ASTContext &Ctx);

  void visit(Decl *D);
  void migrateRef(NamedDecl *D, SourceLocation Loc);
  void migrateMacro(IdentifierInfo *Name, SourceRange Range,
                    SourceLocation DefLoc, const MacroArgs *Args,
                    const MacroDirective *MD);
  void migrateNilDescArg(SourceRange Range);
  void migrateInclude(llvm::StringRef Filename, CharSourceRange FilenameRange,
                      SourceLocation HashLoc, bool isAngled);

  ASTContext &getASTContext() { return Ctx; }
  bool isSTComposeStringFunction(const FunctionDecl *FD);

  static void handleInvocation(CompilerInstance &CI);

private:
  bool isFromSenTestInclude(SourceLocation Loc);
  void keepMacroArgInfo(const MacroArgs *Args, const MacroDirective *MD);
};

} // end namespace arcmt

} // end namespace clang

#endif
