//===---- OverlappingReplacementsTest.cpp - clang-tidy --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "ClangTidyTest.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "gtest/gtest.h"

namespace clang {
namespace tidy {
namespace test {
namespace {

const char BoundDecl[] = "decl";
const char BoundIf[] = "if";

// We define a reduced set of very small checks that allow to test different
// overlapping situations (no overlapping, replacements partially overlap, etc),
// as well as different kinds of diagnostics (one check produces several errors,
// several replacement ranges in an error, etc).
class UseCharCheck : public ClangTidyCheck {
public:
  UseCharCheck(StringRef CheckName, ClangTidyContext *Context)
      : ClangTidyCheck(CheckName, Context) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override {
    using namespace ast_matchers;
    Finder->addMatcher(varDecl(hasType(isInteger())).bind(BoundDecl), this);
  }
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override {
    auto *VD = Result.Nodes.getNodeAs<VarDecl>(BoundDecl);
    diag(VD->getLocStart(), "use char") << FixItHint::CreateReplacement(
        CharSourceRange::getTokenRange(VD->getLocStart(), VD->getLocStart()),
        "char");
  }
};

class IfFalseCheck : public ClangTidyCheck {
public:
  IfFalseCheck(StringRef CheckName, ClangTidyContext *Context)
      : ClangTidyCheck(CheckName, Context) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override {
    using namespace ast_matchers;
    Finder->addMatcher(ifStmt().bind(BoundIf), this);
  }
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override {
    auto *If = Result.Nodes.getNodeAs<IfStmt>(BoundIf);
    auto *Cond = If->getCond();
    SourceRange Range = Cond->getSourceRange();
    if (auto *D = If->getConditionVariable()) {
      Range = SourceRange(D->getLocStart(), D->getLocEnd());
    }
    diag(Range.getBegin(), "the cake is a lie") << FixItHint::CreateReplacement(
        CharSourceRange::getTokenRange(Range), "false");
  }
};

class RefactorCheck : public ClangTidyCheck {
public:
  RefactorCheck(StringRef CheckName, ClangTidyContext *Context)
      : ClangTidyCheck(CheckName, Context), NamePattern("::$") {}
  RefactorCheck(StringRef CheckName, ClangTidyContext *Context,
                StringRef NamePattern)
      : ClangTidyCheck(CheckName, Context), NamePattern(NamePattern) {}
  virtual std::string newName(StringRef OldName) = 0;

  void registerMatchers(ast_matchers::MatchFinder *Finder) final {
    using namespace ast_matchers;
    Finder->addMatcher(varDecl(matchesName(NamePattern)).bind(BoundDecl), this);
  }

  void check(const ast_matchers::MatchFinder::MatchResult &Result) final {
    auto *VD = Result.Nodes.getNodeAs<VarDecl>(BoundDecl);
    std::string NewName = newName(VD->getName());

    auto Diag = diag(VD->getLocation(), "refactor %0 into %1")
                << VD->getName() << NewName
                << FixItHint::CreateReplacement(
                       CharSourceRange::getTokenRange(VD->getLocation(),
                                                      VD->getLocation()),
                       NewName);

    class UsageVisitor : public RecursiveASTVisitor<UsageVisitor> {
    public:
      UsageVisitor(const ValueDecl *VD, StringRef NewName,
                   DiagnosticBuilder &Diag)
          : VD(VD), NewName(NewName), Diag(Diag) {}
      bool VisitDeclRefExpr(DeclRefExpr *E) {
        if (const ValueDecl *D = E->getDecl()) {
          if (VD->getCanonicalDecl() == D->getCanonicalDecl()) {
            Diag << FixItHint::CreateReplacement(
                CharSourceRange::getTokenRange(E->getSourceRange()), NewName);
          }
        }
        return RecursiveASTVisitor<UsageVisitor>::VisitDeclRefExpr(E);
      }

    private:
      const ValueDecl *VD;
      StringRef NewName;
      DiagnosticBuilder &Diag;
    };

    UsageVisitor(VD, NewName, Diag)
        .TraverseDecl(Result.Context->getTranslationUnitDecl());
  }

protected:
  const std::string NamePattern;
};

class StartsWithPotaCheck : public RefactorCheck {
public:
  StartsWithPotaCheck(StringRef CheckName, ClangTidyContext *Context)
      : RefactorCheck(CheckName, Context, "::pota") {}

  std::string newName(StringRef OldName) override {
    return "toma" + OldName.substr(4).str();
  }
};

class EndsWithTatoCheck : public RefactorCheck {
public:
  EndsWithTatoCheck(StringRef CheckName, ClangTidyContext *Context)
      : RefactorCheck(CheckName, Context, "tato$") {}

  std::string newName(StringRef OldName) override {
    return OldName.substr(0, OldName.size() - 4).str() + "melo";
  }
};

} // namespace

TEST(OverlappingReplacementsTest, UseCharCheckTest) {
  const char Code[] =
