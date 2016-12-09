#include "clang-service/Service.h"
#include "clang-service/Messaging.h"
#include "clang-service/Support/InProcessClient.h"
#include "clang-service/Support/ValueUtils.h"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"

#include "gtest/gtest.h"

using namespace ClangService;
using namespace llvm;

namespace {

struct IndexingTest : ::testing::Test {
  std::unique_ptr<Client> C;

#define REGISTER_UID(Name, UIDStr) LazyCSUID Name;
#include "clang-service/ProtocolUIDs.inc"

  void SetUp() {
    C = make_unique<InProcessClient>();

#define REGISTER_UID(Name, UIDStr) Name = {UIDStr};
#include "clang-service/ProtocolUIDs.inc"
  }

  IndexingStatus getIndexerStatus(Value &IndexerResponse) {
    return IndexingStatus(
        IndexerResponse.getDict()[KeyIndexingStatus].getInt64());
  }

  void isCXModule(Value &V) {
    auto &D = V.getDict();
    EXPECT_TRUE(D[KeyToken].isInt64());
    EXPECT_TRUE(D[KeyManager].isInt64());
  }

  void isCXCursor(Value &V) {
    auto &D = V.getDict();
    EXPECT_TRUE(D[KeyToken].isInt64());
    EXPECT_TRUE(D[KeyManager].isInt64());
    EXPECT_TRUE(D[KeyCursorKind].isInt64());
  }

  void isCXIdxLoc(Value &V) {
    auto &D = V.getDict();
    EXPECT_TRUE(!D[KeyFile].isNull());
    ASSERT_EQ(D[KeyIdxClientFile].getInt64(), 1);
    EXPECT_TRUE(D[KeySourceLoc].isDict());
  }

  void isCXIdxIncludedFileInfo(Value &V) {
    auto &D = V.getDict();
    isCXIdxLoc(D[KeyIdxLoc]);
    EXPECT_TRUE(D[KeyName].isStringLike());
    EXPECT_TRUE(!D[KeyFile].isNull());
    EXPECT_TRUE(D[KeyIdxIsImport].isBool());
    EXPECT_TRUE(D[KeyIdxIsAngled].isBool());
    EXPECT_TRUE(D[KeyIdxIsModuleImport].isBool());
  }

  void isCXIdxImportedASTFileInfo(Value &V) {
    auto &D = V.getDict();
    EXPECT_TRUE(!D[KeyFile].isNull());
    isCXModule(D[KeyModule]);
    isCXIdxLoc(D[KeyIdxLoc]);
    EXPECT_TRUE(D[KeyIdxIsImplicit].isBool());
  }

  void isCXIdxContainerInfo(Value &V) {
    if (!V.isNull())
      isCXCursor(V);
  }

  void isCXAttrInfos(Value &V) {
    EXPECT_TRUE(V.isArray());
    for (auto &V : V.getArray()) {
      auto &D = V.getDict();
      EXPECT_TRUE(D[KeyIdxAttrKind].isInt64());
      isCXCursor(D[KeyCursor]);
      isCXIdxLoc(D[KeyIdxLoc]);
    }
  }

  void isCXIdxEntityInfo(Value &V) {
    auto &D = V.getDict();
    EXPECT_TRUE(D[KeyIdxEntityKind].isInt64());
    EXPECT_TRUE(D[KeyIdxEntityCXXTemplateKind].isInt64());
    EXPECT_TRUE(D[KeyIdxEntityLanguage].isInt64());
    EXPECT_TRUE(D[KeyIdxEntityName].isStringLike());
    EXPECT_TRUE(D[KeyIdxEntityUSR].isStringLike());
    isCXCursor(D[KeyCursor]);
    isCXAttrInfos(D[KeyIdxAttrInfos]);
  }

  void isCXIdxDeclInfo(Value &V) {
    auto &D = V.getDict();
    isCXIdxEntityInfo(D[KeyIdxEntityInfo]);
    isCXCursor(D[KeyCursor]);
    isCXIdxLoc(D[KeyIdxLoc]);
    isCXIdxContainerInfo(D[KeyIdxContainerSemInfo]);
    isCXIdxContainerInfo(D[KeyIdxContainerLexInfo]);
    EXPECT_TRUE(D[KeyIdxIsRedeclaration].isBool());
    EXPECT_TRUE(D[KeyIdxIsDefinition].isBool());
    EXPECT_TRUE(D[KeyIdxIsContainer].isBool());
    isCXIdxContainerInfo(D[KeyIdxContainerDeclInfo]);
    EXPECT_TRUE(D[KeyIdxIsImplicit].isBool());
    isCXAttrInfos(D[KeyIdxAttrInfos]);
    EXPECT_TRUE(D[KeyIdxDeclInfoFlags].isInt64());
  }

  void isCXIdxEntityRefInfo(Value &V) {
    auto &D = V.getDict();
    EXPECT_TRUE(D[KeyIdxEntityRefKind].isInt64());
    isCXCursor(D[KeyCursor]);
    isCXIdxLoc(D[KeyIdxLoc]);
    isCXIdxEntityInfo(D[KeyIdxEntityInfo]);
    isCXIdxEntityInfo(D[KeyIdxEntityInfoParent]);
    isCXIdxContainerInfo(D[KeyIdxContainerSemInfo]);
  }
};

TEST_F(IndexingTest, Simple) {
  int CCFd;
  SmallString<128> Path;
  auto EC = sys::fs::createTemporaryFile("simple", "cpp", CCFd, Path);
  if (EC)
    return;
  tool_output_file TOF{Path.c_str(), CCFd};
  TOF.os() << "#include <stdint.h>     \n";
  TOF.os() << "struct S { int64_t x; } \n";
  TOF.os() << "int f(S s) { return s.x; }";
  TOF.os().flush();

  unsigned ParseOpts = CXTranslationUnit_DetailedPreprocessingRecord;

  Value IndexerResponse =
      C->request(Value::dict({{KeyRequest, RequestIndex.c_str()},
                              {KeyName, Path.c_str()},
                              {KeyCmdArgs, toStringArray({Path.c_str()})},
                              {KeyIndexExcludeDeclsFromPCH, true},
                              {KeyIndexDiagnostics, true},
                              {KeyParseOptions, int64_t(ParseOpts)}}));

  int64_t Token;
  IndexingStatus Status;
  while ((Status = getIndexerStatus(IndexerResponse)) != IndexingStatus::Done) {
    auto &RD = IndexerResponse.getDict();
    Token = RD[KeyToken].getInt64();

    auto VContinue = Value::dict(
        {{KeyRequest, RequestIndexContinue.c_str()}, {KeyToken, Token}});
    auto &DContinue = VContinue.getDict();
    switch (Status) {
    case IndexingStatus::AbortQueryEvent:
      DContinue[KeyIndexingDoAbort] = false;
      break;

    case IndexingStatus::DiagnosticsEvent:
      /// Validation for CXDiagnostic is handled in other tests.
      EXPECT_TRUE(RD[KeyDiagnostics].isArray());
      break;

    case IndexingStatus::MainFileEvent:
      /// Validation for CX{File, SourceLocation} is handled in other tests.
      EXPECT_TRUE(RD[KeyFile].isDict());
      DContinue[KeyIdxClientFile] = int64_t(1);
      break;

    case IndexingStatus::PPIncludeEvent:
      isCXIdxIncludedFileInfo(RD[KeyIdxIncludedFileInfo]);
      DContinue[KeyIdxClientFile] = int64_t(1);
      break;

    case IndexingStatus::ImportedASTEvent:
      isCXIdxImportedASTFileInfo(RD[KeyIdxImportedASTFileInfo]);
      DContinue[KeyIdxClientASTFile] = int64_t(1);
      break;

    case IndexingStatus::StartedTUEvent:
      DContinue[KeyIdxClientContainer] = int64_t(1);
      break;

    case IndexingStatus::DeclarationEvent:
      isCXIdxDeclInfo(RD[KeyIdxDeclInfo]);
      break;

    case IndexingStatus::EntityReferenceEvent:
      isCXIdxEntityRefInfo(RD[KeyIdxEntityRefInfo]);
      break;

    default:
      break;
    }

    IndexerResponse = C->request(std::move(VContinue));
  }

  Value CloseResp = C->request(Value::dict(
      {{KeyRequest, RequestIndexClose.c_str()}, {KeyToken, Token}}));
  EXPECT_TRUE(CloseResp.isNull());
}

} // end anonymous namespace
