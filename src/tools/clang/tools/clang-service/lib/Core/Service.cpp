#include "clang-service/Messaging.h"
#include "clang-service/Service.h"
#include "clang-service/SafeLibclang.h"
#include "clang-service/Support/ValueUtils.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"

#include <condition_variable>
#include <fstream>
#include <mutex>
#include <thread>

using llvm::make_unique;
using llvm::StringRef;
using LockGuard = std::unique_lock<std::mutex>;
using namespace Libclang;

namespace ClangService {

namespace {
#define REGISTER_UID(Name, UIDStr) LazyCSUID Name{UIDStr};
#include "clang-service/ProtocolUIDs.inc"
}

namespace {

Value makeError(ErrorKind EK, StringRef Description) {
  auto IOS = InlineOwnedString::create(1 + Description.size() + 1);
  {
    raw_inlinestring_ostream Stream{*IOS.get()};
    Stream << char(EK);
    Stream << Description;
    Stream << '\0';
  }
  return Value::data(std::move(IOS));
}

Value makeRequestInvalidError(StringRef Description) {
  return makeError(ErrorKind::RequestInvalid, Description);
}

Value makeRequestFailedError(StringRef Description) {
  return makeError(ErrorKind::RequestFailed, Description);
}

Value makeLibclangFailedError(CXErrorCode EC) {
  switch (EC) {
  case CXError_Success:
    return makeRequestFailedError("Libclang succeeded");
  case CXError_Failure:
    return makeRequestFailedError("Libclang failed");
  case CXError_Crashed:
    return makeRequestFailedError("Libclang crashed");
  case CXError_InvalidArguments:
    return makeRequestFailedError("Invalid arguments to a libclang API");
  case CXError_ASTReadError:
    return makeRequestFailedError("Libclang couldn't read an AST");
  }
}

#if 0
Value makeRequestInterruptedError(StringRef Description) {
  return makeError(ErrorKind::RequestInterrupted, Description);
}

Value makeRequestCancelledError(StringRef Description) {
  return makeError(ErrorKind::RequestCancelled, Description);
}
#endif

} // end anonymous namespace

struct TokenGenerator {
  /// TokenGenerator generates and recycles unique integer tokens.

  int64_t NextToken;
  llvm::SmallVector<uint64_t, 4> UnusedTokens;

  TokenGenerator() : NextToken(1), UnusedTokens() {}

  int64_t getUniqueToken() {
    if (!UnusedTokens.empty()) {
      uint64_t Token = UnusedTokens.back();
      UnusedTokens.pop_back();
      return Token;
    }
    return NextToken++;
  }

  void recycleToken(int64_t Token) { UnusedTokens.push_back(Token); }
};

template <typename T> using TokenMap = llvm::SmallDenseMap<int64_t, T>;

template <typename T> class TokenManager {
  /// TokenManager owns a set of tokens. It cleans up any metadata associated
  /// with those tokens at the end of its lifetime.

  int64_t ManagerToken;
  std::mutex &TGLock;
  TokenGenerator &TG;
  TokenMap<T> &Mappings;
  llvm::SmallVector<int64_t, 4> Tokens;

public:
  TokenManager(int64_t ManagerToken, std::mutex &TGLock, TokenGenerator &TG,
               TokenMap<T> &Mappings)
      : ManagerToken(ManagerToken), TGLock(TGLock), TG(TG), Mappings(Mappings),
        Tokens() {}

  ~TokenManager() {
    if (Tokens.empty())
      return;
    LockGuard Guard{TGLock};
    for (auto Token : Tokens) {
      Mappings.erase(Token);
      TG.recycleToken(Token);
    }
  }

  TokenManager(const TokenManager &) = delete;

  TokenManager &operator=(const TokenManager &) = delete;

  TokenManager(TokenManager &&TM)
      : ManagerToken(TM.ManagerToken), TGLock(TM.TGLock), TG(TM.TG),
        Mappings(TM.Mappings), Tokens(std::move(TM.Tokens)) {}

  TokenManager &operator=(TokenManager &&TM) {
    assert(Tokens.empty() && "Leaked tokens");
    ManagerToken = TM.ManagerToken;
    TGLock = TM.TGLock;
    TG = TM.TG;
    Mappings = TM.Mappings;
    Tokens = std::move(TM.Tokens);
    return *this;
  }

  int64_t getUniqueToken(T Obj) {
    int64_t Token;
    {
      LockGuard Guard{TGLock};
      Token = TG.getUniqueToken();
      Mappings[Token] = Obj;
    }
    Tokens.push_back(Token);
    return Token;
  }

  int64_t getManagerToken() const { return ManagerToken; }
};

union LocationObject {
  CXFile File;
  CXSourceLocation SLoc;
  CXModule Module;
  CXCursor Cursor;

  LocationObject() : Cursor({}) {}
  LocationObject(void *P) : File(P) {}
  LocationObject(CXSourceLocation SLoc) : SLoc(SLoc) {}
  LocationObject(CXCursor Cursor) : Cursor(Cursor) {}
};

using LocTokenManager = TokenManager<LocationObject>;

struct TokenContext {
  /// TokenContext is a generic context which owns location-like tokens.

  LocTokenManager LTM;

  TokenContext(LocTokenManager LTM) : LTM(std::move(LTM)) {}

  virtual ~TokenContext() {}

  virtual LocTokenManager &getLocTokenManager() { return LTM; }
};

struct CodeCompletionContext : public TokenContext {
  /// CodeCompletionContext stores code completion results.

  SafeCXIndex Idx;
  UniqueCXCodeCompleteResultsPtr Results;

  CodeCompletionContext(LocTokenManager LTM, SafeCXIndex Idx,
                        UniqueCXCodeCompleteResultsPtr Results)
      : TokenContext(std::move(LTM)), Idx(std::move(Idx)),
        Results(std::move(Results)) {}

  virtual ~CodeCompletionContext() {}
};

struct IndexingContext : public TokenContext {
  /// IndexingContext manages an indexing session and stores its results.

  SafeCXIndex Idx;

  // Initialization:
  //
  //   [ Manager Thread ]   ->[ Worker Thread ]
  //     1. lock_mutex()   /    1. lock_mutex()
  //     2. spawn_worker()      2. make(&response)
  //     3. wait(&lock) <-----  3. notify()
  //     4. take(&response)     4. wait(&lock)
  //
  // Continuing the session:
  //
  //   [ Manager Thread ]     [ Worker Thread ]
  //     1. make(&response)  >  1. lock_mutex()
  //     2. lock_mutex()    /   2. make(&response)
  //     3. notify() -------
  //     4. wait(&lock) <-----  3. notify()
  //     5. take(&response)     4. wait(&lock)

  std::mutex SessionLock;
  std::condition_variable ContinueSessionCond;
  LockGuard WorkerGuard;
  std::thread Worker;

  // Use separate values for message-passing to avoid spurious wakeups.
  Value WorkerResponse;
  Value ManagerResponse;

  Value ClientKeyFiles;
  llvm::SmallVector<const char *, 8> CmdArgs;
  llvm::SmallVector<CXUnsavedFile, 4> UnsavedFiles;

  bool DoDiagnostics;
  CXDiagnosticDisplayOptions DiagnosticOpts;
  unsigned ParseOpts;

  IndexingContext(LocTokenManager LTM, SafeCXIndex Idx, Value ClientKeyFiles,
                  llvm::SmallVector<const char *, 8> CmdArgs,
                  llvm::SmallVector<CXUnsavedFile, 4> UnsavedFiles,
                  bool DoDiagnostics, CXDiagnosticDisplayOptions DiagnosticOpts,
                  unsigned ParseOpts)
      : TokenContext(std::move(LTM)), Idx(std::move(Idx)), SessionLock(),
        ContinueSessionCond(), WorkerGuard(), Worker(), WorkerResponse(),
        ManagerResponse(), ClientKeyFiles(std::move(ClientKeyFiles)),
        CmdArgs(std::move(CmdArgs)), UnsavedFiles(std::move(UnsavedFiles)),
        DoDiagnostics(DoDiagnostics), DiagnosticOpts(DiagnosticOpts),
        ParseOpts(ParseOpts) {}

  virtual ~IndexingContext() {}

  Value awaitWorkerResponse(LockGuard &Guard) {
    assert(Guard.owns_lock() && "Calling thread does not own lock");
    ContinueSessionCond.notify_one();
    ContinueSessionCond.wait(Guard, [&] { return !WorkerResponse.isNull(); });

    auto &WRD = WorkerResponse.getDict();
    WRD[KeyToken] = getLocTokenManager().getManagerToken();
    if (WRD[KeyIndexingStatus].getInt64() == int64_t(IndexingStatus::Done))
      Worker.join();

    return std::move(WorkerResponse);
  }

  Value awaitManagerResponse() {
    assert(!WorkerResponse.isNull() && "Manager will not wake up");
    assert(WorkerGuard.owns_lock() && "Worker thread does not own lock");
    ContinueSessionCond.notify_one();
    ContinueSessionCond.wait(WorkerGuard,
                             [&] { return !ManagerResponse.isNull(); });
    return std::move(ManagerResponse);
  }
};

struct ServiceState {
  std::mutex LogLock;

  std::mutex LocTokensLock;
  TokenGenerator LocTokenGen;
  TokenMap<LocationObject> LocTokens;

  std::mutex CtxsLock;
  TokenGenerator CtxTokenGen;
  TokenMap<std::unique_ptr<TokenContext>> Ctxs;

  LocTokenManager createLocTokenManager(int64_t ManagerToken) {
    return {ManagerToken, LocTokensLock, LocTokenGen, LocTokens};
  }
};

Service::Service() : State(new ServiceState) {}

Service::~Service() { delete State; }

void Service::serve() {
  assert(!"Unreachable");
  abort();
}

namespace {

/// UniqueCXString ::= string (Optional)
///
/// Note: This routine assumes that the string will live long enough to be
/// transferred.
Value toValue(UniqueCXString S) {
  if (clang_getCString(S))
    return Value::string(make_unique<OwnedCXString>(std::move(S)));
  return {};
}

/// CXFile ::= {
///   key.token: int64,
///   key.manager: int64
/// }
Value toFileValue(CXFile File, LocTokenManager &LTM) {
  if (File)
    return Value::dict({{KeyToken, LTM.getUniqueToken({File})},
                        {KeyManager, LTM.getManagerToken()}});
  return Value::dict({{KeyToken, 0LL}, {KeyManager, 0LL}});
}

/// CXSourceLocation ::= {
///   key.token: int64,
///   key.manager: int64
/// }
Value toValue(CXSourceLocation Loc, LocTokenManager &LTM) {
  return Value::dict({{KeyToken, LTM.getUniqueToken({Loc})},
                      {KeyManager, LTM.getManagerToken()}});
}

/// CXSourceLocation/Spelling ::= {
///   key.loc.file: CXFile,
///   key.loc.line: int64,
///   key.loc.column: int64,
///   key.loc.offset: int64
/// }
Value toSpellingLocationValue(CXSourceLocation Loc, LocTokenManager &LTM) {
  CXFile File;
  unsigned Line, Col, Offset;
  clang_getSpellingLocation(Loc, &File, &Line, &Col, &Offset);
  return Value::dict({{KeyFile, toFileValue(File, LTM)},
                      {KeyLineOffset, int64_t(Line)},
                      {KeyColOffset, int64_t(Col)},
                      {KeyOffset, int64_t(Offset)}});
}

/// CXModule ::= {
///   key.token: int64,
///   key.manager: int64
/// }
Value toModuleValue(CXModule Module, LocTokenManager &LTM) {
  return Value::dict({{KeyToken, LTM.getUniqueToken({Module})},
                      {KeyManager, LTM.getManagerToken()}});
}

/// CXCursor ::= {
///   key.token: int64,
///   key.manager: int64,
///   key.loc.cursor.kind: int64
/// }
Value toValue(CXCursor Cursor, LocTokenManager &LTM) {
  return Value::dict({{KeyToken, LTM.getUniqueToken({Cursor})},
                      {KeyManager, LTM.getManagerToken()},
                      {KeyCursorKind, int64_t(clang_getCursorKind(Cursor))}});
}

/// CXIdxLoc ::= {
///   key.loc.file: CXFile,
///   key.indexing.client_file: int64,
///   key.loc: CXSourceLocation
/// }
Value toValue(CXIdxLoc ILoc, LocTokenManager &LTM) {
  CXFile File;
  CXIdxClientFile IdxFile;
  clang_indexLoc_getFileLocation(ILoc, &IdxFile, &File, nullptr, nullptr,
                                 nullptr);
  CXSourceLocation Loc = clang_indexLoc_getCXSourceLocation(ILoc);
  return Value::dict({{KeyFile, toFileValue(File, LTM)},
                      {KeyIdxClientFile, int64_t(IdxFile)},
                      {KeySourceLoc, toValue(Loc, LTM)}});
}

/// CXIdxIncludedFileInfo ::= {
///   key.indexing.loc: CXIdxLoc,
///   key.loc.name: string,
///   key.loc.file: CXFile,
///   key.indexing.is_import: bool,
///   key.indexing.is_angled: bool,
///   key.indexing.is_module_import: bool
/// }
Value toIncludedFileInfoValue(const CXIdxIncludedFileInfo *IFI,
                              LocTokenManager &LTM) {
  // FIXME: Use an OwnedString to capture IFI->filename.
  return Value::dict({{KeyIdxLoc, toValue(IFI->hashLoc, LTM)},
                      {KeyName, IFI->filename ? IFI->filename : ""},
                      {KeyFile, toFileValue(IFI->file, LTM)},
                      {KeyIdxIsImport, bool(IFI->isImport)},
                      {KeyIdxIsAngled, bool(IFI->isAngled)},
                      {KeyIdxIsModuleImport, bool(IFI->isModuleImport)}});
}

/// CXIdxImportedASTFileInfo ::= {
///   key.loc.file: CXFile,
///   key.loc.module: CXModule,
///   key.indexing.loc: CXIdxLoc,
///   key.indexing.is_implicit: bool
/// }
Value toImportedASTFileInfoValue(const CXIdxImportedASTFileInfo *IAFI,
                                 LocTokenManager &LTM) {
  return Value::dict({{KeyFile, toFileValue(IAFI->file, LTM)},
                      {KeyModule, toModuleValue(IAFI->module, LTM)},
                      {KeyIdxLoc, toValue(IAFI->loc, LTM)},
                      {KeyIdxIsImplicit, bool(IAFI->isImplicit)}});
}

/// [CXIdxAttrInfo] ::= [{
///   key.indexing.attr_kind: int64,
///   key.loc.cursor: CXCursor,
///   key.indexing.loc: CXIdxLoc
/// }]
Value toValue(const CXIdxAttrInfo *const *Attributes, unsigned NumAttrs,
              LocTokenManager &LTM) {
  auto VAttrs = Value::array({});
  auto &AAttrs = VAttrs.getArray();
  AAttrs.reserve(NumAttrs);
  for (unsigned I = 0; I < NumAttrs; ++I) {
    const CXIdxAttrInfo *AI = Attributes[I];
    AAttrs.push_back(Value::dict({{KeyIdxAttrKind, int64_t(AI->kind)},
                                  {KeyCursor, toValue(AI->cursor, LTM)},
                                  {KeyIdxLoc, toValue(AI->loc, LTM)}}));
  }
  return VAttrs;
}

/// CXIdxContainerInfo ::= CXCursor (Optional)
Value toValue(const CXIdxContainerInfo *CI, LocTokenManager &LTM) {
  if (CI)
    return toValue(CI->cursor, LTM);
  return {};
}

/// CXIdxEntityInfo ::= {
///   key.indexing.entity_kind: int64,
///   key.indexing.entity_cxx_template_kind: int64,
///   key.indexing.entity_lang: int64,
///   key.indexing.entity_name: string,
///   key.indexing.entity_usr: string,
///   key.loc.cursor: CXCursor,
///   key.indexing.attr_infos: [CXIdxAttrInfo]
/// }
Value toValue(const CXIdxEntityInfo *EI, LocTokenManager &LTM) {
  // FIXME: Use an OwnedString to capture EI->{name, USR}.
  return Value::dict(
      {{KeyIdxEntityKind, int64_t(EI->kind)},
       {KeyIdxEntityCXXTemplateKind, int64_t(EI->templateKind)},
       {KeyIdxEntityLanguage, int64_t(EI->lang)},
       {KeyIdxEntityName, EI->name ? EI->name : ""},
       {KeyIdxEntityUSR, EI->USR ? EI->USR : ""},
       {KeyCursor, toValue(EI->cursor, LTM)},
       {KeyIdxAttrInfos, toValue(EI->attributes, EI->numAttributes, LTM)}});
}

/// CXIdxDeclInfo ::= {
///   key.indexing.entity_info: CXIdxEntityInfo,
///   key.loc.cursor: CXCursor,
///   key.indexing.loc: CXIdxLoc,
///   key.indexing.sem.container_info: CXIdxContainerInfo,
///   key.indexing.lex.container_info: CXIdxContainerInfo,
///   key.indexing.is_redeclaration: bool,
///   key.indexing.is_definition: bool,
///   key.indexing.is_container: bool,
///   key.indexing.decl.container_info: CXIdxContainerInfo,
///   key.indexing.is_implicit: bool,
///   key.indexing.attr_infos: [CXIdxAttrInfo],
///   key.indexing.decl_info_flags: int64
/// }
Value toValue(const CXIdxDeclInfo *IDI, LocTokenManager &LTM) {
  return Value::dict(
      {{KeyIdxEntityInfo, toValue(IDI->entityInfo, LTM)},
       {KeyCursor, toValue(IDI->cursor, LTM)},
       {KeyIdxLoc, toValue(IDI->loc, LTM)},
       {KeyIdxContainerSemInfo, toValue(IDI->semanticContainer, LTM)},
       {KeyIdxContainerLexInfo, toValue(IDI->lexicalContainer, LTM)},
       {KeyIdxIsRedeclaration, bool(IDI->isRedeclaration)},
       {KeyIdxIsDefinition, bool(IDI->isDefinition)},
       {KeyIdxIsContainer, bool(IDI->isContainer)},
       {KeyIdxContainerDeclInfo, toValue(IDI->declAsContainer, LTM)},
       {KeyIdxIsImplicit, bool(IDI->isImplicit)},
       {KeyIdxAttrInfos, toValue(IDI->attributes, IDI->numAttributes, LTM)},
       {KeyIdxDeclInfoFlags, int64_t(IDI->flags)}});
}

/// CXIdxEntityRefInfo ::= {
///   key.indexing.entity_ref_kind: int64,
///   key.loc.cursor: CXCursor,
///   key.indexing.loc: CXIdxLoc,
///   key.indexing.entity_info: CXIdxEntityInfo,
///   kex.indexing.entity_info.parent: CXIdxEntityInfo,
///   key.indexing.sem.container_info: CXIdxContainerInfo
/// }
Value toValue(const CXIdxEntityRefInfo *ERI, LocTokenManager &LTM) {
  return Value::dict({{KeyIdxEntityRefKind, int64_t(ERI->kind)},
                      {KeyCursor, toValue(ERI->cursor, LTM)},
                      {KeyIdxLoc, toValue(ERI->loc, LTM)},
                      {KeyIdxEntityInfo, toValue(ERI->referencedEntity, LTM)},
                      {KeyIdxEntityInfoParent, toValue(ERI->parentEntity, LTM)},
                      {KeyIdxContainerSemInfo, toValue(ERI->container, LTM)}});
}

/// CXSourceRange ::= {
///   key.range.start: CXSourceLocation,
///   key.range.end: CXSourceLocation
/// }
Value toValue(CXSourceRange &Range, LocTokenManager &LTM) {
  return Value::dict(
      {{KeyRangeStart, toValue(clang_getRangeStart(Range), LTM)},
       {KeyRangeEnd, toValue(clang_getRangeEnd(Range), LTM)}});
}

/// CXDiagnostic ::= {
///   key.diagnostic.severity: int64,
///   key.diagnostic.string: string,
///   key.diagnostic.location: CXSourceLocation,
///   key.diagnostic.fixits: [[string, CXSourceRange]]
/// }
Value toValue(CXDiagnostic Diagnostic,
              CXDiagnosticDisplayOptions DiagnosticOpts,
              LocTokenManager &LTM) {
  CXDiagnosticSeverity Severity = clang_getDiagnosticSeverity(Diagnostic);

  UniqueCXString DiagStr{clang_formatDiagnostic(Diagnostic, DiagnosticOpts)};
  auto VDiagStr = toValue(std::move(DiagStr));

  CXSourceLocation SourceLoc = clang_getDiagnosticLocation(Diagnostic);

  auto VFixits = Value::array({});
  auto &AFixits = VFixits.getArray();
  unsigned NumFixIts = clang_getDiagnosticNumFixIts(Diagnostic);
  AFixits.reserve(NumFixIts);
  for (unsigned I = 0; I < NumFixIts; ++I) {
    CXSourceRange Replacement;
    UniqueCXString Fixit{clang_getDiagnosticFixIt(Diagnostic, I, &Replacement)};
    AFixits.push_back(
        Value::array({toValue(std::move(Fixit)), toValue(Replacement, LTM)}));
  }

  return Value::dict({{KeyDiagnosticSeverity, int64_t(Severity)},
                      {KeyDiagnosticString, std::move(VDiagStr)},
                      {KeyDiagnosticLocation, toValue(SourceLoc, LTM)},
                      {KeyDiagnosticFixits, std::move(VFixits)}});
}

/// CXDiagnosticSet ::= [CXDiagnostic]
Value toDiagnosticsSetValue(CXDiagnosticSet DS,
                            CXDiagnosticDisplayOptions DiagnosticOpts,
                            LocTokenManager &LTM) {
  auto VDiagnostics = Value::array({});
  auto &ADiagnostics = VDiagnostics.getArray();
  unsigned NumDiagnostics = clang_getNumDiagnosticsInSet(DS);
  ADiagnostics.reserve(NumDiagnostics);
  for (unsigned I = 0; I < NumDiagnostics; ++I) {
    UniqueCXDiagnostic Diag{clang_getDiagnosticInSet(DS, I)};
    ADiagnostics.push_back(toValue(Diag, DiagnosticOpts, LTM));
  }
  return VDiagnostics;
}

// FIXME: Turn the sub-chunks array into an array of tokens; ditto for the
//        annotations array.
//
/// CXCompletionString ::= {
///   key.codecomplete.completion.str.kind: int64 (Optional),
///   key.codecomplete.completion.str.text: string (Optional),
///   key.codecomplete.completion.str.chunks: [CXCompletionString],
///   key.codecomplete.completion.str.availability: int64,
///   key.codecomplete.completion.str.priority: int64,
///   key.codecomplete.completion.str.annotations: [string],
///   key.codecomplete.completion.str.parent: string (Optional),
///   key.codecomplete.completion.str.brief: string (Optional)
/// }
Value toValue(CXCompletionString CompletionStr, bool IncludeBrief) {
  unsigned NumChunks = clang_getNumCompletionChunks(CompletionStr);

  auto VChunks = Value::array({});
  auto &AChunks = VChunks.getArray();
  AChunks.reserve(NumChunks);

  for (unsigned I = 0; I < NumChunks; ++I) {
    CXCompletionString SubCompletion =
        clang_getCompletionChunkCompletionString(CompletionStr, I);
    auto VSubCompletion = toValue(SubCompletion, IncludeBrief);

    auto &DSubCompletion = VSubCompletion.getDict();

    CXCompletionChunkKind Kind = clang_getCompletionChunkKind(CompletionStr, I);
    DSubCompletion[KeyCodeCompleteCompletionStrKind] = int64_t(Kind);

    UniqueCXString SubText{clang_getCompletionChunkText(CompletionStr, I)};
    DSubCompletion[KeyCodeCompleteCompletionStrText] =
        toValue(std::move(SubText));

    AChunks.push_back(std::move(VSubCompletion));
  }

  CXAvailabilityKind Availability =
      clang_getCompletionAvailability(CompletionStr);

  unsigned Priority = clang_getCompletionPriority(CompletionStr);

  unsigned NumAnnotations = clang_getCompletionNumAnnotations(CompletionStr);
  auto VAnnotations = Value::array({});
  auto &AAnnotations = VAnnotations.getArray();
  AAnnotations.reserve(NumAnnotations);
  for (unsigned I = 0; I < NumAnnotations; ++I) {
    UniqueCXString Annotation{clang_getCompletionAnnotation(CompletionStr, I)};
    AAnnotations.push_back(toValue(std::move(Annotation)));
  }

  CXCursorKind ParentKind;
  UniqueCXString Parent{clang_getCompletionParent(CompletionStr, &ParentKind)};
  auto VParent = toValue(std::move(Parent));

  Value VBrief;
  if (IncludeBrief) {
    UniqueCXString Brief{clang_getCompletionBriefComment(CompletionStr)};
    VBrief = toValue(std::move(Brief));
  }

  return Value::dict(
      {{KeyCodeCompleteCompletionStrChunks, std::move(VChunks)},
       {KeyCodeCompleteCompletionStrAvailability, int64_t(Availability)},
       {KeyCodeCompleteCompletionStrPriority, int64_t(Priority)},
       {KeyCodeCompleteCompletionStrAnnotations, std::move(VAnnotations)},
       {KeyCodeCompleteCompletionStrParent, std::move(VParent)},
       {KeyCodeCompleteCompletionStrParentKind, int64_t(ParentKind)},
       {KeyCodeCompleteCompletionStrBrief, std::move(VBrief)}});
}

/// CXCompletionResult ::= {
///   key.codecomplete.completion.kind: int64,
///   key.codecomplete.completion.str: CXCompletionString
/// }
Value toValue(CXCompletionResult *Completion, bool IncludeBrief) {
  return Value::dict(
      {{KeyCodeCompleteCompletionKind, int64_t(Completion->CursorKind)},
       {KeyCodeCompleteCompletionStr,
        toValue(Completion->CompletionString, IncludeBrief)}});
}

// FIXME: Turn the results array into an array of tokens; ditto for diagnostics.
//
/// CXCodeCompleteResults ::= {
///   key.token: int64,
///   key.codecomplete.results: [CXCompletionResult],
///   key.codecomplete.contexts: int64,
///   key.codecomplete.container.kind: int64,
///   key.codecomplete.container.incomplete: bool,
///   key.codecomplete.container.usr: string (Optional),
///   key.codecomplete.objc_selector: string (Optional),
///   key.diagnostics: [CXDiagnostic] (Optional)
/// }
Value toValue(CXCodeCompleteResults *Results, bool DoSort, bool IncludeBrief,
              bool DoDiagnostics, CXDiagnosticDisplayOptions DiagnosticOpts,
              LocTokenManager &LTM) {
  if (DoSort)
    clang_sortCodeCompletionResults(Results->Results, Results->NumResults);

  auto VResults = Value::array({});
  auto &AResults = VResults.getArray();
  AResults.reserve(Results->NumResults);
  for (unsigned I = 0; I < Results->NumResults; ++I)
    AResults.push_back(toValue(Results->Results + I, IncludeBrief));

  unsigned long long Contexts = clang_codeCompleteGetContexts(Results);

  unsigned Incomplete;
  CXCursorKind ContainerKind =
      clang_codeCompleteGetContainerKind(Results, &Incomplete);

  Value VUSR;
  if (ContainerKind != CXCursor_InvalidCode) {
    UniqueCXString USR{clang_codeCompleteGetContainerUSR(Results)};
    VUSR = toValue(std::move(USR));
  }

  UniqueCXString ObjCSelector{clang_codeCompleteGetObjCSelector(Results)};
  Value VObjCSelector = toValue(std::move(ObjCSelector));

  Value Diagnostics;
  if (DoDiagnostics) {
    auto VDiagnostics = Value::array({});
    auto &ADiagnostics = VDiagnostics.getArray();
    unsigned NumDiagnostics = clang_codeCompleteGetNumDiagnostics(Results);
    ADiagnostics.reserve(NumDiagnostics);
    for (unsigned I = 0; I < NumDiagnostics; ++I) {
      UniqueCXDiagnostic Diag{clang_codeCompleteGetDiagnostic(Results, I)};
      ADiagnostics.push_back(toValue(Diag, DiagnosticOpts, LTM));
    }
    Diagnostics = std::move(VDiagnostics);
  }

  return Value::dict({{KeyToken, LTM.getManagerToken()},
                      {KeyCodeCompleteResults, std::move(VResults)},
                      {KeyCodeCompleteContexts, int64_t(Contexts)},
                      {KeyCodeCompleteContainerKind, int64_t(ContainerKind)},
                      {KeyCodeCompleteContainerIncomplete, bool(Incomplete)},
                      {KeyCodeCompleteContainerUSR, std::move(VUSR)},
                      {KeyCodeCompleteObjCSelector, std::move(VObjCSelector)},
                      {KeyDiagnostics, std::move(Diagnostics)}});
}

} // end anonymous namespace

namespace {

/// IndexingStatus ::= {
///   key.token: int64,
///   key.indexing.status: int64,
///   key.error_code: int64 (Optional),
///   key.diagnostics: [CXDiagnostic] (Optional),
///   key.file: CXFile (Optional),
///   key.indexing.included_file_info: CXIdxIncludedFileInfo (Optional),
///   key.indexing.imported_ast_file_info: CXIdxImportedASTFileInfo (Optional),
///   key.indexing.decl_info: CXIdxDeclInfo (Optional),
///   key.indexing.entity_ref_info: CXIdxEntityRefInfo (Optional)
/// }

int handleIndexerAbortQueryEvent(CXClientData CD, void *) {
  auto *Ctx = reinterpret_cast<IndexingContext *>(CD);
  Ctx->WorkerResponse = Value::dict(
      {{KeyIndexingStatus, int64_t(IndexingStatus::AbortQueryEvent)}});
  Value ClientResp = Ctx->awaitManagerResponse();
  bool AbortQuery = ClientResp.getBool();
  return AbortQuery;
}

void handleIndexerDiagnosticEvent(CXClientData CD, CXDiagnosticSet DS, void *) {
  auto *Ctx = reinterpret_cast<IndexingContext *>(CD);
  if (!Ctx->DoDiagnostics) {
    assert(Ctx->WorkerResponse.isNull() &&
           "Indexing context has invalid response");
    return;
  }

  auto Diagnostics = toDiagnosticsSetValue(DS, Ctx->DiagnosticOpts,
                                           Ctx->getLocTokenManager());
  Ctx->WorkerResponse = Value::dict(
      {{KeyIndexingStatus, int64_t(IndexingStatus::DiagnosticsEvent)},
       {KeyDiagnostics, std::move(Diagnostics)}});
  Ctx->awaitManagerResponse();
}

CXIdxClientFile handleIndexerEnteredMainFileEvent(CXClientData CD,
                                                  CXFile MainFile, void *) {
  auto *Ctx = reinterpret_cast<IndexingContext *>(CD);
  Ctx->WorkerResponse = Value::dict(
      {{KeyIndexingStatus, int64_t(IndexingStatus::MainFileEvent)},
       {KeyFile, toFileValue(MainFile, Ctx->getLocTokenManager())}});
  Value ClientResp = Ctx->awaitManagerResponse();
  int64_t ClientFile = ClientResp.getInt64();
  return (void *)ClientFile;
}

CXIdxClientFile handleIndexerPPIncludeEvent(CXClientData CD,
                                            const CXIdxIncludedFileInfo *IFI) {
  auto *Ctx = reinterpret_cast<IndexingContext *>(CD);
  Ctx->WorkerResponse =
      Value::dict({{KeyIndexingStatus, int64_t(IndexingStatus::PPIncludeEvent)},
                   {KeyIdxIncludedFileInfo,
                    toIncludedFileInfoValue(IFI, Ctx->getLocTokenManager())}});
  Value ClientResp = Ctx->awaitManagerResponse();
  int64_t ClientFile = ClientResp.getInt64();
  return (void *)ClientFile;
}

CXIdxClientASTFile
handleIndexerImportedASTEvent(CXClientData CD,
                              const CXIdxImportedASTFileInfo *IAFI) {
  auto *Ctx = reinterpret_cast<IndexingContext *>(CD);
  Ctx->WorkerResponse = Value::dict(
      {{KeyIndexingStatus, int64_t(IndexingStatus::ImportedASTEvent)},
       {KeyIdxImportedASTFileInfo,
        toImportedASTFileInfoValue(IAFI, Ctx->getLocTokenManager())}});
  Value ClientResp = Ctx->awaitManagerResponse();
  int64_t ClientASTFile = ClientResp.getInt64();
  return (void *)ClientASTFile;
}

CXIdxClientContainer handleIndexerStartedTUEvent(CXClientData CD, void *) {
  auto *Ctx = reinterpret_cast<IndexingContext *>(CD);
  Ctx->WorkerResponse = Value::dict(
      {{KeyIndexingStatus, int64_t(IndexingStatus::StartedTUEvent)}});
  Value ClientResp = Ctx->awaitManagerResponse();
  int64_t ClientContainer = ClientResp.getInt64();
  return (void *)ClientContainer;
}

void handleIndexerDeclarationEvent(CXClientData CD, const CXIdxDeclInfo *IDI) {
  auto *Ctx = reinterpret_cast<IndexingContext *>(CD);
  Ctx->WorkerResponse = Value::dict(
      {{KeyIndexingStatus, int64_t(IndexingStatus::DeclarationEvent)},
       {KeyIdxDeclInfo, toValue(IDI, Ctx->getLocTokenManager())}});
  Ctx->awaitManagerResponse();
}

void handleIndexerEntityReferenceEvent(CXClientData CD,
                                       const CXIdxEntityRefInfo *ERI) {
  auto *Ctx = reinterpret_cast<IndexingContext *>(CD);
  Ctx->WorkerResponse = Value::dict(
      {{KeyIndexingStatus, int64_t(IndexingStatus::EntityReferenceEvent)},
       {KeyIdxEntityRefInfo, toValue(ERI, Ctx->getLocTokenManager())}});
  Ctx->awaitManagerResponse();
}

// Note: Every callback is required to clear Ctx->WorkerResponse.
static IndexerCallbacks IndexerCBs = {
    handleIndexerAbortQueryEvent,      handleIndexerDiagnosticEvent,
    handleIndexerEnteredMainFileEvent, handleIndexerPPIncludeEvent,
    handleIndexerImportedASTEvent,     handleIndexerStartedTUEvent,
    handleIndexerDeclarationEvent,     handleIndexerEntityReferenceEvent};

void driveIndexingSession(IndexingContext *Ctx, CXIndexAction Act,
                          unsigned IndexingOpts) {
  Ctx->WorkerGuard = LockGuard(Ctx->SessionLock);
  auto Err = (CXErrorCode)clang_indexSourceFile(
      Act, (CXClientData)Ctx, &IndexerCBs, sizeof(IndexerCBs), IndexingOpts,
      nullptr, Ctx->CmdArgs.data(), Ctx->CmdArgs.size(),
      Ctx->UnsavedFiles.data(), Ctx->UnsavedFiles.size(), /*OutTU=*/nullptr,
      Ctx->ParseOpts);
  Ctx->WorkerResponse =
      Value::dict({{KeyIndexingStatus, int64_t(IndexingStatus::Done)},
                   {KeyError, makeLibclangFailedError(Err)}});
  Ctx->ContinueSessionCond.notify_one();
  Ctx->WorkerGuard.unlock();
}

} // end anonymous namespace

class ResponseBuilder {
  ServiceState *State;

  /// State needed to describe a request.
  Value::Dict &ClientDict;
  StringRef RequestStr;
  CSUID RequestUID;

  /// Parameters needed by various handlers.
  int64_t ReqToken;
  int64_t ReqManager;
  int64_t ReqLHSToken;
  int64_t ReqRHSToken;
  StringRef Filename;
  uint64_t LineOffset;
  uint64_t ColOffset;
  llvm::SmallVector<const char *, 8> CmdArgs;
  llvm::SmallVector<CXUnsavedFile, 4> UnsavedFiles;

  /// Optional parameters used to control libclang's behavior.
  CXGlobalOptFlags IndexOpts;
  unsigned IndexExcludeDeclsFromPCH;
  unsigned IndexDiagnostics;
  CXIndexOptFlags IndexingOpts;
  bool DoDiagnostics;
  CXDiagnosticDisplayOptions DiagnosticOpts;
  unsigned ParseOpts;
  bool IncludeBrief;
  unsigned CodeCompleteOpts;
  bool DoSort;
  unsigned ReparseOpts;

  /// Set ReqToken.
  bool setKeyToken() {
    Value &ClientKeyToken = ClientDict[KeyToken];
    if (ClientKeyToken.isInt64())
      ReqToken = ClientKeyToken.getInt64();
    return ClientKeyToken.isInt64();
  }

  /// Set ReqManager.
  bool setKeyManager() {
    Value &ClientKeyManager = ClientDict[KeyManager];
    if (ClientKeyManager.isInt64())
      ReqManager = ClientKeyManager.getInt64();
    return ClientKeyManager.isInt64();
  }

  /// Set ReqLHSToken.
  bool setKeyLHSToken() {
    Value &ClientKeyLHSToken = ClientDict[KeyLHSToken];
    if (ClientKeyLHSToken.isInt64())
      ReqLHSToken = ClientKeyLHSToken.getInt64();
    return ClientKeyLHSToken.isInt64();
  }

  /// Set ReqRHSToken.
  bool setKeyRHSToken() {
    Value &ClientKeyRHSToken = ClientDict[KeyRHSToken];
    if (ClientKeyRHSToken.isInt64())
      ReqRHSToken = ClientKeyRHSToken.getInt64();
    return ClientKeyRHSToken.isInt64();
  }

  /// Set Filename.
  bool setKeyName() {
    Value &ClientKeyName = ClientDict[KeyName];
    if (ClientKeyName.isStringLike())
      Filename = ClientKeyName.getStringRef();
    return ClientKeyName.isStringLike();
  }

  // Set LineOffset.
  bool setKeyLineOffset() {
    Value &ClientKeyLineOffset = ClientDict[KeyLineOffset];
    if (ClientKeyLineOffset.isInt64())
      LineOffset = uint64_t(ClientKeyLineOffset.getInt64());
    return ClientKeyLineOffset.isInt64();
  }

  /// Set ColOffset.
  bool setKeyColOffset() {
    Value &ClientKeyColOffset = ClientDict[KeyColOffset];
    if (ClientKeyColOffset.isInt64())
      ColOffset = uint64_t(ClientKeyColOffset.getInt64());
    return ClientKeyColOffset.isInt64();
  }

  /// Set CmdArgs (Optional).
  void setKeyCmdArgs() {
    Value &ClientKeyCmdArgs = ClientDict[KeyCmdArgs];
    if (ClientKeyCmdArgs.isArray())
      toStringArray(ClientKeyCmdArgs, CmdArgs);
    else
      CmdArgs.clear();
  }

  /// Set UnsavedFiles (Optional).
  void setKeyFiles() {
    /// key.files
    Value &ClientKeyFiles = ClientDict[KeyFiles];
    if (ClientKeyFiles.isArray())
      toUnsavedFilesArray(ClientKeyFiles, UnsavedFiles);
    else
      UnsavedFiles.clear();
  }

  /// Set the parameters needed to create CXIndex objects.
  void setCXIndexParameters() {
    Value &ClientKeyIndexOptions = ClientDict[KeyIndexOptions];
    IndexOpts = ClientKeyIndexOptions.isInt64()
                    ? (CXGlobalOptFlags)ClientKeyIndexOptions.getInt64()
                    : CXGlobalOpt_None;

    Value &ClientKeyIndexExcludeDeclsFromPCH =
        ClientDict[KeyIndexExcludeDeclsFromPCH];
    IndexExcludeDeclsFromPCH = ClientKeyIndexExcludeDeclsFromPCH.isBool()
                                   ? ClientKeyIndexExcludeDeclsFromPCH.getBool()
                                   : 0;

    Value &ClientKeyIndexDiagnostics = ClientDict[KeyIndexDiagnostics];
    IndexDiagnostics = ClientKeyIndexDiagnostics.isBool()
                           ? ClientKeyIndexDiagnostics.getBool()
                           : 0;
  }

  /// Set the parameters needed to control indexing options.
  void setKeyIndexingOptions() {
    Value &ClientKeyIndexingOptions = ClientDict[KeyIndexingOptions];
    IndexingOpts = ClientKeyIndexingOptions.isInt64()
                       ? (CXIndexOptFlags)ClientKeyIndexingOptions.getInt64()
                       : CXIndexOpt_None;
  }

  /// Set the parameters needed to generate (or skip) diagnostics.
  void setDiagnosticsParameters() {
    Value &ClientKeyDiagnosticsEnabled = ClientDict[KeyDiagnosticsEnabled];
    DoDiagnostics = ClientKeyDiagnosticsEnabled.isBool()
                        ? ClientKeyDiagnosticsEnabled.getBool()
                        : false;

    Value &ClientKeyDiagnosticsOptions = ClientDict[KeyDiagnosticsOptions];
    DiagnosticOpts = (CXDiagnosticDisplayOptions)(
        ClientKeyDiagnosticsOptions.isInt64()
            ? ClientKeyDiagnosticsOptions.getInt64()
            : 0);
    if (DiagnosticOpts)
      DoDiagnostics = true;
    if (DoDiagnostics && !DiagnosticOpts)
      DiagnosticOpts = (CXDiagnosticDisplayOptions)(
          CXDiagnostic_DisplaySourceLocation | CXDiagnostic_DisplayColumn |
          CXDiagnostic_DisplaySourceRanges | CXDiagnostic_DisplayOption);
  }

  /// Set the parameters passed to 'parseTranslationUnit' and friends.
  void setParseParameters() {
    Value &ParseOptions = ClientDict[KeyParseOptions];
    if (ParseOptions.isInt64())
      ParseOpts = ParseOptions.getInt64();
    else
      ParseOpts = CXTranslationUnit_DetailedPreprocessingRecord |
                  clang_defaultEditingTranslationUnitOptions();

    IncludeBrief =
        ParseOpts & CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;
  }

  /// Set the parameters passed to codeCompleteAt.
  void setCodeCompleteParameters() {
    Value &CodeCompleteOptions = ClientDict[KeyCodeCompleteOptions];
    if (CodeCompleteOptions.isInt64())
      CodeCompleteOpts = CodeCompleteOptions.getInt64();
    else
      CodeCompleteOpts = clang_defaultCodeCompleteOptions();

    Value &ClientKeySort = ClientDict[KeySort];
    DoSort = ClientKeySort.isBool() && ClientKeySort.getBool();
  }

  /// Set the parameters passed to 'reparseTranslationUnit'.
  void setReparseParameters(UniqueCXTranslationUnit &TU) {
    // FIXME: Expose this to clients.
    ReparseOpts = clang_defaultReparseOptions(TU);
  }

  /// Create a SafeCXIndex (after setting any user-provided parameters).
  SafeCXIndex createSafeCXIndex() {
    setCXIndexParameters();
    SafeCXIndex Idx{
        clang_createIndex(IndexExcludeDeclsFromPCH, IndexDiagnostics)};
    clang_CXIndex_setGlobalOptions(Idx, IndexOpts);
    return Idx;
  }

  Value handleRequestVersion() {
    /// RequestVersion ::= {}
    ///
    /// Version ::= {
    ///   key.version_major: int64,
    ///   key.version_minor: int64
    /// }
    return Value::dict({{KeyVersionMajor, int64_t(CINDEX_VERSION_MAJOR)},
                        {KeyVersionMinor, int64_t(CINDEX_VERSION_MINOR)}});
  }

  Value handleRequestFileComparison() {
    /// RequestFileComparison ::= {
    ///   key.lhs_token: int64,
    ///   key.rhs_token: int64
    /// }
    if (!setKeyLHSToken() || !setKeyRHSToken())
      return makeRequestInvalidError("Expected: two tokens");

    CXFile LHS, RHS;
    {
      LockGuard Guard{State->LocTokensLock};
      LHS = State->LocTokens[ReqLHSToken].File;
      RHS = State->LocTokens[ReqRHSToken].File;
    }

    return {bool(clang_File_isEqual(LHS, RHS))};
  }

  Value handleRequestSourceLocationSpelling() {
    /// RequestSourceLocationSpelling ::= {
    ///   key.token: int64,
    ///   key.manager: int64
    /// }
    if (!setKeyToken() || !setKeyManager())
      return makeRequestInvalidError("Expected: token and manager");

    CXSourceLocation Loc;
    {
      LockGuard Guard{State->LocTokensLock};
      assert(State->LocTokens.count(ReqToken) && "Bad token");
      Loc = State->LocTokens[ReqToken].SLoc;
    }

    LocTokenManager *LTM;
    {
      LockGuard Guard{State->CtxsLock};
      assert(State->Ctxs.count(ReqManager) && "Bad manager");
      LTM = &State->Ctxs[ReqManager]->getLocTokenManager();
    }

    return toSpellingLocationValue(Loc, *LTM);
  }

  // FIXME: Add support for code completion with existing TU's. Do this by
  // requiring clients to pass in a TU token.
  Value handleRequestCodeComplete() {
    if (RequestUID == RequestCodeCompleteOpen) {
      if (!setKeyName() || !setKeyLineOffset() || !setKeyColOffset())
        return makeRequestInvalidError("Expected: CodeComplete init");
    } else if (RequestUID == RequestCodeCompleteUpdate) {
      if (!setKeyLineOffset() || !setKeyColOffset() || !setKeyToken())
        return makeRequestInvalidError("Expected: CodeComplete request");
    }

    if (RequestUID == RequestCodeCompleteClose) {
      /// RequestCodeCompleteClose ::= { key.token: int64 }
      if (!setKeyToken())
        return makeRequestInvalidError("Expected: CodeComplete token");

      {
        LockGuard Guard{State->CtxsLock};
        State->Ctxs.erase(ReqToken);
        State->CtxTokenGen.recycleToken(ReqToken);
      }
      return {};
    }

    setKeyCmdArgs();
    setKeyFiles();
    setParseParameters();
    setDiagnosticsParameters();
    setCodeCompleteParameters();

    CodeCompletionContext *Ctx;
    if (RequestUID != RequestCodeCompleteOpen) {
      LockGuard Guard{State->CtxsLock};
      Ctx = static_cast<CodeCompletionContext *>(State->Ctxs[ReqToken].get());
      if (!Ctx)
        return makeRequestInvalidError("No CodeCompletionContext");
    }

    if (RequestUID == RequestCodeCompleteOpen) {
      /// RequestCodeCompleteOpen ::= {
      ///   key.loc.name: string,
      ///   key.loc.line: int64,
      ///   key.loc.column: int64,
      ///   key.sort: bool (Optional),
      ///   key.cmd.args: [string] (Optional),
      ///   key.files: [{ key.loc.name: string, key.buffer: data }] (Optional),
      ///   key.parse.options: int64 (Optional),
      ///   key.codecomplete.options: int64 (Optional),
      ///   key.index.options : int64 (Optional),
      ///   key.index.exclude_decls_pch : bool (Optional),
      ///   key.index.diagnostics : bool (Optional),
      ///   key.diagnostics.enabled : bool (Optional),
      ///   key.diagnostics.options : int64 (Optional)
      /// }
      SafeCXIndex Idx = createSafeCXIndex();

      CXErrorCode Err;
      UniqueCXTranslationUnit &TU = Idx.parseTranslationUnit(
          nullptr, CmdArgs.data(), CmdArgs.size(), nullptr, 0, ParseOpts, Err);
      if (Err != CXError_Success)
        return makeLibclangFailedError(Err);

      // FIXME: The fact that we **have to** re-parse a TU immediately after
      // parsing it seems like a bug. However, we fail the complete-macros.c
      // test if we skip this step.
      setReparseParameters(TU);
      Err = (CXErrorCode)clang_reparseTranslationUnit(TU, 0, nullptr,
                                                      ReparseOpts);
      if (Err != CXError_Success)
        return makeLibclangFailedError(Err);

      UniqueCXCodeCompleteResultsPtr Results{clang_codeCompleteAt(
          TU, Filename.data(), LineOffset, ColOffset, UnsavedFiles.data(),
          UnsavedFiles.size(), CodeCompleteOpts)};

      {
        LockGuard Guard{State->CtxsLock};
        ReqToken = State->CtxTokenGen.getUniqueToken();
        auto LTM = State->createLocTokenManager(ReqToken);
        auto NewCtx = make_unique<CodeCompletionContext>(
            std::move(LTM), std::move(Idx), std::move(Results));
        Ctx = NewCtx.get();
        State->Ctxs[ReqToken] = std::move(NewCtx);
      }

      return toValue(Ctx->Results, DoSort, IncludeBrief, DoDiagnostics,
                     DiagnosticOpts, Ctx->getLocTokenManager());

    } else if (RequestUID == RequestCodeCompleteUpdate) {
      /// RequestCodeCompleteUpdate ::= {
      ///   key.token: int64,
      ///   key.loc.line: int64,
      ///   key.loc.column: int64,
      ///   key.sort: bool (Optional),
      ///   key.files: [{ key.loc.name: string, key.buffer: data }] (Optional),
      ///   key.parse.options: int64 (Optional),
      ///   key.codecomplete.options: int64 (Optional),
      ///   key.diagnostics.enabled : bool (Optional),
      ///   key.diagnostics.options : int64 (Optional)
      /// }
      setReparseParameters(Ctx->Idx.getTU());
      int Err =
          clang_reparseTranslationUnit(Ctx->Idx.getTU(), UnsavedFiles.size(),
                                       UnsavedFiles.data(), ReparseOpts);
      if (Err)
        return makeRequestFailedError("Couldn't reparse TU");

      UniqueCXCodeCompleteResultsPtr Results{clang_codeCompleteAt(
          Ctx->Idx.getTU(), Filename.data(), LineOffset, ColOffset,
          UnsavedFiles.data(), UnsavedFiles.size(), CodeCompleteOpts)};

      Ctx->Results = std::move(Results);

      return toValue(Ctx->Results, DoSort, IncludeBrief, DoDiagnostics,
                     DiagnosticOpts, Ctx->getLocTokenManager());
    }

    return makeRequestInvalidError("Unknown code complete request");
  }

  Value handleRequestIndex() {
    if (RequestUID == RequestIndex) {
      /// RequestIndex ::= {
      ///   key.loc.name: string,
      ///   key.index.source.options: int64 (Optional),
      ///   key.cmd.args: [string] (Optional),
      ///   key.files: [{ key.loc.name: string, key.buffer: data }] (Optional),
      ///   key.parse.options: int64 (Optional),
      ///   key.index.options : int64 (Optional),
      ///   key.index.exclude_decls_pch : bool (Optional),
      ///   key.index.diagnostics : bool (Optional),
      ///   key.diagnostics.enabled : bool (Optional),
      ///   key.diagnostics.options : int64 (Optional)
      /// }
      if (!setKeyName())
        return makeRequestInvalidError("Expected: Indexing request");

      setKeyIndexingOptions();
      setKeyCmdArgs();
      setKeyFiles();
      setParseParameters();
      setDiagnosticsParameters();

      SafeCXIndex Idx = createSafeCXIndex();
      UniqueCXIndexAction &Act = Idx.createIndexAction();

      IndexingContext *Ctx;
      {
        LockGuard Guard{State->CtxsLock};
        ReqToken = State->CtxTokenGen.getUniqueToken();
        auto LTM = State->createLocTokenManager(ReqToken);
        auto NewCtx = make_unique<IndexingContext>(
            std::move(LTM), std::move(Idx), std::move(ClientDict[KeyFiles]),
            std::move(CmdArgs), std::move(UnsavedFiles), DoDiagnostics,
            DiagnosticOpts, ParseOpts);
        Ctx = NewCtx.get();
        State->Ctxs[ReqToken] = std::move(NewCtx);
      }

      LockGuard Guard{Ctx->SessionLock};
      std::thread Worker{driveIndexingSession, Ctx, (CXIndexAction)Act,
                         IndexingOpts};
      Ctx->Worker = std::move(Worker);

      return Ctx->awaitWorkerResponse(Guard);

    } else if (RequestUID == RequestIndexContinue) {
      /// RequestIndexContinue ::= {
      ///   key.token: int64,
      ///   key.indexing.do_abort: bool (Optional),
      ///   key.indexing.client_file: int64 (Optional),
      ///   key.indexing.client_ast_file: int64 (Optional),
      ///   key.indexing.client_container: int64 (Optional)
      /// }
      if (!setKeyToken())
        return makeRequestInvalidError("Expected: Indexing token");

      IndexingContext *Ctx;
      {
        LockGuard Guard{State->CtxsLock};
        Ctx = static_cast<IndexingContext *>(State->Ctxs[ReqToken].get());
        if (!Ctx)
          return makeRequestInvalidError("No indexing context");
      }

      if (ClientDict[KeyIndexingDoAbort].isBool())
        Ctx->ManagerResponse = std::move(ClientDict[KeyIndexingDoAbort]);
      else if (ClientDict[KeyIdxClientFile].isInt64())
        Ctx->ManagerResponse = std::move(ClientDict[KeyIdxClientFile]);
      else if (ClientDict[KeyIdxClientASTFile].isInt64())
        Ctx->ManagerResponse = std::move(ClientDict[KeyIdxClientASTFile]);
      else if (ClientDict[KeyIdxClientContainer].isInt64())
        Ctx->ManagerResponse = std::move(ClientDict[KeyIdxClientContainer]);
      else
        Ctx->ManagerResponse = Value(true);

      LockGuard Guard{Ctx->SessionLock};
      return Ctx->awaitWorkerResponse(Guard);

    } else if (RequestUID == RequestIndexClose) {
      /// RequestIndexClose ::= {
      ///   key.token: int64
      /// }
      if (!setKeyToken())
        return makeRequestInvalidError("Expected: Indexing token");

      {
        LockGuard Guard{State->CtxsLock};
        State->Ctxs.erase(ReqToken);
        State->CtxTokenGen.recycleToken(ReqToken);
      }
      return {};
    }

    return makeRequestInvalidError("Unknown indexing request");
  }

public:
  ResponseBuilder(ServiceState *State, Value::Dict &ClientDict,
                  StringRef RequestStr, CSUID RequestUID)
      : State(State), ClientDict(ClientDict), RequestStr(RequestStr),
        RequestUID(RequestUID) {}

  Value build() {
    /// FIXME: Check the codegen for this switch; optimize it.

    if (RequestUID == RequestVersion)
      return handleRequestVersion();

    if (RequestUID == RequestFileComparison)
      return handleRequestFileComparison();

    if (RequestUID == RequestSourceLocationSpelling)
      return handleRequestSourceLocationSpelling();

    if (RequestUID == RequestCodeCompleteOpen ||
        RequestUID == RequestCodeCompleteUpdate ||
        RequestUID == RequestCodeCompleteClose)
      return handleRequestCodeComplete();

    if (RequestUID == RequestIndex ||
        RequestUID == RequestIndexContinue ||
        RequestUID == RequestIndexClose)
      return handleRequestIndex();

    return makeRequestInvalidError("Unknown request");
  }
};

Value Service::handle(Value Request) {
  log("Handling incoming request:");
  log(Request);

  if (!Request.isDict())
    return makeRequestInvalidError("!Request.isDict()");

  Value::Dict &ClientDict = Request.getDict();
  Value &ClientKeyRequest = ClientDict[KeyRequest];
  if (!ClientKeyRequest.isStringLike())
    return makeRequestInvalidError("!ClientKeyRequest.isStringLike()");

  StringRef RequestStr = ClientKeyRequest.getStringRef();
  if (!CSUID::isValidIdentifier(RequestStr))
    return makeRequestInvalidError("!isValidIdentifier(RequestStr)");

  CSUID RequestUID{RequestStr};

  ResponseBuilder RB{State, ClientDict, RequestStr, RequestUID};
  return RB.build();
}

void Service::log(Value &V) {
  LockGuard Guard{State->LogLock};
  std::ofstream LogFile("/tmp/ClangXPC.log",
                        std::ofstream::out | std::ofstream::app);
  llvm::raw_os_ostream Log(LogFile);
  Log << "[ClangService] Value.structure: ";
  V.structure(Log);
  Log << "[ClangService] Value.dump     : ";
  V.dump(Log);
  Log.flush();
  LogFile.close();
}

void Service::log(StringRef Msg) {
  LockGuard Guard{State->LogLock};
  std::ofstream LogFile("/tmp/ClangXPC.log",
                        std::ofstream::out | std::ofstream::app);
  llvm::raw_os_ostream Log(LogFile);
  Log << "[ClangService] " << Msg << '\n';
  Log.flush();
  LogFile.close();
}

} // end namespace ClangService
