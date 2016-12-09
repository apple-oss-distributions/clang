#ifndef LLVM_CLANG_CINDEXTEST_LIBCLANG_IFACE_H
#define LLVM_CLANG_CINDEXTEST_LIBCLANG_IFACE_H

#include "clang-c/Index.h"

#include "clang-service/Client.h"
#include "clang-service/SafeLibclang.h"
#include "clang-service/Support/Value.h"

#include "llvm/ADT/STLExtras.h"

using namespace ClangService;
using namespace Libclang;

/// The following interfaces make code reuse possible when working with
/// libclang and the clang service.

class FileIFace {
public:
  virtual ~FileIFace() {}

  virtual bool exists() = 0;

  virtual bool isLibclangObject() = 0;

  virtual bool equals(FileIFace &Other) = 0;
};

class SourceLocationIFace {
public:
  virtual ~SourceLocationIFace() {}

  virtual std::unique_ptr<FileIFace>
  getSpellingLocation(unsigned *Line, unsigned *Column, unsigned *Offset) = 0;

  virtual bool isLibclangObject() = 0;

  virtual bool equals(SourceLocationIFace &Other) = 0;
};

class SourceRangeIFace {
public:
  virtual ~SourceRangeIFace() {}

  virtual std::unique_ptr<SourceLocationIFace> getStart() = 0;

  virtual std::unique_ptr<SourceLocationIFace> getEnd() = 0;
};

class DiagnosticIFace {
public:
  virtual ~DiagnosticIFace() {}

  virtual CXDiagnosticSeverity getSeverity() = 0;

  virtual CUniqueStr getFormattedDiagnostic(unsigned DisplayOpts) = 0;

  virtual std::unique_ptr<SourceLocationIFace> getLocation() = 0;

  virtual unsigned getNumFixIts() = 0;

  virtual std::pair<CUniqueStr, std::unique_ptr<SourceRangeIFace>>
  getFixIt(unsigned I) = 0;
};

class CompletionStringIFace {
public:
  virtual ~CompletionStringIFace() {}

  virtual unsigned getNumChunks() = 0;

  virtual CXCompletionChunkKind getChunkKind(unsigned I) = 0;

  virtual std::unique_ptr<CompletionStringIFace> getChunk(unsigned I) = 0;

  virtual CUniqueStr getChunkText(unsigned I) = 0;

  virtual unsigned getPriority() = 0;

  virtual CXAvailabilityKind getAvailability() = 0;

  virtual unsigned getNumAnnotations() = 0;

  virtual CUniqueStr getAnnotation(unsigned I) = 0;

  virtual std::pair<CUniqueStr, CXCursorKind> getParent() = 0;

  virtual CUniqueStr getBriefComment() = 0;
};

class CompletionResultIFace {
public:
  virtual ~CompletionResultIFace() {}

  virtual CXCursorKind getCursorKind() = 0;

  virtual std::unique_ptr<CompletionStringIFace> getCompletionString() = 0;
};


/// Some helper routines.

CUniqueStr wrapString(UniqueCXString &S) {
  const char *Str = clang_getCString(S) ? clang_getCString(S) : "";
  return CUniqueStr(strdup(Str));
}

CUniqueStr wrapString(Value &V) {
  if (V.isNull())
    return CUniqueStr(strdup(""));

  auto SR = V.getStringRef();
  return CUniqueStr(strndup(SR.data(), SR.size()));
}


/// The following classes derive the interfaces defined above to provide
/// libclang support.

class CXFileIFace : public FileIFace {
  CXFile File;

public:
  CXFileIFace(CXFile File) : File(File) {}

  virtual ~CXFileIFace() {}

  virtual bool exists() { return File; }

  virtual bool isLibclangObject() { return true; }

  virtual bool equals(FileIFace &Other) {
    return Other.isLibclangObject() &&
           clang_File_isEqual(File, static_cast<CXFileIFace &>(Other).File);
  }
};

class CXSourceLocationIFace : public SourceLocationIFace {
  CXSourceLocation Loc;

public:
  CXSourceLocationIFace(CXSourceLocation Loc) : Loc(Loc) {}

  virtual ~CXSourceLocationIFace() {}

  virtual std::unique_ptr<FileIFace>
  getSpellingLocation(unsigned *Line, unsigned *Column, unsigned *Offset) {
    CXFile File;
    clang_getSpellingLocation(Loc, &File, Line, Column, Offset);
    return llvm::make_unique<CXFileIFace>(File);
  }

  virtual bool isLibclangObject() { return true; }

  virtual bool equals(SourceLocationIFace &Other) {
    return Other.isLibclangObject() &&
           clang_equalLocations(
               Loc, static_cast<CXSourceLocationIFace &>(Other).Loc);
  }
};

class CXSourceRangeIFace : public SourceRangeIFace {
  CXSourceRange Range;

public:
  CXSourceRangeIFace(CXSourceRange Range) : Range(Range) {}

  virtual ~CXSourceRangeIFace() {}

  virtual std::unique_ptr<SourceLocationIFace> getStart() {
    return llvm::make_unique<CXSourceLocationIFace>(clang_getRangeStart(Range));
  }

  virtual std::unique_ptr<SourceLocationIFace> getEnd() {
    return llvm::make_unique<CXSourceLocationIFace>(clang_getRangeEnd(Range));
  }
};

class CXDiagnosticIFace : public DiagnosticIFace {
  CXDiagnostic Diag;

public:
  CXDiagnosticIFace(CXDiagnostic Diag) : Diag(Diag) {}

  virtual ~CXDiagnosticIFace() {}

  virtual CXDiagnosticSeverity getSeverity() {
    return clang_getDiagnosticSeverity(Diag);
  }

  virtual CUniqueStr getFormattedDiagnostic(unsigned DisplayOpts) {
    UniqueCXString S{clang_formatDiagnostic(Diag, DisplayOpts)};
    return wrapString(S);
  }

  virtual std::unique_ptr<SourceLocationIFace> getLocation() {
    return llvm::make_unique<CXSourceLocationIFace>(
        clang_getDiagnosticLocation(Diag));
  }

  virtual unsigned getNumFixIts() { return clang_getDiagnosticNumFixIts(Diag); }

  virtual std::pair<CUniqueStr, std::unique_ptr<SourceRangeIFace>>
  getFixIt(unsigned I) {
    CXSourceRange Range;
    UniqueCXString FixIt{clang_getDiagnosticFixIt(Diag, I, &Range)};
    return {wrapString(FixIt), llvm::make_unique<CXSourceRangeIFace>(Range)};
  }
};

class CXCompletionStringIFace : public CompletionStringIFace {
  CXCompletionString Completion;

public:
  CXCompletionStringIFace(CXCompletionString Completion)
      : Completion(Completion) {}

  virtual ~CXCompletionStringIFace() {}

  virtual unsigned getNumChunks() {
    return clang_getNumCompletionChunks(Completion);
  }

  virtual CXCompletionChunkKind getChunkKind(unsigned I) {
    return clang_getCompletionChunkKind(Completion, I);
  }

  virtual std::unique_ptr<CompletionStringIFace> getChunk(unsigned I) {
    return llvm::make_unique<CXCompletionStringIFace>(
        clang_getCompletionChunkCompletionString(Completion, I));
  }

  virtual CUniqueStr getChunkText(unsigned I) {
    UniqueCXString Text{clang_getCompletionChunkText(Completion, I)};
    return wrapString(Text);
  }

  virtual unsigned getPriority() {
    return clang_getCompletionPriority(Completion);
  }

  virtual CXAvailabilityKind getAvailability() {
    return clang_getCompletionAvailability(Completion);
  }

  virtual unsigned getNumAnnotations() {
    return clang_getCompletionNumAnnotations(Completion);
  }

  virtual CUniqueStr getAnnotation(unsigned I) {
    UniqueCXString Annotation{clang_getCompletionAnnotation(Completion, I)};
    return wrapString(Annotation);
  }

  virtual std::pair<CUniqueStr, CXCursorKind> getParent() {
    CXCursorKind ParentKind;
    UniqueCXString Parent{clang_getCompletionParent(Completion, &ParentKind)};
    return {wrapString(Parent), ParentKind};
  }

  virtual CUniqueStr getBriefComment() {
    UniqueCXString Brief{clang_getCompletionBriefComment(Completion)};
    return wrapString(Brief);
  }
};

class CXCompletionResultIFace : public CompletionResultIFace {
  CXCompletionResult &Result;

public:
  CXCompletionResultIFace(CXCompletionResult &Result) : Result(Result) {}

  virtual ~CXCompletionResultIFace() {}

  virtual CXCursorKind getCursorKind() { return Result.CursorKind; }

  virtual std::unique_ptr<CompletionStringIFace> getCompletionString() {
    return llvm::make_unique<CXCompletionStringIFace>(Result.CompletionString);
  }
};


/// The following classes derive the interfaces defined above to provide clang
/// service support.

namespace {
#define REGISTER_UID(Name, UIDStr) LazyCSUID Name{UIDStr};
#include "clang-service/ProtocolUIDs.inc"
}

class CSFileIFace : public FileIFace {
  Client &C;
  Value File;

public:
  CSFileIFace(Client &C, Value File) : C(C), File(std::move(File)) {}

  virtual ~CSFileIFace() {}

  virtual bool exists() { return File.getDict()[KeyToken].getInt64() > 0; }

  virtual bool isLibclangObject() { return false; }

  virtual bool equals(FileIFace &Other) {
    if (Other.isLibclangObject())
      return false;

    auto &OtherFile = static_cast<CSFileIFace &>(Other);

    Value FileEqual = C.request(Value::dict(
        {{KeyRequest, RequestFileComparison.c_str()},
         {KeyLHSToken, File.getDict()[KeyToken].getInt64()},
         {KeyRHSToken, OtherFile.File.getDict()[KeyToken].getInt64()}}));
    return FileEqual.getBool();
  }
};

class CSSourceLocationIFace : public SourceLocationIFace {
  Client &C;
  Value &Loc;

public:
  CSSourceLocationIFace(Client &C, Value &Loc) : C(C), Loc(Loc) {}

  virtual ~CSSourceLocationIFace() {}

  virtual std::unique_ptr<FileIFace>
  getSpellingLocation(unsigned *Line, unsigned *Column, unsigned *Offset) {
    Value ReqSpelling = Loc.clone();
    ReqSpelling.getDict()[KeyRequest] = RequestSourceLocationSpelling.c_str();
    Value Spelling = C.request(std::move(ReqSpelling));
    auto &Loc = Spelling.getDict();
    if (Line)
      *Line = (unsigned)Loc[KeyLineOffset].getInt64();
    if (Column)
      *Column = (unsigned)Loc[KeyColOffset].getInt64();
    if (Offset)
      *Offset = (unsigned)Loc[KeyOffset].getInt64();
    return llvm::make_unique<CSFileIFace>(C, Loc[KeyFile].clone());
  }

  virtual bool isLibclangObject() { return false; }

  virtual bool equals(SourceLocationIFace &Other) {
    if (Other.isLibclangObject())
      return false;

    unsigned l_line, l_col, l_off;
    unsigned r_line, r_col, r_off;
    auto &CSOther = static_cast<CSSourceLocationIFace &>(Other);
    auto l_file = getSpellingLocation(&l_line, &l_col, &l_off);
    auto r_file = CSOther.getSpellingLocation(&r_line, &r_col, &r_off);
    return l_line == r_line && l_col == r_col && l_off == r_off &&
           l_file->equals(*r_file);
  }
};

class CSSourceRangeIFace : public SourceRangeIFace {
  Client &C;
  Value::Dict &Range;

public:
  CSSourceRangeIFace(Client &C, Value &Range) : C(C), Range(Range.getDict()) {}

  virtual ~CSSourceRangeIFace() {}

  virtual std::unique_ptr<SourceLocationIFace> getStart() {
    return llvm::make_unique<CSSourceLocationIFace>(C, Range[KeyRangeStart]);
  }

  virtual std::unique_ptr<SourceLocationIFace> getEnd() {
    return llvm::make_unique<CSSourceLocationIFace>(C, Range[KeyRangeEnd]);
  }
};

class CSDiagnosticIFace : public DiagnosticIFace {
  Client &C;
  Value::Dict &Diag;

public:
  CSDiagnosticIFace(Client &C, Value &Diag) : C(C), Diag(Diag.getDict()) {}

  virtual ~CSDiagnosticIFace() {}

  virtual CXDiagnosticSeverity getSeverity() {
    return (CXDiagnosticSeverity)Diag[KeyDiagnosticSeverity].getInt64();
  }

  virtual CUniqueStr getFormattedDiagnostic(unsigned DisplayOpts) {
    return wrapString(Diag[KeyDiagnosticString]);
  }

  virtual std::unique_ptr<SourceLocationIFace> getLocation() {
    return llvm::make_unique<CSSourceLocationIFace>(
        C, Diag[KeyDiagnosticLocation]);
  }

  virtual unsigned getNumFixIts() {
    return (unsigned)Diag[KeyDiagnosticFixits].getArray().size();
  }

  virtual std::pair<CUniqueStr, std::unique_ptr<SourceRangeIFace>>
  getFixIt(unsigned I) {
    auto &Fixit = Diag[KeyDiagnosticFixits].getArray()[I].getArray();
    return {wrapString(Fixit[0]),
            llvm::make_unique<CSSourceRangeIFace>(C, Fixit[1])};
  }
};

class CSCompletionStringIFace : public CompletionStringIFace {
  Value::Dict &Completion;
  Value::Array &Chunks;

public:
  CSCompletionStringIFace(Value &Completion)
      : Completion(Completion.getDict()),
        Chunks(Completion.getDict()[KeyCodeCompleteCompletionStrChunks]
                   .getArray()) {}

  virtual ~CSCompletionStringIFace() {}

  virtual unsigned getNumChunks() { return Chunks.size(); }

  virtual CXCompletionChunkKind getChunkKind(unsigned I) {
    return (CXCompletionChunkKind)Chunks[I]
        .getDict()[KeyCodeCompleteCompletionStrKind]
        .getInt64();
  }

  virtual std::unique_ptr<CompletionStringIFace> getChunk(unsigned I) {
    return llvm::make_unique<CSCompletionStringIFace>(Chunks[I]);
  }

  virtual CUniqueStr getChunkText(unsigned I) {
    return wrapString(Chunks[I].getDict()[KeyCodeCompleteCompletionStrText]);
  }

  virtual unsigned getPriority() {
    return Completion[KeyCodeCompleteCompletionStrPriority].getInt64();
  }

  virtual CXAvailabilityKind getAvailability() {
    return (CXAvailabilityKind)
        Completion[KeyCodeCompleteCompletionStrAvailability]
            .getInt64();
  }

  virtual unsigned getNumAnnotations() {
    return Completion[KeyCodeCompleteCompletionStrAnnotations]
        .getArray()
        .size();
  }

  virtual CUniqueStr getAnnotation(unsigned I) {
    return wrapString(
        Completion[KeyCodeCompleteCompletionStrAnnotations].getArray()[I]);
  }

  virtual std::pair<CUniqueStr, CXCursorKind> getParent() {
    auto ParentKind =
        (CXCursorKind)Completion[KeyCodeCompleteCompletionStrParentKind]
            .getInt64();
    return {wrapString(Completion[KeyCodeCompleteCompletionStrParent]),
            ParentKind};
  }

  virtual CUniqueStr getBriefComment() {
    return wrapString(Completion[KeyCodeCompleteCompletionStrBrief]);
  }
};

class CSCompletionResultIFace : public CompletionResultIFace {
  Value::Dict &Result;

public:
  CSCompletionResultIFace(Value &Result) : Result(Result.getDict()) {}

  virtual ~CSCompletionResultIFace() {}

  virtual CXCursorKind getCursorKind() {
    return (CXCursorKind)Result[KeyCodeCompleteCompletionKind].getInt64();
  }

  virtual std::unique_ptr<CompletionStringIFace> getCompletionString() {
    return llvm::make_unique<CSCompletionStringIFace>(
        Result[KeyCodeCompleteCompletionStr]);
  }
};

#endif // LLVM_CLANG_CINDEXTEST_LIBCLANG_IFACE_H
