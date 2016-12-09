#include "service.h"
#include "libclang_iface.h"

#ifdef CLANG_HAVE_SERVICE
#include "clang-service/Messaging.h"
#include "clang-service/Support/InProcessClient.h"
#include "clang-service/Support/Value.h"
#include "clang-service/Support/ValueUtils.h"
#endif

void PrintDiagnosticImpl(DiagnosticIFace &Diagnostic, FILE *out) {
  if (Diagnostic.getSeverity() == CXDiagnostic_Ignored)
    return;

  unsigned display_opts =
      CXDiagnostic_DisplaySourceLocation | CXDiagnostic_DisplayColumn |
      CXDiagnostic_DisplaySourceRanges | CXDiagnostic_DisplayOption;

  CUniqueStr FormattedStr = Diagnostic.getFormattedDiagnostic(display_opts);
  fprintf(out, "%s\n", FormattedStr.get());

  auto loc = Diagnostic.getLocation();
  auto file = loc->getSpellingLocation(nullptr, nullptr, nullptr);
  if (!file->exists())
    return;

  unsigned num_fixits = Diagnostic.getNumFixIts();
  fprintf(stderr, "Number FIX-ITs = %d\n", (int)num_fixits);

  for (unsigned i = 0; i != num_fixits; ++i) {
    std::pair<CUniqueStr, std::unique_ptr<SourceRangeIFace>> FixItResult =
        Diagnostic.getFixIt(i);

    char *insertion_text = FixItResult.first.get();
    SourceRangeIFace &range = *FixItResult.second;

    auto start = range.getStart();
    auto end = range.getEnd();

    unsigned start_line, start_column, end_line, end_column;

    auto start_file =
        start->getSpellingLocation(&start_line, &start_column, nullptr);

    auto end_file = end->getSpellingLocation(&end_line, &end_column, nullptr);

    if (start->equals(*end)) {
      /* Insertion. */
      if (start_file->equals(*file))
        fprintf(out, "FIX-IT: Insert \"%s\" at %d:%d\n", insertion_text,
                start_line, start_column);
    } else if (strcmp(insertion_text, "") == 0) {
      /* Removal. */
      if (start_file->equals(*file) && end_file->equals(*file)) {
        fprintf(out, "FIX-IT: Remove ");
        PrintExtent(out, start_line, start_column, end_line, end_column);
        fprintf(out, "\n");
      }
    } else {
      /* Replacement. */
      if (start_file->equals(*end_file)) {
        fprintf(out, "FIX-IT: Replace ");
        PrintExtent(out, start_line, start_column, end_line, end_column);
        fprintf(out, " with \"%s\"\n", insertion_text);
      }
    }
  }
}

void print_completion_string_impl(CompletionStringIFace &Str, FILE *file) {
  unsigned N = Str.getNumChunks();
  for (unsigned I = 0; I != N; ++I) {
    CXCompletionChunkKind Kind = Str.getChunkKind(I);

    if (Kind == CXCompletionChunk_Optional) {
      fprintf(file, "{Optional ");
      auto SubChunk = Str.getChunk(I);
      print_completion_string_impl(*SubChunk, file);
      fprintf(file, "}");
      continue;
    }

    if (Kind == CXCompletionChunk_VerticalSpace) {
      fprintf(file, "{VerticalSpace  }");
      continue;
    }

    CUniqueStr cstr = Str.getChunkText(I);
    fprintf(file, "{%s %s}", clang_getCompletionChunkKindSpelling(Kind),
            cstr ? cstr.get() : "");
  }
}

void print_completion_result_impl(CompletionResultIFace &Result, FILE *file) {
  UniqueCXString ks{clang_getCursorKindSpelling(Result.getCursorKind())};
  fprintf(file, "%s:", clang_getCString(ks));

  auto CompletionString = Result.getCompletionString();
  print_completion_string_impl(*CompletionString, file);

  fprintf(file, " (%u)", CompletionString->getPriority());

  switch (CompletionString->getAvailability()) {
  case CXAvailability_Available:
    break;

  case CXAvailability_Deprecated:
    fprintf(file, " (deprecated)");
    break;

  case CXAvailability_NotAvailable:
    fprintf(file, " (unavailable)");
    break;

  case CXAvailability_NotAccessible:
    fprintf(file, " (inaccessible)");
    break;
  }

  unsigned annotationCount = CompletionString->getNumAnnotations();
  if (annotationCount) {
    unsigned i;
    fprintf(file, " (");
    for (i = 0; i < annotationCount; ++i) {
      if (i != 0)
        fprintf(file, ", ");
      CUniqueStr Annotation = CompletionString->getAnnotation(i);
      fprintf(file, "\"%s\"", Annotation.get());
    }
    fprintf(file, ")");
  }

  if (!getenv("CINDEXTEST_NO_COMPLETION_PARENTS")) {
    auto CompletionParent = CompletionString->getParent();
    const char *ParentName = CompletionParent.first.get();
    CXCursorKind ParentKind = CompletionParent.second;

    if (ParentKind != CXCursor_NotImplemented) {
      UniqueCXString KindSpelling{clang_getCursorKindSpelling(ParentKind)};
      fprintf(file, " (parent: %s '%s')", clang_getCString(KindSpelling),
              ParentName);
    }
  }

  CUniqueStr BriefComment = CompletionString->getBriefComment();
  if (strlen(BriefComment.get())) {
    fprintf(file, "(brief comment: %s)", BriefComment.get());
  }

  fprintf(file, "\n");
}

void print_container_kind_impl(CXCursorKind containerKind,
                               int containerIsIncomplete, CUniqueStr USR,
                               CUniqueStr selectorString, FILE *file) {
  if (containerKind == CXCursor_InvalidCode)
    return;

  /* We have found a container */
  UniqueCXString containerKindSpelling{
      clang_getCursorKindSpelling(containerKind)};
  fprintf(file, "Container Kind: %s\n",
          clang_getCString(containerKindSpelling));

  if (containerIsIncomplete) {
    fprintf(file, "Container is incomplete\n");
  } else {
    fprintf(file, "Container is complete\n");
  }

  fprintf(file, "Container USR: %s\n", USR.get());

  if (strlen(selectorString.get()) > 0)
    fprintf(file, "Objective-C selector: %s\n", selectorString.get());
}

extern "C" {

void PrintExtent(FILE *out, unsigned begin_line, unsigned begin_column,
                 unsigned end_line, unsigned end_column) {
  fprintf(out, "[%d:%d - %d:%d]", begin_line, begin_column, end_line,
          end_column);
}

void PrintDiagnostic(CXDiagnostic diag) {
  CXDiagnosticIFace diagnostic_wrapper{diag};
  PrintDiagnosticImpl(diagnostic_wrapper, stderr);
}

void print_completion_string(CXCompletionString completion_string, FILE *file) {
  CXCompletionStringIFace completion_string_wrapper{completion_string};
  print_completion_string_impl(completion_string_wrapper, file);
}

void print_completion_result(CXCompletionResult *completion_result,
                             FILE *file) {
  CXCompletionResultIFace completion_result_wrapper{*completion_result};
  print_completion_result_impl(completion_result_wrapper, file);
}

void print_completion_contexts(unsigned long long contexts, FILE *file) {
  fprintf(file, "Completion contexts:\n");
  if (contexts == CXCompletionContext_Unknown) {
    fprintf(file, "Unknown\n");
  }
  if (contexts & CXCompletionContext_AnyType) {
    fprintf(file, "Any type\n");
  }
  if (contexts & CXCompletionContext_AnyValue) {
    fprintf(file, "Any value\n");
  }
  if (contexts & CXCompletionContext_ObjCObjectValue) {
    fprintf(file, "Objective-C object value\n");
  }
  if (contexts & CXCompletionContext_ObjCSelectorValue) {
    fprintf(file, "Objective-C selector value\n");
  }
  if (contexts & CXCompletionContext_CXXClassTypeValue) {
    fprintf(file, "C++ class type value\n");
  }
  if (contexts & CXCompletionContext_DotMemberAccess) {
    fprintf(file, "Dot member access\n");
  }
  if (contexts & CXCompletionContext_ArrowMemberAccess) {
    fprintf(file, "Arrow member access\n");
  }
  if (contexts & CXCompletionContext_ObjCPropertyAccess) {
    fprintf(file, "Objective-C property access\n");
  }
  if (contexts & CXCompletionContext_EnumTag) {
    fprintf(file, "Enum tag\n");
  }
  if (contexts & CXCompletionContext_UnionTag) {
    fprintf(file, "Union tag\n");
  }
  if (contexts & CXCompletionContext_StructTag) {
    fprintf(file, "Struct tag\n");
  }
  if (contexts & CXCompletionContext_ClassTag) {
    fprintf(file, "Class name\n");
  }
  if (contexts & CXCompletionContext_Namespace) {
    fprintf(file, "Namespace or namespace alias\n");
  }
  if (contexts & CXCompletionContext_NestedNameSpecifier) {
    fprintf(file, "Nested name specifier\n");
  }
  if (contexts & CXCompletionContext_ObjCInterface) {
    fprintf(file, "Objective-C interface\n");
  }
  if (contexts & CXCompletionContext_ObjCProtocol) {
    fprintf(file, "Objective-C protocol\n");
  }
  if (contexts & CXCompletionContext_ObjCCategory) {
    fprintf(file, "Objective-C category\n");
  }
  if (contexts & CXCompletionContext_ObjCInstanceMessage) {
    fprintf(file, "Objective-C instance method\n");
  }
  if (contexts & CXCompletionContext_ObjCClassMessage) {
    fprintf(file, "Objective-C class method\n");
  }
  if (contexts & CXCompletionContext_ObjCSelectorName) {
    fprintf(file, "Objective-C selector name\n");
  }
  if (contexts & CXCompletionContext_MacroName) {
    fprintf(file, "Macro name\n");
  }
  if (contexts & CXCompletionContext_NaturalLanguage) {
    fprintf(file, "Natural language\n");
  }
}

void print_container_kind(CXCursorKind containerKind,
                          int containerIsIncomplete,
                          CXCodeCompleteResults *results, FILE *file) {
  UniqueCXString containerUSR{clang_codeCompleteGetContainerUSR(results)};
  UniqueCXString objCSelector{clang_codeCompleteGetObjCSelector(results)};
  print_container_kind_impl(containerKind, containerIsIncomplete,
                            wrapString(containerUSR), wrapString(objCSelector),
                            file);
}

const char *
clang_getCompletionChunkKindSpelling(CXCompletionChunkKind Kind) {
  switch (Kind) {
  case CXCompletionChunk_Optional:
    return "Optional";
  case CXCompletionChunk_TypedText:
    return "TypedText";
  case CXCompletionChunk_Text:
    return "Text";
  case CXCompletionChunk_Placeholder:
    return "Placeholder";
  case CXCompletionChunk_Informative:
    return "Informative";
  case CXCompletionChunk_CurrentParameter:
    return "CurrentParameter";
  case CXCompletionChunk_LeftParen:
    return "LeftParen";
  case CXCompletionChunk_RightParen:
    return "RightParen";
  case CXCompletionChunk_LeftBracket:
    return "LeftBracket";
  case CXCompletionChunk_RightBracket:
    return "RightBracket";
  case CXCompletionChunk_LeftBrace:
    return "LeftBrace";
  case CXCompletionChunk_RightBrace:
    return "RightBrace";
  case CXCompletionChunk_LeftAngle:
    return "LeftAngle";
  case CXCompletionChunk_RightAngle:
    return "RightAngle";
  case CXCompletionChunk_Comma:
    return "Comma";
  case CXCompletionChunk_ResultType:
    return "ResultType";
  case CXCompletionChunk_Colon:
    return "Colon";
  case CXCompletionChunk_SemiColon:
    return "SemiColon";
  case CXCompletionChunk_Equal:
    return "Equal";
  case CXCompletionChunk_HorizontalSpace:
    return "HorizontalSpace";
  case CXCompletionChunk_VerticalSpace:
    return "VerticalSpace";
  }

  return "Unknown";
}

#ifdef CLANG_HAVE_SERVICE
int clangservice_perform_code_completion(
    const char *source_filename, unsigned complete_line,
    unsigned complete_column, const char *const *command_line_args,
    int num_command_line_args, struct CXUnsavedFile *unsaved_files,
    unsigned num_unsaved_files, unsigned parsing_options,
    unsigned completion_options, int timing_only, FILE *outfile) {
  auto C = llvm::make_unique<InProcessClient>();

  unsigned display_opts =
      CXDiagnostic_DisplaySourceLocation | CXDiagnostic_DisplayColumn |
      CXDiagnostic_DisplaySourceRanges | CXDiagnostic_DisplayOption;

  Value OpenReq = Value::dict(
      {{KeyRequest, RequestCodeCompleteOpen.c_str()},
       {KeyName, source_filename},
       {KeyLineOffset, int64_t(complete_line)},
       {KeyColOffset, int64_t(complete_column)},
       {KeyCmdArgs, toStringArray(llvm::ArrayRef<const char *>(
                        command_line_args, num_command_line_args))},
       {KeyFiles, toUnsavedFilesArray(llvm::ArrayRef<CXUnsavedFile *>(
                      &unsaved_files, num_unsaved_files))},
       {KeySort, !timing_only},
       {KeyParseOptions, int64_t(parsing_options)},
       {KeyCodeCompleteOptions, int64_t(completion_options)},
       {KeyDiagnosticsEnabled, true},
       {KeyDiagnosticsOptions, int64_t(display_opts)}});

  Value OpenResp = C->request(std::move(OpenReq));
  if (getResponseErrorKind(OpenResp) != ErrorKind::NoError) {
    fprintf(stderr, "ClangService error: %s\n",
            getResponseErrorString(OpenResp).data());
    return -1;
  }

  Value::Dict &RespDict = OpenResp.getDict();

  int64_t Token = RespDict[KeyToken].getInt64();

  Value::Array &Results = RespDict[KeyCodeCompleteResults].getArray();
  if (!timing_only) {
    for (Value &Result : Results) {
      CSCompletionResultIFace CSResult{Result};
      print_completion_result_impl(CSResult, outfile);
    }
  }

  if (RespDict[KeyDiagnostics].isArray()) {
    auto &ADiagnostics = RespDict[KeyDiagnostics].getArray();
    for (auto &Diagnostic : ADiagnostics) {
      CSDiagnosticIFace Diag{*C, Diagnostic};
      PrintDiagnosticImpl(Diag, stderr);
    }
  }

  print_completion_contexts(
      (unsigned long long)RespDict[KeyCodeCompleteContexts].getInt64(),
      outfile);

  auto CK = (CXCursorKind)RespDict[KeyCodeCompleteContainerKind].getInt64();
  print_container_kind_impl(
      CK, RespDict[KeyCodeCompleteContainerIncomplete].getBool(),
      wrapString(RespDict[KeyCodeCompleteContainerUSR]),
      wrapString(RespDict[KeyCodeCompleteObjCSelector]), outfile);

  Value CloseResp = C->request(Value::dict(
      {{KeyRequest, RequestCodeCompleteClose.c_str()}, {KeyToken, Token}}));

  return CloseResp.isNull() ? 0 : -1;
}
#endif

} // end extern "C"
