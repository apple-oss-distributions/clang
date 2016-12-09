#ifndef LLVM_CLANG_CINDEXTEST_SERVICE_H
#define LLVM_CLANG_CINDEXTEST_SERVICE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "clang-c/Index.h"

#include <stdio.h>

#ifdef __APPLE__
#define CLANG_HAVE_SERVICE
#endif

void PrintExtent(FILE *out, unsigned begin_line, unsigned begin_column,
                 unsigned end_line, unsigned end_column);

void PrintDiagnostic(CXDiagnostic diag);

void print_completion_string(CXCompletionString completion_string, FILE *file);

void print_completion_result(CXCompletionResult *completion_result, FILE *file);

void print_completion_contexts(unsigned long long contexts, FILE *file);

void print_container_kind(enum CXCursorKind containerKind,
                          int containerIsIncomplete,
                          CXCodeCompleteResults *results, FILE *file);

const char *
clang_getCompletionChunkKindSpelling(enum CXCompletionChunkKind Kind);

#ifdef CLANG_HAVE_SERVICE
int clangservice_perform_code_completion(
    const char *source_filename, unsigned complete_line,
    unsigned complete_column, const char *const *command_line_args,
    int num_command_line_args, struct CXUnsavedFile *unsaved_files,
    unsigned num_unsaved_files, unsigned parsing_options,
    unsigned completion_options, int timing_only, FILE *outfile);
#endif

#ifdef __cplusplus
}
#endif

#endif // LLVM_CLANG_CINDEXTEST_SERVICE_H
