#ifndef LLVM_CLANG_CLANGSERVICE_SUPPORT_VALUEUTILS_H
#define LLVM_CLANG_CLANGSERVICE_SUPPORT_VALUEUTILS_H

#include "clang-service/Support/Value.h"

#include "clang-c/Index.h"

#include "llvm/ADT/ArrayRef.h"

namespace ClangService {

/// Convert an array of C-style strings to a Value.
///
/// This API assumes that the lifetime of each string in \p Arr is **at least
/// as long** as the lifetime of the returned Value.
Value toStringArray(llvm::ArrayRef<const char *> Arr);

/// Parse a Value as an array of C-style strings. Return true if successful.
bool toStringArray(Value &V, llvm::SmallVectorImpl<const char *> &Out);

/// Convert an array of CXUnsavedFiles to a Value.
///
/// This API assumes that the lifetime of each CXUnsavedFile in \p Arr is **at
/// least as long** as the lifetime of the returned Value.
Value toUnsavedFilesArray(llvm::ArrayRef<CXUnsavedFile *> Arr);

/// Parse a Value as an array of CXUnsavedFiles. Return true if successful.
bool toUnsavedFilesArray(Value &V, llvm::SmallVectorImpl<CXUnsavedFile> &Out);

} // end namespace ClangService

#endif // LLVM_CLANG_CLANGSERVICE_SUPPORT_VALUEUTILS_H
