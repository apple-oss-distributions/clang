#ifndef LLVM_CLANG_CLANGSERVICE_MESSAGING_H
#define LLVM_CLANG_CLANGSERVICE_MESSAGING_H

#include "llvm/ADT/StringRef.h"

#include <cstdint>

namespace ClangService {

class Value;

// FIXME: Share definitions with sourcekitd/Internal-XPC.h.
static const char *KeyInternalMsg = "internal_msg";
static const char *KeySemaEditorDelay = "semantic_editor_delay";
static const char *KeyTracingEnabled = "tracing_enabled";
static const char *KeyMsgResponse = "response";

enum class Message : char {
  Initialization,
  Notification,
  UIDSynchronization,
  TraceMessage
};

enum class ErrorKind : char {
  RequestInvalid,
  RequestFailed,
  RequestInterrupted,
  RequestCancelled,
  Fatal,
  NoError
};

ErrorKind getResponseErrorKind(Value &Response);

llvm::StringRef getResponseErrorString(Value &Response);

enum class IndexingStatus : int64_t {
  AbortQueryEvent,
  DiagnosticsEvent,
  MainFileEvent,
  PPIncludeEvent,
  ImportedASTEvent,
  StartedTUEvent,
  DeclarationEvent,
  EntityReferenceEvent,
  Done
};

} // end namespace ClangService

#endif // LLVM_CLANG_CLANGSERVICE_MESSAGING_H
