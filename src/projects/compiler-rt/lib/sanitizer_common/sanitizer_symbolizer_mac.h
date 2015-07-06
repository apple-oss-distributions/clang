//===-- sanitizer_symbolizer_mac.h ------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is shared between AddressSanitizer and ThreadSanitizer
// run-time libraries.
// Header for Mac-specific "atos" symbolizer.
//===----------------------------------------------------------------------===//

#ifndef SANITIZER_SYMBOLIZER_MAC_H
#define SANITIZER_SYMBOLIZER_MAC_H

#include "sanitizer_platform.h"
#if SANITIZER_MAC

#include "sanitizer_allocator_internal.h"
#include "sanitizer_common.h"
#include "sanitizer_symbolizer.h"

#include <unistd.h>
#include <util.h>
#include <sys/errno.h>

namespace __sanitizer {

class AtosSymbolizer : public ExternalSymbolizerInterface {
 public:
  explicit AtosSymbolizer(const char *atos_path) : atos_path_(atos_path) {}

  void ParseSymbolizedStackFrames(const char *str, SymbolizedStack *res);
  char *SendCommand(uptr addr, bool is_data, const char *module_name,
                    uptr module_offset);

 private:
  bool StartSubprocessIfNotStarted();

  const char *atos_path_;
  int fd_to_child_ = 0;

  static const uptr kBufferSize = 16 * 1024;
  char buffer_[kBufferSize];
  bool forkfailed_ = false;
};

} // namespace __sanitizer

#endif // SANITIZER_MAC

#endif // SANITIZER_SYMBOLIZER_MAC_H
