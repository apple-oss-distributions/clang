//===-- sanitizer_mac_spi.cc ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains primitives that rely on SPI.
//===----------------------------------------------------------------------===//

#include "sanitizer_platform.h"
#if SANITIZER_MAC
#include <sys/types.h>
#include <unistd.h>

// Forward declare SPI from sandbox/private.h.
extern "C" {
enum sandbox_filter_type {
  SANDBOX_FILTER_NONE,
  SANDBOX_FILTER_PATH,
  SANDBOX_FILTER_GLOBAL_NAME,
  SANDBOX_FILTER_LOCAL_NAME,
  SANDBOX_FILTER_APPLEEVENT_DESTINATION,
  SANDBOX_FILTER_RIGHT_NAME,
  SANDBOX_FILTER_PREFERENCE_DOMAIN,
  SANDBOX_FILTER_KEXT_BUNDLE_ID,
  SANDBOX_FILTER_INFO_TYPE,
  SANDBOX_FILTER_NOTIFICATION,
};
extern const enum sandbox_filter_type SANDBOX_CHECK_NO_REPORT;
extern const enum sandbox_filter_type SANDBOX_CHECK_CANONICAL;
int sandbox_check(pid_t pid, const char *operation,
                  enum sandbox_filter_type type, ...);
}

namespace __sanitizer {
// Use the sandboxing SPI to check if we are allowed to perform an operation.
bool sandbox_allows_to_perform(const char *operation) {
  const enum sandbox_filter_type filter =
    (enum sandbox_filter_type)(SANDBOX_FILTER_NONE | SANDBOX_CHECK_NO_REPORT);
  if ((&sandbox_check != nullptr) &&
      (sandbox_check(getpid(), operation, filter) > 0) ) {
    return false;
  }
  return true;
}

}  // namespace __sanitizer

#endif  // SANITIZER_MAC
