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
#include "sanitizer_common.h"
#include "sanitizer_mac_spi.h"
#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <os/trace.h>
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

struct dynamic_interpose_entry {
  void *wrapper_function;
  void *real_function;
};

const int kMaxDynamicInterposeEntries = 10;
static
dynamic_interpose_entry dynamic_interpose_entries[kMaxDynamicInterposeEntries];
static int dynamic_interpose_entry_count = 0;
static bool dynamic_interpose_called = false;

void dynamic_interpose_add(void *wrapper, void *real) {
  CHECK(!dynamic_interpose_called);
  CHECK_GT(kMaxDynamicInterposeEntries, dynamic_interpose_entry_count);
  if (!real) return;
  dynamic_interpose_entries[dynamic_interpose_entry_count] = {wrapper, real};
  dynamic_interpose_entry_count++;
}

extern "C" void dyld_dynamic_interpose(const mach_header *mh, void *array,
                                       size_t count);

static void dynamic_interpose_image_callback(const mach_header *mh,
                                             intptr_t vmaddr_slide) {
  if (mh == (mach_header *)&__dso_handle) return;  // Do not interpose ourself.

  dyld_dynamic_interpose(mh, (void *)dynamic_interpose_entries,
                         dynamic_interpose_entry_count);
}

void dynamic_interpose() {
  CHECK(!dynamic_interpose_called);  // Must not be called twice.
  dynamic_interpose_called = true;
  if (dynamic_interpose_entry_count > 0) {
    _dyld_register_func_for_add_image(&dynamic_interpose_image_callback);
  }
}

}  // namespace __sanitizer

#endif  // SANITIZER_MAC
