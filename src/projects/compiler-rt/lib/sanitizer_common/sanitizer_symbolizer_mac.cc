//===-- sanitizer_symbolizer_mac.cc ---------------------------------------===//
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
// Implementation of Mac-specific "atos" symbolizer.
//===----------------------------------------------------------------------===//

#include "sanitizer_platform.h"
#if SANITIZER_MAC

#include "sanitizer_symbolizer.h"
#include "sanitizer_symbolizer_mac.h"

#include <dlfcn.h>
#include <stdlib.h>

namespace __sanitizer {

bool AtosSymbolizer::StartSubprocessIfNotStarted() {
  if (fd_to_child_)
    return true;

  if (forkfailed_)
    return false;

  int main_process_pid = getpid();
  int fd;

  // Use forkpty to disable buffering in the new terminal.
  int pid = forkpty(&fd, 0, 0, 0);
  if (pid == -1) {
    // Fork() failed.
    Report("WARNING: failed to fork external symbolizer (errno: %d)\n", errno);
    forkfailed_ = true;
    return false;
  } else if (pid == 0) {
    // Child subprocess.

    // Make `atos` work in the iOS Simulator.
    unsetenv("DYLD_ROOT_PATH");

    char pid_str[16] = {0};
    internal_snprintf(pid_str, sizeof(pid_str), "%d", main_process_pid);
    execl(atos_path_, atos_path_, "-p", pid_str, (char *)0);
    internal__exit(1);
  }

  // Continue execution in parent process.
  fd_to_child_ = fd;

  // Disable echo in the new terminal.
  struct termios termflags;
  tcgetattr(fd_to_child_, &termflags);
  termflags.c_lflag &= ~ECHO;
  tcsetattr(fd_to_child_, TCSANOW, &termflags);

  return true;
}

static const char *ExtractToken(const char *str, const char *delimiter,
                                char **result) {
  const char *found_delimiter = internal_strstr(str, delimiter);
  uptr prefix_len =
      found_delimiter ? found_delimiter - str : internal_strlen(str);
  *result = (char *)InternalAlloc(prefix_len + 1);
  internal_memcpy(*result, str, prefix_len);
  (*result)[prefix_len] = '\0';
  const char *prefix_end = str + prefix_len;
  if (*prefix_end != '\0')
    prefix_end += internal_strlen(delimiter);
  return prefix_end;
}

static void DlAddrIntoStringBuffer(uptr addr, char *buffer, uptr size) {
  Dl_info info;
  int result = dladdr((const void *)addr, &info);
  if (result) {
    uptr offset = addr - (uptr)info.dli_saddr;
    internal_snprintf(buffer, size, "%s (in %s) + 0x%x\n", info.dli_sname,
                      info.dli_fname, offset);
  } else {
    internal_snprintf(buffer, size, "0x%x\n", addr);
  }
}

void AtosSymbolizer::ParseSymbolizedStackFrames(const char *str,
                                                SymbolizedStack *res) {
  // The line from `atos` is in of these formats:
  //   myfunction (in library.dylib) (sourcefile.c:17)
  //   myfunction (in library.dylib) + 0x1fe
  //   0xdeadbeef (in library.dylib)
  //   0xdeadbeef

  if (internal_strstr(str, "atos cannot examine process") ||
      internal_strstr(str, "unable to get permission to examine process") ||
      internal_strstr(str, "An admin user name and password is required")) {
    Report("atos returned: %s\n", str);
    fd_to_child_ = 0;
    forkfailed_ = true;

    DlAddrIntoStringBuffer(res->info.address, buffer_, kBufferSize);
    str = buffer_;
  }

  const char *newstr = str;
  newstr = ExtractToken(newstr, " (in", &res->info.function);

  char *extracted_module_name;
  newstr = ExtractToken(newstr, ") ", &extracted_module_name);
  InternalFree(extracted_module_name);

  if (newstr[0] == '(') {
    newstr++;
    newstr = ExtractToken(newstr, ":", &res->info.file);
    char *extracted_line_number;
    newstr = ExtractToken(newstr, ")", &extracted_line_number);
    res->info.line = internal_atoll(extracted_line_number);
    InternalFree(extracted_line_number);
  }
}

char *AtosSymbolizer::SendCommand(uptr addr, bool is_data,
                                  const char *module_name, uptr module_offset) {
  bool result = StartSubprocessIfNotStarted();

  uptr length;
  if (result && !forkfailed_) {
    length = internal_snprintf(buffer_, kBufferSize, "0x%zx\n", addr);
    uptr write_len;
    write_len = internal_write(fd_to_child_, buffer_, length);
    if (write_len == 0 || write_len == (uptr)-1) {
      Report("WARNING: Can't write to symbolizer at fd %d\n", fd_to_child_);
      fd_to_child_ = 0;
      forkfailed_ = true;
    }
  }

  length = 0;
  while (!forkfailed_ && true) {
    uptr just_read =
        internal_read(fd_to_child_, buffer_ + length, kBufferSize - length - 1);
    if (just_read == 0 || just_read == (uptr)-1) {
      Report("WARNING: Can't read from symbolizer at fd %d\n", fd_to_child_);
      fd_to_child_ = 0;
      forkfailed_ = true;
      break;
    }
    length += just_read;
    char *pos = internal_strstr(buffer_, "\n");
    if (length >= 1 && pos) {
      length = pos - buffer_;
      buffer_[length] = '\0';
      break;
    }
  }

  if (forkfailed_) {
    DlAddrIntoStringBuffer(addr, buffer_, kBufferSize);
  } else {
    buffer_[length] = '\0';
  }
  return buffer_;
}

} // namespace __sanitizer

#endif // SANITIZER_MAC
