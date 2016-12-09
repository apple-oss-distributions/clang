//===-- sanitizer_mac_spi.h ------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares subrutines implementing features that use SPI.
//
//===----------------------------------------------------------------------===//
#ifndef SANITIZER_MAC_SPI_H
#define SANITIZER_MAC_SPI_H
#if SANITIZER_MAC

namespace __sanitizer {

// Check if the operation can be performed under the sandbox.
bool sandbox_allows_to_perform(const char *operation);

void dynamic_interpose_add(void *wrapper, void *real);
void dynamic_interpose();

}  // namespace __sanitizer

#endif  // SANITIZER_MAC
#endif  // SANITIZER_MAC_SPI_H
