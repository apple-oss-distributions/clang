//===-- ARM64FixupKinds.h - ARM64 Specific Fixup Entries --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ARM64FIXUPKINDS_H
#define LLVM_ARM64FIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace ARM64 {

enum Fixups {
  // fixup_arm64_pcrel_adrlo - The low two bits of a 21-bit pc-relative
  // immediate.
  fixup_arm64_pcrel_adrlo = FirstTargetFixupKind,

  // fixup_arm64_pcrel_adrhi - The high 19 bits of a 21-bit pc-relative
  // immediate. Always used as part of an adrlo/adrhi pair.
  fixup_arm64_pcrel_adrhi,

  // fixup_arm64_imm12 - 12-bit fixup for add/sub instructions.
  //     No alignment adjustment. All value bits are encoded.
  fixup_arm64_imm12,

  // fixup_arm64_pcrel_imm14 - The high 14 bits of a 21-bit pc-relative
  // immediate.
  fixup_arm64_pcrel_branch14,

  // fixup_arm64_pcrel_imm19 - The high 19 bits of a 21-bit pc-relative
  // immediate. Same encoding as fixup_arm64_pcrel_adrhi, except this
  // is not used as part of a lo/hi pair and thus generates relocations
  // directly when necessary.
  fixup_arm64_pcrel_imm19,

  // fixup_arm64_pcrel_branch26 - The high 26 bits of a 28-bit pc-relative
  // immediate.
  fixup_arm64_pcrel_branch26,

  // Marker
  LastTargetFixupKind,
  NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
};

} // end namespace ARM64
} // end namespace llvm

#endif
