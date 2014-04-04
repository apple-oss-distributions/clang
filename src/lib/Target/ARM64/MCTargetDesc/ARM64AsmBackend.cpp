//===-- ARM64AsmBackend.cpp - ARM64 Assembler Backend ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "ARM64.h"
#include "MCTargetDesc/ARM64FixupKinds.h"
#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSectionMachO.h"
#include "llvm/Object/MachOFormat.h"
#include "llvm/Support/ErrorHandling.h"
using namespace llvm;

namespace {

class ARM64AsmBackend : public MCAsmBackend {
  static const unsigned PCRelFlagVal =
    MCFixupKindInfo::FKF_IsAlignedDownTo32Bits | MCFixupKindInfo::FKF_IsPCRel;
public:
  ARM64AsmBackend(const Target &T) : MCAsmBackend() {}

  unsigned getNumFixupKinds() const { return ARM64::NumTargetFixupKinds; }

  const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const {
    const static MCFixupKindInfo Infos[ARM64::NumTargetFixupKinds] = {
// This table *must* be in the order that the fixup_* kinds are defined in
// ARM64FixupKinds.h.
//
// Name                        Offset (bits) Size (bits)     Flags
{ "fixup_arm64_pcrel_adrlo",    5,            2, PCRelFlagVal },
{ "fixup_arm64_pcrel_adrhi",    5,           19, PCRelFlagVal },
{ "fixup_arm64_imm12",          2,           12, 0 },
{ "fixup_arm64_pcrel_branch14", 5,           14, PCRelFlagVal },
{ "fixup_arm64_pcrel_imm19",    5,           19, PCRelFlagVal },
{ "fixup_arm64_pcrel_branch26", 0,           26, PCRelFlagVal },
    };

    if (Kind < FirstTargetFixupKind)
      return MCAsmBackend::getFixupKindInfo(Kind);

    assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
           "Invalid kind!");
    return Infos[Kind - FirstTargetFixupKind];
  }

  bool mayNeedRelaxation(const MCInst &Inst) const;
  bool fixupNeedsRelaxation(const MCFixup &Fixup,
                            uint64_t Value,
                            const MCRelaxableFragment *DF,
                            const MCAsmLayout &Layout) const;
  void relaxInstruction(const MCInst &Inst, MCInst &Res) const;
  bool writeNopData(uint64_t Count, MCObjectWriter *OW) const;

  void HandleAssemblerFlag(MCAssemblerFlag Flag) {
    switch (Flag) {
    default: break;
    }
  }

  unsigned getPointerSize() const { return 8; }

};

} // end anonymous namespace

bool ARM64AsmBackend::mayNeedRelaxation(const MCInst &Inst) const {
  return false;
}

bool ARM64AsmBackend::fixupNeedsRelaxation(const MCFixup &Fixup,
                                           uint64_t Value,
                                           const MCRelaxableFragment *DF,
                                           const MCAsmLayout &Layout) const {
  // FIXME:  This isn't correct for ARM64. Just moving the "generic" logic
  // into the targets for now.
  //
  // Relax if the value is too big for a (signed) i8.
  return int64_t(Value) != int64_t(int8_t(Value));
}

void ARM64AsmBackend::relaxInstruction(const MCInst &Inst, MCInst &Res) const {
  assert(0 && "ARM64AsmBackend::relaxInstruction() unimplemented");
  return;
}

bool ARM64AsmBackend::writeNopData(uint64_t Count, MCObjectWriter *OW) const {
  // If the count is not 4-byte aligned, we must be writing data into the text
  // section (otherwise we have unaligned instructions, and thus have far
  // bigger problems), so just write zeros instead.
  if ((Count & 3) != 0) {
    for (uint64_t i = 0, e = (Count & 3); i != e; ++i)
      OW->Write8(0);
  }

  // We are properly aligned, so write NOPs as requested.
  Count /= 4;
  for (uint64_t i = 0; i != Count; ++i)
    OW->Write32(0xd503201f);
  return true;
}

namespace {

// FIXME: This should be in a separate file.
class DarwinARM64AsmBackend : public ARM64AsmBackend {
public:
  DarwinARM64AsmBackend(const Target &T) : ARM64AsmBackend(T) { }
  void applyFixup(const MCFixup &Fixup, char *Data, unsigned DataSize,
                  uint64_t Value) const;

  MCObjectWriter *createObjectWriter(raw_ostream &OS) const {
    return createARM64MachObjectWriter(OS,
                                       object::mach::CTM_ARM64,
                                       object::mach::CSARM64_ALL);
  }

  virtual bool doesSectionRequireSymbols(const MCSection &Section) const {
    // Any section for which the linker breaks things into atoms needs to
    // preserve symbols, including assembler local symbols, to identify
    // those atoms. These sections are:
    // Sections of type:
    //    S_CSTRING_LITERALS  (e.g. __cstring)
    //    S_LITERAL_POINTERS  (e.g.  objc selector pointers)
    //    S_16BYTE_LITERALS, S_8BYTE_LITERALS, S_4BYTE_LITERALS
    // Sections named:
    //    __TEXT,__eh_frame
    //    __TEXT,__ustring
    //    __DATA,__cfstring
    //    __DATA,__objc_classrefs
    //    __DATA,__objc_catlist
    //
    // FIXME: It would be better if the compiler used actual linker local
    // symbols for each of these sections rather than preserving what
    // are ostensibly assembler local symbols.
    const MCSectionMachO &SMO = static_cast<const MCSectionMachO&>(Section);
    return (SMO.getType() == MCSectionMachO::S_CSTRING_LITERALS ||
            SMO.getType() == MCSectionMachO::S_4BYTE_LITERALS ||
            SMO.getType() == MCSectionMachO::S_8BYTE_LITERALS ||
            SMO.getType() == MCSectionMachO::S_16BYTE_LITERALS ||
            SMO.getType() == MCSectionMachO::S_LITERAL_POINTERS ||
            (SMO.getSegmentName() == "__TEXT" &&
             (SMO.getSectionName() == "__eh_frame" ||
              SMO.getSectionName() == "__ustring")) ||
            (SMO.getSegmentName() == "__DATA" &&
             (SMO.getSectionName() == "__cfstring" ||
              SMO.getSectionName() == "__objc_classrefs" ||
              SMO.getSectionName() == "__objc_catlist")) );
  }
};

} // end anonymous namespace

/// getFixupKindNumBytes - The number of bytes the fixup may change.
static unsigned getFixupKindNumBytes(unsigned Kind) {
  switch (Kind) {
  default:
    assert (0 && "Unknown fixup kind!");

  case FK_Data_1:
  case ARM64::fixup_arm64_pcrel_adrlo:
    return 1;

  case FK_Data_2:
  case ARM64::fixup_arm64_imm12:
    return 2;

  case ARM64::fixup_arm64_pcrel_branch14:
  case ARM64::fixup_arm64_pcrel_adrhi:
  case ARM64::fixup_arm64_pcrel_imm19:
    return 3;

  case ARM64::fixup_arm64_pcrel_branch26:
  case FK_Data_4:
    return 4;

  case FK_Data_8:
    return 8;
  }
}

static uint64_t adjustFixupValue(unsigned Kind, uint64_t Value) {
  int64_t SignedValue = static_cast<int64_t>(Value);
  switch (Kind) {
  default:
    assert (0 && "Unknown fixup kind!");
  case ARM64::fixup_arm64_pcrel_adrlo:
    return (Value & 3);
  case ARM64::fixup_arm64_pcrel_adrhi:
  case ARM64::fixup_arm64_pcrel_imm19:
    // Signed 21-bit immediate
    if(SignedValue > 2097151 || SignedValue < -2097152)
      report_fatal_error("fixup value out of range");
    // Low two bits are not encoded.
    return (Value >> 2) & 0x7ffff;
  case ARM64::fixup_arm64_imm12:
    // Unsigned 12-bit immediate
    // NOTE: Because the relocation computation returns the full offset of the
    // symbol as the addend for an @PAGEOFF expression, we can't assert that
    // the immediate is in range here.
    return Value & 0xfff;
  case ARM64::fixup_arm64_pcrel_branch14:
    // Signed 16-bit immediate
    if (SignedValue > 32767 || SignedValue < -32768)
      report_fatal_error("fixup value out of range");
    // Low two bits are not encoded (4-byte alignment assumed).
    if (Value & 0x3)
      report_fatal_error("fixup not sufficiently aligned");
    return (Value >> 2) & 0x3fff;
  case ARM64::fixup_arm64_pcrel_branch26:
    // Signed 28-bit immediate
    if (SignedValue > 134217727 || SignedValue < -134217728)
      report_fatal_error("fixup value out of range");
    // Low two bits are not encoded (4-byte alignment assumed).
    if (Value & 0x3)
      report_fatal_error("fixup not sufficiently aligned");
    return (Value >> 2) & 0x3ffffff;
  case FK_Data_1:
  case FK_Data_2:
  case FK_Data_4:
  case FK_Data_8:
    return Value;
  }
}

void DarwinARM64AsmBackend::applyFixup(const MCFixup &Fixup, char *Data,
                                       unsigned DataSize,
                                       uint64_t Value) const {
  unsigned NumBytes = getFixupKindNumBytes(Fixup.getKind());
  if (!Value) return;           // Doesn't change encoding.
  MCFixupKindInfo Info = getFixupKindInfo(Fixup.getKind());
  // Apply any target-specific value adjustments.
  Value = adjustFixupValue(Fixup.getKind(), Value);

  // Shift the value into position.
  Value <<= Info.TargetOffset;

  unsigned Offset = Fixup.getOffset();
  assert(Offset + NumBytes <= DataSize && "Invalid fixup offset!");

  // For each byte of the fragment that the fixup touches, mask in the
  // bits from the fixup value.
  for (unsigned i = 0; i != NumBytes; ++i)
    Data[Offset + i] |= uint8_t((Value >> (i * 8)) & 0xff);
}

MCAsmBackend *llvm::createARM64AsmBackend(const Target &T, StringRef TT,
                                          StringRef CPU) {
  Triple TheTriple(TT);

  if (TheTriple.isOSDarwin())
    return new DarwinARM64AsmBackend(T);

  assert(0 && "Non-Darwin not supported on ARM64");
  return 0;
}
