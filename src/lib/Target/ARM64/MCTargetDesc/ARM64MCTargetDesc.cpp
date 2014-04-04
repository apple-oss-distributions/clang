//===-- ARM64MCTargetDesc.cpp - ARM64 Target Descriptions -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides ARM64 specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "ARM64MCTargetDesc.h"
#include "ARM64MCAsmInfo.h"
#include "InstPrinter/ARM64InstPrinter.h"
#include "llvm/MC/MCCodeGenInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_INSTRINFO_MC_DESC
#include "ARM64GenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "ARM64GenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "ARM64GenRegisterInfo.inc"

using namespace llvm;


static MCInstrInfo *createARM64MCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitARM64MCInstrInfo(X);
  return X;
}

static MCSubtargetInfo *createARM64MCSubtargetInfo(StringRef TT, StringRef CPU,
                                                   StringRef FS) {
  MCSubtargetInfo *X = new MCSubtargetInfo();
  InitARM64MCSubtargetInfo(X, TT, CPU, FS);
  return X;
}

static MCRegisterInfo *createARM64MCRegisterInfo(StringRef Triple) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitARM64MCRegisterInfo(X, ARM64::LR);
  return X;
}

static MCAsmInfo *createARM64MCAsmInfo(const Target &T, StringRef TT) {
  MCAsmInfo *MAI = new ARM64MCAsmInfo();

   // Initial state of the frame pointer is SP.
  MachineLocation Dst(MachineLocation::VirtualFP);
  MachineLocation Src(ARM64::SP, 0);
  MAI->addInitialFrameState(0, Dst, Src);

  return MAI;
}

MCCodeGenInfo *createARM64MCCodeGenInfo(StringRef TT, Reloc::Model RM,
                                        CodeModel::Model CM,
                                        CodeGenOpt::Level OL) {
  MCCodeGenInfo *X = new MCCodeGenInfo();
  // ARM64 is always PIC.
  X->InitMCCodeGenInfo(Reloc::PIC_, CM, OL);
  return X;
}

static MCInstPrinter *createARM64MCInstPrinter(const Target &T,
                                               unsigned SyntaxVariant,
                                               const MCAsmInfo &MAI,
                                               const MCInstrInfo &MII,
                                               const MCRegisterInfo &MRI,
                                               const MCSubtargetInfo &STI) {
  if (SyntaxVariant == 0)
    return new ARM64InstPrinter(MAI, MII, MRI, STI);
  return 0;
}

static MCStreamer *createMCStreamer(const Target &T, StringRef TT,
                                    MCContext &Ctx, MCAsmBackend &TAB,
                                    raw_ostream &OS,
                                    MCCodeEmitter *Emitter,
                                    bool RelaxAll, bool NoExecStack) {
  Triple TheTriple(TT);

  if (TheTriple.isOSDarwin())
    return createMachOStreamer(Ctx, TAB, OS, Emitter, RelaxAll,
                               /*LabelSections*/true);

  llvm_unreachable("Unsupported object file format.");
  return NULL;
}


// Force static initialization.
extern "C" void LLVMInitializeARM64TargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn X(TheARM64Target, createARM64MCAsmInfo);

  // Register the MC codegen info.
  TargetRegistry::RegisterMCCodeGenInfo(TheARM64Target, createARM64MCCodeGenInfo);

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(TheARM64Target, createARM64MCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(TheARM64Target, createARM64MCRegisterInfo);

  // Register the MC subtarget info.
  TargetRegistry::RegisterMCSubtargetInfo(TheARM64Target,
                                          createARM64MCSubtargetInfo);

  // Register the asm backend.
  TargetRegistry::RegisterMCAsmBackend(TheARM64Target, createARM64AsmBackend);

  // Register the MC Code Emitter
  TargetRegistry::RegisterMCCodeEmitter(TheARM64Target, createARM64MCCodeEmitter);

  // Register the object streamer.
  TargetRegistry::RegisterMCObjectStreamer(TheARM64Target, createMCStreamer);

  // Register the MCInstPrinter.
  TargetRegistry::RegisterMCInstPrinter(TheARM64Target,
                                        createARM64MCInstPrinter);
}
