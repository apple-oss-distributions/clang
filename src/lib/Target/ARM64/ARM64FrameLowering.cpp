//===- ARM64FrameLowering.cpp - ARM64 Frame Lowering -----------*- C++ -*-====//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the ARM64 implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "frame-info"
#include "ARM64FrameLowering.h"
#include "ARM64InstrInfo.h"
#include "ARM64MachineFunctionInfo.h"
#include "ARM64Subtarget.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

static cl::opt<bool>
EnableRedZone("arm64-redzone",
              cl::desc("enable use of redzone on ARM64"),
              cl::init(false), cl::Hidden);

STATISTIC(NumRedZoneFunctions, "Number of functions using red zone");

static unsigned estimateStackSize(MachineFunction &MF) {
  const MachineFrameInfo *FFI = MF.getFrameInfo();
  int Offset = 0;
  for (int i = FFI->getObjectIndexBegin(); i != 0; ++i) {
    int FixedOff = -FFI->getObjectOffset(i);
    if (FixedOff > Offset) Offset = FixedOff;
  }
  for (unsigned i = 0, e = FFI->getObjectIndexEnd(); i != e; ++i) {
    if (FFI->isDeadObjectIndex(i))
      continue;
    Offset += FFI->getObjectSize(i);
    unsigned Align = FFI->getObjectAlignment(i);
    // Adjust to alignment boundary
    Offset = (Offset+Align-1)/Align*Align;
  }
  // This does not include the 16 bytes used for fp and lr.
  return (unsigned)Offset;
}

bool ARM64FrameLowering::canUseRedZone(const MachineFunction &MF) const {
  if (!EnableRedZone)
    return false;
  // Don't use the red zone if the function explicitly asks us not to.
  // This is typically used for kernel code.
  if (MF.getFunction()->getAttributes().
      hasAttribute(AttributeSet::FunctionIndex, Attribute::NoRedZone))
    return false;

  const MachineFrameInfo *MFI = MF.getFrameInfo();
  const ARM64FunctionInfo *AFI = MF.getInfo<ARM64FunctionInfo>();
  unsigned NumBytes = AFI->getLocalStackSize();

  // Note: currently hasFP() is always true for hasCalls(), but that's an
  // implementation detail of the current code, not a strict requirement,
  // so stay safe here and check both.
  if (MFI->hasCalls() || hasFP(MF) || NumBytes > 128)
    return false;
  return true;
}

/// hasFP - Return true if the specified function should have a dedicated frame
/// pointer register.
bool ARM64FrameLowering::hasFP(const MachineFunction &MF) const {
    const MachineFrameInfo *MFI = MF.getFrameInfo();
    const TargetRegisterInfo *RegInfo = MF.getTarget().getRegisterInfo();
    assert(!RegInfo->needsStackRealignment(MF)
           && "No stack realignment on ARM64!");

    return (MFI->hasCalls() ||
            MFI->hasVarSizedObjects() ||
            MFI->isFrameAddressTaken());
}

/// hasReservedCallFrame - Under normal circumstances, when a frame pointer is
/// not required, we reserve argument space for call sites in the function
/// immediately on entry to the current function.  This eliminates the need for
/// add/sub sp brackets around call sites.  Returns true if the call frame is
/// included as part of the stack frame.
bool ARM64FrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  return !MF.getFrameInfo()->hasVarSizedObjects();
}

void ARM64FrameLowering::emitCalleeSavedFrameMoves(MachineFunction &MF,
                                                   MCSymbol *Label,
                                                   unsigned FramePtr) const {
  MachineFrameInfo *MFI = MF.getFrameInfo();
  MachineModuleInfo &MMI = MF.getMMI();

  // Add callee saved registers to move list.
  const std::vector<CalleeSavedInfo> &CSI = MFI->getCalleeSavedInfo();
  if (CSI.empty()) return;

  std::vector<MachineMove> &Moves = MMI.getFrameMoves();
  const DataLayout *TD = MF.getTarget().getDataLayout();
  bool HasFP = hasFP(MF);

  // Calculate amount of bytes used for return address storing.
  int stackGrowth = -TD->getPointerSize(0);

  // Calculate offsets.
  int64_t saveOffset = (HasFP ? 2 : 1) * stackGrowth;
  for (std::vector<CalleeSavedInfo>::const_iterator
         I = CSI.begin(), E = CSI.end(); I != E; ++I) {
    unsigned Reg = I->getReg();
    int64_t Offset = MFI->getObjectOffset(I->getFrameIdx()) -
      getOffsetOfLocalArea() + saveOffset;

    // Don't output a new machine move if we're re-saving the frame
    // pointer. This happens when the PrologEpilogInserter has inserted an extra
    // "PUSH" of the frame pointer -- the "emitPrologue" method automatically
    // generates one when frame pointers are used. If we generate a "machine
    // move" for this extra "PUSH", the linker will lose track of the fact that
    // the frame pointer should have the value of the first "PUSH" when it's
    // trying to unwind.
    //
    if (HasFP && FramePtr == Reg)
      continue;

    MachineLocation CSDst(MachineLocation::VirtualFP, Offset);
    MachineLocation CSSrc(Reg);
    Moves.push_back(MachineMove(Label, CSDst, CSSrc));
  }
}

// Encode compact unwind stack adjustment for frameless functions.
// See UNWIND_ARM64_FRAMELESS_STACK_SIZE_MASK in compact_unwind_encoding.h.
// The stack size always needs to be 16 byte aligned.
static uint32_t encodeStackAdjustment(uint32_t StackSize) {
  return (StackSize / 16) << 12;
}

uint32_t ARM64FrameLowering::
getCompactUnwindEncoding(MachineFunction &MF) const {
  MachineBasicBlock &MBB = MF.front(); // Prologue is in entry BB.
  ARM64FunctionInfo *AFI = MF.getInfo<ARM64FunctionInfo>();
  uint32_t CompactUnwindEncoding = 0;

  if (!hasFP(MF)) {
    // Indicate that this is a 'frameless' leaf function.
    CompactUnwindEncoding |= 0x02000000;

    const uint32_t StackSize = AFI->getLocalStackSize();

    // With compact unwind info we can only represent stack adjustments of
    // up to 65520 bytes.
    if (StackSize > 65520)
      return 0;

    // Add stack adjustment to compact unwind info.
    CompactUnwindEncoding |= encodeStackAdjustment(StackSize);
  }

  for (MachineBasicBlock::iterator
         MBBI = MBB.begin(), MBBE = MBB.end(); MBBI != MBBE; ++MBBI) {
    MachineInstr &MI = *MBBI;
    unsigned Opc = MI.getOpcode();
    if (Opc == ARM64::PROLOG_LABEL) continue;
    if (!MI.getFlag(MachineInstr::FrameSetup)) break;

    switch (Opc) {
    default: break;
    case ARM64::STPXpre: {
      // X19/X20 pair = 0x00000001,
      // X21/X22 pair = 0x00000002,
      // X23/X24 pair = 0x00000004,
      // X25/X26 pair = 0x00000008,
      // X27/X28 pair = 0x00000010
      //
      // The encodings must be in register number order.
      unsigned Reg1 = MI.getOperand(1).getReg();
      unsigned Reg2 = MI.getOperand(0).getReg();
      if (Reg1 == ARM64::LR && Reg2 == ARM64::FP) {
        // Indicate that the function has a frame.
        CompactUnwindEncoding |= 0x04000000;
      } else if (Reg1 == ARM64::X19 && Reg2 == ARM64::X20 &&
          (CompactUnwindEncoding & 0x1E) == 0)
        CompactUnwindEncoding |= 0x00000001;
      else if (Reg1 == ARM64::X21 && Reg2 == ARM64::X22 &&
               (CompactUnwindEncoding & 0x1C) == 0)
        CompactUnwindEncoding |= 0x00000002;
      else if (Reg1 == ARM64::X23 && Reg2 == ARM64::X24 &&
               (CompactUnwindEncoding & 0x18) == 0)
        CompactUnwindEncoding |= 0x00000004;
      else if (Reg1 == ARM64::X25 && Reg2 == ARM64::X26 &&
               (CompactUnwindEncoding & 0x10) == 0)
        CompactUnwindEncoding |= 0x00000008;
      else if (Reg1 == ARM64::X27 && Reg2 == ARM64::X28)
        CompactUnwindEncoding |= 0x00000010;
      else
        // A pair was pushed which we cannot handle.
        return 0;
      break;
    }
    case ARM64::STPDpre: {
      // D8/D9 pair   = 0x00000100,
      // D10/D11 pair = 0x00000200,
      // D12/D13 pair = 0x00000400,
      // D14/D15 pair = 0x00000800
      //
      // The encodings must be in register number order.
      unsigned Reg1 = MI.getOperand(1).getReg();
      unsigned Reg2 = MI.getOperand(0).getReg();
      if (Reg1 == ARM64::D8 && Reg2 == ARM64::D9 &&
          (CompactUnwindEncoding & 0xE00) == 0)
        CompactUnwindEncoding |= 0x00000100;
      else if (Reg1 == ARM64::D10 && Reg2 == ARM64::D11 &&
               (CompactUnwindEncoding & 0xC00) == 0)
        CompactUnwindEncoding |= 0x00000200;
      else if (Reg1 == ARM64::D12 && Reg2 == ARM64::D13 &&
               (CompactUnwindEncoding & 0x800) == 0)
        CompactUnwindEncoding |= 0x00000400;
      else if (Reg1 == ARM64::D14 && Reg2 == ARM64::D15)
        CompactUnwindEncoding |= 0x00000800;
      else
        // A pair was pushed which we cannot handle.
        return 0;
      break;
    }
    }
  }

  assert((CompactUnwindEncoding & 0x06000000) != 0 &&
         "Invalid compact unwind encoding!");

  return CompactUnwindEncoding;
}

void ARM64FrameLowering::emitPrologue(MachineFunction &MF) const {
  MachineBasicBlock &MBB = MF.front();
  MachineBasicBlock::iterator MBBI = MBB.begin();
  const MachineFrameInfo *MFI = MF.getFrameInfo();
  ARM64FunctionInfo *AFI = MF.getInfo<ARM64FunctionInfo>();
  const ARM64InstrInfo *TII =
    static_cast<const ARM64InstrInfo*>(MF.getTarget().getInstrInfo());
  const ARM64RegisterInfo *RegInfo =
    static_cast<const ARM64RegisterInfo*>(MF.getTarget().getRegisterInfo());
  bool needsFrameMoves = (MF.getMMI().hasDebugInfo() ||
                          !MF.getFunction()->doesNotThrow() ||
                          MF.getFunction()->needsUnwindTableEntry());
  DebugLoc DL = MBB.findDebugLoc(MBBI);

  int NumBytes = (int)MFI->getStackSize();
  if (!AFI->hasStackFrame()) {
    // All of the stack allocation is for locals.
    AFI->setLocalStackSize(NumBytes);

    // Indicate that this is a 'frameless' leaf function.
    uint32_t CompactUnwindEncoding = 0x02000000;

    // Label used to tie together the PROLOG_LABEL and the MachineMoves.
    MCSymbol *FrameLabel = MF.getMMI().getContext().CreateTempSymbol();

    // REDZONE: If the stack size is less than 128 bytes, we don't need
    // to actually allocate.
    if (NumBytes && !canUseRedZone(MF)) {
      emitFrameOffset(MBB, MBBI, DL, ARM64::SP, ARM64::SP, -NumBytes, TII,
                      MachineInstr::FrameSetup);

      // Encode the stack size of the leaf function.
      std::vector<MachineMove> &Moves = MF.getMMI().getFrameMoves();
      Moves.push_back(MachineMove(FrameLabel,
                                  MachineLocation(MachineLocation::VirtualFP),
                                  MachineLocation(MachineLocation::VirtualFP,
                                                  -NumBytes)));

      // With compact unwind info we can only represent stack adjustments of
      // up to 65520 bytes.
      if (NumBytes > 65520)
        return;

      // Add stack adjustment to compact unwind info.
      CompactUnwindEncoding |= encodeStackAdjustment(NumBytes);
    } else if (NumBytes)
      ++NumRedZoneFunctions;

    // Adding a PROLOG_LABEL so that AsmPrinter::emitPrologLabel() can pick it
    // up and perform the actual compact unwind emission.
    BuildMI(MBB, MBBI, DL, TII->get(ARM64::PROLOG_LABEL))
      .addSym(FrameLabel)
      .setMIFlag(MachineInstr::FrameSetup);

    MF.getMMI().setCompactUnwindEncoding(CompactUnwindEncoding);
    return;
  }

  // If LR is returned for @llvm.returnaddress and it is already livein to
  // the function, then there is a use of the LR later.
  // See ARM64TargetLowering::LowerRETURNADDR.
  bool LRLiveIn = MF.getRegInfo().isLiveIn(ARM64::LR);
  bool LRKill = !(LRLiveIn && MF.getFrameInfo()->isReturnAddressTaken());
  if (!LRLiveIn)
    MBB.addLiveIn(ARM64::LR);
  MBB.addLiveIn(ARM64::FP);

  // Only set up FP if we actually need to.
  if (hasFP(MF)) {
    // Save FP and LR with an updating store-pair ("stp fp, lr, [sp, #-16]!").
    BuildMI(MBB, MBBI, DL, TII->get(ARM64::STPXpre))
      .addReg(ARM64::FP, RegState::Kill)
      .addReg(ARM64::LR, getKillRegState(LRKill))
      .addReg(ARM64::SP)
      .addImm(-2)                 // [sp, #-16]!
      .setMIFlag(MachineInstr::FrameSetup);

    // Set up the frame pointer.
    TII->copyPhysReg(MBB, MBBI, DL, ARM64::FP, ARM64::SP, false);

    // Make sure that all of the instructions generated by the copyPhysReg call
    // have the 'FrameSetup' flag set.
    for (MachineBasicBlock::iterator I = MBB.begin(); I != MBBI; ++I)
      I->setFlag(MachineInstr::FrameSetup);
  }

  // Move past the saves of the callee-saved registers.
  while (MBBI->getOpcode() == ARM64::STPXpre ||
         MBBI->getOpcode() == ARM64::STPDpre) {
    ++MBBI;
    NumBytes -= 16;
  }
  assert(NumBytes >= 0 && "Negative stack allocation size!?");

  // All of the remaining stack allocation is for locals.
  AFI->setLocalStackSize(NumBytes);

  // Allocate space for the rest of the frame.
  if (NumBytes) {
    // If we're a leaf function, try using the red zone.
    if (!canUseRedZone(MF))
      emitFrameOffset(MBB, MBBI, DL, ARM64::SP, ARM64::SP, -NumBytes, TII,
                      MachineInstr::FrameSetup);
  }

  // If we need a base pointer, set it up here. It's whatever the value
  // of the stack pointer is at this point. Any variable size objects
  // will be allocated after this, so we can still use the base pointer
  // to reference locals.
  // FIXME: Clarify FrameSetup flags here.
  if (RegInfo->hasBasePointer(MF))
    TII->copyPhysReg(MBB, MBBI, DL, ARM64::X19, ARM64::SP, false);


  if (needsFrameMoves) {
    const DataLayout *TD = MF.getTarget().getDataLayout();
    int stackGrowth = -TD->getPointerSize(0);
    std::vector<MachineMove> &Moves = MF.getMMI().getFrameMoves();
    unsigned FramePtr = RegInfo->getFrameRegister(MF);
    MCSymbol *FrameLabel = MF.getMMI().getContext().CreateTempSymbol();

    BuildMI(MBB, MBBI, DL, TII->get(ARM64::PROLOG_LABEL))
      .addSym(FrameLabel)
      .setMIFlag(MachineInstr::FrameSetup);

    if (hasFP(MF)) {
      // Define the current CFA rule to use the provided FP.
      MachineLocation SPDst(MachineLocation::VirtualFP);
      MachineLocation SPSrc(FramePtr, -2 * stackGrowth);
      Moves.push_back(MachineMove(FrameLabel, SPDst, SPSrc));

      // Record the location of the stored LR
      Moves.push_back(MachineMove(FrameLabel,
                                  MachineLocation(MachineLocation::VirtualFP,
                                                  stackGrowth),
                                  MachineLocation(ARM64::LR)));
      // Record the location of the stored FP
      Moves.push_back(MachineMove(FrameLabel,
                                  MachineLocation(MachineLocation::VirtualFP,
                                                  2 * stackGrowth),
                                  MachineLocation(FramePtr)));
    } else {
      // Encode the stack size of the leaf function.
      Moves.push_back(MachineMove(FrameLabel,
                                  MachineLocation(MachineLocation::VirtualFP),
                                  MachineLocation(MachineLocation::VirtualFP,
                                                  -MFI->getStackSize())));
    }

    // Now emit the moves for whatever callee saved regs we have.
    emitCalleeSavedFrameMoves(MF, FrameLabel, FramePtr);
  }

  // Darwin 10.7 and greater has support for compact unwind encoding.
  MF.getMMI().setCompactUnwindEncoding(getCompactUnwindEncoding(MF));
}

static bool isCalleeSavedRegister(unsigned Reg, const uint16_t *CSRegs) {
  for (unsigned i = 0; CSRegs[i]; ++i)
    if (Reg == CSRegs[i])
      return true;
  return false;
}

static bool isCSRestore(MachineInstr *MI, const uint16_t *CSRegs) {
  if (MI->getOpcode() == ARM64::LDPXpost ||
      MI->getOpcode() == ARM64::LDPDpost) {
    if (!isCalleeSavedRegister(MI->getOperand(0).getReg(), CSRegs) ||
        !isCalleeSavedRegister(MI->getOperand(1).getReg(), CSRegs) ||
        MI->getOperand(2).getReg() != ARM64::SP)
      return false;
    return true;
  }

  return false;
}

void ARM64FrameLowering::emitEpilogue(MachineFunction &MF,
                                      MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  assert(MBBI->isReturn() &&
         "Can only insert epilog into returning blocks");
  MachineFrameInfo *MFI = MF.getFrameInfo();
  ARM64FunctionInfo *AFI = MF.getInfo<ARM64FunctionInfo>();
  const ARM64InstrInfo *TII =
    static_cast<const ARM64InstrInfo*>(MF.getTarget().getInstrInfo());
  const ARM64RegisterInfo *RegInfo =
    static_cast<const ARM64RegisterInfo*>(MF.getTarget().getRegisterInfo());
  DebugLoc DL = MBBI->getDebugLoc();

  unsigned NumBytes = MFI->getStackSize();
  unsigned NumRestores = 0;
  // Move past the restores of the callee-saved registers.
  MachineBasicBlock::iterator LastPopI = MBBI;
  const uint16_t *CSRegs = RegInfo->getCalleeSavedRegs();
  if (LastPopI != MBB.begin()) {
    do {
      ++NumRestores;
      --LastPopI;
    } while (LastPopI != MBB.begin() && isCSRestore(LastPopI, CSRegs));
    if (!isCSRestore(LastPopI, CSRegs)) {
      ++LastPopI;
      --NumRestores;
    }
  }
  NumBytes -= NumRestores * 16;
  assert(NumBytes >= 0 && "Negative stack allocation size!?");

  if (!AFI->hasStackFrame() || !hasFP(MF)) {
    // If this was a redzone leaf function, we don't need to restore the
    // stack pointer.
    if (!canUseRedZone(MF))
      emitFrameOffset(MBB, LastPopI, DL, ARM64::SP, ARM64::SP, NumBytes, TII);
    return;
  }

  // Restore the original stack pointer.
  // FIXME: Rather than doing the math here, we should instead just use
  // non-post-indexed loads for the restores if we aren't actually going to
  // be able to save any instructions.
  if (NumBytes || MFI->hasVarSizedObjects())
    emitFrameOffset(MBB, LastPopI, DL, ARM64::SP, ARM64::FP, -NumRestores * 16,
                    TII, MachineInstr::NoFlags);

  // Reload FP and LR with a post-inc load-pair ("ldp fp, lr, [sp], #16").
  BuildMI(MBB, MBBI, DL, TII->get(ARM64::LDPXpost))
    .addReg(ARM64::FP, getDefRegState(true))
    .addReg(ARM64::LR, getDefRegState(true))
    .addReg(ARM64::SP).addImm(2); // [sp], #16
}

/// getFrameIndexOffset - Returns the displacement from the frame register to
/// the stack frame of the specified index.
int ARM64FrameLowering::getFrameIndexOffset(const MachineFunction &MF,
                                            int FI) const {
  unsigned FrameReg;
  return getFrameIndexReference(MF, FI, FrameReg);
}

/// getFrameIndexReference - Provide a base+offset reference to an FI slot for
/// debug info.  It's the same as what we use for resolving the code-gen
/// references for now.  FIXME: This can go wrong when references are
/// SP-relative and simple call frames aren't used.
int
ARM64FrameLowering::getFrameIndexReference(const MachineFunction &MF, int FI,
                                           unsigned &FrameReg) const {
  return resolveFrameIndexReference(MF, FI, FrameReg);
}

int
ARM64FrameLowering::resolveFrameIndexReference(const MachineFunction &MF,
                                               int FI,
                                               unsigned &FrameReg) const {
  const MachineFrameInfo *MFI = MF.getFrameInfo();
  const ARM64RegisterInfo *RegInfo =
    static_cast<const ARM64RegisterInfo*>(MF.getTarget().getRegisterInfo());
  const ARM64FunctionInfo *AFI = MF.getInfo<ARM64FunctionInfo>();
  int FPOffset = MFI->getObjectOffset(FI) - getOffsetOfLocalArea();
  int Offset = FPOffset + MFI->getStackSize();
  bool isFixed = MFI->isFixedObjectIndex(FI);

  // Use frame pointer to reference fixed objects. Use it for locals if
  // there are VLAs (and thus the SP isn't reliable as a base).
  // Make sure useFPForScavengingIndex() does the right thing for the emergency
  // spill slot.
  bool UseFP = false;
  if (AFI->hasStackFrame()) {
    // Note: Keeping the following as multiple 'if' statements rather than
    // merging to a single expression for readability.
    //
    // Argument access should always use the FP.
    if (isFixed) {
      UseFP = hasFP(MF);
    } else if (hasFP(MF) && !RegInfo->hasBasePointer(MF)) {
      // Use SP or FP, whichever gives us the best chance of the offset
      // being in range for direct access. If the FPOffset is positive,
      // that'll always be best, as the SP will be even further away.
      // If the FPOffset is negative, we have to keep in mind that the
      // available offset range for negative offsets is smaller than for
      // positive ones. If we have variable sized objects, we're stuck with
      // using the FP regardless, though, as the SP offset is unknown
      // and we don't have a base pointer available. If an offset is
      // available via the FP and the SP, use whichever is closest.
      if (MFI->hasVarSizedObjects() || FPOffset >= 0 ||
          (FPOffset >= -256 && Offset > -FPOffset))
        UseFP = true;
    }
  }

  if (UseFP) {
    FrameReg = RegInfo->getFrameRegister(MF);
    return FPOffset;
  }

  // Use the base pointer if we have one.
  if (RegInfo->hasBasePointer(MF))
    FrameReg = RegInfo->getBaseRegister();
  else {
    FrameReg = ARM64::SP;
    // If we're using the red zone for this function, the SP won't actually
    // be adjusted, so the offsets will be negative. They're also all
    // within range of the signed 9-bit immediate instructions.
    if (canUseRedZone(MF))
      Offset -= AFI->getLocalStackSize();
  }

  return Offset;
}

bool ARM64FrameLowering::spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                                        MachineBasicBlock::iterator MI,
                                        const std::vector<CalleeSavedInfo> &CSI,
                                        const TargetRegisterInfo *TRI) const {
  MachineFunction &MF = *MBB.getParent();
  const TargetInstrInfo &TII = *MF.getTarget().getInstrInfo();
  unsigned Count = CSI.size();
  DebugLoc DL;
  assert((Count & 1) == 0 && "Odd number of callee-saved regs to spill!");

  if (MI != MBB.end()) DL = MI->getDebugLoc();

  for (unsigned i = 0; i < Count; i += 2) {
    unsigned Reg1 = CSI[i].getReg();
    unsigned Reg2 = CSI[i+1].getReg();
    // GPRs and FPRs are saved in pairs of 64-bit regs. We expect the CSI
    // list to come in sorted by frame index so that we can issue the store
    // pair instructions directly. Assert if we see anything otherwise.
    //
    // The order of the registers in the list is controlled by
    // getCalleeSavedRegs(), so they will always be in-order, as well.
    assert(CSI[i].getFrameIdx() + 1 == CSI[i + 1].getFrameIdx() &&
           "Out of order callee saved regs!");
    unsigned StrOpc;
    if (ARM64::GPR64RegClass.contains(Reg1)) {
      assert(ARM64::GPR64RegClass.contains(Reg2) &&
             "Expected GPR64 callee-saved register pair!");
      StrOpc = ARM64::STPXpre;
    } else if (ARM64::FPR64RegClass.contains(Reg1)) {
      assert(ARM64::FPR64RegClass.contains(Reg2) &&
             "Expected FPR64 callee-saved register pair!");
      StrOpc = ARM64::STPDpre;
    } else
      assert(0 && "Unexpected callee saved register!");
    DEBUG(dbgs() << "CSR spill: (" << TRI->getName(Reg1)
                 << ", " << TRI->getName(Reg2) << ") -> fi#("
                 << CSI[i].getFrameIdx() << ", "
                 << CSI[i+1].getFrameIdx() << ")\n");
    BuildMI(MBB, MI, DL, TII.get(StrOpc))
      .addReg(Reg2, getKillRegState(true))
      .addReg(Reg1, getKillRegState(true))
      .addReg(ARM64::SP)
      .addImm(-2)                 // [sp, #-16]!
      .setMIFlag(MachineInstr::FrameSetup);
  }
  return true;
}

bool ARM64FrameLowering::restoreCalleeSavedRegisters(MachineBasicBlock &MBB,
                                        MachineBasicBlock::iterator MI,
                                        const std::vector<CalleeSavedInfo> &CSI,
                                        const TargetRegisterInfo *TRI) const {
  MachineFunction &MF = *MBB.getParent();
  const TargetInstrInfo &TII = *MF.getTarget().getInstrInfo();
  unsigned Count = CSI.size();
  DebugLoc DL;
  assert((Count & 1) == 0 && "Odd number of callee-saved regs to spill!");

  if (MI != MBB.end()) DL = MI->getDebugLoc();

  for (unsigned i = 0; i < Count; i += 2) {
    unsigned idx = Count - i - 2;
    unsigned Reg1 = CSI[idx].getReg();
    unsigned Reg2 = CSI[idx+1].getReg();
    // GPRs and FPRs are saved in pairs of 64-bit regs. We expect the CSI
    // list to come in sorted by frame index so that we can issue the store
    // pair instructions directly. Assert if we see anything otherwise.
    assert(CSI[idx].getFrameIdx() + 1 == CSI[idx + 1].getFrameIdx() &&
           "Out of order callee saved regs!");
    unsigned LdrOpc;
    if (ARM64::GPR64RegClass.contains(Reg1)) {
      assert(ARM64::GPR64RegClass.contains(Reg2) &&
             "Expected GPR64 callee-saved register pair!");
      LdrOpc = ARM64::LDPXpost;
    } else if (ARM64::FPR64RegClass.contains(Reg1)) {
      assert(ARM64::FPR64RegClass.contains(Reg2) &&
             "Expected FPR64 callee-saved register pair!");
      LdrOpc = ARM64::LDPDpost;
    } else
      assert(0 && "Unexpected callee saved register!");
    DEBUG(dbgs() << "CSR restore: (" << TRI->getName(Reg1)
                 << ", " << TRI->getName(Reg2) << ") -> fi#("
                 << CSI[idx].getFrameIdx() << ", "
                 << CSI[idx+1].getFrameIdx() << ")\n");
    BuildMI(MBB, MI, DL, TII.get(LdrOpc))
      .addReg(Reg2, getDefRegState(true))
      .addReg(Reg1, getDefRegState(true))
      .addReg(ARM64::SP)
      .addImm(2); // [sp], #16
  }
  return true;
}

void
ARM64FrameLowering::processFunctionBeforeCalleeSavedScan(MachineFunction &MF,
                                                       RegScavenger *RS) const {
  const ARM64RegisterInfo *RegInfo =
    static_cast<const ARM64RegisterInfo*>(MF.getTarget().getRegisterInfo());
  ARM64FunctionInfo *AFI = MF.getInfo<ARM64FunctionInfo>();
  MachineRegisterInfo *MRI = &MF.getRegInfo();
  SmallVector<unsigned, 4> UnspilledCSGPRs;
  SmallVector<unsigned, 4> UnspilledCSFPRs;

  // Spill the BasePtr if it's used. Do this first thing so that the
  // getCalleeSavedRegs() below will get the right answer.
  if (RegInfo->hasBasePointer(MF))
    MRI->setPhysRegUsed(RegInfo->getBaseRegister());

  // If any callee-saved registers are used, the frame cannot be eliminated.
  unsigned NumGPRSpilled = 0;
  unsigned NumFPRSpilled = 0;
  bool ExtraCSSpill = false;
  bool CanEliminateFrame = true;
  DEBUG(dbgs() << "*** processFunctionBeforeCalleeSavedScan\nUsed CSRs:");
  const uint16_t *CSRegs = RegInfo->getCalleeSavedRegs();

  // Check pairs of consecutive callee-saved registers.
  for (unsigned i = 0; CSRegs[i]; i += 2) {
    assert(CSRegs[i + 1] && "Odd number of callee-saved registers!");

    const unsigned EvenReg = CSRegs[i];
    const unsigned OddReg  = CSRegs[i + 1];
    assert(
      (ARM64::GPR64RegClass.contains(EvenReg)
       && ARM64::GPR64RegClass.contains(OddReg))
      ^ (ARM64::FPR64RegClass.contains(EvenReg)
         && ARM64::FPR64RegClass.contains(OddReg))
      && "Register class mismatch!");

    const bool EvenRegUsed = MRI->isPhysRegUsed(EvenReg);
    const bool OddRegUsed  = MRI->isPhysRegUsed(OddReg);

    // Early exit if none of the registers in the register pair is actually
    // used.
    if (!EvenRegUsed && !OddRegUsed) {
      if (ARM64::GPR64RegClass.contains(EvenReg)) {
        UnspilledCSGPRs.push_back(EvenReg);
        UnspilledCSGPRs.push_back(OddReg);
      } else {
        UnspilledCSFPRs.push_back(EvenReg);
        UnspilledCSFPRs.push_back(OddReg);
      }
      continue;
    }

    unsigned Reg = ARM64::NoRegister;
    // If only one of the registers of the register pair is used, make sure to
    // mark the other one as used as well.
    if (EvenRegUsed ^ OddRegUsed) {
      // Find out which register is the additional spill.
      Reg = EvenRegUsed ? OddReg : EvenReg;
      MRI->setPhysRegUsed(Reg);
    }

    DEBUG(dbgs() << ' ' << PrintReg(EvenReg, RegInfo));
    DEBUG(dbgs() << ' ' << PrintReg(OddReg, RegInfo));

    assert((RegInfo->getEncodingValue(EvenReg) + 1
            == RegInfo->getEncodingValue(OddReg))
           && "Register pair of non-adjacent registers!");
    if (ARM64::GPR64RegClass.contains(EvenReg)) {
      NumGPRSpilled += 2;
      // If it's not a reserved register, we can use it in lieu of an
      // emergency spill slot for the register scavenger.
      // FIXME: It would be better to instead keep looking and choose another
      // unspilled register that isn't reserved, if there is one.
      if (Reg != ARM64::NoRegister && !RegInfo->isReservedReg(MF, Reg))
        ExtraCSSpill = true;
    } else
      NumFPRSpilled += 2;

    CanEliminateFrame = false;
  }

  // FIXME: Set BigStack if any stack slot references may be out of range.
  // For now, just conservatively guestimate based on unscaled indexing
  // range. We'll end up allocating an unnecessary spill slot a lot, but
  // realistically that's not a big deal at this stage of the game.
  // The CSR spill slots have not been allocated yet, so estimateStackSize
  // won't include them.
  MachineFrameInfo *MFI = MF.getFrameInfo();
  unsigned CFSize = estimateStackSize(MF) + 8*(NumGPRSpilled + NumFPRSpilled);
  DEBUG(dbgs() << "Estimated stack frame size: " << CFSize << " bytes.\n");
  bool BigStack = (CFSize >= 256);
  if (BigStack || !CanEliminateFrame || RegInfo->cannotEliminateFrame(MF))
    AFI->setHasStackFrame(true);

  // Estimate if we might need to scavenge a register at some point in order
  // to materialize a stack offset. If so, either spill one additional
  // callee-saved register or reserve a special spill slot to facilitate
  // register scavenging. If we already spilled an extra callee-saved register
  // above to keep the number of spills even, we don't need to do anything else
  // here.
  if (BigStack && !ExtraCSSpill) {

    // If we're adding a register to spill here, we have to add two of them
    // to keep the number of regs to spill even.
    unsigned Count = 0;
    while (!UnspilledCSGPRs.empty() && Count < 2) {
      unsigned Reg = UnspilledCSGPRs.back();
      UnspilledCSGPRs.pop_back();
      if (!RegInfo->isReservedReg(MF, Reg)) {
        DEBUG(dbgs() << "Spilling " << PrintReg(Reg, RegInfo)
                     << " to get a scratch register.\n");
        MRI->setPhysRegUsed(Reg);
        ExtraCSSpill = true;
        ++Count;
      }
    }

    // If we didn't find an extra callee-saved register to spill, create
    // an emergency spill slot.
    if (!ExtraCSSpill) {
      const TargetRegisterClass *RC = &ARM64::GPR64RegClass;
      int FI = MFI->CreateStackObject(RC->getSize(), RC->getAlignment(), false);
      RS->setScavengingFrameIndex(FI);
      DEBUG(dbgs() << "No available CS registers, allocated fi#" << FI
                   << " as the emergency spill slot.\n");
    }
  }
}
