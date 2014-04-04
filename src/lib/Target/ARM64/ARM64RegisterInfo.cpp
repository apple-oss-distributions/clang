//===- ARM64RegisterInfo.cpp - ARM64 Register Information -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the ARM64 implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "ARM64RegisterInfo.h"
#include "ARM64FrameLowering.h"
#include "ARM64InstrInfo.h"
#include "MCTargetDesc/ARM64AddressingModes.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetFrameLowering.h"
#include "llvm/Target/TargetOptions.h"

#define GET_REGINFO_TARGET_DESC
#include "ARM64GenRegisterInfo.inc"

using namespace llvm;

ARM64RegisterInfo::ARM64RegisterInfo(const ARM64InstrInfo *tii)
  : ARM64GenRegisterInfo(ARM64::LR), TII(tii) {
}

const uint16_t *ARM64RegisterInfo::
getCalleeSavedRegs(const MachineFunction*) const {
  return CSR_ARM64_AAPCS_SaveList;
}

const uint32_t *ARM64RegisterInfo::getCallPreservedMask(CallingConv::ID) const {
  return CSR_ARM64_AAPCS_RegMask;
}

BitVector ARM64RegisterInfo::
getReservedRegs(const MachineFunction &MF) const {
  // FIXME: avoid re-calculating this everytime.
  BitVector Reserved(getNumRegs());
  Reserved.set(ARM64::SP);
  Reserved.set(ARM64::XZR);
  Reserved.set(ARM64::WSP);
  Reserved.set(ARM64::WZR);
  Reserved.set(ARM64::LR);
  Reserved.set(ARM64::W30);
  Reserved.set(ARM64::FP);
  Reserved.set(ARM64::W29);

  Reserved.set(ARM64::X18); // Platform register
  Reserved.set(ARM64::W18);

  if (hasBasePointer(MF)) {
    Reserved.set(ARM64::X19);
    Reserved.set(ARM64::W19);
  }

  return Reserved;
}

bool ARM64RegisterInfo::isReservedReg(const MachineFunction &MF,
                                      unsigned Reg) const {
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();

  switch (Reg) {
  default: break;
  case ARM64::SP:
  case ARM64::XZR:
  case ARM64::WSP:
  case ARM64::WZR:
  case ARM64::LR:
  case ARM64::W30:
  case ARM64::X18:
  case ARM64::W18:
    return true;
  case ARM64::FP:
  case ARM64::W29:
    return TFI->hasFP(MF);
  case ARM64::W19:
  case ARM64::X19:
    return hasBasePointer(MF);
  }

  return false;
}


const TargetRegisterClass *
ARM64RegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                      unsigned Kind) const {
  return &ARM64::GPR64RegClass;
}

const TargetRegisterClass *
ARM64RegisterInfo::getCrossCopyRegClass(const TargetRegisterClass *RC) const {
  if (RC == &ARM64::CCRRegClass)
    return NULL; // Can't copy CPSR.
  return RC;
}

unsigned ARM64RegisterInfo::getBaseRegister() const {
  return ARM64::X19;
}

bool ARM64RegisterInfo::hasBasePointer(const MachineFunction &MF) const {
  const MachineFrameInfo *MFI = MF.getFrameInfo();

  // In the presence of variable sized objects, if the fixed stack size is
  // large enough that referencing from the FP won't result in things being
  // in range relatively often, we can use a base pointer to allow access
  // from the other direction like the SP normally works.
  if (MFI->hasVarSizedObjects()) {
    // Conservatively estimate whether the negative offset from the frame
    // pointer will be sufficient to reach. If a function has a smallish
    // frame, it's less likely to have lots of spills and callee saved
    // space, so it's all more likely to be within range of the frame pointer.
    // If it's wrong, we'll materialize the constant and still get to the
    // object; it's just suboptimal. Negative offsets use the unscaled
    // load/store instructions, which have a 9-bit signed immediate.
    if (MFI->getLocalFrameSize() < 256)
      return false;
    return true;
  }

  return false;
}

void ARM64RegisterInfo::eliminateCallFramePseudoInstr(MachineFunction &MF,
                                          MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator I) const {
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();
  if (!TFI->hasReservedCallFrame(MF)) {
    // If we have alloca, convert as follows:
    // ADJCALLSTACKDOWN -> sub, sp, sp, amount
    // ADJCALLSTACKUP   -> add, sp, sp, amount
    MachineInstr *Old = I;
    DebugLoc DL = Old->getDebugLoc();
    unsigned Amount = Old->getOperand(0).getImm();
    if (Amount != 0) {
      // We need to keep the stack aligned properly.  To do this, we round the
      // amount of space needed for the outgoing arguments up to the next
      // alignment boundary.
      unsigned Align = TFI->getStackAlignment();
      Amount = (Amount+Align-1)/Align*Align;

      // Replace the pseudo instruction with a new instruction...
      unsigned Opc = Old->getOpcode();
      if (Opc == ARM64::ADJCALLSTACKDOWN) {
        emitFrameOffset(MBB, I, DL, ARM64::SP, ARM64::SP, -Amount, TII);
      } else {
        assert(Opc == ARM64::ADJCALLSTACKUP && "expected ADJCALLSTACKUP");
        emitFrameOffset(MBB, I, DL, ARM64::SP, ARM64::SP, Amount, TII);
      }
    }
  }
  MBB.erase(I);
}

unsigned ARM64RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();

  return TFI->hasFP(MF) ? ARM64::FP : ARM64::SP;
}

bool ARM64RegisterInfo::
requiresRegisterScavenging(const MachineFunction &MF) const {
  return true;
}

bool ARM64RegisterInfo::
requiresVirtualBaseRegisters(const MachineFunction &MF) const {
  return true;
}

bool ARM64RegisterInfo::
useFPForScavengingIndex(const MachineFunction &MF) const {
  const MachineFrameInfo *MFI = MF.getFrameInfo();
  // ARM64FrameLowering::resolveFrameIndexReference() can always fall back
  // to the stack pointer, so only put the emergency spill slot next to the
  // FP when there's no better way to access it (SP or base pointer).
  return MFI->hasVarSizedObjects() && !hasBasePointer(MF);
}

bool ARM64RegisterInfo::
requiresFrameIndexScavenging(const MachineFunction &MF) const {
  return true;
}

bool ARM64RegisterInfo::cannotEliminateFrame(const MachineFunction &MF) const {
  const MachineFrameInfo *MFI = MF.getFrameInfo();
  // Only consider eliminating leaf frames.
  if (MFI->hasCalls() ||
      (MF.getTarget().Options.DisableFramePointerElim(MF) &&
       MFI->adjustsStack()))
    return true;
  return MFI->hasVarSizedObjects() || MFI->isFrameAddressTaken();
}

/// needsFrameBaseReg - Returns true if the instruction's frame index
/// reference would be better served by a base register other than FP
/// or SP. Used by LocalStackFrameAllocation to determine which frame index
/// references it should create new base registers for.
bool ARM64RegisterInfo::
needsFrameBaseReg(MachineInstr *MI, int64_t Offset) const {
  for (unsigned i = 0; !MI->getOperand(i).isFI(); ++i)
    assert(i < MI->getNumOperands() &&"Instr doesn't have FrameIndex operand!");

  // It's the load/store FI references that cause issues, as it can be difficult
  // to materialize the offset if it won't fit in the literal field. Estimate
  // based on the size of the local frame and some conservative assumptions
  // about the rest of the stack frame (note, this is pre-regalloc, so
  // we don't know everything for certain yet) whether this offset is likely
  // to be out of range of the immediate. Return true if so.

  // We only generate virtual base registers for loads and stores, so
  // return false for everything else.
  if (!MI->mayLoad() && !MI->mayStore())
    return false;

  // Without a virtual base register, if the function has variable sized
  // objects, all fixed-size local references will be via the frame pointer,
  // Approximate the offset and see if it's legal for the instruction.
  // Note that the incoming offset is based on the SP value at function entry,
  // so it'll be negative.
  MachineFunction &MF = *MI->getParent()->getParent();
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();
  MachineFrameInfo *MFI = MF.getFrameInfo();

  // Estimate an offset from the frame pointer.
  // Conservatively assume all GPR callee-saved registers get pushed.
  // FP, LR, X19-X28, D8-D15. 64-bits each.
  int64_t FPOffset = Offset - 16 * 20;
  // Estimate an offset from the stack pointer.
  // The incoming offset is relating to the SP at the start of the function,
  // but when we access the local it'll be relative to the SP after local
  // allocation, so adjust our SP-relative offset by that allocation size.
  Offset += MFI->getLocalFrameSize();
  // Assume that we'll have at least some spill slots allocated.
  // FIXME: This is a total SWAG number. We should run some statistics
  //        and pick a real one.
  Offset += 128; // 128 bytes of spill slots

  // If there is a frame pointer, try using it.
  // The FP is only available if there is no dynamic realignment. We
  // don't know for sure yet whether we'll need that, so we guess based
  // on whether there are any local variables that would trigger it.
  if (TFI->hasFP(MF) && isFrameOffsetLegal(MI, FPOffset))
    return false;

  // If we can reference via the stack pointer or base pointer, try that.
  // FIXME: This (and the code that resolves the references) can be improved
  //        to only disallow SP relative references in the live range of
  //        the VLA(s). In practice, it's unclear how much difference that
  //        would make, but it may be worth doing.
  if (isFrameOffsetLegal(MI, Offset))
    return false;

  // The offset likely isn't legal; we want to allocate a virtual base register.
  return true;
}

bool ARM64RegisterInfo::isFrameOffsetLegal(const MachineInstr *MI,
                                           int64_t Offset) const {
  assert(Offset <= INT_MAX && "Offset too big to fit in int.");
  assert(MI && "Unable to get the legal offset for nil instruction.");
  int SaveOffset = Offset;
  return isARM64FrameOffsetLegal(*MI, SaveOffset) & ARM64FrameOffsetIsLegal;
}

/// Insert defining instruction(s) for BaseReg to be a pointer to FrameIdx
/// at the beginning of the basic block.
void ARM64RegisterInfo::
materializeFrameBaseRegister(MachineBasicBlock *MBB,
                             unsigned BaseReg, int FrameIdx,
                             int64_t Offset) const {
  MachineBasicBlock::iterator Ins = MBB->begin();
  DebugLoc DL;                  // Defaults to "unknown"
  if (Ins != MBB->end())
    DL = Ins->getDebugLoc();

  const MCInstrDesc &MCID = TII->get(ARM64::ADDXri);
  MachineRegisterInfo &MRI = MBB->getParent()->getRegInfo();
  const MachineFunction &MF = *MBB->getParent();
  MRI.constrainRegClass(BaseReg, TII->getRegClass(MCID, 0, this, MF));
  unsigned Shifter = ARM64_AM::getShifterImm(ARM64_AM::LSL, 0);

  BuildMI(*MBB, Ins, DL, MCID, BaseReg).addFrameIndex(FrameIdx).addImm(Offset)
    .addImm(Shifter);
}


void
ARM64RegisterInfo::resolveFrameIndex(MachineBasicBlock::iterator I,
                                     unsigned BaseReg, int64_t Offset) const {
  MachineInstr &MI = *I;
  int Off = Offset; // ARM doesn't need the general 64-bit offsets
  unsigned i = 0;

  while (!MI.getOperand(i).isFI()) {
    ++i;
    assert(i < MI.getNumOperands() && "Instr doesn't have FrameIndex operand!");
  }
  bool Done = rewriteARM64FrameIndex(MI, i, BaseReg, Off, TII);
  assert (Done && "Unable to resolve frame index!");
  (void)Done;
}

void ARM64RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                            int SPAdj, unsigned FIOperandNum,
                                            RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected");

  MachineInstr &MI = *II;
  MachineBasicBlock &MBB = *MI.getParent();
  MachineFunction &MF = *MBB.getParent();
  const ARM64FrameLowering *TFI =
    static_cast<const ARM64FrameLowering*>(MF.getTarget().getFrameLowering());

  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  unsigned FrameReg;
  int Offset = TFI->resolveFrameIndexReference(MF, FrameIndex, FrameReg);

  // Special handling of dbg_value instructions.
  if (MI.isDebugValue()) {
    MI.getOperand(FIOperandNum).ChangeToRegister(FrameReg, false /*isDef*/);
    MI.getOperand(FIOperandNum+1).ChangeToImmediate(Offset);
    return;
  }

  // Modify MI as necessary to handle as much of 'Offset' as possible
  if (rewriteARM64FrameIndex(MI, FIOperandNum, FrameReg, Offset, TII))
    return;

  assert((!RS || RS->getScavengingFrameIndex() != FrameIndex) &&
         "Emergency spill slot is out of reach");

  // If we get here, the immediate doesn't fit into the instruction.  We folded
  // as much as possible above.  Handle the rest, providing a register that is
  // SP+LargeImm.
  unsigned ScratchReg =
    MF.getRegInfo().createVirtualRegister(&ARM64::GPR64RegClass);
  emitFrameOffset(MBB, II, MI.getDebugLoc(), ScratchReg, FrameReg,
                  Offset, TII);
  MI.getOperand(FIOperandNum).ChangeToRegister(ScratchReg, false, false, true);
}

namespace llvm {
unsigned getWRegFromXReg(unsigned Reg) {
  switch (Reg) {
  case ARM64::X0: return ARM64::W0;
  case ARM64::X1: return ARM64::W1;
  case ARM64::X2: return ARM64::W2;
  case ARM64::X3: return ARM64::W3;
  case ARM64::X4: return ARM64::W4;
  case ARM64::X5: return ARM64::W5;
  case ARM64::X6: return ARM64::W6;
  case ARM64::X7: return ARM64::W7;
  case ARM64::X8: return ARM64::W8;
  case ARM64::X9: return ARM64::W9;
  case ARM64::X10: return ARM64::W10;
  case ARM64::X11: return ARM64::W11;
  case ARM64::X12: return ARM64::W12;
  case ARM64::X13: return ARM64::W13;
  case ARM64::X14: return ARM64::W14;
  case ARM64::X15: return ARM64::W15;
  case ARM64::X16: return ARM64::W16;
  case ARM64::X17: return ARM64::W17;
  case ARM64::X18: return ARM64::W18;
  case ARM64::X19: return ARM64::W19;
  case ARM64::X20: return ARM64::W20;
  case ARM64::X21: return ARM64::W21;
  case ARM64::X22: return ARM64::W22;
  case ARM64::X23: return ARM64::W23;
  case ARM64::X24: return ARM64::W24;
  case ARM64::X25: return ARM64::W25;
  case ARM64::X26: return ARM64::W26;
  case ARM64::X27: return ARM64::W27;
  case ARM64::X28: return ARM64::W28;
  case ARM64::FP: return ARM64::W29;
  case ARM64::LR: return ARM64::W30;
  case ARM64::SP: return ARM64::WSP;
  case ARM64::XZR: return ARM64::WZR;
  }
  // For anything else, return it unchanged.
  return Reg;
}

unsigned getXRegFromWReg(unsigned Reg) {
  switch (Reg) {
  case ARM64::W0: return ARM64::X0;
  case ARM64::W1: return ARM64::X1;
  case ARM64::W2: return ARM64::X2;
  case ARM64::W3: return ARM64::X3;
  case ARM64::W4: return ARM64::X4;
  case ARM64::W5: return ARM64::X5;
  case ARM64::W6: return ARM64::X6;
  case ARM64::W7: return ARM64::X7;
  case ARM64::W8: return ARM64::X8;
  case ARM64::W9: return ARM64::X9;
  case ARM64::W10: return ARM64::X10;
  case ARM64::W11: return ARM64::X11;
  case ARM64::W12: return ARM64::X12;
  case ARM64::W13: return ARM64::X13;
  case ARM64::W14: return ARM64::X14;
  case ARM64::W15: return ARM64::X15;
  case ARM64::W16: return ARM64::X16;
  case ARM64::W17: return ARM64::X17;
  case ARM64::W18: return ARM64::X18;
  case ARM64::W19: return ARM64::X19;
  case ARM64::W20: return ARM64::X20;
  case ARM64::W21: return ARM64::X21;
  case ARM64::W22: return ARM64::X22;
  case ARM64::W23: return ARM64::X23;
  case ARM64::W24: return ARM64::X24;
  case ARM64::W25: return ARM64::X25;
  case ARM64::W26: return ARM64::X26;
  case ARM64::W27: return ARM64::X27;
  case ARM64::W28: return ARM64::X28;
  case ARM64::W29: return ARM64::FP;
  case ARM64::W30: return ARM64::LR;
  case ARM64::WSP: return ARM64::SP;
  case ARM64::WZR: return ARM64::XZR;
  }
  // For anything else, return it unchanged.
  return Reg;
}

unsigned ARM64RegisterInfo::getRegPressureLimit(const TargetRegisterClass *RC,
                                                MachineFunction &MF) const {
  switch (RC->getID()) {
  default:
    return 0;
  case ARM64::GPR32RegClassID:
  case ARM64::GPR32spRegClassID:
  case ARM64::GPR64spRegClassID:
  case ARM64::GPR64RegClassID:
  case ARM64::GPR32commonRegClassID:
  case ARM64::GPR64commonRegClassID:
    return 27; // XZR, SP, FP, LR, and X18 are reserved.

  case ARM64::FPR8RegClassID:
  case ARM64::FPR16RegClassID:
  case ARM64::FPR32RegClassID:
  case ARM64::FPR64RegClassID:
  case ARM64::FPR128RegClassID:
    return 32;

  case ARM64::DDRegClassID:
  case ARM64::DDDRegClassID:
  case ARM64::DDDDRegClassID:
  case ARM64::QQRegClassID:
  case ARM64::QQQRegClassID:
  case ARM64::QQQQRegClassID:
    return 32;

  case ARM64::FPR128_loRegClassID:
    return 16;
  }

}

} // namespace llvm
