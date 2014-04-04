//===-- ARM64ExpandPseudoInsts.cpp - Expand pseudo instructions ---*- C++ -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that expands pseudo instructions into target
// instructions to allow proper scheduling and other late optimizations.  This
// pass should be run after register allocation but before the post-regalloc
// scheduling pass.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/ARM64AddressingModes.h"
#include "ARM64InstrInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/Support/MathExtras.h"
using namespace llvm;

namespace {
  class ARM64ExpandPseudo : public MachineFunctionPass {
  public:
    static char ID;
    ARM64ExpandPseudo() : MachineFunctionPass(ID) {}

    const ARM64InstrInfo *TII;

    virtual bool runOnMachineFunction(MachineFunction &Fn);

    virtual const char *getPassName() const {
      return "ARM64 pseudo instruction expansion pass";
    }

  private:
    void TransferImpOps(MachineInstr &OldMI,
                        MachineInstrBuilder &UseMI, MachineInstrBuilder &DefMI);
    bool ExpandMBB(MachineBasicBlock &MBB);
    bool ExpandMI(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI);
    bool ExpandMOVImm(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                      unsigned BitSize);
  };
  char ARM64ExpandPseudo::ID = 0;
}

/// TransferImpOps - Transfer implicit operands on the pseudo instruction to
/// the instructions created from the expansion.
void ARM64ExpandPseudo::TransferImpOps(MachineInstr &OldMI,
                                       MachineInstrBuilder &UseMI,
                                       MachineInstrBuilder &DefMI) {
  const MCInstrDesc &Desc = OldMI.getDesc();
  for (unsigned i = Desc.getNumOperands(), e = OldMI.getNumOperands();
       i != e; ++i) {
    const MachineOperand &MO = OldMI.getOperand(i);
    assert(MO.isReg() && MO.getReg());
    if (MO.isUse())
      UseMI.addOperand(MO);
    else
      DefMI.addOperand(MO);
  }
}

/// ExpandMOVImm - Expand a MOVi32imm or MOVi64imm pseudo instruction to one or
/// more real move-immediate instructions to synthesize the immediate.
bool ARM64ExpandPseudo::ExpandMOVImm(MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator MBBI,
                                     unsigned BitSize) {
  MachineInstr &MI = *MBBI;
  uint64_t Imm = MI.getOperand(1).getImm();

  // Try a MOVI instruction (aka ORR-immediate with the zero register).
  uint64_t UImm = Imm << (64 - BitSize) >> (64 - BitSize);
  uint64_t Encoding;
  if (ARM64_AM::processLogicalImmediate(UImm, BitSize, Encoding)) {
    unsigned Opc = (BitSize == 32 ? ARM64::ORRWri : ARM64::ORRXri);
    MachineInstrBuilder MIB =
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(Opc))
      .addOperand(MI.getOperand(0))
      .addReg(BitSize == 32 ? ARM64::WZR : ARM64::XZR)
      .addImm(Encoding);
    TransferImpOps(MI, MIB, MIB);
    MI.eraseFromParent();
    return true;
  }

  // Use a MOVZ or MOVN instruction to set the high bits, followed by one or
  // more MOVK instructions to insert additional 16-bit portions into the
  // lower bits.
  bool isNeg = false;
  if ((int64_t)Imm < 0) {
    isNeg = true;
    Imm = ~Imm;
  }
  unsigned FirstOpc;
  if (BitSize == 32) {
    Imm &= (1LL << 32) - 1;
    FirstOpc = (isNeg ? ARM64::MOVNWi : ARM64::MOVZWi);
  } else {
    FirstOpc = (isNeg ? ARM64::MOVNXi : ARM64::MOVZXi);
  }
  unsigned Shift = 0; // LSL amount for high bits with MOVZ/MOVN
  unsigned LastShift = 0; // LSL amount for last MOVK
  if (Imm != 0) {
    unsigned LZ = CountLeadingZeros_64(Imm);
    unsigned TZ = CountTrailingZeros_64(Imm);
    Shift = ((63 - LZ) / 16) * 16;
    LastShift = (TZ / 16) * 16;
  }
  unsigned Imm16 = (Imm >> Shift) & 0xffff;
  unsigned DstReg = MI.getOperand(0).getReg();
  bool DstIsDead = MI.getOperand(0).isDead();
  MachineInstrBuilder MIB1 =
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(FirstOpc))
    .addReg(DstReg, RegState::Define |
            getDeadRegState(DstIsDead && Shift == LastShift))
    .addImm(Imm16)
    .addImm(ARM64_AM::getShifterImm(ARM64_AM::LSL, Shift));

  // If a MOVN was used for the high bits of a negative value, flip the rest
  // of the bits back for use with MOVK.
  if (isNeg)
    Imm = ~Imm;

  if (Shift == LastShift) {
    TransferImpOps(MI, MIB1, MIB1);
    MI.eraseFromParent();
    return true;
  }

  MachineInstrBuilder MIB2;
  unsigned Opc = (BitSize == 32 ? ARM64::MOVKWi : ARM64::MOVKXi);
  while (Shift != LastShift) {
    Shift -= 16;
    Imm16 = (Imm >> Shift) & 0xffff;
    if (Imm16 == (isNeg ? 0xffff : 0))
      continue; // This 16-bit portion is already set correctly.
    MIB2 = BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(Opc))
      .addReg(DstReg, RegState::Define |
              getDeadRegState(DstIsDead && Shift == LastShift))
      .addImm(Imm16)
      .addImm(ARM64_AM::getShifterImm(ARM64_AM::LSL, Shift));
  }

  TransferImpOps(MI, MIB1, MIB2);
  MI.eraseFromParent();
  return true;
}

/// ExpandMI - If MBBI references a pseudo instruction that should be expanded
/// here, do the expansion and return true.  Otherwise return false.
bool ARM64ExpandPseudo::ExpandMI(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator MBBI) {
  MachineInstr &MI = *MBBI;
  unsigned Opcode = MI.getOpcode();
  switch (Opcode) {
  default: break;

  case ARM64::ADDWrr:
  case ARM64::SUBWrr:
  case ARM64::ADDXrr:
  case ARM64::SUBXrr:
  case ARM64::ADDSWrr:
  case ARM64::SUBSWrr:
  case ARM64::ADDSXrr:
  case ARM64::SUBSXrr:
  case ARM64::ANDWrr:
  case ARM64::ANDXrr:
  case ARM64::BICWrr:
  case ARM64::BICXrr:
  case ARM64::EONWrr:
  case ARM64::EONXrr:
  case ARM64::EORWrr:
  case ARM64::EORXrr:
  case ARM64::ORNWrr:
  case ARM64::ORNXrr:
  case ARM64::ORRWrr:
  case ARM64::ORRXrr: {
    unsigned Opcode;
    switch (MI.getOpcode()) {
      default: return false;
      case ARM64::ADDWrr:  Opcode = ARM64::ADDWrs; break;
      case ARM64::SUBWrr:  Opcode = ARM64::SUBWrs; break;
      case ARM64::ADDXrr:  Opcode = ARM64::ADDXrs; break;
      case ARM64::SUBXrr:  Opcode = ARM64::SUBXrs; break;
      case ARM64::ADDSWrr: Opcode = ARM64::ADDSWrs; break;
      case ARM64::SUBSWrr: Opcode = ARM64::SUBSWrs; break;
      case ARM64::ADDSXrr: Opcode = ARM64::ADDSXrs; break;
      case ARM64::SUBSXrr: Opcode = ARM64::SUBSXrs; break;
      case ARM64::ANDWrr:  Opcode = ARM64::ANDWrs; break;
      case ARM64::ANDXrr:  Opcode = ARM64::ANDXrs; break;
      case ARM64::BICWrr:  Opcode = ARM64::BICWrs; break;
      case ARM64::BICXrr:  Opcode = ARM64::BICXrs; break;
      case ARM64::EONWrr:  Opcode = ARM64::EONWrs; break;
      case ARM64::EONXrr:  Opcode = ARM64::EONXrs; break;
      case ARM64::EORWrr:  Opcode = ARM64::EORWrs; break;
      case ARM64::EORXrr:  Opcode = ARM64::EORXrs; break;
      case ARM64::ORNWrr:  Opcode = ARM64::ORNWrs; break;
      case ARM64::ORNXrr:  Opcode = ARM64::ORNXrs; break;
      case ARM64::ORRWrr:  Opcode = ARM64::ORRWrs; break;
      case ARM64::ORRXrr:  Opcode = ARM64::ORRXrs; break;
    }
    MachineInstrBuilder MIB1 = BuildMI(MBB, MBBI, MI.getDebugLoc(),
                                       TII->get(Opcode),
                                       MI.getOperand(0).getReg())
      .addOperand(MI.getOperand(1))
      .addOperand(MI.getOperand(2))
      .addImm(ARM64_AM::getShifterImm(ARM64_AM::LSL, 0));
    TransferImpOps(MI, MIB1, MIB1);
    MI.eraseFromParent();
    return true;
  }

  case ARM64::LOADgot: {
    // Expand into ADRP + LDR.
    unsigned DstReg = MI.getOperand(0).getReg();
    const MachineOperand &MO1 = MI.getOperand(1);
    MachineInstrBuilder MIB1 = BuildMI(MBB, MBBI, MI.getDebugLoc(),
                                       TII->get(ARM64::ADRP), DstReg)
      .addGlobalAddress(MO1.getGlobal(), 0,
                        ARM64II::MO_GOT | ARM64II::MO_PAGE);
    MachineInstrBuilder MIB2 = BuildMI(MBB, MBBI, MI.getDebugLoc(),
                                       TII->get(ARM64::LDRXui))
      .addOperand(MI.getOperand(0))
      .addReg(DstReg)
      .addGlobalAddress(MO1.getGlobal(), 0,
                        ARM64II::MO_GOT | ARM64II::MO_PAGEOFF);
    TransferImpOps(MI, MIB1, MIB2);
    MI.eraseFromParent();
    return true;
  }

  case ARM64::MOVaddr:
  case ARM64::MOVaddrJT:
  case ARM64::MOVaddrCP:
  case ARM64::MOVaddrBA: {
    // Expand into ADRP + ADD.

    unsigned DstReg = MI.getOperand(0).getReg();
    MachineInstrBuilder MIB1 = BuildMI(MBB, MBBI, MI.getDebugLoc(),
                                       TII->get(ARM64::ADRP), DstReg)
      .addOperand(MI.getOperand(1));

    MachineInstrBuilder MIB2 = BuildMI(MBB, MBBI, MI.getDebugLoc(),
                                       TII->get(ARM64::ADDXri))
      .addOperand(MI.getOperand(0))
      .addReg(DstReg)
      .addOperand(MI.getOperand(2))
      .addImm(0);

    TransferImpOps(MI, MIB1, MIB2);
    MI.eraseFromParent();
    return true;
  }

  case ARM64::MOVi32imm:
    return ExpandMOVImm(MBB, MBBI, 32);

  case ARM64::MOVi64imm:
    return ExpandMOVImm(MBB, MBBI, 64);
  }
  return false;
}

/// ExpandMBB - Iterate over the instructions in basic block MBB and expand
/// any pseudo instructions.  Return true if anything was modified.
bool ARM64ExpandPseudo::ExpandMBB(MachineBasicBlock &MBB) {
  bool Modified = false;

  MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
  while (MBBI != E) {
    MachineBasicBlock::iterator NMBBI = llvm::next(MBBI);
    Modified |= ExpandMI(MBB, MBBI);
    MBBI = NMBBI;
  }

  return Modified;
}

bool ARM64ExpandPseudo::runOnMachineFunction(MachineFunction &MF) {
  TII = static_cast<const ARM64InstrInfo*>(MF.getTarget().getInstrInfo());

  bool Modified = false;
  for (MachineFunction::iterator MFI = MF.begin(), E = MF.end(); MFI != E;
       ++MFI)
    Modified |= ExpandMBB(*MFI);
  return Modified;
}

/// createARM64ExpandPseudoPass - Returns an instance of the pseudo instruction
/// expansion pass.
FunctionPass *llvm::createARM64ExpandPseudoPass() {
  return new ARM64ExpandPseudo();
}
