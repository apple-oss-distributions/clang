//===- ARM64RegisterInfo.h - ARM64 Register Information Impl ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the ARM64 implementation of the MRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TARGET_ARM64REGISTERINFO_H
#define LLVM_TARGET_ARM64REGISTERINFO_H

#define GET_REGINFO_HEADER
#include "ARM64GenRegisterInfo.inc"

namespace llvm {

class ARM64InstrInfo;
class MachineFunction;
class RegScavenger;
class TargetRegisterClass;

struct ARM64RegisterInfo : public ARM64GenRegisterInfo {
private:
  const ARM64InstrInfo *TII;

public:
  ARM64RegisterInfo(const ARM64InstrInfo *tii);

  /// Code Generation virtual methods...
  bool isReservedReg(const MachineFunction &MF, unsigned Reg) const;
  const uint16_t *getCalleeSavedRegs(const MachineFunction *MF = 0) const;
  const uint32_t *getCallPreservedMask(CallingConv::ID) const;

  BitVector getReservedRegs(const MachineFunction &MF) const;
  const TargetRegisterClass* getPointerRegClass(const MachineFunction &MF,
                                                unsigned Kind = 0) const;
  const TargetRegisterClass*
  getCrossCopyRegClass(const TargetRegisterClass *RC) const;
  void eliminateCallFramePseudoInstr(MachineFunction &MF,
                                     MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator I) const;

  bool requiresRegisterScavenging(const MachineFunction &MF) const;
  bool useFPForScavengingIndex(const MachineFunction &MF) const;
  bool requiresFrameIndexScavenging(const MachineFunction &MF) const;

  bool needsFrameBaseReg(MachineInstr *MI, int64_t Offset) const;
  bool isFrameOffsetLegal(const MachineInstr *MI, int64_t Offset) const;
  void materializeFrameBaseRegister(MachineBasicBlock *MBB,
                                    unsigned BaseReg, int FrameIdx,
                                    int64_t Offset) const;
  void resolveFrameIndex(MachineBasicBlock::iterator I,
                         unsigned BaseReg, int64_t Offset) const;
  void eliminateFrameIndex(MachineBasicBlock::iterator II, int SPAdj,
                           unsigned FIOperandNum,
                           RegScavenger *RS = NULL) const;

  bool cannotEliminateFrame(const MachineFunction &MF) const;
  bool requiresVirtualBaseRegisters(const MachineFunction &MF) const;
  bool hasBasePointer(const MachineFunction &MF) const;
  unsigned getBaseRegister() const;

  // Debug information queries.
  unsigned getFrameRegister(const MachineFunction &MF) const;

  unsigned getRegPressureLimit(const TargetRegisterClass *RC,
                               MachineFunction &MF) const;
};

unsigned getWRegFromXReg(unsigned Reg);
unsigned getXRegFromWReg(unsigned Reg);

} // end namespace llvm

#endif // LLVM_TARGET_ARM64REGISTERINFO_H
