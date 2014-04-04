//===- ARM64MachineFuctionInfo.h - ARM64 machine function info --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares ARM64-specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef ARM64MACHINEFUNCTIONINFO_H
#define ARM64MACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

/// ARM64FunctionInfo - This class is derived from MachineFunctionInfo and
/// contains private ARM64-specific information for each MachineFunction.
class ARM64FunctionInfo : public MachineFunctionInfo {

  /// HasStackFrame - True if this function has a stack frame. Set by
  /// processFunctionBeforeCalleeSavedScan().
  bool HasStackFrame;

  /// \brief Amount of stack frame size, not including callee-saved registers.
  unsigned LocalStackSize;

  /// \brief FrameIndex for start of varargs area.
  int VarArgsFrameIndex;
public:
  ARM64FunctionInfo() : HasStackFrame(false), VarArgsFrameIndex(0) {}

  explicit ARM64FunctionInfo(MachineFunction &MF) :
    HasStackFrame(false), VarArgsFrameIndex(0) { (void)MF; }

  bool hasStackFrame() const { return HasStackFrame; }
  void setHasStackFrame(bool s) { HasStackFrame = s; }

  void setLocalStackSize(unsigned Size) { LocalStackSize = Size; }
  unsigned getLocalStackSize() const { return LocalStackSize; }

  int getVarArgsFrameIndex() const { return VarArgsFrameIndex; }
  void setVarArgsFrameIndex(int Index) { VarArgsFrameIndex = Index; }
};
} // End llvm namespace

#endif // ARM64MACHINEFUNCTIONINFO_H
