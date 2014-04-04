//===-- ARM64MCInstLower.cpp - Convert ARM64 MachineInstr to an MCInst---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains code to lower ARM64 MachineInstrs to their corresponding
// MCInst records.
//
//===----------------------------------------------------------------------===//

#include "ARM64MCInstLower.h"
#include "MCTargetDesc/ARM64BaseInfo.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Target/Mangler.h"
using namespace llvm;

MCSymbol *ARM64MCInstLower::
GetGlobalAddressSymbol(const MachineOperand &MO) const {
  return Printer.Mang->getSymbol(MO.getGlobal());
}

MCSymbol *ARM64MCInstLower::
GetExternalSymbolSymbol(const MachineOperand &MO) const {
  return Printer.GetExternalSymbolSymbol(MO.getSymbolName());
}

MCOperand ARM64MCInstLower::
LowerSymbolOperand(const MachineOperand &MO, MCSymbol *Sym) const {
  // FIXME: We would like an efficient form for this, so we don't have to do a
  // lot of extra uniquing.
  MCSymbolRefExpr::VariantKind RefKind = MCSymbolRefExpr::VK_None;
  if ((MO.getTargetFlags() & ARM64II::MO_GOT) != 0) {
    if ((MO.getTargetFlags() & ARM64II::MO_PAGE) != 0)
      RefKind = MCSymbolRefExpr::VK_GOTPAGE;
    else if ((MO.getTargetFlags() & ARM64II::MO_PAGEOFF) != 0)
      RefKind = MCSymbolRefExpr::VK_GOTPAGEOFF;
    else
      assert(0 && "Unexpected target flags with MO_GOT on GV operand");
  } else {
    if ((MO.getTargetFlags() & ARM64II::MO_PAGE) != 0)
      RefKind = MCSymbolRefExpr::VK_PAGE;
    else if ((MO.getTargetFlags() & ARM64II::MO_PAGEOFF) != 0)
      RefKind = MCSymbolRefExpr::VK_PAGEOFF;
  }
  const MCExpr *Expr = MCSymbolRefExpr::Create(Sym, RefKind, Ctx);
  if (!MO.isJTI() && MO.getOffset())
    Expr = MCBinaryExpr::CreateAdd(Expr,
                                   MCConstantExpr::Create(MO.getOffset(), Ctx),
                                   Ctx);
  return MCOperand::CreateExpr(Expr);
}

bool ARM64MCInstLower::lowerOperand(const MachineOperand &MO, MCOperand &MCOp) const {
  switch (MO.getType()) {
  default:
    assert(0 && "unknown operand type");
  case MachineOperand::MO_Register:
    // Ignore all implicit register operands.
    if (MO.isImplicit()) return false;
    MCOp = MCOperand::CreateReg(MO.getReg());
    break;
  case MachineOperand::MO_RegisterMask:
    // Regmasks are like implicit defs.
    return false;
  case MachineOperand::MO_Immediate:
    MCOp = MCOperand::CreateImm(MO.getImm());
    break;
  case MachineOperand::MO_MachineBasicBlock:
    MCOp = MCOperand::CreateExpr(MCSymbolRefExpr::Create(
        MO.getMBB()->getSymbol(), Ctx));
    break;
  case MachineOperand::MO_GlobalAddress:
    MCOp = LowerSymbolOperand(MO, GetGlobalAddressSymbol(MO));
    break;
  case MachineOperand::MO_ExternalSymbol:
    MCOp = LowerSymbolOperand(MO, GetExternalSymbolSymbol(MO));
    break;
  case MachineOperand::MO_JumpTableIndex:
    MCOp = LowerSymbolOperand(MO, Printer.GetJTISymbol(MO.getIndex()));
    break;
  case MachineOperand::MO_ConstantPoolIndex:
    MCOp = LowerSymbolOperand(MO, Printer.GetCPISymbol(MO.getIndex()));
    break;
  case MachineOperand::MO_BlockAddress:
    MCOp = LowerSymbolOperand(MO,
                              Printer.GetBlockAddressSymbol(MO.getBlockAddress()));
    break;
  }
  return true;
}

void ARM64MCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const {
  OutMI.setOpcode(MI->getOpcode());

  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    MCOperand MCOp;
    if (lowerOperand(MI->getOperand(i), MCOp))
      OutMI.addOperand(MCOp);
  }
}
