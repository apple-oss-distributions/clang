//===-- ARM64InstPrinter.cpp - Convert ARM64 MCInst to assembly syntax ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class prints an ARM64 MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "asm-printer"
#include "ARM64InstPrinter.h"
#include "MCTargetDesc/ARM64AddressingModes.h"
#include "MCTargetDesc/ARM64BaseInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define GET_INSTRUCTION_NAME
#include "ARM64GenAsmWriter.inc"

ARM64InstPrinter::ARM64InstPrinter(const MCAsmInfo &MAI,
                                   const MCInstrInfo &MII,
                                   const MCRegisterInfo &MRI,
                                   const MCSubtargetInfo &STI) :
  MCInstPrinter(MAI, MII, MRI) {
  // Initialize the set of available features.
  setAvailableFeatures(STI.getFeatureBits());
}

// FIXME: Code duplicated from ARM64RegisterInfo.cpp
static unsigned getWRegFromXReg(unsigned Reg) {
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

void ARM64InstPrinter::printRegName(raw_ostream &OS, unsigned RegNo) const {
  // This is for .cfi directives.
  OS << getRegisterName(RegNo);
}

void ARM64InstPrinter::printInst(const MCInst *MI, raw_ostream &O,
                                 StringRef Annot) {
  // Check for special encodings and print the cannonical alias instead.

  unsigned Opcode = MI->getOpcode();

  // Move to/from SP is encoded as add of #0.
  //   "add <Wd|WSP>, <Wn|WSP>, #0" is "mov <Wd|WSP>, <Wn|WSP>"
  //   "add <Xd|SP>, <Xd|SP>, #0" is "mov <Xd|SP>, <Xn|SP>"
  if (Opcode == ARM64::ADDXri || Opcode == ARM64::ADDWri) {
    const MCOperand &Op0 = MI->getOperand(0);
    const MCOperand &Op1 = MI->getOperand(1);
    if (MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0 &&
        MI->getOperand(3).getImm() == 0 &&
        (Op0.getReg() == ARM64::SP || Op0.getReg() == ARM64::WSP ||
         Op1.getReg() == ARM64::SP || Op1.getReg() == ARM64::WSP)) {
      O << '\t' << "mov" << '\t'
        << getRegisterName(Op0.getReg()) << ", "
        << getRegisterName(Op1.getReg());
      printAnnotation(O, Annot);
      return;
    }
  }

  // Register copy is encoded as ORR.
  //   "orr Wd, WZR, Wn" is "mov Wd, Wn"
  //   "orr Xd, XZR, Xn" is "mov Xd, Xn"
  if (Opcode == ARM64::ORRXrs || Opcode == ARM64::ORRWrs) {
    const MCOperand &Op0 = MI->getOperand(0);
    const MCOperand &Op1 = MI->getOperand(1);
    const MCOperand &Op2 = MI->getOperand(2);
    if ((Op1.getReg() == ARM64::WZR || Op1.getReg() == ARM64::XZR) &&
        MI->getOperand(3).getImm() == 0) {
      O << '\t' << "mov" << '\t'
        << getRegisterName(Op0.getReg()) << ", "
        << getRegisterName(Op2.getReg());
      printAnnotation(O, Annot);
      return;
    }
  }

  if (Opcode == ARM64::SYS || Opcode == ARM64::SYSxt)
    if (printSysAlias(MI, O)) {
      printAnnotation(O, Annot);
      return;
    }

  // TBZ/TBNZ should print the register operand as a Wreg if the bit
  // number is < 32.
  if ((Opcode == ARM64::TBNZ || Opcode == ARM64::TBZ) &&
      MI->getOperand(1).getImm() < 32) {
    MCInst newMI = *MI;
    unsigned Reg = MI->getOperand(0).getReg();
    newMI.getOperand(0).setReg(getWRegFromXReg(Reg));
    printInstruction(&newMI, O);
    printAnnotation(O, Annot);
    return;
  }

  // SBFM/UBFM should print to a nicer aliased form if possible.
  if (Opcode == ARM64::SBFMXri || Opcode == ARM64::SBFMWri ||
      Opcode == ARM64::UBFMXri || Opcode == ARM64::UBFMWri) {
    const MCOperand &Op0 = MI->getOperand(0);
    const MCOperand &Op1 = MI->getOperand(1);
    const MCOperand &Op2 = MI->getOperand(2);
    const MCOperand &Op3 = MI->getOperand(3);

    if (Op2.isImm() && Op2.getImm() == 0 && Op3.isImm()) {
      bool IsSigned = (Opcode == ARM64::SBFMXri || Opcode == ARM64::SBFMWri);
      const char *AsmMnemonic = 0;

      switch (Op3.getImm()) {
      default: break;
      case 7:
        AsmMnemonic = IsSigned ? "sxtb" : "uxtb";
        break;
      case 15:
        AsmMnemonic = IsSigned ? "sxth" : "uxth";
        break;
      case 31:
        AsmMnemonic = IsSigned ? "sxtw" : "uxtw";
        break;
      }

      if (AsmMnemonic) {
        O << '\t' << AsmMnemonic << '\t'
          << getRegisterName(Op0.getReg())
          << ", " << getRegisterName(Op1.getReg());
        printAnnotation(O, Annot);
        return;
      }
    }
    // All immediate shifts are aliases, implemented using the Bitfield
    // instruction. In all cases the immediate shift amount shift must be in
    // the range 0 to (reg.size -1).
    if (Op2.isImm() && Op3.isImm()) {
      const char *AsmMnemonic = 0;
      int shift = 0;
      int64_t immr = Op2.getImm();
      int64_t imms = Op3.getImm();
      if (Opcode == ARM64::UBFMWri && imms != 0x1F && ((imms + 1) == immr)) {
        AsmMnemonic = "lsl";
        shift = 31 - imms;
      } else if (Opcode == ARM64::UBFMXri && imms != 0x3f &&
                 ((imms + 1 == immr))) {
        AsmMnemonic = "lsl";
        shift = 63 - imms;
      } else if (Opcode == ARM64::UBFMWri && imms == 0x1f) {
        AsmMnemonic = "lsr";
        shift = immr;
      } else if (Opcode == ARM64::UBFMXri && imms == 0x3f) {
        AsmMnemonic = "lsr";
        shift = immr;
      } else if (Opcode == ARM64::SBFMWri && imms == 0x1f) {
        AsmMnemonic = "asr";
        shift = immr;
      } else if (Opcode == ARM64::SBFMXri && imms == 0x3f) {
        AsmMnemonic = "asr";
        shift = immr;
      }
      if (AsmMnemonic) {
        O << '\t' << AsmMnemonic << '\t'
          << getRegisterName(Op0.getReg())
          << ", " << getRegisterName(Op1.getReg())
          << ", #" << shift;
        printAnnotation(O, Annot);
        return;
      }
    }
  }

  // Symbolic operands for MOVZ, MOVN and MOVK already imply a shift
  // (e.g. :gottprel_g1: is always going to be "lsl #16") so it should not be
  // printed.
  if ((Opcode == ARM64::MOVZXi || Opcode == ARM64::MOVZWi ||
       Opcode == ARM64::MOVNXi || Opcode == ARM64::MOVNWi) &&
      MI->getOperand(1).isExpr()) {
    if (Opcode == ARM64::MOVZXi || Opcode == ARM64::MOVZWi)
      O << "\tmovz\t";
    else
      O << "\tmovn\t";

    O << getRegisterName(MI->getOperand(0).getReg()) << ", #"
      << *MI->getOperand(1).getExpr();
    return;
  }

  if ((Opcode == ARM64::MOVKXi || Opcode == ARM64::MOVKWi) &&
      MI->getOperand(2).isExpr()) {
    O << "\tmovk\t"
      << getRegisterName(MI->getOperand(0).getReg()) << ", #"
      << *MI->getOperand(2).getExpr();
    return;
  }

  // HINT #[0-5] should print as nop, yield, wfe, wfi, sev, and sevl,
  // respectively.
  if (Opcode == ARM64::HINT && MI->getOperand(0).getImm() < 6) {
    static const char *Mnemonic[] = {
      "nop", "yield", "wfe", "wfi", "sev", "sevl"
    };
    O << '\t' << Mnemonic[MI->getOperand(0).getImm()];
    printAnnotation(O, Annot);
    return;
  }

  // ANDS WZR, Wn, #imm ==> TST Wn, #imm
  // ANDS XZR, Xn, #imm ==> TST Xn, #imm
  if (Opcode == ARM64::ANDSWri && MI->getOperand(0).getReg() == ARM64::WZR) {
    O << "\ttst\t" << getRegisterName(MI->getOperand(1).getReg()) << ", ";
    printLogicalImm32(MI, 2, O);
    return;
  }
  if (Opcode == ARM64::ANDSXri && MI->getOperand(0).getReg() == ARM64::XZR) {
    O << "\ttst\t" << getRegisterName(MI->getOperand(1).getReg()) << ", ";
    printLogicalImm64(MI, 2, O);
    return;
  }
  // ANDS WZR, Wn, Wm{, lshift #imm} ==> TST Wn{, lshift #imm}
  // ANDS XZR, Xn, Xm{, lshift #imm} ==> TST Xn{, lshift #imm}
  if ((Opcode == ARM64::ANDSWrs && MI->getOperand(0).getReg() == ARM64::WZR) ||
      (Opcode == ARM64::ANDSXrs && MI->getOperand(0).getReg() == ARM64::XZR)) {
    O << "\ttst\t" << getRegisterName(MI->getOperand(1).getReg()) << ", ";
    printShiftedRegister(MI, 2, O);
    return;
  }

  // SUBS WZR, Wn, #imm ==> CMP Wn, #imm
  // SUBS XZR, Xn, #imm ==> CMP Xn, #imm
  if ((Opcode == ARM64::SUBSWri && MI->getOperand(0).getReg() == ARM64::WZR) ||
      (Opcode == ARM64::SUBSXri && MI->getOperand(0).getReg() == ARM64::XZR)) {
    O << "\tcmp\t" << getRegisterName(MI->getOperand(1).getReg()) << ", ";
    printAddSubImm(MI, 2 ,O);
    return;
  }
  // SUBS WZR, Wn, Wm{, lshift #imm} ==> CMP Wn, Wm{, lshift #imm}
  // SUBS XZR, Xn, Xm{, lshift #imm} ==> CMP Xn, Xm{, lshift #imm}
  if ((Opcode == ARM64::SUBSWrs && MI->getOperand(0).getReg() == ARM64::WZR) ||
      (Opcode == ARM64::SUBSXrs && MI->getOperand(0).getReg() == ARM64::XZR)) {
    O << "\tcmp\t" << getRegisterName(MI->getOperand(1).getReg()) << ", ";
    printShiftedRegister(MI, 2, O);
    return;
  }
  // SUBS XZR, Xn, Wm, uxtb #imm ==> CMP Xn, uxtb #imm
  if (Opcode == ARM64::SUBSXrx && MI->getOperand(0).getReg() == ARM64::XZR) {
    O << "\tcmp\t" << getRegisterName(MI->getOperand(1).getReg()) << ", ";
    printExtendedRegister(MI, 2, O);
    return;
  }
  // SUBS XZR, Xn, Xm, uxtx #imm ==> CMP Xn, uxtb #imm
  if (Opcode == ARM64::SUBSXrx64 && MI->getOperand(0).getReg() == ARM64::XZR) {
    O << "\tcmp\t" << getRegisterName(MI->getOperand(1).getReg()) << ", "
      << getRegisterName(MI->getOperand(2).getReg());
    printExtend(MI, 3, O);
    return;
  }

  // ADDS WZR, Wn, #imm ==> CMN Wn, #imm
  // ADDS XZR, Xn, #imm ==> CMN Xn, #imm
  if ((Opcode == ARM64::ADDSWri && MI->getOperand(0).getReg() == ARM64::WZR) ||
      (Opcode == ARM64::ADDSXri && MI->getOperand(0).getReg() == ARM64::XZR)) {
    O << "\tcmn\t" << getRegisterName(MI->getOperand(1).getReg()) << ", ";
    printAddSubImm(MI, 2 ,O);
    return;
  }
  // ADDS WZR, Wn, Wm{, lshift #imm} ==> CMN Wn, Wm{, lshift #imm}
  // ADDS XZR, Xn, Xm{, lshift #imm} ==> CMN Xn, Xm{, lshift #imm}
  if ((Opcode == ARM64::ADDSWrs && MI->getOperand(0).getReg() == ARM64::WZR) ||
      (Opcode == ARM64::ADDSXrs && MI->getOperand(0).getReg() == ARM64::XZR)) {
    O << "\tcmn\t" << getRegisterName(MI->getOperand(1).getReg()) << ", ";
    printShiftedRegister(MI, 2, O);
    return;
  }
  // ADDS XZR, Xn, Wm, uxtb #imm ==> CMN Xn, uxtb #imm
  if (Opcode == ARM64::ADDSXrx && MI->getOperand(0).getReg() == ARM64::XZR) {
    O << "\tcmn\t" << getRegisterName(MI->getOperand(1).getReg()) << ", ";
    printExtendedRegister(MI, 2, O);
    return;
  }
  // ADDS XZR, Xn, Xm, uxtx #imm ==> CMN Xn, uxtb #imm
  if (Opcode == ARM64::ADDSXrx64 && MI->getOperand(0).getReg() == ARM64::XZR) {
    O << "\tcmn\t" << getRegisterName(MI->getOperand(1).getReg()) << ", "
      << getRegisterName(MI->getOperand(2).getReg());
    printExtend(MI, 3, O);
    return;
  }

  // ORN Wd, WZR, Wm{, lshift #imm} ==> MVN Wd, Wm{, lshift #imm}
  // ORN Xd, XZR, Xm{, lshift #imm} ==> MVN Xd, Xm{, lshift #imm}
  if ((Opcode == ARM64::ORNWrs && MI->getOperand(1).getReg() == ARM64::WZR) ||
      (Opcode == ARM64::ORNXrs && MI->getOperand(1).getReg() == ARM64::XZR)) {
    O << "\tmvn\t" << getRegisterName(MI->getOperand(0).getReg()) << ", ";
    printShiftedRegister(MI, 2, O);
    return;
  }


  printInstruction(MI, O);
  printAnnotation(O, Annot);
}

bool ARM64InstPrinter::printSysAlias(const MCInst *MI, raw_ostream &O) {
  unsigned Opcode = MI->getOpcode();
  assert((Opcode == ARM64::SYS || Opcode == ARM64::SYSxt) &&
         "Invalid opcode for SYS alias!");

  const char *Asm = 0;
  const MCOperand &Op1 = MI->getOperand(0);
  const MCOperand &Cn  = MI->getOperand(1);
  const MCOperand &Cm  = MI->getOperand(2);
  const MCOperand &Op2 = MI->getOperand(3);

  unsigned Op1Val = Op1.getImm();
  unsigned CnVal  = Cn.getImm();
  unsigned CmVal  = Cm.getImm();
  unsigned Op2Val = Op2.getImm();

  if (CnVal == 7) {
    switch (CmVal) {
    default: break;

    // IC aliases
    case 1:
      if (Op1Val == 0 && Op2Val == 0)
        Asm = "ic\tialluis";
      break;
    case 5:
      if (Op1Val == 0 && Op2Val == 0)
        Asm = "ic\tiallu";
      else if (Op1Val == 3 && Op2Val == 1)
        Asm = "ic\tivau";
      break;

    // DC aliases
    case 4:
      if (Op1Val == 3 && Op2Val == 1)
        Asm = "dc\tzva";
      break;
    case 6:
      if (Op1Val == 0 && Op2Val == 1)
        Asm = "dc\tivac";
      if (Op1Val == 0 && Op2Val == 2)
        Asm = "dc\tisw";
      break;
    case 10:
      if (Op1Val == 3 && Op2Val == 1)
        Asm = "dc\tcvac";
      else if (Op1Val == 0 && Op2Val == 2)
        Asm = "dc\tcsw";
      break;
    case 11:
      if (Op1Val == 3 && Op2Val == 1)
        Asm = "dc\tcvau";
      break;
    case 14:
      if (Op1Val == 3 && Op2Val == 1)
        Asm = "dc\tcivac";
      else if (Op1Val == 0 && Op2Val == 2)
        Asm = "dc\tcisw";
      break;

    // AT aliases
    case 8:
      switch (Op1Val) {
      default: break;
      case 0:
        switch (Op2Val) {
        default: break;
        case 0: Asm = "at\ts1e1r"; break;
        case 1: Asm = "at\ts1e1w"; break;
        case 2: Asm = "at\ts1e0r"; break;
        case 3: Asm = "at\ts1e0w"; break;
        }
        break;
      case 4:
        switch (Op2Val) {
        default: break;
        case 0: Asm = "at\ts1e2r";  break;
        case 1: Asm = "at\ts1e2w";  break;
        case 4: Asm = "at\ts12e1r"; break;
        case 5: Asm = "at\ts12e1w"; break;
        case 6: Asm = "at\ts12e0r"; break;
        case 7: Asm = "at\ts12e0w"; break;
        }
        break;
      case 6:
        switch (Op2Val) {
        default: break;
        case 0: Asm = "at\ts1e3r"; break;
        case 1: Asm = "at\ts1e3w"; break;
        }
        break;
      }
      break;
    }
  } else if (CnVal == 8) {
    // TLBI aliases
    switch (CmVal) {
    default: break;
    case 3:
      switch (Op1Val) {
      default: break;
      case 0:
        switch (Op2Val) {
        default: break;
        case 0: Asm = "tlbi\tvmalle1is"; break;
        case 1: Asm = "tlbi\tvae1is";    break;
        case 2: Asm = "tlbi\taside1is";  break;
        case 3: Asm = "tlbi\tvaae1is";   break;
        case 5: Asm = "tlbi\tvale1is";   break;
        case 7: Asm = "tlbi\tvaale1is";  break;
        }
        break;
      case 4:
        switch (Op2Val) {
        default: break;
        case 0: Asm = "tlbi\talle2is";   break;
        case 1: Asm = "tlbi\tvae2is";    break;
        case 4: Asm = "tlbi\talle1is";   break;
        case 5: Asm = "tlbi\tvale2is";   break;
        case 6: Asm = "tlbi\tvmalls12e1is"; break;
        }
        break;
      case 6:
        switch (Op2Val) {
        default: break;
        case 0: Asm = "tlbi\talle3is";   break;
        case 1: Asm = "tlbi\tvae3is";    break;
        case 5: Asm = "tlbi\tvale3is";   break;
        }
        break;
      }
      break;
    case 4:
      switch (Op1Val) {
      default: break;
      case 4:
        switch (Op2Val) {
        default: break;
        case 1: Asm = "tlbi\tipas2e1";  break;
        case 5: Asm = "tlbi\tipas2le1"; break;
        }
        break;
      }
      break;
    case 7:
      switch (Op1Val) {
      default: break;
      case 0:
        switch (Op2Val) {
        default: break;
        case 0: Asm = "tlbi\tvmalle1";   break;
        case 1: Asm = "tlbi\tvae1";      break;
        case 2: Asm = "tlbi\taside1";    break;
        case 3: Asm = "tlbi\tvaae1";     break;
        case 5: Asm = "tlbi\tvale1";     break;
        case 7: Asm = "tlbi\tvaale1";    break;
        }
        break;
      case 4:
        switch (Op2Val) {
        default: break;
        case 0: Asm = "tlbi\talle2";     break;
        case 1: Asm = "tlbi\tvae2";      break;
        case 4: Asm = "tlbi\talle1";     break;
        case 5: Asm = "tlbi\tvale2";     break;
        case 6: Asm = "tlbi\tvmalls12e1"; break;
        }
        break;
      case 6:
        switch (Op2Val) {
        default: break;
        case 0: Asm = "tlbi\talle3";     break;
        case 1: Asm = "tlbi\tvae3";      break;
        case 5: Asm = "tlbi\tvale3";     break;
        }
        break;
      }
      break;
    }
  }

  if (Asm) {
    O << '\t' << Asm;
    if (MI->getNumOperands() == 5)
      O << ", " << getRegisterName(MI->getOperand(4).getReg());
  }

  return Asm != 0;
}

void ARM64InstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                    raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNo);
  if (Op.isReg()) {
    unsigned Reg = Op.getReg();
    O << getRegisterName(Reg);
  } else if (Op.isImm()) {
    O << '#' << Op.getImm();
  } else {
    assert(Op.isExpr() && "unknown operand kind in printOperand");
    O << *Op.getExpr();
  }
}

void ARM64InstPrinter::printPostIncOperand(const MCInst *MI, unsigned OpNo,
                                           unsigned Imm, raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNo);
  if (Op.isReg()) {
    unsigned Reg = Op.getReg();
    if (Reg == ARM64::XZR)
      O << "#" << Imm;
    else
      O << getRegisterName(Reg);
  } else
    assert("unknown operand kind in printPostIncOperand64");
}

void ARM64InstPrinter::printPostIncOperand1(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 1, O);
}

void ARM64InstPrinter::printPostIncOperand2(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 2, O);
}

void ARM64InstPrinter::printPostIncOperand3(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 3, O);
}

void ARM64InstPrinter::printPostIncOperand4(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 4, O);
}

void ARM64InstPrinter::printPostIncOperand6(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 6, O);
}

void ARM64InstPrinter::printPostIncOperand8(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 8, O);
}

void ARM64InstPrinter::printPostIncOperand12(const MCInst *MI, unsigned OpNo,
                                             raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 12, O);
}

void ARM64InstPrinter::printPostIncOperand16(const MCInst *MI, unsigned OpNo,
                                             raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 16, O);
}

void ARM64InstPrinter::printPostIncOperand24(const MCInst *MI, unsigned OpNo,
                                             raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 24, O);
}

void ARM64InstPrinter::printPostIncOperand32(const MCInst *MI, unsigned OpNo,
                                             raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 32, O);
}

void ARM64InstPrinter::printPostIncOperand48(const MCInst *MI, unsigned OpNo,
                                             raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 48, O);
}

void ARM64InstPrinter::printPostIncOperand64(const MCInst *MI, unsigned OpNo,
                                             raw_ostream &O) {
  printPostIncOperand(MI, OpNo, 64, O);
}

void ARM64InstPrinter::printVRegOperand(const MCInst *MI, unsigned OpNo,
                                        raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNo);
  assert (Op.isReg() && "Non-register vreg operand!");
  unsigned Reg = Op.getReg();
  O << getRegisterName(Reg, ARM64::vreg);
}

void ARM64InstPrinter::printSysCROperand(const MCInst *MI, unsigned OpNo,
                                         raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNo);
  assert(Op.isImm() && "System instruction C[nm] operands must be immediates!");
  O << "c" << Op.getImm();
}

void ARM64InstPrinter::printAddSubImm(const MCInst *MI, unsigned OpNum,
                                      raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(OpNum);
  if (MO.isImm()) {
    unsigned Val = (MO.getImm() & 0xfff);
    assert(Val == MO.getImm() && "Add/sub immediate out of range!");
    unsigned Shift = ARM64_AM::getShiftValue(MI->getOperand(OpNum+1).getImm());
    O << '#' << (Val << Shift);
    // Distinguish "0, lsl #12" from "0, lsl #0".
    if (Val == 0 && Shift != 0)
      printShifter(MI, OpNum+1, O);
  } else {
    assert(MO.isExpr() && "Unexpected operand type!");
    O << *MO.getExpr();
    printShifter(MI, OpNum+1, O);
  }
}

void ARM64InstPrinter::printLogicalImm32(const MCInst *MI, unsigned OpNum,
                                         raw_ostream &O) {
  uint64_t Val = MI->getOperand(OpNum).getImm();
  O << "#0x";
  O.write_hex(ARM64_AM::decodeLogicalImmediate(Val, 32));
}

void ARM64InstPrinter::printLogicalImm64(const MCInst *MI, unsigned OpNum,
                                         raw_ostream &O) {
  uint64_t Val = MI->getOperand(OpNum).getImm();
  O << "#0x";
  O.write_hex(ARM64_AM::decodeLogicalImmediate(Val, 64));
}

void ARM64InstPrinter::printShifter(const MCInst *MI, unsigned OpNum,
                                    raw_ostream &O) {
  unsigned Val = MI->getOperand(OpNum).getImm();
  // LSL #0 should not be printed.
  if (ARM64_AM::getShiftType(Val) == ARM64_AM::LSL &&
      ARM64_AM::getShiftValue(Val) == 0)
    return;
  O << ", " << ARM64_AM::getShiftName(ARM64_AM::getShiftType(Val))
    << " #" << ARM64_AM::getShiftValue(Val);
}

void ARM64InstPrinter::printShiftedRegister(const MCInst *MI, unsigned OpNum,
                                            raw_ostream &O) {
  O << getRegisterName(MI->getOperand(OpNum).getReg());
  printShifter(MI, OpNum+1, O);
}

void ARM64InstPrinter::printExtendedRegister(const MCInst *MI, unsigned OpNum,
                                            raw_ostream &O) {
  O << getRegisterName(MI->getOperand(OpNum).getReg());
  printExtend(MI, OpNum+1, O);
}

void ARM64InstPrinter::printExtend(const MCInst *MI, unsigned OpNum,
                                   raw_ostream &O) {
  unsigned Val = MI->getOperand(OpNum).getImm();
  ARM64_AM::ExtendType ExtType = ARM64_AM::getArithExtendType(Val);
  unsigned ShiftVal = ARM64_AM::getArithShiftValue(Val);

  // If the destination or first source register operand is [W]SP, print
  // UXTW/UXTX as LSL, and if the shift amount is also zero, print nothing at
  // all.
  if (ExtType == ARM64_AM::UXTW || ExtType == ARM64_AM::UXTX) {
    unsigned Dest = MI->getOperand(0).getReg();
    unsigned Src1 = MI->getOperand(1).getReg();
    if (Dest == ARM64::SP || Dest == ARM64::WSP ||
        Src1 == ARM64::SP || Src1 == ARM64::WSP) {
      if (ShiftVal != 0)
        O << ", lsl #" << ShiftVal;
      return;
    }
  }
  O << ", " << ARM64_AM::getExtendName(ExtType);
  if (ShiftVal != 0)
    O << " #" << ShiftVal;
}

void ARM64InstPrinter::printDotCondCode(const MCInst *MI, unsigned OpNum,
                                        raw_ostream &O) {
  ARM64CC::CondCode CC = (ARM64CC::CondCode)MI->getOperand(OpNum).getImm();
  if (CC != ARM64CC::AL)
    O << '.' << ARM64CC::getCondCodeName(CC);
}

void ARM64InstPrinter::printCondCode(const MCInst *MI, unsigned OpNum,
                                     raw_ostream &O) {
  ARM64CC::CondCode CC = (ARM64CC::CondCode)MI->getOperand(OpNum).getImm();
  O << ARM64CC::getCondCodeName(CC);
}

void ARM64InstPrinter::printAMNoIndex(const MCInst *MI, unsigned OpNum,
                                      raw_ostream &O) {
  O << '[' << getRegisterName(MI->getOperand(OpNum).getReg()) << ']';
}

void ARM64InstPrinter::printImmScale4(const MCInst *MI, unsigned OpNum,
                                      raw_ostream &O) {
  O << '#' << 4 * MI->getOperand(OpNum).getImm();
}

void ARM64InstPrinter::printImmScale8(const MCInst *MI, unsigned OpNum,
                                      raw_ostream &O) {
  O << '#' << 8 * MI->getOperand(OpNum).getImm();
}

void ARM64InstPrinter::printImmScale16(const MCInst *MI, unsigned OpNum,
                                       raw_ostream &O) {
  O << '#' << 16 * MI->getOperand(OpNum).getImm();
}

void ARM64InstPrinter::printAMIndexed(const MCInst *MI, unsigned OpNum,
                                      unsigned Scale, raw_ostream &O) {
  const MCOperand MO1 = MI->getOperand(OpNum + 1);
  O << '[' << getRegisterName(MI->getOperand(OpNum).getReg());
  if (MO1.isImm()) {
    if (MO1.getImm() != 0)
      O << ", #" << (MO1.getImm() * Scale);
  } else {
    assert (MO1.isExpr() && "Unexpected operand type!");
    O << ", " << *MO1.getExpr();
  }
  O << ']';
}

void ARM64InstPrinter::printPrefetchOp(const MCInst *MI, unsigned OpNum,
                                       raw_ostream &O) {
  unsigned prfop = MI->getOperand(OpNum).getImm();
  if (ARM64_AM::isNamedPrefetchOp(prfop))
    O << ARM64_AM::getPrefetchOpName((ARM64_AM::PrefetchOp)prfop);
  else
    O << '#' << prfop;
}

void ARM64InstPrinter::printMemoryPostIndexed32(const MCInst *MI,unsigned OpNum,
                                             raw_ostream &O) {
  O << '[' << getRegisterName(MI->getOperand(OpNum).getReg()) << ']'
    << ", #" << 4 * MI->getOperand(OpNum + 1).getImm();
}

void ARM64InstPrinter::printMemoryPostIndexed64(const MCInst *MI,unsigned OpNum,
                                             raw_ostream &O) {
  O << '[' << getRegisterName(MI->getOperand(OpNum).getReg()) << ']'
    << ", #" << 8 * MI->getOperand(OpNum + 1).getImm();
}

void ARM64InstPrinter::printMemoryPostIndexed128(const MCInst *MI,
                                                 unsigned OpNum,
                                                 raw_ostream &O) {
  O << '[' << getRegisterName(MI->getOperand(OpNum).getReg()) << ']'
    << ", #" << 16 * MI->getOperand(OpNum + 1).getImm();
}

void ARM64InstPrinter::printMemoryPostIndexed(const MCInst *MI, unsigned OpNum,
                                             raw_ostream &O) {
  O << '[' << getRegisterName(MI->getOperand(OpNum).getReg()) << ']'
    << ", #" << MI->getOperand(OpNum + 1).getImm();
}

void ARM64InstPrinter::printMemoryRegOffset(const MCInst *MI, unsigned OpNum,
                                            raw_ostream &O, int LegalShiftAmt) {
  O << '[' << getRegisterName(MI->getOperand(OpNum).getReg()) << ", "
           << getRegisterName(MI->getOperand(OpNum+1).getReg());

  unsigned Val = MI->getOperand(OpNum+2).getImm();
  ARM64_AM::ExtendType ExtType = ARM64_AM::getMemExtendType(Val);
  bool DoShift = ARM64_AM::getMemDoShift(Val);

  if (ExtType == ARM64_AM::UXTX) {
    if (DoShift)
      O << ", lsl";
  } else
    O << ", " << ARM64_AM::getExtendName(ExtType);

  if (DoShift)
    O << " #" << LegalShiftAmt;

  O << "]";
}

void ARM64InstPrinter::printMemoryRegOffset8(const MCInst *MI, unsigned OpNum,
                                            raw_ostream &O) {
  printMemoryRegOffset(MI, OpNum, O, 0);
}

void ARM64InstPrinter::printMemoryRegOffset16(const MCInst *MI, unsigned OpNum,
                                             raw_ostream &O) {
  printMemoryRegOffset(MI, OpNum, O, 1);
}

void ARM64InstPrinter::printMemoryRegOffset32(const MCInst *MI, unsigned OpNum,
                                             raw_ostream &O) {
  printMemoryRegOffset(MI, OpNum, O, 2);
}

void ARM64InstPrinter::printMemoryRegOffset64(const MCInst *MI, unsigned OpNum,
                                             raw_ostream &O) {
  printMemoryRegOffset(MI, OpNum, O, 3);
}

void ARM64InstPrinter::printMemoryRegOffset128(const MCInst *MI, unsigned OpNum,
                                               raw_ostream &O) {
  printMemoryRegOffset(MI, OpNum, O, 4);
}

void ARM64InstPrinter::printFPImmOperand(const MCInst *MI, unsigned OpNum,
                                         raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(OpNum);
  O << '#';
  if (MO.isFPImm())
    // FIXME: Should this ever happen?
    O << MO.getFPImm();
  else
    O << ARM64_AM::getFPImmFloat(MO.getImm());
}

static unsigned getNextVectorRegister(unsigned Reg, unsigned Stride = 1) {
  while(Stride--) {
    switch (Reg) {
    default: assert (0 && "Vector register expected!");
    case ARM64::Q0: Reg = ARM64::Q1; break;
    case ARM64::Q1: Reg = ARM64::Q2; break;
    case ARM64::Q2: Reg = ARM64::Q3; break;
    case ARM64::Q3: Reg = ARM64::Q4; break;
    case ARM64::Q4: Reg = ARM64::Q5; break;
    case ARM64::Q5: Reg = ARM64::Q6; break;
    case ARM64::Q6: Reg = ARM64::Q7; break;
    case ARM64::Q7: Reg = ARM64::Q8; break;
    case ARM64::Q8: Reg = ARM64::Q9; break;
    case ARM64::Q9: Reg = ARM64::Q10; break;
    case ARM64::Q10: Reg = ARM64::Q11; break;
    case ARM64::Q11: Reg = ARM64::Q12; break;
    case ARM64::Q12: Reg = ARM64::Q13; break;
    case ARM64::Q13: Reg = ARM64::Q14; break;
    case ARM64::Q14: Reg = ARM64::Q15; break;
    case ARM64::Q15: Reg = ARM64::Q16; break;
    case ARM64::Q16: Reg = ARM64::Q17; break;
    case ARM64::Q17: Reg = ARM64::Q18; break;
    case ARM64::Q18: Reg = ARM64::Q19; break;
    case ARM64::Q19: Reg = ARM64::Q20; break;
    case ARM64::Q20: Reg = ARM64::Q21; break;
    case ARM64::Q21: Reg = ARM64::Q22; break;
    case ARM64::Q22: Reg = ARM64::Q23; break;
    case ARM64::Q23: Reg = ARM64::Q24; break;
    case ARM64::Q24: Reg = ARM64::Q25; break;
    case ARM64::Q25: Reg = ARM64::Q26; break;
    case ARM64::Q26: Reg = ARM64::Q27; break;
    case ARM64::Q27: Reg = ARM64::Q28; break;
    case ARM64::Q28: Reg = ARM64::Q29; break;
    case ARM64::Q29: Reg = ARM64::Q30; break;
    case ARM64::Q30: Reg = ARM64::Q31; break;
                     // Vector lists can wrap around.
    case ARM64::Q31: Reg = ARM64::Q0; break;
    }
  }
  return Reg;
}

template<unsigned NumRegs, unsigned RegSize>
void ARM64InstPrinter::printImplicitlyTypedVectorList(const MCInst *MI,
                                                      unsigned OpNum,
                                                      raw_ostream &O) {
  unsigned Reg = MI->getOperand(OpNum).getReg();

  // Get the first register in the tupple if needed
  if (NumRegs > 1)
    Reg = MRI.getSubReg(Reg, RegSize == 64 ? ARM64::dsub0 : ARM64::qsub0);

  // Convert it to a Q-register if needed.
  if (RegSize == 64) {
    const MCRegisterClass &FPR128RC = MRI.getRegClass(ARM64::FPR128RegClassID);
    Reg = MRI.getMatchingSuperReg(Reg, ARM64::dsub, &FPR128RC);
  }

  O << "{ ";

  for (unsigned i = 0; i < NumRegs; ++i, Reg = getNextVectorRegister(Reg)) {
    O << getRegisterName(Reg, ARM64::vreg);
    if (i + 1 != NumRegs) O << ", ";
  }

  O << " }";
}

void ARM64InstPrinter::printVectorIndex(const MCInst *MI, unsigned OpNum,
                                        raw_ostream &O) {
  O << "[" << MI->getOperand(OpNum).getImm() << "]";
}

void ARM64InstPrinter::printAlignedBranchTarget(const MCInst *MI,
                                                unsigned OpNum,
                                                raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNum);

  // If the label has already been resolved to an immediate offset (say, when
  // we're running the disassembler), just print the immediate.
  if (Op.isImm()) {
    O << "#" << (Op.getImm() << 2);
    return;
  }

  // If the branch target is simply an address then print it in hex.
  const MCConstantExpr *BranchTarget =
    dyn_cast<MCConstantExpr>(MI->getOperand(OpNum).getExpr());
  int64_t Address;
  if (BranchTarget && BranchTarget->EvaluateAsAbsolute(Address)) {
    O << "0x";
    O.write_hex(Address);
  }
  else {
    // Otherwise, just print the expression.
    O << *MI->getOperand(OpNum).getExpr();
  }
}

void ARM64InstPrinter::printAdrpLabel(const MCInst *MI, unsigned OpNum,
                                      raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNum);

  // If the label has already been resolved to an immediate offset (say, when
  // we're running the disassembler), just print the immediate.
  if (Op.isImm()) {
    O << "#" << (Op.getImm() << 12);
    return;
  }

  // Otherwise, just print the expression.
  O << *MI->getOperand(OpNum).getExpr();
}

void ARM64InstPrinter::printBarrierOption(const MCInst *MI, unsigned OpNo,
                                           raw_ostream &O) {
  unsigned Val = MI->getOperand(OpNo).getImm();
  const char *Name =
      ARM64SYS::getBarrierOptName((ARM64SYS::BarrierOption)Val);
  if (Name)
    O << Name;
  else
    O << "#" << Val;
}

void ARM64InstPrinter::printSystemRegister(const MCInst *MI, unsigned OpNo,
                                           raw_ostream &O) {
  unsigned Val = MI->getOperand(OpNo).getImm();
  const char *Name =
      ARM64SYS::getSystemRegisterName((ARM64SYS::SystemRegister)Val);
  if (Name) {
    O << Name;
    return;
  }

  unsigned Op0 = 2 | ((Val >> 14) & 1);
  unsigned Op1 = (Val >> 11) & 7;
  unsigned CRn = (Val >> 7) & 0xf;
  unsigned CRm = (Val >> 3) & 0xf;
  unsigned Op2 = Val & 7;

  O << 'S' << Op0 << '_' << Op1 << "_C" << CRn << "_C" << CRm << '_' << Op2;
}

void ARM64InstPrinter::printSystemCPSRField(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &O) {
  unsigned Val = MI->getOperand(OpNo).getImm();
  const char *Name =
      ARM64SYS::getCPSRFieldName((ARM64SYS::CPSRField)Val);
  O << Name;
}

void ARM64InstPrinter::printSIMDType10Operand(const MCInst *MI, unsigned OpNo,
                                              raw_ostream &O) {
  unsigned RawVal = MI->getOperand(OpNo).getImm();
  uint64_t Val = ARM64_AM::decodeAdvSIMDModImmType10(RawVal);
  O << format("#%#016lx", Val);
}
