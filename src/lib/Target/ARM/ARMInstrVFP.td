//===-- ARMInstrVFP.td - VFP support for ARM ---------------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes the ARM VFP instruction set.
//
//===----------------------------------------------------------------------===//

def SDT_FTOI    : SDTypeProfile<1, 1, [SDTCisVT<0, f32>, SDTCisFP<1>]>;
def SDT_ITOF    : SDTypeProfile<1, 1, [SDTCisFP<0>, SDTCisVT<1, f32>]>;
def SDT_CMPFP0  : SDTypeProfile<0, 1, [SDTCisFP<0>]>;
def SDT_VMOVDRR : SDTypeProfile<1, 2, [SDTCisVT<0, f64>, SDTCisVT<1, i32>,
                                       SDTCisSameAs<1, 2>]>;

def arm_ftoui  : SDNode<"ARMISD::FTOUI",   SDT_FTOI>;
def arm_ftosi  : SDNode<"ARMISD::FTOSI",   SDT_FTOI>;
def arm_sitof  : SDNode<"ARMISD::SITOF",   SDT_ITOF>;
def arm_uitof  : SDNode<"ARMISD::UITOF",   SDT_ITOF>;
def arm_fmstat : SDNode<"ARMISD::FMSTAT",  SDTNone, [SDNPInGlue, SDNPOutGlue]>;
def arm_cmpfp  : SDNode<"ARMISD::CMPFP",   SDT_ARMCmp, [SDNPOutGlue]>;
def arm_cmpfp0 : SDNode<"ARMISD::CMPFPw0", SDT_CMPFP0, [SDNPOutGlue]>;
def arm_fmdrr  : SDNode<"ARMISD::VMOVDRR", SDT_VMOVDRR>;


//===----------------------------------------------------------------------===//
// Operand Definitions.
//

// 8-bit floating-point immediate encodings.
def FPImmOperand : AsmOperandClass {
  let Name = "FPImm";
  let ParserMethod = "parseFPImm";
}

def vfp_f32imm : Operand<f32>,
                 PatLeaf<(f32 fpimm), [{
      return ARM_AM::getFP32Imm(N->getValueAPF()) != -1;
    }], SDNodeXForm<fpimm, [{
      APFloat InVal = N->getValueAPF();
      uint32_t enc = ARM_AM::getFP32Imm(InVal);
      return CurDAG->getTargetConstant(enc, MVT::i32);
    }]>> {
  let PrintMethod = "printFPImmOperand";
  let ParserMatchClass = FPImmOperand;
}

def vfp_f64imm : Operand<f64>,
                 PatLeaf<(f64 fpimm), [{
      return ARM_AM::getFP64Imm(N->getValueAPF()) != -1;
    }], SDNodeXForm<fpimm, [{
      APFloat InVal = N->getValueAPF();
      uint32_t enc = ARM_AM::getFP64Imm(InVal);
      return CurDAG->getTargetConstant(enc, MVT::i32);
    }]>> {
  let PrintMethod = "printFPImmOperand";
  let ParserMatchClass = FPImmOperand;
}

def alignedload32 : PatFrag<(ops node:$ptr), (load node:$ptr), [{
  return cast<LoadSDNode>(N)->getAlignment() >= 4;
}]>;

def alignedstore32 : PatFrag<(ops node:$val, node:$ptr),
                             (store node:$val, node:$ptr), [{
  return cast<StoreSDNode>(N)->getAlignment() >= 4;
}]>;

// The VCVT to/from fixed-point instructions encode the 'fbits' operand
// (the number of fixed bits) differently than it appears in the assembly
// source. It's encoded as "Size - fbits" where Size is the size of the
// fixed-point representation (32 or 16) and fbits is the value appearing
// in the assembly source, an integer in [0,16] or (0,32], depending on size.
def fbits32_asm_operand : AsmOperandClass { let Name = "FBits32"; }
def fbits32 : Operand<i32> {
  let PrintMethod = "printFBits32";
  let ParserMatchClass = fbits32_asm_operand;
}

def fbits16_asm_operand : AsmOperandClass { let Name = "FBits16"; }
def fbits16 : Operand<i32> {
  let PrintMethod = "printFBits16";
  let ParserMatchClass = fbits16_asm_operand;
}

//===----------------------------------------------------------------------===//
//  Load / store Instructions.
//

let canFoldAsLoad = 1, isReMaterializable = 1 in {

def VLDRD : ADI5<0b1101, 0b01, (outs DPR:$Dd), (ins addrmode5:$addr),
                 IIC_fpLoad64, "vldr", "\t$Dd, $addr",
                 [(set DPR:$Dd, (f64 (alignedload32 addrmode5:$addr)))]>;

def VLDRS : ASI5<0b1101, 0b01, (outs SPR:$Sd), (ins addrmode5:$addr),
                 IIC_fpLoad32, "vldr", "\t$Sd, $addr",
                 [(set SPR:$Sd, (load addrmode5:$addr))]> {
  // Some single precision VFP instructions may be executed on both NEON and VFP
  // pipelines.
  let D = VFPNeonDomain;
}

} // End of 'let canFoldAsLoad = 1, isReMaterializable = 1 in'

def VSTRD : ADI5<0b1101, 0b00, (outs), (ins DPR:$Dd, addrmode5:$addr),
                 IIC_fpStore64, "vstr", "\t$Dd, $addr",
                 [(alignedstore32 (f64 DPR:$Dd), addrmode5:$addr)]>;

def VSTRS : ASI5<0b1101, 0b00, (outs), (ins SPR:$Sd, addrmode5:$addr),
                 IIC_fpStore32, "vstr", "\t$Sd, $addr",
                 [(store SPR:$Sd, addrmode5:$addr)]> {
  // Some single precision VFP instructions may be executed on both NEON and VFP
  // pipelines.
  let D = VFPNeonDomain;
}

//===----------------------------------------------------------------------===//
//  Load / store multiple Instructions.
//

multiclass vfp_ldst_mult<string asm, bit L_bit,
                         InstrItinClass itin, InstrItinClass itin_upd> {
  // Double Precision
  def DIA :
    AXDI4<(outs), (ins GPR:$Rn, pred:$p, dpr_reglist:$regs, variable_ops),
          IndexModeNone, itin,
          !strconcat(asm, "ia${p}\t$Rn, $regs"), "", []> {
    let Inst{24-23} = 0b01;       // Increment After
    let Inst{21}    = 0;          // No writeback
    let Inst{20}    = L_bit;
  }
  def DIA_UPD :
    AXDI4<(outs GPR:$wb), (ins GPR:$Rn, pred:$p, dpr_reglist:$regs,
                               variable_ops),
          IndexModeUpd, itin_upd,
          !strconcat(asm, "ia${p}\t$Rn!, $regs"), "$Rn = $wb", []> {
    let Inst{24-23} = 0b01;       // Increment After
    let Inst{21}    = 1;          // Writeback
    let Inst{20}    = L_bit;
  }
  def DDB_UPD :
    AXDI4<(outs GPR:$wb), (ins GPR:$Rn, pred:$p, dpr_reglist:$regs,
                               variable_ops),
          IndexModeUpd, itin_upd,
          !strconcat(asm, "db${p}\t$Rn!, $regs"), "$Rn = $wb", []> {
    let Inst{24-23} = 0b10;       // Decrement Before
    let Inst{21}    = 1;          // Writeback
    let Inst{20}    = L_bit;
  }

  // Single Precision
  def SIA :
    AXSI4<(outs), (ins GPR:$Rn, pred:$p, spr_reglist:$regs, variable_ops),
          IndexModeNone, itin,
          !strconcat(asm, "ia${p}\t$Rn, $regs"), "", []> {
    let Inst{24-23} = 0b01;       // Increment After
    let Inst{21}    = 0;          // No writeback
    let Inst{20}    = L_bit;

    // Some single precision VFP instructions may be executed on both NEON and
    // VFP pipelines.
    let D = VFPNeonDomain;
  }
  def SIA_UPD :
    AXSI4<(outs GPR:$wb), (ins GPR:$Rn, pred:$p, spr_reglist:$regs,
                               variable_ops),
          IndexModeUpd, itin_upd,
          !strconcat(asm, "ia${p}\t$Rn!, $regs"), "$Rn = $wb", []> {
    let Inst{24-23} = 0b01;       // Increment After
    let Inst{21}    = 1;          // Writeback
    let Inst{20}    = L_bit;

    // Some single precision VFP instructions may be executed on both NEON and
    // VFP pipelines.
    let D = VFPNeonDomain;
  }
  def SDB_UPD :
    AXSI4<(outs GPR:$wb), (ins GPR:$Rn, pred:$p, spr_reglist:$regs,
                               variable_ops),
          IndexModeUpd, itin_upd,
          !strconcat(asm, "db${p}\t$Rn!, $regs"), "$Rn = $wb", []> {
    let Inst{24-23} = 0b10;       // Decrement Before
    let Inst{21}    = 1;          // Writeback
    let Inst{20}    = L_bit;

    // Some single precision VFP instructions may be executed on both NEON and
    // VFP pipelines.
    let D = VFPNeonDomain;
  }
}

let neverHasSideEffects = 1 in {

let mayLoad = 1, hasExtraDefRegAllocReq = 1 in
defm VLDM : vfp_ldst_mult<"vldm", 1, IIC_fpLoad_m, IIC_fpLoad_mu>;

let mayStore = 1, hasExtraSrcRegAllocReq = 1 in
defm VSTM : vfp_ldst_mult<"vstm", 0, IIC_fpLoad_m, IIC_fpLoad_mu>;

} // neverHasSideEffects

def : MnemonicAlias<"vldm", "vldmia">;
def : MnemonicAlias<"vstm", "vstmia">;

def : InstAlias<"vpush${p} $r", (VSTMDDB_UPD SP, pred:$p, dpr_reglist:$r)>,
                Requires<[HasVFP2]>;
def : InstAlias<"vpush${p} $r", (VSTMSDB_UPD SP, pred:$p, spr_reglist:$r)>,
                Requires<[HasVFP2]>;
def : InstAlias<"vpop${p} $r",  (VLDMDIA_UPD SP, pred:$p, dpr_reglist:$r)>,
                Requires<[HasVFP2]>;
def : InstAlias<"vpop${p} $r",  (VLDMSIA_UPD SP, pred:$p, spr_reglist:$r)>,
                Requires<[HasVFP2]>;
defm : VFPDTAnyInstAlias<"vpush${p}", "$r",
                         (VSTMSDB_UPD SP, pred:$p, spr_reglist:$r)>;
defm : VFPDTAnyInstAlias<"vpush${p}", "$r",
                         (VSTMDDB_UPD SP, pred:$p, dpr_reglist:$r)>;
defm : VFPDTAnyInstAlias<"vpop${p}", "$r",
                         (VLDMSIA_UPD SP, pred:$p, spr_reglist:$r)>;
defm : VFPDTAnyInstAlias<"vpop${p}", "$r",
                         (VLDMDIA_UPD SP, pred:$p, dpr_reglist:$r)>;

// FLDMX, FSTMX - mixing S/D registers for pre-armv6 cores

//===----------------------------------------------------------------------===//
// FP Binary Operations.
//

let TwoOperandAliasConstraint = "$Dn = $Dd" in
def VADDD  : ADbI<0b11100, 0b11, 0, 0,
                  (outs DPR:$Dd), (ins DPR:$Dn, DPR:$Dm),
                  IIC_fpALU64, "vadd", ".f64\t$Dd, $Dn, $Dm",
                  [(set DPR:$Dd, (fadd DPR:$Dn, (f64 DPR:$Dm)))]>;

let TwoOperandAliasConstraint = "$Sn = $Sd" in
def VADDS  : ASbIn<0b11100, 0b11, 0, 0,
                   (outs SPR:$Sd), (ins SPR:$Sn, SPR:$Sm),
                   IIC_fpALU32, "vadd", ".f32\t$Sd, $Sn, $Sm",
                   [(set SPR:$Sd, (fadd SPR:$Sn, SPR:$Sm))]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

let TwoOperandAliasConstraint = "$Dn = $Dd" in
def VSUBD  : ADbI<0b11100, 0b11, 1, 0,
                  (outs DPR:$Dd), (ins DPR:$Dn, DPR:$Dm),
                  IIC_fpALU64, "vsub", ".f64\t$Dd, $Dn, $Dm",
                  [(set DPR:$Dd, (fsub DPR:$Dn, (f64 DPR:$Dm)))]>;

let TwoOperandAliasConstraint = "$Sn = $Sd" in
def VSUBS  : ASbIn<0b11100, 0b11, 1, 0,
                   (outs SPR:$Sd), (ins SPR:$Sn, SPR:$Sm),
                   IIC_fpALU32, "vsub", ".f32\t$Sd, $Sn, $Sm",
                   [(set SPR:$Sd, (fsub SPR:$Sn, SPR:$Sm))]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

let TwoOperandAliasConstraint = "$Dn = $Dd" in
def VDIVD  : ADbI<0b11101, 0b00, 0, 0,
                  (outs DPR:$Dd), (ins DPR:$Dn, DPR:$Dm),
                  IIC_fpDIV64, "vdiv", ".f64\t$Dd, $Dn, $Dm",
                  [(set DPR:$Dd, (fdiv DPR:$Dn, (f64 DPR:$Dm)))]>;

let TwoOperandAliasConstraint = "$Sn = $Sd" in
def VDIVS  : ASbI<0b11101, 0b00, 0, 0,
                  (outs SPR:$Sd), (ins SPR:$Sn, SPR:$Sm),
                  IIC_fpDIV32, "vdiv", ".f32\t$Sd, $Sn, $Sm",
                  [(set SPR:$Sd, (fdiv SPR:$Sn, SPR:$Sm))]>;

let TwoOperandAliasConstraint = "$Dn = $Dd" in
def VMULD  : ADbI<0b11100, 0b10, 0, 0,
                  (outs DPR:$Dd), (ins DPR:$Dn, DPR:$Dm),
                  IIC_fpMUL64, "vmul", ".f64\t$Dd, $Dn, $Dm",
                  [(set DPR:$Dd, (fmul DPR:$Dn, (f64 DPR:$Dm)))]>;

let TwoOperandAliasConstraint = "$Sn = $Sd" in
def VMULS  : ASbIn<0b11100, 0b10, 0, 0,
                   (outs SPR:$Sd), (ins SPR:$Sn, SPR:$Sm),
                   IIC_fpMUL32, "vmul", ".f32\t$Sd, $Sn, $Sm",
                   [(set SPR:$Sd, (fmul SPR:$Sn, SPR:$Sm))]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VNMULD : ADbI<0b11100, 0b10, 1, 0,
                  (outs DPR:$Dd), (ins DPR:$Dn, DPR:$Dm),
                  IIC_fpMUL64, "vnmul", ".f64\t$Dd, $Dn, $Dm",
                  [(set DPR:$Dd, (fneg (fmul DPR:$Dn, (f64 DPR:$Dm))))]>;

def VNMULS : ASbI<0b11100, 0b10, 1, 0,
                  (outs SPR:$Sd), (ins SPR:$Sn, SPR:$Sm),
                  IIC_fpMUL32, "vnmul", ".f32\t$Sd, $Sn, $Sm",
                  [(set SPR:$Sd, (fneg (fmul SPR:$Sn, SPR:$Sm)))]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

// Match reassociated forms only if not sign dependent rounding.
def : Pat<(fmul (fneg DPR:$a), (f64 DPR:$b)),
          (VNMULD DPR:$a, DPR:$b)>, Requires<[NoHonorSignDependentRounding]>;
def : Pat<(fmul (fneg SPR:$a), SPR:$b),
          (VNMULS SPR:$a, SPR:$b)>, Requires<[NoHonorSignDependentRounding]>;

// These are encoded as unary instructions.
let Defs = [FPSCR_NZCV] in {
def VCMPED : ADuI<0b11101, 0b11, 0b0100, 0b11, 0,
                  (outs), (ins DPR:$Dd, DPR:$Dm),
                  IIC_fpCMP64, "vcmpe", ".f64\t$Dd, $Dm",
                  [(arm_cmpfp DPR:$Dd, (f64 DPR:$Dm))]>;

def VCMPES : ASuI<0b11101, 0b11, 0b0100, 0b11, 0,
                  (outs), (ins SPR:$Sd, SPR:$Sm),
                  IIC_fpCMP32, "vcmpe", ".f32\t$Sd, $Sm",
                  [(arm_cmpfp SPR:$Sd, SPR:$Sm)]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

// FIXME: Verify encoding after integrated assembler is working.
def VCMPD  : ADuI<0b11101, 0b11, 0b0100, 0b01, 0,
                  (outs), (ins DPR:$Dd, DPR:$Dm),
                  IIC_fpCMP64, "vcmp", ".f64\t$Dd, $Dm",
                  [/* For disassembly only; pattern left blank */]>;

def VCMPS  : ASuI<0b11101, 0b11, 0b0100, 0b01, 0,
                  (outs), (ins SPR:$Sd, SPR:$Sm),
                  IIC_fpCMP32, "vcmp", ".f32\t$Sd, $Sm",
                  [/* For disassembly only; pattern left blank */]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}
} // Defs = [FPSCR_NZCV]

//===----------------------------------------------------------------------===//
// FP Unary Operations.
//

def VABSD  : ADuI<0b11101, 0b11, 0b0000, 0b11, 0,
                  (outs DPR:$Dd), (ins DPR:$Dm),
                  IIC_fpUNA64, "vabs", ".f64\t$Dd, $Dm",
                  [(set DPR:$Dd, (fabs (f64 DPR:$Dm)))]>;

def VABSS  : ASuIn<0b11101, 0b11, 0b0000, 0b11, 0,
                   (outs SPR:$Sd), (ins SPR:$Sm),
                   IIC_fpUNA32, "vabs", ".f32\t$Sd, $Sm",
                   [(set SPR:$Sd, (fabs SPR:$Sm))]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

let Defs = [FPSCR_NZCV] in {
def VCMPEZD : ADuI<0b11101, 0b11, 0b0101, 0b11, 0,
                   (outs), (ins DPR:$Dd),
                   IIC_fpCMP64, "vcmpe", ".f64\t$Dd, #0",
                   [(arm_cmpfp0 (f64 DPR:$Dd))]> {
  let Inst{3-0} = 0b0000;
  let Inst{5}   = 0;
}

def VCMPEZS : ASuI<0b11101, 0b11, 0b0101, 0b11, 0,
                   (outs), (ins SPR:$Sd),
                   IIC_fpCMP32, "vcmpe", ".f32\t$Sd, #0",
                   [(arm_cmpfp0 SPR:$Sd)]> {
  let Inst{3-0} = 0b0000;
  let Inst{5}   = 0;

  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

// FIXME: Verify encoding after integrated assembler is working.
def VCMPZD  : ADuI<0b11101, 0b11, 0b0101, 0b01, 0,
                   (outs), (ins DPR:$Dd),
                   IIC_fpCMP64, "vcmp", ".f64\t$Dd, #0",
                   [/* For disassembly only; pattern left blank */]> {
  let Inst{3-0} = 0b0000;
  let Inst{5}   = 0;
}

def VCMPZS  : ASuI<0b11101, 0b11, 0b0101, 0b01, 0,
                   (outs), (ins SPR:$Sd),
                   IIC_fpCMP32, "vcmp", ".f32\t$Sd, #0",
                   [/* For disassembly only; pattern left blank */]> {
  let Inst{3-0} = 0b0000;
  let Inst{5}   = 0;

  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}
} // Defs = [FPSCR_NZCV]

def VCVTDS  : ASuI<0b11101, 0b11, 0b0111, 0b11, 0,
                   (outs DPR:$Dd), (ins SPR:$Sm),
                   IIC_fpCVTDS, "vcvt", ".f64.f32\t$Dd, $Sm",
                   [(set DPR:$Dd, (fextend SPR:$Sm))]> {
  // Instruction operands.
  bits<5> Dd;
  bits<5> Sm;

  // Encode instruction operands.
  let Inst{3-0}   = Sm{4-1};
  let Inst{5}     = Sm{0};
  let Inst{15-12} = Dd{3-0};
  let Inst{22}    = Dd{4};
}

// Special case encoding: bits 11-8 is 0b1011.
def VCVTSD  : VFPAI<(outs SPR:$Sd), (ins DPR:$Dm), VFPUnaryFrm,
                    IIC_fpCVTSD, "vcvt", ".f32.f64\t$Sd, $Dm",
                    [(set SPR:$Sd, (fround DPR:$Dm))]> {
  // Instruction operands.
  bits<5> Sd;
  bits<5> Dm;

  // Encode instruction operands.
  let Inst{3-0}   = Dm{3-0};
  let Inst{5}     = Dm{4};
  let Inst{15-12} = Sd{4-1};
  let Inst{22}    = Sd{0};

  let Inst{27-23} = 0b11101;
  let Inst{21-16} = 0b110111;
  let Inst{11-8}  = 0b1011;
  let Inst{7-6}   = 0b11;
  let Inst{4}     = 0;
}

// Between half-precision and single-precision.  For disassembly only.

// FIXME: Verify encoding after integrated assembler is working.
def VCVTBHS: ASuI<0b11101, 0b11, 0b0010, 0b01, 0, (outs SPR:$Sd), (ins SPR:$Sm),
                 /* FIXME */ IIC_fpCVTSH, "vcvtb", ".f32.f16\t$Sd, $Sm",
                 [/* For disassembly only; pattern left blank */]>;

def VCVTBSH: ASuI<0b11101, 0b11, 0b0011, 0b01, 0, (outs SPR:$Sd), (ins SPR:$Sm),
                 /* FIXME */ IIC_fpCVTHS, "vcvtb", ".f16.f32\t$Sd, $Sm",
                 [/* For disassembly only; pattern left blank */]>;

def : Pat<(f32_to_f16 SPR:$a),
          (i32 (COPY_TO_REGCLASS (VCVTBSH SPR:$a), GPR))>;

def : Pat<(f16_to_f32 GPR:$a),
          (VCVTBHS (COPY_TO_REGCLASS GPR:$a, SPR))>;

def VCVTTHS: ASuI<0b11101, 0b11, 0b0010, 0b11, 0, (outs SPR:$Sd), (ins SPR:$Sm),
                 /* FIXME */ IIC_fpCVTSH, "vcvtt", ".f32.f16\t$Sd, $Sm",
                 [/* For disassembly only; pattern left blank */]>;

def VCVTTSH: ASuI<0b11101, 0b11, 0b0011, 0b11, 0, (outs SPR:$Sd), (ins SPR:$Sm),
                 /* FIXME */ IIC_fpCVTHS, "vcvtt", ".f16.f32\t$Sd, $Sm",
                 [/* For disassembly only; pattern left blank */]>;

// Between half-precision and double-precision.
class BaseVCVTHxD<string opc, string extAndOps, bit top, bit convert_from_half,
                  dag oops, dag iops>
      : VFPAI<oops, iops, VFPUnaryFrm, IIC_fpCVTSD, opc, extAndOps, []>,
      Requires<[HasV8]> {
  let Inst{27-23} = 0b11101;
  let Inst{21-17} = 0b11001;
  let Inst{16}    = convert_from_half;
  let Inst{11-9}  = 0b101;
  let Inst{8}     = 0b1;     // uses_double
  let Inst{7}     = top;
  let Inst{6}     = 0b1;
  let Inst{4}     = 0;
}
class BaseVCVTDH<string opc, string extAndOps, bit top>
      : BaseVCVTHxD<opc, extAndOps, top, 0b1, (outs DPR:$Dd), (ins SPR:$Sm)> {
  // Instruction operands.
  bits<5> Dd;
  bits<5> Sm;

  // Encode instruction operands. This encoding is used for convert_from_half
  let Inst{3-0}   = Sm{4-1};
  let Inst{5}     = Sm{0};
  let Inst{15-12} = Dd{3-0};
  let Inst{22}    = Dd{4};
}
class BaseVCVTHD<string opc, string extAndOps, bit top>
      : BaseVCVTHxD<opc, extAndOps, top, 0b0, (outs SPR:$Sd), (ins DPR:$Dm)> {
  // Instruction operands.
  bits<5> Sd;
  bits<5> Dm;

  // Encode instruction operands. This encoding is used for !convert_from_half
  let Inst{3-0}   = Dm{3-0};
  let Inst{5}     = Dm{4};
  let Inst{15-12} = Sd{4-1};
  let Inst{22}    = Sd{0};
}
def VCVTTDH  : BaseVCVTDH<"vcvtt", ".f64.f16\t$Dd, $Sm", 0b1>;
def VCVTBDH  : BaseVCVTDH<"vcvtb", ".f64.f16\t$Dd, $Sm", 0b0>;
def VCVTTHD  : BaseVCVTHD<"vcvtt", ".f16.f64\t$Sd, $Dm", 0b1>;
def VCVTBHD  : BaseVCVTHD<"vcvtb", ".f16.f64\t$Sd, $Dm", 0b0>;

// VFP Floating-point Round to Integral Floating-point --- Round to Zero and
// Round using FPSCR
class BaseVFPVRINT<string opc, bit op, bit op2, string extAndOps, bit sz,
                dag oops, dag iops>
      : VFPAI<oops, iops, VFPUnaryFrm, IIC_fpCVTSD, opc, extAndOps, []>,
      Requires<[HasV8]> {
  let Inst{27-23} = 0b11101;
  let Inst{21-20} = 0b11;
  let Inst{19-17} = 0b011;
  let Inst{16} = op;
  let Inst{11-9} = 0b101;
  let Inst{8} = sz;
  let Inst{7} = op2;
  let Inst{6} = 1;
  let Inst{4} = 0;
}
class VRINT32<string opc, bit op, bit op2>
    : BaseVFPVRINT<opc, op, op2, ".f32.f32\t$Sd,$Sm", 0b0, (outs SPR:$Sd),
                (ins SPR:$Sm)> {
  bits<5> Sd;
  bits<5> Sm;
  let Inst{22} = Sd{0};
  let Inst{15-12} = Sd{4-1};
  let Inst{5} = Sm{0};
  let Inst{3-0} = Sm{4-1};
}
class VRINT64<string opc, bit op, bit op2>
    : BaseVFPVRINT<opc, op, op2, ".f64.f64\t$Dd,$Dm", 0b1, (outs DPR:$Dd), 
                (ins DPR:$Dm)> {
  bits<5> Dd;
  bits<5> Dm;
  let Inst{22} = Dd{4};
  let Inst{15-12} = Dd{3-0};
  let Inst{5} = Dm{4};
  let Inst{3-0} = Dm{3-0};
}
multiclass VRINT<string opc, bit op, bit op2> {
  def F32 : VRINT32<opc, op, op2>;
  def F64 : VRINT64<opc, op, op2>;
}
defm VRINTZ : VRINT<"vrintz", 0b0, 0b1>;
defm VRINTR : VRINT<"vrintr", 0b0, 0b0>;
defm VRINTX : VRINT<"vrintx", 0b1, 0b0>;

// VRINT without conditional
class BaseVRINTnoCond<string opc, bits<2> RM, string extAndOps, bit sz, 
                      dag oops, dag iops>
      : AInoP<oops, iops, MiscFrm, NoItinerary, opc, extAndOps, []>,
      Requires<[HasV8]> {
  let Inst{31-28} = 0b1111;
  let Inst{27-23} = 0b11101;
  let Inst{21-20} = 0b11;
  let Inst{19-18} = 0b10;
  let Inst{17-16} = RM;
  let Inst{11-9} = 0b101;
  let Inst{8} = sz;
  let Inst{7} = 0b0;
  let Inst{6} = 0b1;
  let Inst{4} = 0b0;
}
class VRINT32noCond<string opc, bits<2> RM>
    : BaseVRINTnoCond<opc, RM, ".f32.f32\t$Sd,$Sm", 0b0, (outs SPR:$Sd),
                     (ins SPR:$Sm)> {
  bits<5> Sd;
  bits<5> Sm;
  let Inst{22} = Sd{0};
  let Inst{15-12} = Sd{4-1};
  let Inst{5} = Sm{0};
  let Inst{3-0} = Sm{4-1};
}
class VRINT64noCond<string opc, bits<2> RM>
    : BaseVRINTnoCond<opc, RM, ".f64.f64\t$Dd,$Dm", 0b1, (outs DPR:$Dd),
                      (ins DPR:$Dm)> {
  bits<5> Dd;
  bits<5> Dm;
  let Inst{22} = Dd{4};
  let Inst{15-12} = Dd{3-0};
  let Inst{5} = Dm{4};
  let Inst{3-0} = Dm{3-0};
}
multiclass VRINTnoCond<string opc, bits<2> RM> {
  def F32 : VRINT32noCond<opc, RM>;
  def F64 : VRINT64noCond<opc, RM>;
}
defm VRINTA : VRINTnoCond<"vrinta", 0b00>;
defm VRINTN : VRINTnoCond<"vrintn", 0b01>;
defm VRINTP : VRINTnoCond<"vrintp", 0b10>;
defm VRINTM : VRINTnoCond<"vrintm", 0b11>;

// SIMD Floating-point Round
class BASEFRINTSIMD<string opc, bits<3> op, string extAndOps, bit Q, dag oops,
                    dag iops>
      : AInoP<oops, iops, MiscFrm, NoItinerary, opc, extAndOps, []>,
      Requires<[HasV8]> {
  let Inst{31-28} = 0b1111;
  let Inst{27-23} = 0b00111;
  let Inst{21-20} = 0b11;
  let Inst{19-18} = 0b10;
  let Inst{17-16} = 0b10;
  let Inst{11} = 0b0;
  let Inst{10} = 0b1;
  let Inst{9-7} = op;
  let Inst{6} = Q;
  let Inst{4} = 0b0;
}
class VRINTSIMDQ<string opc, bits<3> op>
  : BASEFRINTSIMD<opc, op, ".f32.f32\t$Qd, $Qm", 0b1, (outs QPR:$Qd),
                  (ins QPR:$Qm)> {
  bits<5> Qd;
  bits<5> Qm;
  let Inst{22} = Qd{4};
  let Inst{15-12} = Qd{3-0};
  let Inst{5} = Qm{4};
  let Inst{3-0} = Qm{3-0};
}
class VRINTSIMDD<string opc, bits<3> op> 
  : BASEFRINTSIMD<opc, op, ".f32.f32\t$Dd, $Dm", 0b0, (outs DPR:$Dd),
                  (ins DPR:$Dm)> {
  bits<5> Dd;
  bits<5> Dm;
  let Inst{22} = Dd{4};
  let Inst{15-12} = Dd{3-0};
  let Inst{5} = Dm{4};
  let Inst{3-0} = Dm{3-0};
}
multiclass VRINT_SIMD<string opc, bits<3> op> {
  def Q : VRINTSIMDQ<opc, op>;
  def D : VRINTSIMDD<opc, op>;
}
defm VRINT_SIMDN : VRINT_SIMD<"vrintn", 0b000>;
defm VRINT_SIMDX : VRINT_SIMD<"vrintx", 0b001>;
defm VRINT_SIMDA : VRINT_SIMD<"vrinta", 0b010>;
defm VRINT_SIMDZ : VRINT_SIMD<"vrintz", 0b011>;
defm VRINT_SIMDM : VRINT_SIMD<"vrintm", 0b101>;
defm VRINT_SIMDP : VRINT_SIMD<"vrintp", 0b111>;

def VNEGD  : ADuI<0b11101, 0b11, 0b0001, 0b01, 0,
                  (outs DPR:$Dd), (ins DPR:$Dm),
                  IIC_fpUNA64, "vneg", ".f64\t$Dd, $Dm",
                  [(set DPR:$Dd, (fneg (f64 DPR:$Dm)))]>;

def VNEGS  : ASuIn<0b11101, 0b11, 0b0001, 0b01, 0,
                   (outs SPR:$Sd), (ins SPR:$Sm),
                   IIC_fpUNA32, "vneg", ".f32\t$Sd, $Sm",
                   [(set SPR:$Sd, (fneg SPR:$Sm))]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VSQRTD : ADuI<0b11101, 0b11, 0b0001, 0b11, 0,
                  (outs DPR:$Dd), (ins DPR:$Dm),
                  IIC_fpSQRT64, "vsqrt", ".f64\t$Dd, $Dm",
                  [(set DPR:$Dd, (fsqrt (f64 DPR:$Dm)))]>;

def VSQRTS : ASuI<0b11101, 0b11, 0b0001, 0b11, 0,
                  (outs SPR:$Sd), (ins SPR:$Sm),
                  IIC_fpSQRT32, "vsqrt", ".f32\t$Sd, $Sm",
                  [(set SPR:$Sd, (fsqrt SPR:$Sm))]>;

let neverHasSideEffects = 1 in {
def VMOVD  : ADuI<0b11101, 0b11, 0b0000, 0b01, 0,
                  (outs DPR:$Dd), (ins DPR:$Dm),
                  IIC_fpUNA64, "vmov", ".f64\t$Dd, $Dm", []>;

def VMOVS  : ASuI<0b11101, 0b11, 0b0000, 0b01, 0,
                  (outs SPR:$Sd), (ins SPR:$Sm),
                  IIC_fpUNA32, "vmov", ".f32\t$Sd, $Sm", []>;
} // neverHasSideEffects

//===----------------------------------------------------------------------===//
// FP <-> GPR Copies.  Int <-> FP Conversions.
//

def VMOVRS : AVConv2I<0b11100001, 0b1010,
                      (outs GPR:$Rt), (ins SPR:$Sn),
                      IIC_fpMOVSI, "vmov", "\t$Rt, $Sn",
                      [(set GPR:$Rt, (bitconvert SPR:$Sn))]> {
  // Instruction operands.
  bits<4> Rt;
  bits<5> Sn;

  // Encode instruction operands.
  let Inst{19-16} = Sn{4-1};
  let Inst{7}     = Sn{0};
  let Inst{15-12} = Rt;

  let Inst{6-5}   = 0b00;
  let Inst{3-0}   = 0b0000;

  // Some single precision VFP instructions may be executed on both NEON and VFP
  // pipelines.
  let D = VFPNeonDomain;
}

// Bitcast i32 -> f32.  NEON prefers to use VMOVDRR.
def VMOVSR : AVConv4I<0b11100000, 0b1010,
                      (outs SPR:$Sn), (ins GPR:$Rt),
                      IIC_fpMOVIS, "vmov", "\t$Sn, $Rt",
                      [(set SPR:$Sn, (bitconvert GPR:$Rt))]>,
             Requires<[HasVFP2, UseVMOVSR]> {
  // Instruction operands.
  bits<5> Sn;
  bits<4> Rt;

  // Encode instruction operands.
  let Inst{19-16} = Sn{4-1};
  let Inst{7}     = Sn{0};
  let Inst{15-12} = Rt;

  let Inst{6-5}   = 0b00;
  let Inst{3-0}   = 0b0000;

  // Some single precision VFP instructions may be executed on both NEON and VFP
  // pipelines.
  let D = VFPNeonDomain;
}

let neverHasSideEffects = 1 in {
def VMOVRRD  : AVConv3I<0b11000101, 0b1011,
                        (outs GPR:$Rt, GPR:$Rt2), (ins DPR:$Dm),
                        IIC_fpMOVDI, "vmov", "\t$Rt, $Rt2, $Dm",
                 [/* FIXME: Can't write pattern for multiple result instr*/]> {
  // Instruction operands.
  bits<5> Dm;
  bits<4> Rt;
  bits<4> Rt2;

  // Encode instruction operands.
  let Inst{3-0}   = Dm{3-0};
  let Inst{5}     = Dm{4};
  let Inst{15-12} = Rt;
  let Inst{19-16} = Rt2;

  let Inst{7-6} = 0b00;

  // Some single precision VFP instructions may be executed on both NEON and VFP
  // pipelines.
  let D = VFPNeonDomain;
}

def VMOVRRS  : AVConv3I<0b11000101, 0b1010,
                      (outs GPR:$Rt, GPR:$Rt2), (ins SPR:$src1, SPR:$src2),
                 IIC_fpMOVDI, "vmov", "\t$Rt, $Rt2, $src1, $src2",
                 [/* For disassembly only; pattern left blank */]> {
  bits<5> src1;
  bits<4> Rt;
  bits<4> Rt2;

  // Encode instruction operands.
  let Inst{3-0}   = src1{4-1};
  let Inst{5}     = src1{0};
  let Inst{15-12} = Rt;
  let Inst{19-16} = Rt2;

  let Inst{7-6} = 0b00;

  // Some single precision VFP instructions may be executed on both NEON and VFP
  // pipelines.
  let D = VFPNeonDomain;
  let DecoderMethod = "DecodeVMOVRRS";
}
} // neverHasSideEffects

// FMDHR: GPR -> SPR
// FMDLR: GPR -> SPR

def VMOVDRR : AVConv5I<0b11000100, 0b1011,
                      (outs DPR:$Dm), (ins GPR:$Rt, GPR:$Rt2),
                      IIC_fpMOVID, "vmov", "\t$Dm, $Rt, $Rt2",
                      [(set DPR:$Dm, (arm_fmdrr GPR:$Rt, GPR:$Rt2))]> {
  // Instruction operands.
  bits<5> Dm;
  bits<4> Rt;
  bits<4> Rt2;

  // Encode instruction operands.
  let Inst{3-0}   = Dm{3-0};
  let Inst{5}     = Dm{4};
  let Inst{15-12} = Rt;
  let Inst{19-16} = Rt2;

  let Inst{7-6}   = 0b00;

  // Some single precision VFP instructions may be executed on both NEON and VFP
  // pipelines.
  let D = VFPNeonDomain;
}

let neverHasSideEffects = 1 in
def VMOVSRR : AVConv5I<0b11000100, 0b1010,
                     (outs SPR:$dst1, SPR:$dst2), (ins GPR:$src1, GPR:$src2),
                IIC_fpMOVID, "vmov", "\t$dst1, $dst2, $src1, $src2",
                [/* For disassembly only; pattern left blank */]> {
  // Instruction operands.
  bits<5> dst1;
  bits<4> src1;
  bits<4> src2;

  // Encode instruction operands.
  let Inst{3-0}   = dst1{4-1};
  let Inst{5}     = dst1{0};
  let Inst{15-12} = src1;
  let Inst{19-16} = src2;

  let Inst{7-6} = 0b00;

  // Some single precision VFP instructions may be executed on both NEON and VFP
  // pipelines.
  let D = VFPNeonDomain;

  let DecoderMethod = "DecodeVMOVSRR";
}

// FMRDH: SPR -> GPR
// FMRDL: SPR -> GPR
// FMRRS: SPR -> GPR
// FMRX:  SPR system reg -> GPR
// FMSRR: GPR -> SPR
// FMXR:  GPR -> VFP system reg


// Int -> FP:

class AVConv1IDs_Encode<bits<5> opcod1, bits<2> opcod2, bits<4> opcod3,
                        bits<4> opcod4, dag oops, dag iops,
                        InstrItinClass itin, string opc, string asm,
                        list<dag> pattern>
  : AVConv1I<opcod1, opcod2, opcod3, opcod4, oops, iops, itin, opc, asm,
             pattern> {
  // Instruction operands.
  bits<5> Dd;
  bits<5> Sm;

  // Encode instruction operands.
  let Inst{3-0}   = Sm{4-1};
  let Inst{5}     = Sm{0};
  let Inst{15-12} = Dd{3-0};
  let Inst{22}    = Dd{4};
}

class AVConv1InSs_Encode<bits<5> opcod1, bits<2> opcod2, bits<4> opcod3,
                         bits<4> opcod4, dag oops, dag iops,InstrItinClass itin,
                         string opc, string asm, list<dag> pattern>
  : AVConv1In<opcod1, opcod2, opcod3, opcod4, oops, iops, itin, opc, asm,
              pattern> {
  // Instruction operands.
  bits<5> Sd;
  bits<5> Sm;

  // Encode instruction operands.
  let Inst{3-0}   = Sm{4-1};
  let Inst{5}     = Sm{0};
  let Inst{15-12} = Sd{4-1};
  let Inst{22}    = Sd{0};
}

def VSITOD : AVConv1IDs_Encode<0b11101, 0b11, 0b1000, 0b1011,
                               (outs DPR:$Dd), (ins SPR:$Sm),
                               IIC_fpCVTID, "vcvt", ".f64.s32\t$Dd, $Sm",
                               [(set DPR:$Dd, (f64 (arm_sitof SPR:$Sm)))]> {
  let Inst{7} = 1; // s32
}

def VSITOS : AVConv1InSs_Encode<0b11101, 0b11, 0b1000, 0b1010,
                                (outs SPR:$Sd),(ins SPR:$Sm),
                                IIC_fpCVTIS, "vcvt", ".f32.s32\t$Sd, $Sm",
                                [(set SPR:$Sd, (arm_sitof SPR:$Sm))]> {
  let Inst{7} = 1; // s32

  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VUITOD : AVConv1IDs_Encode<0b11101, 0b11, 0b1000, 0b1011,
                               (outs DPR:$Dd), (ins SPR:$Sm),
                               IIC_fpCVTID, "vcvt", ".f64.u32\t$Dd, $Sm",
                               [(set DPR:$Dd, (f64 (arm_uitof SPR:$Sm)))]> {
  let Inst{7} = 0; // u32
}

def VUITOS : AVConv1InSs_Encode<0b11101, 0b11, 0b1000, 0b1010,
                                (outs SPR:$Sd), (ins SPR:$Sm),
                                IIC_fpCVTIS, "vcvt", ".f32.u32\t$Sd, $Sm",
                                [(set SPR:$Sd, (arm_uitof SPR:$Sm))]> {
  let Inst{7} = 0; // u32

  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

// FP -> Int:

class AVConv1IsD_Encode<bits<5> opcod1, bits<2> opcod2, bits<4> opcod3,
                        bits<4> opcod4, dag oops, dag iops,
                        InstrItinClass itin, string opc, string asm,
                        list<dag> pattern>
  : AVConv1I<opcod1, opcod2, opcod3, opcod4, oops, iops, itin, opc, asm,
             pattern> {
  // Instruction operands.
  bits<5> Sd;
  bits<5> Dm;

  // Encode instruction operands.
  let Inst{3-0}   = Dm{3-0};
  let Inst{5}     = Dm{4};
  let Inst{15-12} = Sd{4-1};
  let Inst{22}    = Sd{0};
}

class AVConv1InsS_Encode<bits<5> opcod1, bits<2> opcod2, bits<4> opcod3,
                         bits<4> opcod4, dag oops, dag iops,
                         InstrItinClass itin, string opc, string asm,
                         list<dag> pattern>
  : AVConv1In<opcod1, opcod2, opcod3, opcod4, oops, iops, itin, opc, asm,
              pattern> {
  // Instruction operands.
  bits<5> Sd;
  bits<5> Sm;

  // Encode instruction operands.
  let Inst{3-0}   = Sm{4-1};
  let Inst{5}     = Sm{0};
  let Inst{15-12} = Sd{4-1};
  let Inst{22}    = Sd{0};
}

// Always set Z bit in the instruction, i.e. "round towards zero" variants.
def VTOSIZD : AVConv1IsD_Encode<0b11101, 0b11, 0b1101, 0b1011,
                                (outs SPR:$Sd), (ins DPR:$Dm),
                                IIC_fpCVTDI, "vcvt", ".s32.f64\t$Sd, $Dm",
                                [(set SPR:$Sd, (arm_ftosi (f64 DPR:$Dm)))]> {
  let Inst{7} = 1; // Z bit
}

def VTOSIZS : AVConv1InsS_Encode<0b11101, 0b11, 0b1101, 0b1010,
                                 (outs SPR:$Sd), (ins SPR:$Sm),
                                 IIC_fpCVTSI, "vcvt", ".s32.f32\t$Sd, $Sm",
                                 [(set SPR:$Sd, (arm_ftosi SPR:$Sm))]> {
  let Inst{7} = 1; // Z bit

  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VTOUIZD : AVConv1IsD_Encode<0b11101, 0b11, 0b1100, 0b1011,
                               (outs SPR:$Sd), (ins DPR:$Dm),
                               IIC_fpCVTDI, "vcvt", ".u32.f64\t$Sd, $Dm",
                               [(set SPR:$Sd, (arm_ftoui (f64 DPR:$Dm)))]> {
  let Inst{7} = 1; // Z bit
}

def VTOUIZS : AVConv1InsS_Encode<0b11101, 0b11, 0b1100, 0b1010,
                                 (outs SPR:$Sd), (ins SPR:$Sm),
                                 IIC_fpCVTSI, "vcvt", ".u32.f32\t$Sd, $Sm",
                                 [(set SPR:$Sd, (arm_ftoui SPR:$Sm))]> {
  let Inst{7} = 1; // Z bit

  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

// And the Z bit '0' variants, i.e. use the rounding mode specified by FPSCR.
let Uses = [FPSCR] in {
// FIXME: Verify encoding after integrated assembler is working.
def VTOSIRD : AVConv1IsD_Encode<0b11101, 0b11, 0b1101, 0b1011,
                                (outs SPR:$Sd), (ins DPR:$Dm),
                                IIC_fpCVTDI, "vcvtr", ".s32.f64\t$Sd, $Dm",
                                [(set SPR:$Sd, (int_arm_vcvtr (f64 DPR:$Dm)))]>{
  let Inst{7} = 0; // Z bit
}

def VTOSIRS : AVConv1InsS_Encode<0b11101, 0b11, 0b1101, 0b1010,
                                 (outs SPR:$Sd), (ins SPR:$Sm),
                                 IIC_fpCVTSI, "vcvtr", ".s32.f32\t$Sd, $Sm",
                                 [(set SPR:$Sd, (int_arm_vcvtr SPR:$Sm))]> {
  let Inst{7} = 0; // Z bit
}

def VTOUIRD : AVConv1IsD_Encode<0b11101, 0b11, 0b1100, 0b1011,
                                (outs SPR:$Sd), (ins DPR:$Dm),
                                IIC_fpCVTDI, "vcvtr", ".u32.f64\t$Sd, $Dm",
                                [(set SPR:$Sd, (int_arm_vcvtru(f64 DPR:$Dm)))]>{
  let Inst{7} = 0; // Z bit
}

def VTOUIRS : AVConv1InsS_Encode<0b11101, 0b11, 0b1100, 0b1010,
                                 (outs SPR:$Sd), (ins SPR:$Sm),
                                 IIC_fpCVTSI, "vcvtr", ".u32.f32\t$Sd, $Sm",
                                 [(set SPR:$Sd, (int_arm_vcvtru SPR:$Sm))]> {
  let Inst{7} = 0; // Z bit
}
}

// Convert between floating-point and fixed-point
// Data type for fixed-point naming convention:
//   S16 (U=0, sx=0) -> SH
//   U16 (U=1, sx=0) -> UH
//   S32 (U=0, sx=1) -> SL
//   U32 (U=1, sx=1) -> UL

let Constraints = "$a = $dst" in {

// FP to Fixed-Point:

// Single Precision register
class AVConv1XInsS_Encode<bits<5> op1, bits<2> op2, bits<4> op3, bits<4> op4,
                          bit op5, dag oops, dag iops, InstrItinClass itin,
                          string opc, string asm, list<dag> pattern>
  : AVConv1XI<op1, op2, op3, op4, op5, oops, iops, itin, opc, asm, pattern> {
  bits<5> dst;
  // if dp_operation then UInt(D:Vd) else UInt(Vd:D);
  let Inst{22} = dst{0};
  let Inst{15-12} = dst{4-1};
}

// Double Precision register
class AVConv1XInsD_Encode<bits<5> op1, bits<2> op2, bits<4> op3, bits<4> op4,
                          bit op5, dag oops, dag iops, InstrItinClass itin,
                          string opc, string asm, list<dag> pattern>
  : AVConv1XI<op1, op2, op3, op4, op5, oops, iops, itin, opc, asm, pattern> {
  bits<5> dst;
  // if dp_operation then UInt(D:Vd) else UInt(Vd:D);
  let Inst{22} = dst{4};
  let Inst{15-12} = dst{3-0};
}

def VTOSHS : AVConv1XInsS_Encode<0b11101, 0b11, 0b1110, 0b1010, 0,
                       (outs SPR:$dst), (ins SPR:$a, fbits16:$fbits),
                 IIC_fpCVTSI, "vcvt", ".s16.f32\t$dst, $a, $fbits", []> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VTOUHS : AVConv1XInsS_Encode<0b11101, 0b11, 0b1111, 0b1010, 0,
                       (outs SPR:$dst), (ins SPR:$a, fbits16:$fbits),
                 IIC_fpCVTSI, "vcvt", ".u16.f32\t$dst, $a, $fbits", []> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VTOSLS : AVConv1XInsS_Encode<0b11101, 0b11, 0b1110, 0b1010, 1,
                       (outs SPR:$dst), (ins SPR:$a, fbits32:$fbits),
                 IIC_fpCVTSI, "vcvt", ".s32.f32\t$dst, $a, $fbits", []> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VTOULS : AVConv1XInsS_Encode<0b11101, 0b11, 0b1111, 0b1010, 1,
                       (outs SPR:$dst), (ins SPR:$a, fbits32:$fbits),
                 IIC_fpCVTSI, "vcvt", ".u32.f32\t$dst, $a, $fbits", []> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VTOSHD : AVConv1XInsD_Encode<0b11101, 0b11, 0b1110, 0b1011, 0,
                       (outs DPR:$dst), (ins DPR:$a, fbits16:$fbits),
                 IIC_fpCVTDI, "vcvt", ".s16.f64\t$dst, $a, $fbits", []>;

def VTOUHD : AVConv1XInsD_Encode<0b11101, 0b11, 0b1111, 0b1011, 0,
                       (outs DPR:$dst), (ins DPR:$a, fbits16:$fbits),
                 IIC_fpCVTDI, "vcvt", ".u16.f64\t$dst, $a, $fbits", []>;

def VTOSLD : AVConv1XInsD_Encode<0b11101, 0b11, 0b1110, 0b1011, 1,
                       (outs DPR:$dst), (ins DPR:$a, fbits32:$fbits),
                 IIC_fpCVTDI, "vcvt", ".s32.f64\t$dst, $a, $fbits", []>;

def VTOULD : AVConv1XInsD_Encode<0b11101, 0b11, 0b1111, 0b1011, 1,
                       (outs DPR:$dst), (ins DPR:$a, fbits32:$fbits),
                 IIC_fpCVTDI, "vcvt", ".u32.f64\t$dst, $a, $fbits", []>;

// Fixed-Point to FP:

def VSHTOS : AVConv1XInsS_Encode<0b11101, 0b11, 0b1010, 0b1010, 0,
                       (outs SPR:$dst), (ins SPR:$a, fbits16:$fbits),
                 IIC_fpCVTIS, "vcvt", ".f32.s16\t$dst, $a, $fbits", []> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VUHTOS : AVConv1XInsS_Encode<0b11101, 0b11, 0b1011, 0b1010, 0,
                       (outs SPR:$dst), (ins SPR:$a, fbits16:$fbits),
                 IIC_fpCVTIS, "vcvt", ".f32.u16\t$dst, $a, $fbits", []> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VSLTOS : AVConv1XInsS_Encode<0b11101, 0b11, 0b1010, 0b1010, 1,
                       (outs SPR:$dst), (ins SPR:$a, fbits32:$fbits),
                 IIC_fpCVTIS, "vcvt", ".f32.s32\t$dst, $a, $fbits", []> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VULTOS : AVConv1XInsS_Encode<0b11101, 0b11, 0b1011, 0b1010, 1,
                       (outs SPR:$dst), (ins SPR:$a, fbits32:$fbits),
                 IIC_fpCVTIS, "vcvt", ".f32.u32\t$dst, $a, $fbits", []> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def VSHTOD : AVConv1XInsD_Encode<0b11101, 0b11, 0b1010, 0b1011, 0,
                       (outs DPR:$dst), (ins DPR:$a, fbits16:$fbits),
                 IIC_fpCVTID, "vcvt", ".f64.s16\t$dst, $a, $fbits", []>;

def VUHTOD : AVConv1XInsD_Encode<0b11101, 0b11, 0b1011, 0b1011, 0,
                       (outs DPR:$dst), (ins DPR:$a, fbits16:$fbits),
                 IIC_fpCVTID, "vcvt", ".f64.u16\t$dst, $a, $fbits", []>;

def VSLTOD : AVConv1XInsD_Encode<0b11101, 0b11, 0b1010, 0b1011, 1,
                       (outs DPR:$dst), (ins DPR:$a, fbits32:$fbits),
                 IIC_fpCVTID, "vcvt", ".f64.s32\t$dst, $a, $fbits", []>;

def VULTOD : AVConv1XInsD_Encode<0b11101, 0b11, 0b1011, 0b1011, 1,
                       (outs DPR:$dst), (ins DPR:$a, fbits32:$fbits),
                 IIC_fpCVTID, "vcvt", ".f64.u32\t$dst, $a, $fbits", []>;

} // End of 'let Constraints = "$a = $dst" in'

//===----------------------------------------------------------------------===//
// FP Multiply-Accumulate Operations.
//

def VMLAD : ADbI<0b11100, 0b00, 0, 0,
                 (outs DPR:$Dd), (ins DPR:$Ddin, DPR:$Dn, DPR:$Dm),
                 IIC_fpMAC64, "vmla", ".f64\t$Dd, $Dn, $Dm",
                 [(set DPR:$Dd, (fadd_mlx (fmul_su DPR:$Dn, DPR:$Dm),
                                          (f64 DPR:$Ddin)))]>,
              RegConstraint<"$Ddin = $Dd">,
              Requires<[HasVFP2,UseFPVMLx,DontUseFusedMAC]>;

def VMLAS : ASbIn<0b11100, 0b00, 0, 0,
                  (outs SPR:$Sd), (ins SPR:$Sdin, SPR:$Sn, SPR:$Sm),
                  IIC_fpMAC32, "vmla", ".f32\t$Sd, $Sn, $Sm",
                  [(set SPR:$Sd, (fadd_mlx (fmul_su SPR:$Sn, SPR:$Sm),
                                           SPR:$Sdin))]>,
              RegConstraint<"$Sdin = $Sd">,
              Requires<[HasVFP2,DontUseNEONForFP,UseFPVMLx,DontUseFusedMAC]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def : Pat<(fadd_mlx DPR:$dstin, (fmul_su DPR:$a, (f64 DPR:$b))),
          (VMLAD DPR:$dstin, DPR:$a, DPR:$b)>,
          Requires<[HasVFP2,UseFPVMLx,DontUseFusedMAC]>;
def : Pat<(fadd_mlx SPR:$dstin, (fmul_su SPR:$a, SPR:$b)),
          (VMLAS SPR:$dstin, SPR:$a, SPR:$b)>,
          Requires<[HasVFP2,DontUseNEONForFP, UseFPVMLx,DontUseFusedMAC]>;

def VMLSD : ADbI<0b11100, 0b00, 1, 0,
                 (outs DPR:$Dd), (ins DPR:$Ddin, DPR:$Dn, DPR:$Dm),
                 IIC_fpMAC64, "vmls", ".f64\t$Dd, $Dn, $Dm",
                 [(set DPR:$Dd, (fadd_mlx (fneg (fmul_su DPR:$Dn,DPR:$Dm)),
                                          (f64 DPR:$Ddin)))]>,
              RegConstraint<"$Ddin = $Dd">,
              Requires<[HasVFP2,UseFPVMLx,DontUseFusedMAC]>;

def VMLSS : ASbIn<0b11100, 0b00, 1, 0,
                  (outs SPR:$Sd), (ins SPR:$Sdin, SPR:$Sn, SPR:$Sm),
                  IIC_fpMAC32, "vmls", ".f32\t$Sd, $Sn, $Sm",
                  [(set SPR:$Sd, (fadd_mlx (fneg (fmul_su SPR:$Sn, SPR:$Sm)),
                                           SPR:$Sdin))]>,
              RegConstraint<"$Sdin = $Sd">,
              Requires<[HasVFP2,DontUseNEONForFP,UseFPVMLx,DontUseFusedMAC]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def : Pat<(fsub_mlx DPR:$dstin, (fmul_su DPR:$a, (f64 DPR:$b))),
          (VMLSD DPR:$dstin, DPR:$a, DPR:$b)>,
          Requires<[HasVFP2,UseFPVMLx,DontUseFusedMAC]>;
def : Pat<(fsub_mlx SPR:$dstin, (fmul_su SPR:$a, SPR:$b)),
          (VMLSS SPR:$dstin, SPR:$a, SPR:$b)>,
          Requires<[HasVFP2,DontUseNEONForFP,UseFPVMLx,DontUseFusedMAC]>;

def VNMLAD : ADbI<0b11100, 0b01, 1, 0,
                  (outs DPR:$Dd), (ins DPR:$Ddin, DPR:$Dn, DPR:$Dm),
                  IIC_fpMAC64, "vnmla", ".f64\t$Dd, $Dn, $Dm",
                  [(set DPR:$Dd,(fsub_mlx (fneg (fmul_su DPR:$Dn,DPR:$Dm)),
                                          (f64 DPR:$Ddin)))]>,
                RegConstraint<"$Ddin = $Dd">,
                Requires<[HasVFP2,UseFPVMLx,DontUseFusedMAC]>;

def VNMLAS : ASbI<0b11100, 0b01, 1, 0,
                  (outs SPR:$Sd), (ins SPR:$Sdin, SPR:$Sn, SPR:$Sm),
                  IIC_fpMAC32, "vnmla", ".f32\t$Sd, $Sn, $Sm",
                  [(set SPR:$Sd, (fsub_mlx (fneg (fmul_su SPR:$Sn, SPR:$Sm)),
                                           SPR:$Sdin))]>,
                RegConstraint<"$Sdin = $Sd">,
                Requires<[HasVFP2,DontUseNEONForFP,UseFPVMLx,DontUseFusedMAC]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def : Pat<(fsub_mlx (fneg (fmul_su DPR:$a, (f64 DPR:$b))), DPR:$dstin),
          (VNMLAD DPR:$dstin, DPR:$a, DPR:$b)>,
          Requires<[HasVFP2,UseFPVMLx,DontUseFusedMAC]>;
def : Pat<(fsub_mlx (fneg (fmul_su SPR:$a, SPR:$b)), SPR:$dstin),
          (VNMLAS SPR:$dstin, SPR:$a, SPR:$b)>,
          Requires<[HasVFP2,DontUseNEONForFP,UseFPVMLx,DontUseFusedMAC]>;

def VNMLSD : ADbI<0b11100, 0b01, 0, 0,
                  (outs DPR:$Dd), (ins DPR:$Ddin, DPR:$Dn, DPR:$Dm),
                  IIC_fpMAC64, "vnmls", ".f64\t$Dd, $Dn, $Dm",
                  [(set DPR:$Dd, (fsub_mlx (fmul_su DPR:$Dn, DPR:$Dm),
                                           (f64 DPR:$Ddin)))]>,
               RegConstraint<"$Ddin = $Dd">,
               Requires<[HasVFP2,UseFPVMLx,DontUseFusedMAC]>;

def VNMLSS : ASbI<0b11100, 0b01, 0, 0,
                  (outs SPR:$Sd), (ins SPR:$Sdin, SPR:$Sn, SPR:$Sm),
                  IIC_fpMAC32, "vnmls", ".f32\t$Sd, $Sn, $Sm",
             [(set SPR:$Sd, (fsub_mlx (fmul_su SPR:$Sn, SPR:$Sm), SPR:$Sdin))]>,
                         RegConstraint<"$Sdin = $Sd">,
                Requires<[HasVFP2,DontUseNEONForFP,UseFPVMLx,DontUseFusedMAC]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines on A8.
  let D = VFPNeonA8Domain;
}

def : Pat<(fsub_mlx (fmul_su DPR:$a, (f64 DPR:$b)), DPR:$dstin),
          (VNMLSD DPR:$dstin, DPR:$a, DPR:$b)>,
          Requires<[HasVFP2,UseFPVMLx,DontUseFusedMAC]>;
def : Pat<(fsub_mlx (fmul_su SPR:$a, SPR:$b), SPR:$dstin),
          (VNMLSS SPR:$dstin, SPR:$a, SPR:$b)>,
          Requires<[HasVFP2,DontUseNEONForFP,UseFPVMLx,DontUseFusedMAC]>;

//===----------------------------------------------------------------------===//
// ARMv8 VFP Scalar Floating-point
//
def VSELCondCodeOperand : AsmOperandClass { 
  let Name = "VSELCondCode"; 
  let DiagnosticType = "VSELcond";
}
def vsel_pred : PredicateOperand<OtherVT, (ops i32imm, i32imm),
                                     (ops (i32 14), (i32 zero_reg))> {
  let PrintMethod = "printPredicateOperand";
  let ParserMatchClass = VSELCondCodeOperand;
  let EncoderMethod = "getVSELcondValue";
  let DecoderMethod = "DecodeVSELPredicateOperand";
}

// Floating-point Selection: vsel<fc>
class BaseVSEL<string asm, bit sz, dag oops, dag iops>
      : InstARM<AddrModeNone, 4, IndexModeNone, MiscFrm, GenericDomain, "",
                NoItinerary>,
      Requires<[HasV8]> {
  let OutOperandList = oops;
  let InOperandList = !con(iops, (ins vsel_pred:$p));
  bits<2> p;
  let AsmString = !strconcat("vsel", "${p}", asm);
  let Pattern = [];
  list<Predicate> Predicates = [IsARM];
  let Inst{31-28} = 0b1111;
  let Inst{27-23} = 0b11100;
  let Inst{21-20} = p;
  let Inst{11-9} = 0b101;
  let Inst{8} = sz;
  let Inst{6} = 0;
  let Inst{4} = 0;
}
def VSEL32 
    : BaseVSEL<".f32\t$Sd,$Sn,$Sm", 0b0, (outs SPR:$Sd), 
               (ins SPR:$Sn, SPR:$Sm)> {
  bits<5> Sd;
  bits<5> Sn;
  bits<5> Sm;
  let Inst{22} = Sd{0};
  let Inst{19-16} = Sn{4-1};
  let Inst{15-12} = Sd{4-1};
  let Inst{7} = Sn{0};
  let Inst{5} = Sm{0};
  let Inst{3-0} = Sm{4-1};
}
def VSEL64
    : BaseVSEL<".f64\t$Dd,$Dn,$Dm", 0b1, (outs DPR:$Dd),
               (ins DPR:$Dn, DPR:$Dm)> {
  bits<5> Dd;
  bits<5> Dn;
  bits<5> Dm;
  let Inst{22} = Dd{4};
  let Inst{19-16} = Dn{3-0};
  let Inst{15-12} = Dd{3-0};
  let Inst{7} = Dn{4};
  let Inst{5} = Dm{4};
  let Inst{3-0} = Dm{3-0};
}

// VFP vmax and vmin
def VMAXNM64 : ADbIv8A32const<0b11101, 0b00, 0b0, 0b0,
                     (outs DPR:$Dd), (ins DPR:$Dn, DPR:$Dm),
                     // FIXME: need a FP conditional move itinerary
                     NoItinerary,
                     "vmaxnm", ".f64\t$Dd, $Dn, $Dm",
                []>, Requires<[HasV8]>;
def VMAXNM32 : ASbIv8A32const<0b11101, 0b00, 0b0, 0b0,
                     (outs SPR:$Sd), (ins SPR:$Sn, SPR:$Sm),
                     // FIXME: need a FP conditional move itinerary
                     NoItinerary,
                     "vmaxnm", ".f32\t$Sd, $Sn, $Sm",
                []>, Requires<[HasV8]>;

def VMINNM64 : ADbIv8A32const<0b11101, 0b00, 0b1, 0b0,
                     (outs DPR:$Dd), (ins DPR:$Dn, DPR:$Dm),
                     // FIXME: need a FP conditional move itinerary
                     NoItinerary,
                     "vminnm", ".f64\t$Dd, $Dn, $Dm",
                []>, Requires<[HasV8]>;
def VMINNM32 : ASbIv8A32const<0b11101, 0b00, 0b1, 0b0,
                     (outs SPR:$Sd), (ins SPR:$Sn, SPR:$Sm),
                     // FIXME: need a FP conditional move itinerary
                     NoItinerary,
                     "vminnm", ".f32\t$Sd, $Sn, $Sm",
                []>, Requires<[HasV8]>;

// SIMD vmax and vmin
// ARMv8 A32, Quad, binary, const
class BaseSIMDV8<bits<5> opcod1, bits<2> opcod2, bit op6, bit op4, dag oops,
                dag iops, string opc, string asm>
      : VFPV8A32const<oops, iops, AddrModeNone, 4, IndexModeNone, VFPBinaryFrm, 
                NoItinerary, opc, asm, "", []>, Requires<[HasV8]> {
  let Inst{27-23} = opcod1;
  let Inst{21-20} = opcod2;
  let Inst{11-8}  = 0b1111;
  let Inst{6}     = op6;
  let Inst{4}     = op4;
}

// ARMv8 A32, Quad, binary, const
class SIMDV8Q<bits<5> opcod1, bits<2> opcod2, bit op6, bit op4, dag oops,
              dag iops, string opc, string asm>
      : BaseSIMDV8<opcod1, opcod2, op6, op4, oops, iops, opc, asm> {
  // Instruction operands.
  bits<5> Qd;
  bits<5> Qn;
  bits<5> Qm;

  // Encode instruction operands.
  let Inst{3-0}   = Qm{3-0};
  let Inst{5}     = Qm{4};
  let Inst{19-16} = Qn{3-0};
  let Inst{7}     = Qn{4};
  let Inst{15-12} = Qd{3-0};
  let Inst{22}    = Qd{4};
}

class SIMDV8D<bits<5> opcod1, bits<2> opcod2, bit op6, bit op4, dag oops,
              dag iops, string opc, string asm>
      : BaseSIMDV8<opcod1, opcod2, op6, op4, oops, iops, opc, asm> {
  // Instruction operands.
  bits<5> Dd;
  bits<5> Dn;
  bits<5> Dm;

  // Encode instruction operands.
  let Inst{3-0}   = Dm{3-0};
  let Inst{5}     = Dm{4};
  let Inst{19-16} = Dn{3-0};
  let Inst{7}     = Dn{4};
  let Inst{15-12} = Dd{3-0};
  let Inst{22}    = Dd{4};
}

def VMAXNMV32q : SIMDV8Q<0b11110, 0b00, 0b1, 0b1, (outs QPR:$Qd),
                         (ins QPR:$Qn, QPR:$Qm), "vmaxnm", 
                         ".f32\t$Qd, $Qn, $Qm">;
def VMINNMV32q : SIMDV8Q<0b11110, 0b10, 0b1, 0b1, (outs QPR:$Qd), 
                         (ins QPR:$Qn, QPR:$Qm), "vminnm", 
                         ".f32\t$Qd, $Qn, $Qm">;
def VMAXNMV32d : SIMDV8D<0b11110, 0b00, 0b0, 0b1, (outs DPR:$Dd),
                         (ins DPR:$Dn, DPR:$Dm), "vmaxnm", 
                         ".f32\t$Dd, $Dn, $Dm">;
def VMINNMV32d : SIMDV8D<0b11110, 0b10, 0b0, 0b1, (outs DPR:$Dd), 
                         (ins DPR:$Dn, DPR:$Dm), "vminnm",
                         ".f32\t$Dd, $Dn, $Dm">;

// Super class for  VFP VCVTs.  Bits for encoding registers are handled in
// subclasses.
class CommonVCVT_VFP<dag oops, dag iops, string rndMode, string ext, bits<2> RM,
                      bits<1> isDouble, bits<1> isSigned>
      : AInoP<oops, iops, MiscFrm, NoItinerary,
           !strconcat("vcvt", rndMode), ext, []>, 
      Requires<[HasV8]> {
  bits<5> Sd;
  let Inst{31-28} = 0b1111;
  let Inst{27-23} = 0b11101;
  let Inst{22} = Sd{4};
  let Inst{21-20} = 0b11;
  let Inst{19-18} = 0b11;
  let Inst{17-16} = RM;
  let Inst{15-12} = Sd{3-0};
  let Inst{11-9} = 0b101;
  let Inst{8} = isDouble;
  let Inst{7} = isSigned;
  let Inst{6} = 0b1;
  let Inst{4} = 0b0;
}
class BaseVCVTMultiSD<string rndMode, string ext, bits<2> RM, bits<1> isSigned>
      : CommonVCVT_VFP<(outs SPR:$Sd), (ins DPR:$Dm), rndMode,
                       !strconcat(ext, "\t$Sd, $Dm"), RM, 0b1, isSigned> {
  bits<5> Dm;
  let Inst{5} = Dm{4};
  let Inst{3-0} = Dm{3-0};
}
class BaseVCVTMultiSS<string rndMode, string ext, bits<2> RM, bits<1> isSigned>
      : CommonVCVT_VFP<(outs SPR:$Sd), (ins SPR:$Sm), rndMode,
                       !strconcat(ext, "\t$Sd, $Sm"), RM, 0b0, isSigned> {
  bits<5> Sm;
  let Inst{5} = Sm{4};
  let Inst{3-0} = Sm{3-0};
}
multiclass VCVTMulti<string rndMode, bits<2> RM> {
    def S32F64 : BaseVCVTMultiSD<rndMode, ".s32.f64", RM, 0b1>;
    def S32F32 : BaseVCVTMultiSS<rndMode, ".s32.f32", RM, 0b1>;
    def U32F64 : BaseVCVTMultiSD<rndMode, ".u32.f64", RM, 0b0>;
    def U32F32 : BaseVCVTMultiSS<rndMode, ".u32.f32", RM, 0b0>;
}
defm VCVTA : VCVTMulti<"a", 0b00>;
defm VCVTN : VCVTMulti<"n", 0b01>;
defm VCVTP : VCVTMulti<"p", 0b10>;
defm VCVTM : VCVTMulti<"m", 0b11>;

// SIMD vcvt
// 31-28: 0b1111 27-23: 0b11111 22: D 21-20: 0b11 19-18: size 17-16: 0b11
// 15-12: Vd 11: 0b0 10: 0b0 9-8: RM 7: op 6: Q 5: M 4: 0b0 3-0: Vm
// VCVT<r>.S32.F32 Dd, Dm;  VCVT<r>.S32.F32 Qd, Qm
// VCVT<r>.U32.F32 Dd, Dm; VCVT<r>.U32.F32 Qd, Qm
// Super class for  SIMD VCVTs.  Bits for encoding registers are handled in
// subclasses.
class CommonVCVT_SIMD<dag oops, dag iops, string rndMode, string ext, 
                      bits<2> RM, bits<1> isQ, bits<1> isUnSigned>
      : AInoP<oops, iops, MiscFrm, NoItinerary,
             !strconcat("vcvt", rndMode), ext, []>,
      Requires<[HasV8]> {
  bits<5> Sd;
  let Inst{31-28} = 0b1111;
  let Inst{27-23} = 0b11111;
  let Inst{21-20} = 0b11;
  let Inst{19-18} = 0b10;
  let Inst{17-16} = 0b11;
  let Inst{11} = 0b0;
  let Inst{10} = 0b0;
  let Inst{9-8} = RM;
  let Inst{7} = isUnSigned;
  let Inst{6} = isQ;
  let Inst{4} = 0b0;
}
class BaseVCVTMultiQ<string rndMode, string ext, bits<2> RM, bits<1> isUnSigned>
      : CommonVCVT_SIMD<(outs QPR:$Qd), (ins QPR:$Qm), rndMode,
                        !strconcat(ext, "\t$Qd, $Qm"), RM, 0b1, isUnSigned> {
  bits<5> Qm;
  bits<5> Qd;
  let Inst{22} = Qd{4};
  let Inst{15-12} = Qd{3-0};
  let Inst{5} = Qm{4};
  let Inst{3-0} = Qm{3-0};
}
class BaseVCVTMultiD<string rndMode, string ext, bits<2> RM, bits<1> isUnSigned>
      : CommonVCVT_SIMD<(outs DPR:$Dd), (ins DPR:$Dm), rndMode,
                        !strconcat(ext, "\t$Dd, $Dm"), RM, 0b0, isUnSigned> {
  // CHECK encoding
  bits<5> Dm;
  bits<5> Dd;
  let Inst{22} = Dd{4};
  let Inst{15-12} = Dd{3-0};
  let Inst{5} = Dm{4};
  let Inst{3-0} = Dm{3-0};
}
multiclass VCVTMultiSIMD<string rndMode, bits<2> RM> {
    def S32F32Q : BaseVCVTMultiQ<rndMode, ".s32.f32", RM, 0b0>;
    def U32F32Q : BaseVCVTMultiQ<rndMode, ".u32.f32", RM, 0b1>;
    def S32F32D : BaseVCVTMultiD<rndMode, ".s32.f32", RM, 0b0>;
    def U32F32D : BaseVCVTMultiD<rndMode, ".u32.f32", RM, 0b1>;
}
defm VCVTA_SIMD : VCVTMultiSIMD<"a", 0b00>;
defm VCVTN_SIMD : VCVTMultiSIMD<"n", 0b01>;
defm VCVTP_SIMD : VCVTMultiSIMD<"p", 0b10>;
defm VCVTM_SIMD : VCVTMultiSIMD<"m", 0b11>;

//===----------------------------------------------------------------------===//
// Fused FP Multiply-Accumulate Operations.
//
def VFMAD : ADbI<0b11101, 0b10, 0, 0,
                 (outs DPR:$Dd), (ins DPR:$Ddin, DPR:$Dn, DPR:$Dm),
                 IIC_fpFMAC64, "vfma", ".f64\t$Dd, $Dn, $Dm",
                 [(set DPR:$Dd, (fadd_mlx (fmul_su DPR:$Dn, DPR:$Dm),
                                          (f64 DPR:$Ddin)))]>,
              RegConstraint<"$Ddin = $Dd">,
              Requires<[HasVFP4,UseFusedMAC]>;

def VFMAS : ASbIn<0b11101, 0b10, 0, 0,
                  (outs SPR:$Sd), (ins SPR:$Sdin, SPR:$Sn, SPR:$Sm),
                  IIC_fpFMAC32, "vfma", ".f32\t$Sd, $Sn, $Sm",
                  [(set SPR:$Sd, (fadd_mlx (fmul_su SPR:$Sn, SPR:$Sm),
                                           SPR:$Sdin))]>,
              RegConstraint<"$Sdin = $Sd">,
              Requires<[HasVFP4,DontUseNEONForFP,UseFusedMAC]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines.
}

def : Pat<(fadd_mlx DPR:$dstin, (fmul_su DPR:$a, (f64 DPR:$b))),
          (VFMAD DPR:$dstin, DPR:$a, DPR:$b)>,
          Requires<[HasVFP4,UseFusedMAC]>;
def : Pat<(fadd_mlx SPR:$dstin, (fmul_su SPR:$a, SPR:$b)),
          (VFMAS SPR:$dstin, SPR:$a, SPR:$b)>,
          Requires<[HasVFP4,DontUseNEONForFP,UseFusedMAC]>;

// Match @llvm.fma.* intrinsics
// (fma x, y, z) -> (vfms z, x, y)
def : Pat<(f64 (fma DPR:$Dn, DPR:$Dm, DPR:$Ddin)),
          (VFMAD DPR:$Ddin, DPR:$Dn, DPR:$Dm)>,
      Requires<[HasVFP4]>;
def : Pat<(f32 (fma SPR:$Sn, SPR:$Sm, SPR:$Sdin)),
          (VFMAS SPR:$Sdin, SPR:$Sn, SPR:$Sm)>,
      Requires<[HasVFP4]>;

def VFMSD : ADbI<0b11101, 0b10, 1, 0,
                 (outs DPR:$Dd), (ins DPR:$Ddin, DPR:$Dn, DPR:$Dm),
                 IIC_fpFMAC64, "vfms", ".f64\t$Dd, $Dn, $Dm",
                 [(set DPR:$Dd, (fadd_mlx (fneg (fmul_su DPR:$Dn,DPR:$Dm)),
                                          (f64 DPR:$Ddin)))]>,
              RegConstraint<"$Ddin = $Dd">,
              Requires<[HasVFP4,UseFusedMAC]>;

def VFMSS : ASbIn<0b11101, 0b10, 1, 0,
                  (outs SPR:$Sd), (ins SPR:$Sdin, SPR:$Sn, SPR:$Sm),
                  IIC_fpFMAC32, "vfms", ".f32\t$Sd, $Sn, $Sm",
                  [(set SPR:$Sd, (fadd_mlx (fneg (fmul_su SPR:$Sn, SPR:$Sm)),
                                           SPR:$Sdin))]>,
              RegConstraint<"$Sdin = $Sd">,
              Requires<[HasVFP4,DontUseNEONForFP,UseFusedMAC]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines.
}

def : Pat<(fsub_mlx DPR:$dstin, (fmul_su DPR:$a, (f64 DPR:$b))),
          (VFMSD DPR:$dstin, DPR:$a, DPR:$b)>,
          Requires<[HasVFP4,UseFusedMAC]>;
def : Pat<(fsub_mlx SPR:$dstin, (fmul_su SPR:$a, SPR:$b)),
          (VFMSS SPR:$dstin, SPR:$a, SPR:$b)>,
          Requires<[HasVFP4,DontUseNEONForFP,UseFusedMAC]>;

// Match @llvm.fma.* intrinsics
// (fma (fneg x), y, z) -> (vfms z, x, y)
def : Pat<(f64 (fma (fneg DPR:$Dn), DPR:$Dm, DPR:$Ddin)),
          (VFMSD DPR:$Ddin, DPR:$Dn, DPR:$Dm)>,
      Requires<[HasVFP4]>;
def : Pat<(f32 (fma (fneg SPR:$Sn), SPR:$Sm, SPR:$Sdin)),
          (VFMSS SPR:$Sdin, SPR:$Sn, SPR:$Sm)>,
      Requires<[HasVFP4]>;
// (fma x, (fneg y), z) -> (vfms z, x, y)
def : Pat<(f64 (fma DPR:$Dn, (fneg DPR:$Dm), DPR:$Ddin)),
          (VFMSD DPR:$Ddin, DPR:$Dn, DPR:$Dm)>,
      Requires<[HasVFP4]>;
def : Pat<(f32 (fma SPR:$Sn, (fneg SPR:$Sm), SPR:$Sdin)),
          (VFMSS SPR:$Sdin, SPR:$Sn, SPR:$Sm)>,
      Requires<[HasVFP4]>;

def VFNMAD : ADbI<0b11101, 0b01, 1, 0,
                  (outs DPR:$Dd), (ins DPR:$Ddin, DPR:$Dn, DPR:$Dm),
                  IIC_fpFMAC64, "vfnma", ".f64\t$Dd, $Dn, $Dm",
                  [(set DPR:$Dd,(fsub_mlx (fneg (fmul_su DPR:$Dn,DPR:$Dm)),
                                          (f64 DPR:$Ddin)))]>,
                RegConstraint<"$Ddin = $Dd">,
                Requires<[HasVFP4,UseFusedMAC]>;

def VFNMAS : ASbI<0b11101, 0b01, 1, 0,
                  (outs SPR:$Sd), (ins SPR:$Sdin, SPR:$Sn, SPR:$Sm),
                  IIC_fpFMAC32, "vfnma", ".f32\t$Sd, $Sn, $Sm",
                  [(set SPR:$Sd, (fsub_mlx (fneg (fmul_su SPR:$Sn, SPR:$Sm)),
                                           SPR:$Sdin))]>,
                RegConstraint<"$Sdin = $Sd">,
                Requires<[HasVFP4,DontUseNEONForFP,UseFusedMAC]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines.
}

def : Pat<(fsub_mlx (fneg (fmul_su DPR:$a, (f64 DPR:$b))), DPR:$dstin),
          (VFNMAD DPR:$dstin, DPR:$a, DPR:$b)>,
          Requires<[HasVFP4,UseFusedMAC]>;
def : Pat<(fsub_mlx (fneg (fmul_su SPR:$a, SPR:$b)), SPR:$dstin),
          (VFNMAS SPR:$dstin, SPR:$a, SPR:$b)>,
          Requires<[HasVFP4,DontUseNEONForFP,UseFusedMAC]>;

// Match @llvm.fma.* intrinsics
// (fneg (fma x, y, z)) -> (vfnma z, x, y)
def : Pat<(fneg (fma (f64 DPR:$Dn), (f64 DPR:$Dm), (f64 DPR:$Ddin))),
          (VFNMAD DPR:$Ddin, DPR:$Dn, DPR:$Dm)>,
      Requires<[HasVFP4]>;
def : Pat<(fneg (fma (f32 SPR:$Sn), (f32 SPR:$Sm), (f32 SPR:$Sdin))),
          (VFNMAS SPR:$Sdin, SPR:$Sn, SPR:$Sm)>,
      Requires<[HasVFP4]>;
// (fma (fneg x), y, (fneg z)) -> (vfnma z, x, y)
def : Pat<(f64 (fma (fneg DPR:$Dn), DPR:$Dm, (fneg DPR:$Ddin))),
          (VFNMAD DPR:$Ddin, DPR:$Dn, DPR:$Dm)>,
      Requires<[HasVFP4]>;
def : Pat<(f32 (fma (fneg SPR:$Sn), SPR:$Sm, (fneg SPR:$Sdin))),
          (VFNMAS SPR:$Sdin, SPR:$Sn, SPR:$Sm)>,
      Requires<[HasVFP4]>;

def VFNMSD : ADbI<0b11101, 0b01, 0, 0,
                  (outs DPR:$Dd), (ins DPR:$Ddin, DPR:$Dn, DPR:$Dm),
                  IIC_fpFMAC64, "vfnms", ".f64\t$Dd, $Dn, $Dm",
                  [(set DPR:$Dd, (fsub_mlx (fmul_su DPR:$Dn, DPR:$Dm),
                                           (f64 DPR:$Ddin)))]>,
               RegConstraint<"$Ddin = $Dd">,
               Requires<[HasVFP4,UseFusedMAC]>;

def VFNMSS : ASbI<0b11101, 0b01, 0, 0,
                  (outs SPR:$Sd), (ins SPR:$Sdin, SPR:$Sn, SPR:$Sm),
                  IIC_fpFMAC32, "vfnms", ".f32\t$Sd, $Sn, $Sm",
             [(set SPR:$Sd, (fsub_mlx (fmul_su SPR:$Sn, SPR:$Sm), SPR:$Sdin))]>,
                         RegConstraint<"$Sdin = $Sd">,
                  Requires<[HasVFP4,DontUseNEONForFP,UseFusedMAC]> {
  // Some single precision VFP instructions may be executed on both NEON and
  // VFP pipelines.
}

def : Pat<(fsub_mlx (fmul_su DPR:$a, (f64 DPR:$b)), DPR:$dstin),
          (VFNMSD DPR:$dstin, DPR:$a, DPR:$b)>,
          Requires<[HasVFP4,UseFusedMAC]>;
def : Pat<(fsub_mlx (fmul_su SPR:$a, SPR:$b), SPR:$dstin),
          (VFNMSS SPR:$dstin, SPR:$a, SPR:$b)>,
          Requires<[HasVFP4,DontUseNEONForFP,UseFusedMAC]>;

// Match @llvm.fma.* intrinsics

// (fma x, y, (fneg z)) -> (vfnms z, x, y))
def : Pat<(f64 (fma DPR:$Dn, DPR:$Dm, (fneg DPR:$Ddin))),
          (VFNMSD DPR:$Ddin, DPR:$Dn, DPR:$Dm)>,
      Requires<[HasVFP4]>;
def : Pat<(f32 (fma SPR:$Sn, SPR:$Sm, (fneg SPR:$Sdin))),
          (VFNMSS SPR:$Sdin, SPR:$Sn, SPR:$Sm)>,
      Requires<[HasVFP4]>;
// (fneg (fma (fneg x), y, z)) -> (vfnms z, x, y)
def : Pat<(fneg (f64 (fma (fneg DPR:$Dn), DPR:$Dm, DPR:$Ddin))),
          (VFNMSD DPR:$Ddin, DPR:$Dn, DPR:$Dm)>,
      Requires<[HasVFP4]>;
def : Pat<(fneg (f32 (fma (fneg SPR:$Sn), SPR:$Sm, SPR:$Sdin))),
          (VFNMSS SPR:$Sdin, SPR:$Sn, SPR:$Sm)>,
      Requires<[HasVFP4]>;
// (fneg (fma x, (fneg y), z) -> (vfnms z, x, y)
def : Pat<(fneg (f64 (fma DPR:$Dn, (fneg DPR:$Dm), DPR:$Ddin))),
          (VFNMSD DPR:$Ddin, DPR:$Dn, DPR:$Dm)>,
      Requires<[HasVFP4]>;
def : Pat<(fneg (f32 (fma SPR:$Sn, (fneg SPR:$Sm), SPR:$Sdin))),
          (VFNMSS SPR:$Sdin, SPR:$Sn, SPR:$Sm)>,
      Requires<[HasVFP4]>;

//===----------------------------------------------------------------------===//
// FP Conditional moves.
//

let neverHasSideEffects = 1 in {
def VMOVDcc  : ARMPseudoInst<(outs DPR:$Dd), (ins DPR:$Dn, DPR:$Dm, pred:$p),
                    4, IIC_fpUNA64,
                    [/*(set DPR:$Dd, (ARMcmov DPR:$Dn, DPR:$Dm, imm:$cc))*/]>,
                 RegConstraint<"$Dn = $Dd">;

def VMOVScc  : ARMPseudoInst<(outs SPR:$Sd), (ins SPR:$Sn, SPR:$Sm, pred:$p),
                    4, IIC_fpUNA32,
                    [/*(set SPR:$Sd, (ARMcmov SPR:$Sn, SPR:$Sm, imm:$cc))*/]>,
                 RegConstraint<"$Sn = $Sd">;
} // neverHasSideEffects

//===----------------------------------------------------------------------===//
// Move from VFP System Register to ARM core register.
//

class MovFromVFP<bits<4> opc19_16, dag oops, dag iops, string opc, string asm,
                 list<dag> pattern>:
  VFPAI<oops, iops, VFPMiscFrm, IIC_fpSTAT, opc, asm, pattern> {

  // Instruction operand.
  bits<4> Rt;

  let Inst{27-20} = 0b11101111;
  let Inst{19-16} = opc19_16;
  let Inst{15-12} = Rt;
  let Inst{11-8}  = 0b1010;
  let Inst{7}     = 0;
  let Inst{6-5}   = 0b00;
  let Inst{4}     = 1;
  let Inst{3-0}   = 0b0000;
}

// APSR is the application level alias of CPSR. This FPSCR N, Z, C, V flags
// to APSR.
let Defs = [CPSR], Uses = [FPSCR_NZCV], Rt = 0b1111 /* apsr_nzcv */ in
def FMSTAT : MovFromVFP<0b0001 /* fpscr */, (outs), (ins),
                        "vmrs", "\tAPSR_nzcv, fpscr", [(arm_fmstat)]>;

// Application level FPSCR -> GPR
let hasSideEffects = 1, Uses = [FPSCR] in
def VMRS : MovFromVFP<0b0001 /* fpscr */, (outs GPR:$Rt), (ins),
                      "vmrs", "\t$Rt, fpscr",
                      [(set GPR:$Rt, (int_arm_get_fpscr))]>;

// System level FPEXC, FPSID -> GPR
let Uses = [FPSCR] in {
  def VMRS_FPEXC : MovFromVFP<0b1000 /* fpexc */, (outs GPR:$Rt), (ins),
                              "vmrs", "\t$Rt, fpexc", []>;
  def VMRS_FPSID : MovFromVFP<0b0000 /* fpsid */, (outs GPR:$Rt), (ins),
                              "vmrs", "\t$Rt, fpsid", []>;
  def VMRS_MVFR0 : MovFromVFP<0b0111 /* mvfr0 */, (outs GPR:$Rt), (ins),
                              "vmrs", "\t$Rt, mvfr0", []>;
  def VMRS_MVFR1 : MovFromVFP<0b0110 /* mvfr1 */, (outs GPR:$Rt), (ins),
                              "vmrs", "\t$Rt, mvfr1", []>;
}

//===----------------------------------------------------------------------===//
// Move from ARM core register to VFP System Register.
//

class MovToVFP<bits<4> opc19_16, dag oops, dag iops, string opc, string asm,
               list<dag> pattern>:
  VFPAI<oops, iops, VFPMiscFrm, IIC_fpSTAT, opc, asm, pattern> {

  // Instruction operand.
  bits<4> src;

  // Encode instruction operand.
  let Inst{15-12} = src;

  let Inst{27-20} = 0b11101110;
  let Inst{19-16} = opc19_16;
  let Inst{11-8}  = 0b1010;
  let Inst{7}     = 0;
  let Inst{4}     = 1;
}

let Defs = [FPSCR] in {
  // Application level GPR -> FPSCR
  def VMSR : MovToVFP<0b0001 /* fpscr */, (outs), (ins GPR:$src),
                      "vmsr", "\tfpscr, $src", [(int_arm_set_fpscr GPR:$src)]>;
  // System level GPR -> FPEXC
  def VMSR_FPEXC : MovToVFP<0b1000 /* fpexc */, (outs), (ins GPR:$src),
                      "vmsr", "\tfpexc, $src", []>;
  // System level GPR -> FPSID
  def VMSR_FPSID : MovToVFP<0b0000 /* fpsid */, (outs), (ins GPR:$src),
                      "vmsr", "\tfpsid, $src", []>;
}

//===----------------------------------------------------------------------===//
// Misc.
//

// Materialize FP immediates. VFP3 only.
let isReMaterializable = 1 in {
def FCONSTD : VFPAI<(outs DPR:$Dd), (ins vfp_f64imm:$imm),
                    VFPMiscFrm, IIC_fpUNA64,
                    "vmov", ".f64\t$Dd, $imm",
                    [(set DPR:$Dd, vfp_f64imm:$imm)]>, Requires<[HasVFP3]> {
  bits<5> Dd;
  bits<8> imm;

  let Inst{27-23} = 0b11101;
  let Inst{22}    = Dd{4};
  let Inst{21-20} = 0b11;
  let Inst{19-16} = imm{7-4};
  let Inst{15-12} = Dd{3-0};
  let Inst{11-9}  = 0b101;
  let Inst{8}     = 1;          // Double precision.
  let Inst{7-4}   = 0b0000;
  let Inst{3-0}   = imm{3-0};
}

def FCONSTS : VFPAI<(outs SPR:$Sd), (ins vfp_f32imm:$imm),
                     VFPMiscFrm, IIC_fpUNA32,
                     "vmov", ".f32\t$Sd, $imm",
                     [(set SPR:$Sd, vfp_f32imm:$imm)]>, Requires<[HasVFP3]> {
  bits<5> Sd;
  bits<8> imm;

  let Inst{27-23} = 0b11101;
  let Inst{22}    = Sd{0};
  let Inst{21-20} = 0b11;
  let Inst{19-16} = imm{7-4};
  let Inst{15-12} = Sd{4-1};
  let Inst{11-9}  = 0b101;
  let Inst{8}     = 0;          // Single precision.
  let Inst{7-4}   = 0b0000;
  let Inst{3-0}   = imm{3-0};
}
}

//===----------------------------------------------------------------------===//
// Assembler aliases.
//
// A few mnemnoic aliases for pre-unifixed syntax. We don't guarantee to
// support them all, but supporting at least some of the basics is
// good to be friendly.
def : VFP2MnemonicAlias<"flds", "vldr">;
def : VFP2MnemonicAlias<"fldd", "vldr">;
def : VFP2MnemonicAlias<"fmrs", "vmov">;
def : VFP2MnemonicAlias<"fmsr", "vmov">;
def : VFP2MnemonicAlias<"fsqrts", "vsqrt">;
def : VFP2MnemonicAlias<"fsqrtd", "vsqrt">;
def : VFP2MnemonicAlias<"fadds", "vadd.f32">;
def : VFP2MnemonicAlias<"faddd", "vadd.f64">;
def : VFP2MnemonicAlias<"fmrdd", "vmov">;
def : VFP2MnemonicAlias<"fmrds", "vmov">;
def : VFP2MnemonicAlias<"fmrrd", "vmov">;
def : VFP2MnemonicAlias<"fmdrr", "vmov">;
def : VFP2MnemonicAlias<"fmuls", "vmul.f32">;
def : VFP2MnemonicAlias<"fmuld", "vmul.f64">;
def : VFP2MnemonicAlias<"fnegs", "vneg.f32">;
def : VFP2MnemonicAlias<"fnegd", "vneg.f64">;
def : VFP2MnemonicAlias<"ftosizd", "vcvt.s32.f64">;
def : VFP2MnemonicAlias<"ftosid", "vcvtr.s32.f64">;
def : VFP2MnemonicAlias<"ftosizs", "vcvt.s32.f32">;
def : VFP2MnemonicAlias<"ftosis", "vcvtr.s32.f32">;
def : VFP2MnemonicAlias<"ftouizd", "vcvt.u32.f64">;
def : VFP2MnemonicAlias<"ftouid", "vcvtr.u32.f64">;
def : VFP2MnemonicAlias<"ftouizs", "vcvt.u32.f32">;
def : VFP2MnemonicAlias<"ftouis", "vcvtr.u32.f32">;
def : VFP2MnemonicAlias<"fsitod", "vcvt.f64.s32">;
def : VFP2MnemonicAlias<"fsitos", "vcvt.f32.s32">;
def : VFP2MnemonicAlias<"fuitod", "vcvt.f64.u32">;
def : VFP2MnemonicAlias<"fuitos", "vcvt.f32.u32">;
def : VFP2MnemonicAlias<"fsts", "vstr">;
def : VFP2MnemonicAlias<"fstd", "vstr">;
def : VFP2MnemonicAlias<"fmacd", "vmla.f64">;
def : VFP2MnemonicAlias<"fmacs", "vmla.f32">;
def : VFP2MnemonicAlias<"fcpys", "vmov.f32">;
def : VFP2MnemonicAlias<"fcpyd", "vmov.f64">;
def : VFP2MnemonicAlias<"fcmps", "vcmp.f32">;
def : VFP2MnemonicAlias<"fcmpd", "vcmp.f64">;
def : VFP2MnemonicAlias<"fdivs", "vdiv.f32">;
def : VFP2MnemonicAlias<"fdivd", "vdiv.f64">;
def : VFP2MnemonicAlias<"fmrx", "vmrs">;
def : VFP2MnemonicAlias<"fmxr", "vmsr">;

// Be friendly and accept the old form of zero-compare
def : VFP2InstAlias<"fcmpzd${p} $val", (VCMPZD DPR:$val, pred:$p)>;
def : VFP2InstAlias<"fcmpzs${p} $val", (VCMPZS SPR:$val, pred:$p)>;


def : VFP2InstAlias<"fmstat${p}", (FMSTAT pred:$p)>;
def : VFP2InstAlias<"fadds${p} $Sd, $Sn, $Sm",
                    (VADDS SPR:$Sd, SPR:$Sn, SPR:$Sm, pred:$p)>;
def : VFP2InstAlias<"faddd${p} $Dd, $Dn, $Dm",
                    (VADDD DPR:$Dd, DPR:$Dn, DPR:$Dm, pred:$p)>;
def : VFP2InstAlias<"fsubs${p} $Sd, $Sn, $Sm",
                    (VSUBS SPR:$Sd, SPR:$Sn, SPR:$Sm, pred:$p)>;
def : VFP2InstAlias<"fsubd${p} $Dd, $Dn, $Dm",
                    (VSUBD DPR:$Dd, DPR:$Dn, DPR:$Dm, pred:$p)>;

// No need for the size suffix on VSQRT. It's implied by the register classes.
def : VFP2InstAlias<"vsqrt${p} $Sd, $Sm", (VSQRTS SPR:$Sd, SPR:$Sm, pred:$p)>;
def : VFP2InstAlias<"vsqrt${p} $Dd, $Dm", (VSQRTD DPR:$Dd, DPR:$Dm, pred:$p)>;

// VLDR/VSTR accept an optional type suffix.
def : VFP2InstAlias<"vldr${p}.32 $Sd, $addr",
                    (VLDRS SPR:$Sd, addrmode5:$addr, pred:$p)>;
def : VFP2InstAlias<"vstr${p}.32 $Sd, $addr",
                    (VSTRS SPR:$Sd, addrmode5:$addr, pred:$p)>;
def : VFP2InstAlias<"vldr${p}.64 $Dd, $addr",
                    (VLDRD DPR:$Dd, addrmode5:$addr, pred:$p)>;
def : VFP2InstAlias<"vstr${p}.64 $Dd, $addr",
                    (VSTRD DPR:$Dd, addrmode5:$addr, pred:$p)>;

// VMOV can accept optional 32-bit or less data type suffix suffix.
def : VFP2InstAlias<"vmov${p}.8 $Rt, $Sn",
                    (VMOVRS GPR:$Rt, SPR:$Sn, pred:$p)>;
def : VFP2InstAlias<"vmov${p}.16 $Rt, $Sn",
                    (VMOVRS GPR:$Rt, SPR:$Sn, pred:$p)>;
def : VFP2InstAlias<"vmov${p}.32 $Rt, $Sn",
                    (VMOVRS GPR:$Rt, SPR:$Sn, pred:$p)>;
def : VFP2InstAlias<"vmov${p}.8 $Sn, $Rt",
                    (VMOVSR SPR:$Sn, GPR:$Rt, pred:$p)>;
def : VFP2InstAlias<"vmov${p}.16 $Sn, $Rt",
                    (VMOVSR SPR:$Sn, GPR:$Rt, pred:$p)>;
def : VFP2InstAlias<"vmov${p}.32 $Sn, $Rt",
                    (VMOVSR SPR:$Sn, GPR:$Rt, pred:$p)>;

def : VFP2InstAlias<"vmov${p}.f64 $Rt, $Rt2, $Dn",
                    (VMOVRRD GPR:$Rt, GPR:$Rt2, DPR:$Dn, pred:$p)>;
def : VFP2InstAlias<"vmov${p}.f64 $Dn, $Rt, $Rt2",
                    (VMOVDRR DPR:$Dn, GPR:$Rt, GPR:$Rt2, pred:$p)>;

// VMOVS doesn't need the .f32 to disambiguate from the NEON encoding the way
// VMOVD does.
def : VFP2InstAlias<"vmov${p} $Sd, $Sm",
                    (VMOVS SPR:$Sd, SPR:$Sm, pred:$p)>;
