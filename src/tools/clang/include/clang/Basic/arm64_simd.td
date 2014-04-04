//===--- arm64_simd.td - ARM64 SIMD compiler interface --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the TableGen definitions from which the ARM64 SIMD header
//  file will be generated.
//
//===----------------------------------------------------------------------===//

class Op;

def OP_NONE  : Op;
def OP_UNAVAILABLE : Op;
def OP_ADD   : Op;
def OP_ADDL  : Op;
def OP_ADDL_HIGH : Op;
def OP_ADDW  : Op;
def OP_SUB   : Op;
def OP_SUBL  : Op;
def OP_SUBW  : Op;
def OP_MUL   : Op;
def OP_MLA   : Op;
def OP_MLAL  : Op;
def OP_MLS   : Op;
def OP_MLSL  : Op;
def OP_MUL_N : Op;
def OP_MLA_N : Op;
def OP_MLS_N : Op;
def OP_MLAL_N : Op;
def OP_MLSL_N : Op;
def OP_MOVL_HIGH : Op;
def OP_MUL_LN: Op;
def OP_MULL_LN : Op;
def OP_MULL_HIGH : Op;
def OP_MULL_HIGH_N : Op;
def OP_MULL_HIGH_LN : Op;
def OP_MLA_LN: Op;
def OP_MLS_LN: Op;
def OP_MLAL_LN : Op;
def OP_MLAL_HIGH : Op;
def OP_MLAL_HIGH_LN : Op;
def OP_MLAL_HIGH_N : Op;
def OP_MLSL_LN : Op;
def OP_MLSL_HIGH : Op;
def OP_MLSL_HIGH_LN : Op;
def OP_MLSL_HIGH_N : Op;
def OP_QDMULL_LN : Op;
def OP_QDMULL_HIGH_N : Op;
def OP_QDMULL_HIGH_LN : Op;
def OP_QDMLAL_LN : Op;
def OP_QDMLAL_HIGH_LN : Op;
def OP_QDMLAL_HIGH_N : Op;
def OP_QDMLSL_LN : Op;
def OP_QDMLSL_HIGH_LN : Op;
def OP_QDMLSL_HIGH_N : Op;
def OP_QDMULH_LN : Op;
def OP_QRDMULH_LN : Op;
def OP_EQ    : Op;
def OP_GE    : Op;
def OP_LE    : Op;
def OP_GT    : Op;
def OP_LT    : Op;
def OP_NEG   : Op;
def OP_NOT   : Op;
def OP_AND   : Op;
def OP_OR    : Op;
def OP_XOR   : Op;
def OP_ANDN  : Op;
def OP_ORN   : Op;
def OP_CAST  : Op;
def OP_HI    : Op;
def OP_LO    : Op;
def OP_CONC  : Op;
def OP_DUP   : Op;
def OP_DUP_LN: Op;
def OP_SEL   : Op;
def OP_REV64 : Op;
def OP_REV32 : Op;
def OP_REV16 : Op;
def OP_REINT : Op;
def OP_ABDL  : Op;
def OP_ABA   : Op;
def OP_ABAL  : Op;
def OP_QDMLAL : Op;
def OP_QDMLSL : Op;
def OP_QDMLSL_N : Op;
def OP_QDMLAL_N : Op;
def OP_SHL : Op;
def OP_SHR : Op;

class Inst <string n, string p, string t, Op o> {
  string Name = n;
  string Prototype = p;
  string Types = t;
  Op Operand = o;
  bit isShift = 0;
  bit isVCVT_N = 0;

  // Certain intrinsics have different names than their representative
  // instructions. This field allows us to handle this correctly when we
  // are generating tests.
  string InstName = "";

  // Certain intrinsics even though they are not a WOpInst or LOpInst,
  // generate a WOpInst/LOpInst instruction (see below for definition
  // of a WOpInst/LOpInst). For testing purposes we need to know
  // this. Ex: vset_lane which outputs vmov instructions.
  bit isHiddenWInst = 0;
  bit isHiddenLInst = 0;
}

// The following instruction classes are implemented via builtins.
// These declarations are used to generate Builtins.def:
//
// SInst: Instruction with signed/unsigned suffix (e.g., "s8", "u8", "p8")
// IInst: Instruction with generic integer suffix (e.g., "i8")
// WInst: Instruction with only bit size suffix (e.g., "8")
class SInst<string n, string p, string t> : Inst<n, p, t, OP_NONE> {}
class IInst<string n, string p, string t> : Inst<n, p, t, OP_NONE> {}
class WInst<string n, string p, string t> : Inst<n, p, t, OP_NONE> {}

// The following instruction classes are implemented via operators
// instead of builtins. As such these declarations are only used for
// the purpose of generating tests.
//
// SOpInst:       Instruction with signed/unsigned suffix (e.g., "s8",
//                "u8", "p8").
// IOpInst:       Instruction with generic integer suffix (e.g., "i8").
// WOpInst:       Instruction with bit size only suffix (e.g., "8").
// LOpInst:       Logical instruction with no bit size suffix.
// NoTestOpInst:  Intrinsic that has no corresponding instruction.
class SOpInst<string n, string p, string t, Op o> : Inst<n, p, t, o> {}
class IOpInst<string n, string p, string t, Op o> : Inst<n, p, t, o> {}
class WOpInst<string n, string p, string t, Op o> : Inst<n, p, t, o> {}
class LOpInst<string n, string p, string t, Op o> : Inst<n, p, t, o> {}
class NoTestOpInst<string n, string p, string t, Op o> : Inst<n, p, t, o> {}

// prototype: return (arg, arg, ...)
// v: void
// t: best-fit integer (int/poly args)
// x: signed integer   (int/float args)
// u: unsigned integer (int/float args)
// f: float (int args)
// d: default
// g: default, ignore 'Q' size modifier.
// w: double width elements, same num elts
// n: double width elements, half num elts
// h: half width elements, double num elts
// e: half width elements, double num elts, unsigned
// b: half width elements, same num elts
// i: constant int
// l: constant uint64
// s: scalar of element type
// a: scalar of element type (splat to vector type)
// m: scalar of float type (same size as int arg elements)
// o: scalar of signed int type (same size as float arg elements)
// r: scalar of unsigned int type (same size as float arg elements)
// q: scalar of half element type width
// z: scalar of double element type width
// k: default elt width, double num elts
// j: default elt width, half num elts
// #: array of default vectors
// p: pointer type
// c: const pointer type

// sizes:
// c: char
// s: short
// i: int
// l: long
// d: double
// f: float
// h: half-float

// size modifiers:
// U: unsigned
// Q: 128b
// R: 128b without 'q' suffix
// P: polynomial

////////////////////////////////////////////////////////////////////////////////
// Addition
def VADD : Inst<"vadd", "ddd", "csilfUcUsUiUlQcQsQiQlQfQdQUcQUsQUiQUl", OP_ADD>;
def VADDL   : Inst<"vaddl", "wdd", "csiUcUsUi", OP_ADDL>;
def VADDW   : Inst<"vaddw", "wwd", "csiUcUsUi", OP_ADDW>;
def VHADD   : SInst<"vhadd", "ddd", "csiUcUsUiQcQsQiQUcQUsQUi">;
def VRHADD  : SInst<"vrhadd", "ddd", "csiUcUsUiQcQsQiQUcQUsQUi">;
def VQADD   : SInst<"vqadd", "ddd", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl">;
def VUSQADD : IInst<"vusqadd", "ddd", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl">;
def VADDHN  : IInst<"vaddhn", "hkk", "silUsUiUl">;
def VRADDHN : IInst<"vraddhn", "hkk", "silUsUiUl">;
def VADDL_HIGH : Inst<"vaddl_high", "ndd", "HcHsHiHUcHUsHUi", OP_ADDL_HIGH>;
def VADDW_HIGH : SInst<"vaddw_high", "ndd", "HcHsHiHUcHUsHUi">;
def VADDHN_HIGH  : SInst<"vaddhn_high",  "hbdd", "HsHiHlHUsHUiHUl">;
def VRADDHN_HIGH : SInst<"vraddhn_high", "hbdd", "HsHiHlHUsHUiHUl">;

////////////////////////////////////////////////////////////////////////////////
// Multiplication
def VMUL     : Inst<"vmul", "ddd", "csifUcUsUiQcQsQiQfQdQUcQUsQUi", OP_MUL>;
def VMULP    : SInst<"vmul", "ddd", "PcQPc">;
def VMLA     : Inst<"vmla", "dddd", "csifUcUsUiQcQsQiQfQdQUcQUsQUi", OP_MLA>;
def VFMA     : IInst<"vfma", "dddd", "fQfQd">;
def VMLAL    : Inst<"vmlal", "wwdd", "csiUcUsUi", OP_MLAL>;
def VMLS     : Inst<"vmls", "dddd", "csifUcUsUiQcQsQiQfQdQUcQUsQUi", OP_MLS>;
def VFMS     : IInst<"vfms", "dddd", "fQfQd">;
def VMLSL    : Inst<"vmlsl", "wwdd", "csiUcUsUi", OP_MLSL>;
def VQDMULH  : SInst<"vqdmulh", "ddd", "siQsQi">;
def VQRDMULH : SInst<"vqrdmulh", "ddd", "siQsQi">;
def VQDMLAL  : Inst<"vqdmlal", "wwdd", "si", OP_QDMLAL>;
def VQDMLSL  : Inst<"vqdmlsl", "wwdd", "si", OP_QDMLSL>;
def VMULL    : SInst<"vmull", "wdd", "csiUcUsUiPc">;
def VQDMULL  : SInst<"vqdmull", "wdd", "si">;
def VMLAL_HIGH : Inst<"vmlal_high", "nndd", "HcHsHiHUcHUsHUi", OP_MLAL_HIGH>;
def VMLSL_HIGH : Inst<"vmlsl_high", "nndd", "HcHsHiHUcHUsHUi", OP_MLSL_HIGH>;
def VMULL_HIGH : Inst<"vmull_high", "ndd",  "HcHsHiHUcHUsHUi", OP_MULL_HIGH>;
def VQDMLAL_HIGH : SInst<"vqdmlal_high", "nndd", "HsHi">;
def VQDMLSL_HIGH : SInst<"vqdmlsl_high", "nndd", "HsHi">;
def VQDMULL_HIGH : SInst<"vqdmull_high", "ndd",  "HsHi">;
// scalars.
def VQDMULLH : SInst<"vqdmullh", "zss", "s">;
def VQDMULLS : SInst<"vqdmulls", "zss", "i">;
def VQDMULLH_LANE : SInst<"vqdmullh_lane", "zski", "s">;
def VQDMULLS_LANE : SInst<"vqdmulls_lane", "zski", "i">;
def VQDMLALH : Inst<"vqdmlalh", "zzss", "s", OP_QDMLAL>;
def VQDMLALS : Inst<"vqdmlals", "zzss", "i", OP_QDMLAL>;
def VQDMLSLH : Inst<"vqdmlslh", "zzss", "s", OP_QDMLSL>;
def VQDMLSLS : Inst<"vqdmlsls", "zzss", "i", OP_QDMLSL>;
def VQDMLALH_LANE : Inst<"vqdmlalh_lane", "zzski", "s", OP_QDMLAL_LN>;
def VQDMLALS_LANE : Inst<"vqdmlals_lane", "zzski", "i", OP_QDMLAL_LN>;
def VQDMLSLH_LANE : Inst<"vqdmlslh_lane", "zzski", "s", OP_QDMLSL_LN>;
def VQDMLSLS_LANE : Inst<"vqdmlsls_lane", "zzski", "i", OP_QDMLSL_LN>;

////////////////////////////////////////////////////////////////////////////////
// Miscellaneous FP
def VDIV  : SInst<"vdiv",  "ddd", "fQfQd">;
def VMULX : SInst<"vmulx", "ddd", "fQfQd">;
def VMULXS : SInst<"vmulxs", "sss", "f">;
def VMULXD : SInst<"vmulxd", "sss", "d">;
def VMAXNMP : SInst<"vpmaxnm", "ddd", "fQfQd">;
def VMINNMP : SInst<"vpminnm", "ddd", "fQfQd">;
def VRECPSS : SInst<"vrecpss", "sss", "f">;
def VRECPSD : SInst<"vrecpsd", "sss", "d">;
def VSQRT : SInst<"vsqrt", "dd", "fQfQd">;

////////////////////////////////////////////////////////////////////////////////
// Subtraction
def VSUB : Inst<"vsub", "ddd", "csilfUcUsUiUlQcQsQiQlQfQdQUcQUsQUiQUl", OP_SUB>;
def VSUBL   : Inst<"vsubl", "wdd", "csiUcUsUi", OP_SUBL>;
def VSUBW   : Inst<"vsubw", "wwd", "csiUcUsUi", OP_SUBW>;
def VQSUB   : SInst<"vqsub", "ddd", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl">;
def VHSUB   : SInst<"vhsub", "ddd", "csiUcUsUiQcQsQiQUcQUsQUi">;
def VSUBHN  : IInst<"vsubhn", "hkk", "silUsUiUl">;
def VRSUBHN : IInst<"vrsubhn", "hkk", "silUsUiUl">;
def VSUBL_HIGH   : SInst<"vsubl_high",   "ndd", "HcHsHiHUcHUsHUi">;
def VSUBW_HIGH   : SInst<"vsubw_high",   "ndd", "HcHsHiHUcHUsHUi">;
def VRSUBHN_HIGH : SInst<"vrsubhn_high", "hbdd", "HsHiHlHUsHUiHUl">;
def VSUBHN_HIGH  : SInst<"vsubhn_high",   "hbdd", "HsHiHlHUsHUiHUl">;
// Scalar operands.
def VSUBD : Inst<"vsubd", "sss", "lUl", OP_SUB>;
def VADDD : Inst<"vaddd", "sss", "lUl", OP_ADD>;
def VQADDB : SInst<"vqaddb", "sss", "cUc">;
def VQADDH : SInst<"vqaddh", "sss", "sUs">;
def VQADDS : SInst<"vqadds", "sss", "iUi">;
def VQADDD : SInst<"vqaddd", "sss", "lUl">;
def VQSUBB : SInst<"vqsubb", "sss", "cUc">;
def VQSUBH : SInst<"vqsubh", "sss", "sUs">;
def VQSUBS : SInst<"vqsubs", "sss", "iUi">;
def VQSUBD : SInst<"vqsubd", "sss", "lUl">;

////////////////////////////////////////////////////////////////////////////////
// Comparison
def VCEQ  : Inst<"vceq", "udd", "csifUcUsUiPcQcQsQiQlQfQdQUcQUsQUiQUlQPc", OP_EQ>;
def VCGE  : Inst<"vcge", "udd", "csifUcUsUiQcQsQiQlQfQdQUcQUsQUiQUl", OP_GE>;
def VCLE  : Inst<"vcle", "udd", "csifUcUsUiQcQsQiQlQfQdQUcQUsQUiQUl", OP_LE>;
def VCGT  : Inst<"vcgt", "udd", "csifUcUsUiQcQsQiQlQfQdQUcQUsQUiQUl", OP_GT>;
def VCLT  : Inst<"vclt", "udd", "csifUcUsUiQcQsQiQlQfQdQUcQUsQUiQUl", OP_LT>;
def VCAGE : IInst<"vcage", "udd", "fQfQd">;
def VCALE : IInst<"vcale", "udd", "fQfQd">;
def VCAGT : IInst<"vcagt", "udd", "fQfQd">;
def VCALT : IInst<"vcalt", "udd", "fQfQd">;
def VTST  : WInst<"vtst", "udd", "csiUcUsUiPcQcQsQiQlQUcQUsQUiQUlQPc">;
// Scalar
def VCEQS : Inst<"vceqs", "sss", "f", OP_EQ>;
def VCEQD : Inst<"vceqd", "sss", "lUlQUld", OP_EQ>;
def VCLED : Inst<"vcled", "sss", "lUl", OP_LE>;
def VCGTD : Inst<"vcgtd", "sss", "lUl", OP_GT>;
def VTSTD : SInst<"vtstd", "sss", "lUl">;
// Scalar Compare to Zero.
def VCEQZD : SInst<"vceqzd", "ss", "lUl">;
def VCGEZD : SInst<"vcgezd", "ss", "lUl">;
def VCLEZD : SInst<"vclezd", "ss", "lUl">;
def VCGTZD : SInst<"vcgtzd", "ss", "l">;
def VCLTZD : SInst<"vcltzd", "ss", "l">;

////////////////////////////////////////////////////////////////////////////////
// Absolute Difference
def VABD  : SInst<"vabd", "ddd",  "csiUcUsUifQcQsQiQUcQUsQUiQfQd">;
def VABDL : IInst<"vabdl", "wdd",  "csiUcUsUi">;
def VABA  : Inst<"vaba", "dddd", "csiUcUsUiQcQsQiQUcQUsQUi", OP_ABA>;
def VABAL : SInst<"vabal", "wwdd", "csiUcUsUi">;
def VABAL_HIGH : SInst<"vabal_high", "nndd", "HcHsHiHUcHUsHUi">;
def VABDL_HIGH : SInst<"vabdl_high", "ndd",  "HcHsHiHUcHUsHUi">;
def VABDS : SInst<"vabds", "sss", "f">;
def VABDD : SInst<"vabdd", "sss", "d">;

////////////////////////////////////////////////////////////////////////////////
// Max/Min
def VMAX : SInst<"vmax", "ddd", "csiUcUsUifQcQsQiQUcQUsQUiQfQd">;
def VMIN : SInst<"vmin", "ddd", "csiUcUsUifQcQsQiQUcQUsQUiQfQd">;

////////////////////////////////////////////////////////////////////////////////
// Max/Min Number
def VMAXNM : SInst<"vmaxnm", "ddd", "fQfQd">;
def VMINNM : SInst<"vminnm", "ddd", "fQfQd">;

////////////////////////////////////////////////////////////////////////////////
// Pairwise Addition
def VPADD  : IInst<"vpadd", "ddd", "csiUcUsUifQcQsQiQUcQUsQUiQlQUlQfQd">;
def VPADDL : SInst<"vpaddl", "nd",  "csiUcUsUiQcQsQiQUcQUsQUi">;
def VPADAL : SInst<"vpadal", "nnd", "csiUcUsUiQcQsQiQUcQUsQUi">;
def VPADDS : SInst<"vpadds", "sd", "f">;
def VPADDD : SInst<"vpaddd", "sd", "HlHd">;

////////////////////////////////////////////////////////////////////////////////
// Folding Max/Min
def VPMAX : SInst<"vpmax", "ddd", "csiUcUsUifQcQsQiQUcQUsQUiQfQd">;
def VPMIN : SInst<"vpmin", "ddd", "csiUcUsUifQcQsQiQUcQUsQUiQfQd">;

////////////////////////////////////////////////////////////////////////////////
// Reciprocal/Sqrt
def VRECPS  : IInst<"vrecps", "ddd", "fQfQd">;
def VRSQRTS : IInst<"vrsqrts", "ddd", "fQfQd">;
// Scalar.
def VRSQRTSS : SInst<"vrsqrtss", "sss", "f">;
def VRSQRTSD : SInst<"vrsqrtsd", "sss", "d">;

////////////////////////////////////////////////////////////////////////////////
// Shifts by signed variable
def VSHL   : SInst<"vshl", "ddx", "csiUcUsUiQcQsQiQlQUcQUsQUiQUl">;
def VQSHL  : SInst<"vqshl", "ddx", "csiUcUsUiQcQsQiQlQUcQUsQUiQUl">;
def VRSHL  : SInst<"vrshl", "ddx", "csiUcUsUiQcQsQiQlQUcQUsQUiQUl">;
def VQRSHL : SInst<"vqrshl", "ddx", "csiUcUsUiQcQsQiQlQUcQUsQUiQUl">;

// Scalar versions.
def VSHLD  : SInst<"vshld", "sss", "HlHUl">;
def VQSHLB : SInst<"vqshlb", "sss", "HcHUc">;
def VQSHLH : SInst<"vqshlh", "sss", "HsHUs">;
def VQSHLS : SInst<"vqshls", "sss", "HiHUi">;
def VQSHLD : SInst<"vqshld", "sss", "HlHUl">;

def VRSHLD :  SInst<"vrshld", "sss", "HlHUl">;
def VQRSHLB : SInst<"vqrshlb", "sss", "HcHUc">;
def VQRSHLH : SInst<"vqrshlh", "sss", "HsHUs">;
def VQRSHLS : SInst<"vqrshls", "sss", "HiHUi">;
def VQRSHLD : SInst<"vqrshld", "sss", "HlHUl">;
// Aliases w/o the size suffix.
def VSHL_   : SInst<"vshl", "sss", "HlHUl">;
def VRSHL_  : SInst<"vrshl", "sss", "HlHUl">;
def VQSHL_  : SInst<"vqshl", "sss", "HlHUl">;
def VQRSHL_ : SInst<"vqrshl", "sss", "HlHUl">;

////////////////////////////////////////////////////////////////////////////////
// Shifts by constant
let isShift = 1 in {
def VSHR_N     : SInst<"vshr_n", "ddi", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl">;
def VSHL_N     : IInst<"vshl_n", "ddi", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl">;
def VRSHR_N    : SInst<"vrshr_n", "ddi", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl">;
def VSRA_N     : SInst<"vsra_n", "dddi", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl">;
def VRSRA_N    : SInst<"vrsra_n", "dddi", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl">;
def VQSHL_N    : SInst<"vqshl_n", "ddi", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl">;
def VQSHLU_N   : SInst<"vqshlu_n", "udi", "csilQcQsQiQl">;
def VSHRN_N    : IInst<"vshrn_n", "hki", "silUsUiUl">;
def VQSHRUN_N  : SInst<"vqshrun_n", "eki", "sil">;
def VQRSHRUN_N : SInst<"vqrshrun_n", "eki", "sil">;
def VQSHRN_N   : SInst<"vqshrn_n", "hki", "silUsUiUl">;
def VRSHRN_N   : IInst<"vrshrn_n", "hki", "silUsUiUl">;
def VQRSHRN_N  : SInst<"vqrshrn_n", "hki", "silUsUiUl">;
def VSHLL_N    : SInst<"vshll_n", "wdi", "csiUcUsUi">;
def VRSHRN_HIGH_N   : SInst<"vrshrn_high_n", "hbki", "HsHiHlHUsHUiHUl">;
def VSHRN_HIGH_N    : SInst<"vshrn_high_n", "hbki", "HsHiHlHUsHUiHUl">;
def VQSHRUN_HIGH_N  : SInst<"vqshrun_high_n", "hbki", "HsHiHl">;
def VQRSHRUN_HIGH_N : SInst<"vqrshrun_high_n", "hbki", "HsHiHl">;
def VQSHRN_HIGH_N   : SInst<"vqshrn_high_n", "hbki", "HsHiHlHUsHUiHUl">;
def VQRSHRN_HIGH_N  : SInst<"vqrshrn_high_n", "hbki", "HsHiHlHUsHUiHUl">;
def VSHLL_HIGH_N : SInst<"vshll_high_n", "ndi", "HcHsHiHUcHUsHUi">;

////////////////////////////////////////////////////////////////////////////////
// Shifts with insert
def VSRI_N : WInst<"vsri_n", "dddi",
                   "csilUcUsUiUlPcPsQcQsQiQlQUcQUsQUiQUlQPcQPs">;
def VSLI_N : WInst<"vsli_n", "dddi",
                   "csilUcUsUiUlPcPsQcQsQiQlQUcQUsQUiQUlQPcQPs">;
}

////////////////////////////////////////////////////////////////////////////////
// Vector FP->Int Conversions

// F32->Integer
def VCVT_S32     : SInst<"vcvt_s32", "xd",  "fQf">;
def VCVT_U32     : SInst<"vcvt_u32", "ud",  "fQf">;
// Integer->F32
def VCVT_F32     : SInst<"vcvt_f32", "fd", "iUiQiQUi">;
// F64->Integer
def VCVT_S64     : SInst<"vcvt_s64", "xd",  "Qd">;
def VCVT_U64     : SInst<"vcvt_u64", "ud",  "Qd">;
// Integer->F64
def VCVT_F64     : SInst<"vcvtq_f64", "fd", "HlHUl">;


////////////////////////////////////////////////////////////////////////////////
// Vector FP->FP Conversions
// F16->F32
def VCVT_F32_F16 : SInst<"vcvt_f32_f16", "fd", "h">;
// F32->F16
def VCVT_F16     : SInst<"vcvt_f16", "hk",  "f">;
// F32->F64
def VCVT_F64_F32 : SInst<"vcvt_f64_f32", "wd", "f">;
// F32 high ->F64
def VCVT_HIGH_F64_F32 : SInst<"vcvt_high_f64_f32", "wk", "f">;
// F64->F32
def VCVT_F32_F64 : SInst<"vcvt_f32_f64", "bd", "Qd">;
// F64-> high F32
def VCVT_HIGH_F32_F64 : SInst<"vcvt_high_f32_f64", "hbd", "Qd">;

////////////////////////////////////////////////////////////////////////////////
// Vector FP->FP Inexact Narrowing Conversions
// F64->F32 inexact
def VCVTX_F32_F64 : SInst<"vcvtx_f32_f64", "bd", "Qd">;
// F64-> high F32 inexact
def VCVTX_HIGH_F32_F64 : SInst<"vcvtx_high_f32_f64", "hbd", "Qd">;

////////////////////////////////////////////////////////////////////////////////
// Vector FP->Fixed Point Conversions
let isVCVT_N = 1 in {
// f32 -> signed-fixed-point
def VCVT_N_S32   : SInst<"vcvt_n_s32", "xdi", "fQf">;
// f32 -> unsigned-fixed-point
def VCVT_N_U32   : SInst<"vcvt_n_u32", "udi", "fQf">;
// fixed point -> f32
def VCVT_N_F32   : SInst<"vcvt_n_f32", "fdi", "iUiQiQUi">;
// f64 -> signed-fixed-point
def VCVT_N_S64   : SInst<"vcvt_n_s64", "xdi", "Qd">;
// f64 -> unsigned-fixed-point
def VCVT_N_U64   : SInst<"vcvt_n_u64", "udi", "Qd">;
// fixed point -> f64
def VCVT_N_F64   : SInst<"vcvtq_n_f64", "fdi", "HlHUl">;
}
// Scalar int<-->float conversions.
// NOTE: These are not marked isVCVT_N, as that forces overloaded types for
// the intrinsic, which we don't want here as it becomes very difficult to
// then distinguish between the signed an unsigned variants.
def VCVTS_N_F32  : SInst<"vcvts_n_f32", "msi", "iUi">;
def VCVTD_N_F64  : SInst<"vcvtd_n_f64", "msi", "lUl">;
def VCVTS_N_S32  : SInst<"vcvts_n_s32", "osi", "f">;
def VCVTS_N_U32  : SInst<"vcvts_n_u32", "rsi", "f">;
def VCVTD_N_S64  : SInst<"vcvtd_n_s64", "osi", "d">;
def VCVTD_N_U64  : SInst<"vcvtd_n_u64", "rsi", "d">;

// f32/f64 -> integer w/ rounding mode.
def VCVTA_S32 : SInst<"vcvta_s32", "xd", "fQf">;
def VCVTA_U32 : SInst<"vcvta_u32", "ud", "fQf">;
def VCVTA_S64 : SInst<"vcvta_s64", "xd", "Qd">;
def VCVTA_U64 : SInst<"vcvta_u64", "ud", "Qd">;
def VCVTM_S32 : SInst<"vcvtm_s32", "xd", "fQf">;
def VCVTM_U32 : SInst<"vcvtm_u32", "ud", "fQf">;
def VCVTM_S64 : SInst<"vcvtm_s64", "xd", "Qd">;
def VCVTM_U64 : SInst<"vcvtm_u64", "ud", "Qd">;
def VCVTN_S32 : SInst<"vcvtn_s32", "xd", "fQf">;
def VCVTN_U32 : SInst<"vcvtn_u32", "ud", "fQf">;
def VCVTN_S64 : SInst<"vcvtn_s64", "xd", "Qd">;
def VCVTN_U64 : SInst<"vcvtn_u64", "ud", "Qd">;
def VCVTP_S32 : SInst<"vcvtp_s32", "xd", "fQf">;
def VCVTP_U32 : SInst<"vcvtp_u32", "ud", "fQf">;
def VCVTP_S64 : SInst<"vcvtp_s64", "xd", "Qd">;
def VCVTP_U64 : SInst<"vcvtp_u64", "ud", "Qd">;
def VCVTZ_S32 : SInst<"vcvtz_s32", "xd", "fQf">;
def VCVTZ_U32 : SInst<"vcvtz_u32", "ud", "fQf">;
def VCVTZ_S64 : SInst<"vcvtz_s64", "xd", "Qd">;
def VCVTZ_U64 : SInst<"vcvtz_u64", "ud", "Qd">;

// Narrowing inexact
def VCVTXD : SInst<"vcvtxd_f32", "qs", "d">;

// Scalar int<-->float conversions.
def VCVTS_F32 : SInst<"vcvts_f32", "ms", "iUi">;
def VCVTD_F64 : SInst<"vcvtd_f64", "ms", "lUl">;
def VCVTS_S32 : SInst<"vcvts_s32", "os", "f">;
def VCVTS_U32 : SInst<"vcvts_u32", "rs", "f">;
def VCVTD_S64 : SInst<"vcvtd_s64", "os", "d">;
def VCVTD_U64 : SInst<"vcvtd_u64", "rs", "d">;

// Scalar f32/f64 -> integer w/ rounding mode.
def VCVTAS_S32 : SInst<"vcvtas_s32", "os", "f">;
def VCVTAS_U32 : SInst<"vcvtas_u32", "rs", "f">;
def VCVTAD_S64 : SInst<"vcvtad_s64", "os", "d">;
def VCVTAD_U64 : SInst<"vcvtad_u64", "rs", "d">;
def VCVTMS_S32 : SInst<"vcvtms_s32", "os", "f">;
def VCVTMS_U32 : SInst<"vcvtms_u32", "rs", "f">;
def VCVTMD_S64 : SInst<"vcvtmd_s64", "os", "d">;
def VCVTMD_U64 : SInst<"vcvtmd_u64", "rs", "d">;
def VCVTNS_S32 : SInst<"vcvtns_s32", "os", "f">;
def VCVTNS_U32 : SInst<"vcvtns_u32", "rs", "f">;
def VCVTND_S64 : SInst<"vcvtnd_s64", "os", "d">;
def VCVTND_U64 : SInst<"vcvtnd_u64", "rs", "d">;
def VCVTPS_S32 : SInst<"vcvtps_s32", "os", "f">;
def VCVTPS_U32 : SInst<"vcvtps_u32", "rs", "f">;
def VCVTPD_S64 : SInst<"vcvtpd_s64", "os", "d">;
def VCVTPD_U64 : SInst<"vcvtpd_u64", "rs", "d">;
def VCVTZS_S32 : SInst<"vcvtzs_s32", "os", "f">;
def VCVTZS_U32 : SInst<"vcvtzs_u32", "rs", "f">;
def VCVTZD_S64 : SInst<"vcvtzd_s64", "os", "d">;
def VCVTZD_U64 : SInst<"vcvtzd_u64", "rs", "d">;


// vrndz and vrndqz aren't in the official ARM docs, but Numerics has
// requested we keep them for orthogonality. rdar://12057336
// vrnd_* and vrndq_* are aliases for vrndz_* and vrndqz_*.
def VRND_A : SInst<"vrnda", "dd", "fQfQd">;
def VRND_M : SInst<"vrndm", "dd", "fQfQd">;
def VRND_N : SInst<"vrndn", "dd", "fQfQd">;
def VRND_P : SInst<"vrndp", "dd", "fQfQd">;
def VRND_X : SInst<"vrndx", "dd", "fQfQd">;
def VRND_Z : SInst<"vrndz", "dd", "fQfQd">;
def VRND   : SInst<"vrnd", "dd", "fQfQd">;

////////////////////////////////////////////////////////////////////////////////
// Loads and stores of a single vector
def VLD1      : WInst<"vld1", "dc",
                      "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VLD1_LANE : WInst<"vld1_lane", "dcdi",
                      "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VLD1_DUP  : WInst<"vld1_dup", "dc",
                      "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VST1      : WInst<"vst1", "vpd",
                      "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhdfPcPs">;
def VST1_LANE : WInst<"vst1_lane", "vpdi",
                      "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfPcPs">;

////////////////////////////////////////////////////////////////////////////////
// Loads and stores of an N-element structure
def VLD2 : WInst<"vld2", "2c", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VLD3 : WInst<"vld3", "3c", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VLD4 : WInst<"vld4", "4c", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VLD2_DUP  : WInst<"vld2_dup", "2c", "UcUsUiUlcsilhfdPcPs">;
def VLD3_DUP  : WInst<"vld3_dup", "3c", "UcUsUiUlcsilhfdPcPs">;
def VLD4_DUP  : WInst<"vld4_dup", "4c", "UcUsUiUlcsilhfdPcPs">;
def VLD2_LANE : WInst<"vld2_lane", "2c2i", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VLD3_LANE : WInst<"vld3_lane", "3c3i", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VLD4_LANE : WInst<"vld4_lane", "4c4i", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VST2 : WInst<"vst2", "vp2", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VST3 : WInst<"vst3", "vp3", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VST4 : WInst<"vst4", "vp4", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUiUlcsilhfdPcPs">;
def VST2_LANE : WInst<"vst2_lane", "vp2i", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUicsihfdPcPs">;
def VST3_LANE : WInst<"vst3_lane", "vp3i", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUicsihfdPcPs">;
def VST4_LANE : WInst<"vst4_lane", "vp4i", "QUcQUsQUiQUlQcQsQiQlQhQfQdQPcQPsUcUsUicsihfdPcPs">;

////////////////////////////////////////////////////////////////////////////////
// Extract lanes from a vector
def VGET_LANE : SInst<"vget_lane", "sdi",
                      "csilUcUsUiUlfdPcPsQcQsQiQlQUcQUsQUiQUlQfQdQPcQPs">;
// vdup<bhsd>_lane* is a synonym for vget_lane.
def VDUPB_LANE : SInst<"vdupb_lane", "sdi", "HcHUc">;
def VDUPH_LANE : SInst<"vduph_lane", "sdi", "HsHUs">;
def VDUPS_LANE : SInst<"vdups_lane", "sdi", "HiHUiHf">;
def VDUPD_LANE : SInst<"vdupd_lane", "sdi", "HlHUlHd">;
def VDUP_LANE_F64 : SInst<"vdup_lane", "sdi", "Hd">;

////////////////////////////////////////////////////////////////////////////////
// Set lanes within a vector
def VSET_LANE : IInst<"vset_lane", "dsdi",
                      "UcUsUicsiPcPshfdQUcQUsQUiQcQsQiQPcQPsQhQfQdlUlQlQUl">;

////////////////////////////////////////////////////////////////////////////////
// Initialize a vector from bit pattern
def VCREATE: Inst<"vcreate", "dl", "csihfUcUsUiUlPcPsl", OP_CAST>;

////////////////////////////////////////////////////////////////////////////////
// Set all lanes to same value
def VDUP_N : Inst<"vdup_n", "ds",
                  "UcUsUicsidPcPshfQUcQUsQUiQcQsQiQPcQPsQhQfQdlUlQlQUl", OP_DUP>;
def VMOV_N : Inst<"vmov_n", "ds",
                  "UcUsUicsiPcPshfQUcQUsQUiQcQsQiQhQPcQPsQfQdlUlQlQUl", OP_DUP>;
def VDUP_LANE : Inst<"vdup_lane", "dgi",
                  "UcUsUiUlcsilPcPsfQUcQUsQUiQcQsQiQPcQPsQfQdQlQUl",OP_DUP_LN>;

////////////////////////////////////////////////////////////////////////////////
// Copy one element of one vector to an element of another
def VCOPY_LANE : SInst<"vcopy_lane", "ddidi", "QcQsQiQlQfQdQUcQUsQUiQUl">;

////////////////////////////////////////////////////////////////////////////////
// Combining vectors
def VCOMBINE : Inst<"vcombine", "kdd", "csilhfdUcUsUiUlPcPs", OP_CONC>;

////////////////////////////////////////////////////////////////////////////////
// Splitting vectors
def VGET_HIGH : Inst<"vget_high", "dk", "csilhfdUcUsUiUlPcPs", OP_HI>;
def VGET_LOW  : Inst<"vget_low", "dk", "csilhfdUcUsUiUlPcPs", OP_LO>;

////////////////////////////////////////////////////////////////////////////////
// Converting vectors
def VMOVN        : IInst<"vmovn", "hk",  "silUsUiUl">;
def VMOVL        : SInst<"vmovl", "wd",  "csiUcUsUi">;
def VQMOVN       : SInst<"vqmovn", "hk",  "silUsUiUl">;
def VQMOVUN      : SInst<"vqmovun", "ek",  "sil">;
def VQMOVN_HIGH   : SInst<"vqmovn_high",  "hbd", "HsHiHlHUsHUiHUl">;
def VQMOVUN_HIGH  : SInst<"vqmovun_high", "hbd", "HsHiHl">;
def VMOVN_HIGH   : SInst<"vmovn_high",  "hbd", "HsHiHlHUsHUiHUl">;
def VMOVL_HIGH   : Inst<"vmovl_high",  "nd", "HcHsHiHUcHUsHUi", OP_MOVL_HIGH>;
def VXTL_HIGH    : Inst<"vxtl_high",   "nd", "HcHsHiHUcHUsHUi", OP_MOVL_HIGH>;
// Scalar operands.
def VQMOVUNH : SInst<"vqmovunh", "qs", "s">;
def VQMOVUNS : SInst<"vqmovuns", "qs", "i">;
def VQMOVUND : SInst<"vqmovund", "qs", "l">;
def VQMOVNH  : SInst<"vqmovnh",  "qs", "sUs">;
def VQMOVNS  : SInst<"vqmovns",  "qs", "iUi">;
def VQMOVND  : SInst<"vqmovnd",  "qs", "lUl">;

////////////////////////////////////////////////////////////////////////////////
// Table lookup, Extended table lookup
def VTBL1  : Inst<"vtbl1", "ddd",  "UccPc", OP_UNAVAILABLE>;
def VTBL2  : Inst<"vtbl2", "d2d",  "UccPc", OP_UNAVAILABLE>;
def VTBL3  : Inst<"vtbl3", "d3d",  "UccPc", OP_UNAVAILABLE>;
def VTBL4  : Inst<"vtbl4", "d4d",  "UccPc", OP_UNAVAILABLE>;
def VTBL1Q : WInst<"vtbl1", "gdg",  "QUcQcQPc">;
def VTBL2Q : WInst<"vtbl2", "g2g",  "QUcQcQPc">;
def VTBL3Q : WInst<"vtbl3", "g3g",  "QUcQcQPc">;
def VTBL4Q : WInst<"vtbl4", "g4g",  "QUcQcQPc">;
def VQTBL1 : WInst<"vqtbl1", "ddd", "QUcQcQPc">;
def VQTBL2 : WInst<"vqtbl2", "d2d", "QUcQcQPc">;
def VQTBL3 : WInst<"vqtbl3", "d3d", "QUcQcQPc">;
def VQTBL4 : WInst<"vqtbl4", "d4d", "QUcQcQPc">;

def VTBX1  : Inst<"vtbx1", "dd1d",  "UccPc", OP_UNAVAILABLE>;
def VTBX2  : Inst<"vtbx2", "dd2d",  "UccPc", OP_UNAVAILABLE>;
def VTBX3  : Inst<"vtbx3", "dd3d",  "UccPc", OP_UNAVAILABLE>;
def VTBX4  : Inst<"vtbx4", "dd4d",  "UccPc", OP_UNAVAILABLE>;

def VTBX1Q  : WInst<"vtbx1",  "gg1g", "QUcQcQPc">;
def VTBX2Q  : WInst<"vtbx2",  "gg2g", "QUcQcQPc">;
def VTBX3Q  : WInst<"vtbx3",  "gg3g", "QUcQcQPc">;
def VTBX4Q  : WInst<"vtbx4",  "gg4g", "QUcQcQPc">;
def VQTBX1Q : WInst<"vqtbx1", "dddd", "QUcQcQPc">;
def VQTBX2Q : WInst<"vqtbx2", "dd2d", "QUcQcQPc">;
def VQTBX3Q : WInst<"vqtbx3", "dd3d", "QUcQcQPc">;
def VQTBX4Q : WInst<"vqtbx4", "dd4d", "QUcQcQPc">;

////////////////////////////////////////////////////////////////////////////////
// Vector Shuffles
def VTRN1 : WInst<"vtrn1", "ddd", "csiUcUsUifQcQsQiQlQUcQUsQUiQUlQfQd">;
def VTRN2 : WInst<"vtrn2", "ddd", "csiUcUsUifQcQsQiQlQUcQUsQUiQUlQfQd">;
def VZIP1 : WInst<"vzip1", "ddd", "csiUcUsUifQcQsQiQlQUcQUsQUiQUlQfQd">;
def VZIP2 : WInst<"vzip2", "ddd", "csiUcUsUifQcQsQiQlQUcQUsQUiQUlQfQd">;
def VUZP1 : WInst<"vuzp1", "ddd", "csiUcUsUifQcQsQiQlQUcQUsQUiQUlQfQd">;
def VUZP2 : WInst<"vuzp2", "ddd", "csiUcUsUifQcQsQiQlQUcQUsQUiQUlQfQd">;

////////////////////////////////////////////////////////////////////////////////
// Operations with a scalar value
def VMLA_LANE    : Inst<"vmla_lane", "dddgi", "sifUsUiQsQiQfQUsQUi", OP_MLA_LN>;
def VMLA_LANEQ   : Inst<"vmla_laneq", "dddki", "siUsUiQsQiQUsQUi", OP_MLA_LN>;
def VFMA_LANE    : IInst<"vfma_lane", "ddddi", "fQfQd">;
def VMLAL_LANE   : Inst<"vmlal_lane", "wwdgi", "siUsUi", OP_MLAL_LN>;
def VMLAL_LANEQ  : Inst<"vmlal_laneq", "wwdki", "siUsUi", OP_MLAL_LN>;
def VMLAL_HIGH_LANE  : Inst<"vmlal_high_lane", "wwdgi", "HsHiHUsHUi", OP_MLAL_HIGH_LN>;
def VMLAL_HIGH_LANEQ : Inst<"vmlal_high_laneq", "wwddi", "HsHiHUsHUi", OP_MLAL_HIGH_LN>;
def VMLAL_HIGH_N  : Inst<"vmlal_high_n", "wwds", "HsHiHUsHUi", OP_MLAL_HIGH_N>;
def VQDMLAL_LANE : Inst<"vqdmlal_lane", "wwdgi", "si", OP_QDMLAL_LN>;
def VQDMLAL_LANEQ : Inst<"vqdmlal_laneq", "wwdki", "si", OP_QDMLAL_LN>;
def VQDMLAL_HIGH_LANE  : Inst<"vqdmlal_high_lane", "wwdgi", "HsHi", OP_QDMLAL_HIGH_LN>;
def VQDMLAL_HIGH_LANEQ : Inst<"vqdmlal_high_laneq", "wwddi", "HsHi", OP_QDMLAL_HIGH_LN>;
def VQDMLAL_HIGH_N     : Inst<"vqdmlal_high_n", "wwds", "HsHi", OP_QDMLAL_HIGH_N>;
def VMLS_LANE    : Inst<"vmls_lane", "dddgi", "sifUsUiQsQiQfQUsQUi", OP_MLS_LN>;
def VMLS_LANEQ   : Inst<"vmls_laneq", "dddki", "siUsUiQsQiQUsQUi", OP_MLS_LN>;
def VMLSL_HIGH_LANE  : Inst<"vmlsl_high_lane", "wwdgi", "HsHiHUsHUi", OP_MLSL_HIGH_LN>;
def VMLSL_HIGH_LANEQ : Inst<"vmlsl_high_laneq", "wwdki", "HsHiHUsHUi", OP_MLSL_HIGH_LN>;
def VMLSL_HIGH_N  : Inst<"vmlsl_high_n", "wwds", "HsHiHUsHUi", OP_MLSL_HIGH_N>;
def VFMS_LANE    : IInst<"vfms_lane", "ddddi", "fQf">;
def VMLSL_LANE   : Inst<"vmlsl_lane", "wwdgi", "siUsUi", OP_MLSL_LN>;
def VMLSL_LANEQ  : Inst<"vmlsl_laneq", "wwdki", "siUsUi", OP_MLSL_LN>;
def VQDMLSL_LANE : Inst<"vqdmlsl_lane", "wwdgi", "si", OP_QDMLSL_LN>;
def VQDMLSL_LANEQ : Inst<"vqdmlsl_laneq", "wwdki", "si", OP_QDMLSL_LN>;
def VQDMLSL_HIGH_LANE  : Inst<"vqdmlsl_high_lane", "wwdgi", "HsHi", OP_QDMLSL_HIGH_LN>;
def VQDMLSL_HIGH_LANEQ : Inst<"vqdmlsl_high_laneq", "wwdki", "HsHi", OP_QDMLSL_HIGH_LN>;
def VQDMLSL_HIGH_N     : Inst<"vqdmlsl_high_n", "wwds", "HsHi", OP_QDMLSL_HIGH_N>;
def VMUL_N       : Inst<"vmul_n", "dds", "sifUsUiQsQiQfQdQUsQUi", OP_MUL_N>;
def VMUL_LANE    : Inst<"vmul_lane", "ddgi", "sifUsUiQsQiQfQdQUsQUi", OP_MUL_LN>;
def VMUL_LANEQ   : Inst<"vmul_laneq", "ddki", "sifUsUiQsQiQfQdQUsQUi", OP_MUL_LN>;
def VMULL_N      : SInst<"vmull_n", "wda", "siUsUi">;
def VMULL_LANE   : Inst<"vmull_lane", "wdgi", "siUsUi", OP_MULL_LN>;
def VMULL_LANEQ  : Inst<"vmull_laneq", "wdki", "siUsUi", OP_MULL_LN>;
def VMULL_HIGH_N : Inst<"vmull_high_n", "wda", "HsHiHUsHUi", OP_MULL_HIGH_N>;
def VMULL_HIGH_LANE  : Inst<"vmull_high_lane", "wdgi", "HsHiHUsHUi", OP_MULL_HIGH_LN>;
def VMULL_HIGH_LANEQ : Inst<"vmull_high_laneq", "wdki", "HsHiHUsHUi", OP_MULL_HIGH_LN>;
def VQDMULL_N    : SInst<"vqdmull_n", "wda", "si">;
def VQDMULL_HIGH_N : Inst<"vqdmull_high_n", "wda", "HsHi", OP_QDMULL_HIGH_N>;
def VQDMULL_HIGH_LANE  : Inst<"vqdmull_high_lane", "wdgi", "HsHi", OP_QDMULL_HIGH_LN>;
def VQDMULL_HIGH_LANEQ : Inst<"vqdmull_high_laneq", "wdki", "HsHi", OP_QDMULL_HIGH_LN>;
def VQDMULL_LANE : Inst<"vqdmull_lane", "wdgi", "si", OP_QDMULL_LN>;
def VQDMULL_LANEQ : Inst<"vqdmull_laneq", "wdki", "si", OP_QDMULL_LN>;
def VQDMULH_N    : SInst<"vqdmulh_n", "dda", "siQsQi">;
def VQDMULH_LANE : Inst<"vqdmulh_lane", "ddgi", "siQsQi", OP_QDMULH_LN>;
def VQDMULH_LANEQ : Inst<"vqdmulh_laneq", "ddki", "siQsQi", OP_QDMULH_LN>;
def VQRDMULH_N   : SInst<"vqrdmulh_n", "dda", "siQsQi">;
def VQRDMULH_LANE : Inst<"vqrdmulh_lane", "ddgi", "siQsQi", OP_QRDMULH_LN>;
def VQRDMULH_LANEQ : Inst<"vqrdmulh_laneq", "ddki", "siQsQi", OP_QRDMULH_LN>;
def VMLA_N       : Inst<"vmla_n", "ddda", "sifUsUiQsQiQUsQUiQfQd", OP_MLA_N>;
def VFMA_N       : IInst<"vfma_n", "ddda", "fQfQd">;
def VMLAL_N      : Inst<"vmlal_n", "wwda", "siUsUi", OP_MLAL_N>;
def VQDMLAL_N    : Inst<"vqdmlal_n", "wwda", "si", OP_QDMLAL_N>;
def VMLS_N       : Inst<"vmls_n", "ddds", "sifUsUiQsQiQUsQUiQfQd", OP_MLS_N>;
def VMLSL_N      : Inst<"vmlsl_n", "wwda", "siUsUi", OP_MLSL_N>;
def VQDMLSL_N    : Inst<"vqdmlsl_n", "wwda", "si",OP_QDMLSL_N>;
def VMULX_LANE   : SInst<"vmulx_lane", "dddi", "fQfQd">;


////////////////////////////////////////////////////////////////////////////////
// Cross Lane
def ADDV  : SInst<"vaddv",  "sd", "csiUcUsUiQcQsQiQUcQUsQUi">;
def ADDLV : SInst<"vaddlv", "zd", "csiUcUsUiQcQsQiQUcQUsQUi">;
def MAXV  : SInst<"vmaxv",  "sd", "csifUcUsUiQcQsQiQUcQUsQUiQfQd">;
def MINV  : SInst<"vminv",  "sd", "csifUcUsUiQcQsQiQUcQUsQUiQfQd">;
def MAXNMV : SInst<"vmaxnmv", "sd", "fQf">;
def MINMV  : SInst<"vminnmv", "sd", "fQf">;

////////////////////////////////////////////////////////////////////////////////
// Vector Extract
def VEXT : WInst<"vext", "dddi",
                 "cUcPcsUsPsiUilUlfQcQUcQPcQsQUsQPsQiQUiQlQUlQfQd">;

////////////////////////////////////////////////////////////////////////////////
// Reverse vector elements
def VREV64 : Inst<"vrev64", "dd", "csiUcUsUiPcPsfQcQsQiQUcQUsQUiQPcQPsQf",
                  OP_REV64>;
def VREV32 : Inst<"vrev32", "dd", "csUcUsPcPsQcQsQUcQUsQPcQPs", OP_REV32>;
def VREV16 : Inst<"vrev16", "dd", "cUcPcQcQUcQPc", OP_REV16>;

////////////////////////////////////////////////////////////////////////////////
// Other single operand arithmetic
def VABS    : SInst<"vabs", "dd", "csifQcQsQiQlQfQd">;
def VQABS   : SInst<"vqabs", "dd", "csiQcQsQiQl">;
def VNEG    : Inst<"vneg", "dd", "csifQcQsQiQlQfQd", OP_NEG>;
def VQNEG   : SInst<"vqneg", "dd", "csiQcQsQiQl">;
def VCLS    : SInst<"vcls", "dd", "csiQcQsQi">;
def VCLZ    : IInst<"vclz", "dd", "csiUcUsUiQcQsQiQUcQUsQUi">;
def VCNT    : WInst<"vcnt", "dd", "UccPcQUcQcQPc">;
def VRECPE  : SInst<"vrecpe", "dd",  "fUiQfQdQUi">;
def VRSQRTE : SInst<"vrsqrte", "dd", "fUiQfQdQUi">;
def VRBIT   : SInst<"vrbit", "dd", "cUcQcQUc">;
// Scalar operand versions
def VQABSB : SInst<"vqabsb", "ss", "c">;
def VQABSH : SInst<"vqabsh", "ss", "s">;
def VQABSS : SInst<"vqabss", "ss", "i">;
def VQABSD : SInst<"vqabsd", "ss", "l">;
def VQNEGB : SInst<"vqnegb", "ss", "c">;
def VQNEGH : SInst<"vqnegh", "ss", "s">;
def VQNEGS : SInst<"vqnegs", "ss", "i">;
def VQNEGD : SInst<"vqnegd", "ss", "l">;

////////////////////////////////////////////////////////////////////////////////
// Logical operations
def VMVN : Inst<"vmvn", "dd", "csiUcUsUiPcQcQsQiQUcQUsQUiQPc", OP_NOT>;
def VAND : Inst<"vand", "ddd", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl", OP_AND>;
def VORR : Inst<"vorr", "ddd", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl", OP_OR>;
def VEOR : Inst<"veor", "ddd", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl", OP_XOR>;
def VBIC : Inst<"vbic", "ddd", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl", OP_ANDN>;
def VORN : Inst<"vorn", "ddd", "csilUcUsUiUlQcQsQiQlQUcQUsQUiQUl", OP_ORN>;
def VBSL : Inst<"vbsl", "dudd",
                "csilUcUsUiUlfPcPsQcQsQiQlQUcQUsQUiQUlQfQdQPcQPs", OP_SEL>;

////////////////////////////////////////////////////////////////////////////////
// Transposition operations
def VTRN : WInst<"vtrn", "2dd", "csiUcUsUifPcPsQcQsQiQUcQUsQUiQfQPcQPs">;
def VZIP : WInst<"vzip", "2dd", "csiUcUsUifPcPsQcQsQiQUcQUsQUiQfQPcQPs">;
def VUZP : WInst<"vuzp", "2dd", "csiUcUsUifPcPsQcQsQiQUcQUsQUiQfQPcQPs">;

////////////////////////////////////////////////////////////////////////////////
// Vector reinterpret cast operations
def VREINTERPRET
  : Inst<"vreinterpret", "dd",
         "csilUcUsUiUlhfPcPsQcQsQiQlQUcQUsQUiQUlQhQfQPcQPs", OP_REINT>;

////////////////////////////////////////////////////////////////////////////////
// Crypto operations

def VAESE   : SInst<"vaese",   "ddd", "QUc">;
def VAESD   : SInst<"vaesd",   "ddd", "QUc">;
def VAESMC  : SInst<"vaesmc",  "dd",  "QUc">;
def VAESIMC : SInst<"vaesimc", "dd",  "QUc">;

def VSHA1C   : SInst<"vsha1c",   "ddsd", "QUi">;
def VSHA1P   : SInst<"vsha1p",   "ddsd", "QUi">;
def VSHA1M   : SInst<"vsha1m",   "ddsd", "QUi">;
def VSHA1H   : SInst<"vsha1h",   "ss",   "Ui">;
def VSHA1SU0 : SInst<"vsha1su0", "dddd", "QUi">;
def VSHA1SU1 : SInst<"vsha1su1", "ddd",  "QUi">;

def VSHA256H   : SInst<"vsha256h",   "dddd", "QUi">;
def VSHA256H2  : SInst<"vsha256h2",  "dddd", "QUi">;
def VSHA256SU0 : SInst<"vsha256su0", "ddd",  "QUi">;
def VSHA256SU1 : SInst<"vsha256su1", "dddd", "QUi">;
