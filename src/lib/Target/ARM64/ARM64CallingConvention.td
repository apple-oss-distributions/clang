//===- ARM64CallingConv.td - Calling Conventions for ARM64 -*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This describes the calling conventions for ARM64 architecture.
//
//===----------------------------------------------------------------------===//

/// CCIfAlign - Match of the original alignment of the arg
class CCIfAlign<string Align, CCAction A> :
  CCIf<!strconcat("ArgFlags.getOrigAlign() == ", Align), A>;

//===----------------------------------------------------------------------===//
// ARM AAPCS64 Calling Convention
//===----------------------------------------------------------------------===//

def CC_ARM64_AAPCS : CallingConv<[
  CCIfType<[v2f32], CCBitConvertToType<v2i32>>,
  CCIfType<[v2f64, v4f32], CCBitConvertToType<v2i64>>,

  // An SRet is passed in X8, not X0 like a normal pointer parameter.
  CCIfSRet<CCIfType<[i64], CCAssignToRegWithShadow<[X8], [W8]>>>,

  // Handle i1, i8, i16, i32, i64, f32, f64 and v2f64 by passing in registers,
  // up to eight each of GPR and FPR.
  CCIfType<[i1, i8, i16], CCCustom<"CC_ARM64_Custom_i1i8i16_Reg">>,
  CCIfType<[i32], CCAssignToRegWithShadow<[W0, W1, W2, W3, W4, W5, W6, W7],
                                          [X0, X1, X2, X3, X4, X5, X6, X7]>>,
  // i128 is split to two i64s, we can't fit half to register X7.
  CCIfType<[i64], CCIfSplit<CCAssignToRegWithShadow<[X0, X1, X2, X3, X4, X5, X6],
                                          [W0, W1, W2, W3, W4, W5, W6]>>>,
  // i128 is split to two i64s, and its stack alignment is 16 bytes.
  CCIfType<[i64], CCIfSplit<CCAssignToStackWithShadow<8, 16, X7>>>,

  CCIfType<[i64], CCAssignToRegWithShadow<[X0, X1, X2, X3, X4, X5, X6, X7],
                                          [W0, W1, W2, W3, W4, W5, W6, W7]>>,
  CCIfType<[f32], CCAssignToRegWithShadow<[S0, S1, S2, S3, S4, S5, S6, S7],
                                          [Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7]>>,
  CCIfType<[f64], CCAssignToRegWithShadow<[D0, D1, D2, D3, D4, D5, D6, D7],
                                          [Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7]>>,
  CCIfType<[v1i64, v2i32, v4i16, v8i8, v1f64, v2f32],
           CCAssignToRegWithShadow<[D0, D1, D2, D3, D4, D5, D6, D7],
                                   [Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7]>>,
  CCIfType<[v2i64, v4i32, v8i16, v16i8, v4f32, v2f64],
           CCAssignToReg<[Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7]>>,

  // If more than will fit in registers, pass them on the stack instead.
  CCIfType<[i1, i8, i16], CCCustom<"CC_ARM64_Custom_i1i8i16_Stack">>,
  CCIfType<[i32, f32], CCAssignToStack<4, 4>>,
  CCIfType<[i64, f64, v1f64, v2f32, v1i64, v2i32, v4i16, v8i8], CCAssignToStack<8, 8>>,
  CCIfType<[v2i64, v4i32, v8i16, v16i8, v4f32, v2f64],   CCAssignToStack<16, 16>>
]>;

def CC_ARM64_AAPCS_VarArg : CallingConv<[
  CCIfType<[v2f32], CCBitConvertToType<v2i32>>,
  CCIfType<[v2f64, v4f32], CCBitConvertToType<v2i64>>,

  // Handle all scalar types as either i64 or f64.
  CCIfType<[i8, i16, i32], CCPromoteToType<i64>>,
  CCIfType<[f32],          CCPromoteToType<f64>>,

  // Everything is on the stack.
  // i128 is split to two i64s, and its stack alignment is 16 bytes.
  CCIfType<[i64], CCIfSplit<CCAssignToStack<8, 16>>>,
  CCIfType<[i64, f64, v1i64, v2i32, v4i16, v8i8, v1f64, v2f32], CCAssignToStack<8, 8>>,
  CCIfType<[v2i64, v4i32, v8i16, v16i8, v4f32, v2f64],   CCAssignToStack<16, 16>>
]>;

def RetCC_ARM64_AAPCS : CallingConv<[
  CCIfType<[v2f32], CCBitConvertToType<v2i32>>,
  CCIfType<[v2f64, v4f32], CCBitConvertToType<v2i64>>,

  CCIfType<[i32], CCAssignToRegWithShadow<[W0, W1, W2, W3, W4, W5, W6, W7],
                                          [X0, X1, X2, X3, X4, X5, X6, X7]>>,
  CCIfType<[i64], CCAssignToRegWithShadow<[X0, X1, X2, X3, X4, X5, X6, X7],
                                          [W0, W1, W2, W3, W4, W5, W6, W7]>>,
  CCIfType<[f32], CCAssignToRegWithShadow<[S0, S1, S2, S3, S4, S5, S6, S7],
                                          [Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7]>>,
  CCIfType<[f64], CCAssignToRegWithShadow<[D0, D1, D2, D3, D4, D5, D6, D7],
                                          [Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7]>>,
  CCIfType<[v1i64, v2i32, v4i16, v8i8, v1f64, v2f32],
      CCAssignToRegWithShadow<[D0, D1, D2, D3, D4, D5, D6, D7],
                              [Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7]>>,
  CCIfType<[v2i64, v4i32, v8i16, v16i8, v4f32, v2f64],
      CCAssignToReg<[Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7]>>
]>;

def CSR_ARM64_AAPCS : CalleeSavedRegs<(add X19, X20, X21, X22, X23, X24,
                                           X25, X26, X27, X28,
                                           D8,  D9,  D10, D11,
                                           D12, D13, D14, D15)>;
