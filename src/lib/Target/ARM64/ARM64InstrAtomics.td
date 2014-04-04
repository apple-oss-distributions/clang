//===- ARM64InstrAtomics.td - ARM64 Atomic codegen support -*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// ARM64 Atomic operand code-gen constructs.
//
//===----------------------------------------------------------------------===//

def : Pat<(membarrier (i32 imm), (i32 imm), (i32 imm), (i32 imm), (i32 1)),
          (DMB (i32 0xf))>;
def : Pat<(membarrier (i32 imm), (i32 imm), (i32 imm), (i32 imm), (i32 0)),
          (DMB (i32 0xf))>;

// Atomic fences
def : Pat<(atomic_fence (i64 4), (imm)), (DMB (i32 0x9))>;
def : Pat<(atomic_fence (imm), (imm)), (DMB (i32 0xb))>;

// Atomic load/store
// FIXME: Need 128-bit load
// FIXME: We should be using regular load/store instructions for Monotonic.
// (Or alternatively, we could eliminate Acquire/Release fences.  Need to check
// with ARM to see what the preferred model is, because they are both valid,
// but not compatible with each other.)
def : Pat<(atomic_load_8  GPR64sp:$ptr), (LDARB GPR64sp:$ptr)>;
def : Pat<(atomic_load_16 GPR64sp:$ptr), (LDARH GPR64sp:$ptr)>;
def : Pat<(atomic_load_32 GPR64sp:$ptr), (LDARW GPR64sp:$ptr)>;
def : Pat<(atomic_load_64 GPR64sp:$ptr), (LDARX GPR64sp:$ptr)>;
def : Pat<(atomic_store_8  GPR64sp:$ptr, GPR32:$val),
          (STLRB GPR32:$val, GPR64sp:$ptr)>;
def : Pat<(atomic_store_16 GPR64sp:$ptr, GPR32:$val),
          (STLRH GPR32:$val, GPR64sp:$ptr)>;
def : Pat<(atomic_store_32 GPR64sp:$ptr, GPR32:$val),
          (STLRW GPR32:$val, GPR64sp:$ptr)>;
def : Pat<(atomic_store_64 GPR64sp:$ptr, GPR64:$val),
          (STLRX GPR64:$val, GPR64sp:$ptr)>;

let usesCustomInserter = 1 in {
  let Defs = [CPSR] in {
    def ATOMIC_LOAD_ADD_I8 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_add_8 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_SUB_I8 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_sub_8 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_AND_I8 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_and_8 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_OR_I8 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_or_8 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_XOR_I8 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_xor_8 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_NAND_I8 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_nand_8 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_ADD_I16 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_add_16 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_SUB_I16 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_sub_16 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_AND_I16 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_and_16 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_OR_I16 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_or_16 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_XOR_I16 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_xor_16 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_NAND_I16 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_nand_16 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_ADD_I32 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_add_32 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_SUB_I32 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_sub_32 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_AND_I32 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_and_32 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_OR_I32 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_or_32 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_XOR_I32 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_xor_32 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_NAND_I32 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$incr),
      [(set GPR32:$dst, (atomic_load_nand_32 GPR64sp:$ptr, GPR32:$incr))], "">;
    def ATOMIC_LOAD_ADD_I64 : Pseudo<
      (outs GPR64:$dst), (ins GPR64sp:$ptr, GPR64:$incr),
      [(set GPR64:$dst, (atomic_load_add_64 GPR64sp:$ptr, GPR64:$incr))], "">;
    def ATOMIC_LOAD_SUB_I64 : Pseudo<
      (outs GPR64:$dst), (ins GPR64sp:$ptr, GPR64:$incr),
      [(set GPR64:$dst, (atomic_load_sub_64 GPR64sp:$ptr, GPR64:$incr))], "">;
    def ATOMIC_LOAD_AND_I64 : Pseudo<
      (outs GPR64:$dst), (ins GPR64sp:$ptr, GPR64:$incr),
      [(set GPR64:$dst, (atomic_load_and_64 GPR64sp:$ptr, GPR64:$incr))], "">;
    def ATOMIC_LOAD_OR_I64 : Pseudo<
      (outs GPR64:$dst), (ins GPR64sp:$ptr, GPR64:$incr),
      [(set GPR64:$dst, (atomic_load_or_64 GPR64sp:$ptr, GPR64:$incr))], "">;
    def ATOMIC_LOAD_XOR_I64 : Pseudo<
      (outs GPR64:$dst), (ins GPR64sp:$ptr, GPR64:$incr),
      [(set GPR64:$dst, (atomic_load_xor_64 GPR64sp:$ptr, GPR64:$incr))], "">;
    def ATOMIC_LOAD_NAND_I64 : Pseudo<
      (outs GPR64:$dst), (ins GPR64sp:$ptr, GPR64:$incr),
      [(set GPR64:$dst, (atomic_load_nand_64 GPR64sp:$ptr, GPR64:$incr))], "">;

    def ATOMIC_SWAP_I8 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$new),
      [(set GPR32:$dst, (atomic_swap_8 GPR64sp:$ptr, GPR32:$new))], "">;
    def ATOMIC_SWAP_I16 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$new),
      [(set GPR32:$dst, (atomic_swap_16 GPR64sp:$ptr, GPR32:$new))], "">;
    def ATOMIC_SWAP_I32 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$new),
      [(set GPR32:$dst, (atomic_swap_32 GPR64sp:$ptr, GPR32:$new))], "">;
    def ATOMIC_SWAP_I64 : Pseudo<
      (outs GPR64:$dst), (ins GPR64sp:$ptr, GPR64:$new),
      [(set GPR64:$dst, (atomic_swap_64 GPR64sp:$ptr, GPR64:$new))], "">;

    def ATOMIC_CMP_SWAP_I8 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$old, GPR32:$new),
      [(set GPR32:$dst, (atomic_cmp_swap_8 GPR64sp:$ptr, GPR32:$old,
                                           GPR32:$new))], "">;
    def ATOMIC_CMP_SWAP_I16 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$old, GPR32:$new),
      [(set GPR32:$dst, (atomic_cmp_swap_16 GPR64sp:$ptr, GPR32:$old,
                                            GPR32:$new))], "">;
    def ATOMIC_CMP_SWAP_I32 : Pseudo<
      (outs GPR32:$dst), (ins GPR64sp:$ptr, GPR32:$old, GPR32:$new),
      [(set GPR32:$dst, (atomic_cmp_swap_32 GPR64sp:$ptr, GPR32:$old,
                                            GPR32:$new))], "">;
    def ATOMIC_CMP_SWAP_I64 : Pseudo<
      (outs GPR64:$dst), (ins GPR64sp:$ptr, GPR64:$old, GPR64:$new),
      [(set GPR64:$dst, (atomic_cmp_swap_64 GPR64sp:$ptr, GPR64:$old,
                                            GPR64:$new))], "">;
}
}
