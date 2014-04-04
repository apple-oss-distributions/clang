//===-- ARM64TargetTransformInfo.cpp - ARM64 specific TTI pass ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
/// \file
/// This file implements a TargetTransformInfo analysis pass specific to the
/// ARM64 target machine. It uses the target's detailed information to provide
/// more precise answers to certain TTI queries, while letting the target
/// independent and default TTI implementations handle the rest.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "arm64tti"
#include "ARM64.h"
#include "ARM64TargetMachine.h"
#include "MCTargetDesc/ARM64AddressingModes.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Target/CostTable.h"
#include "llvm/Target/TargetLowering.h"
using namespace llvm;

// Declare the pass initialization routine locally as target-specific passes
// don't havve a target-wide initialization entry point, and so we rely on the
// pass constructor initialization.
namespace llvm {
void initializeARM64TTIPass(PassRegistry &);
}

namespace {

class ARM64TTI : public ImmutablePass, public TargetTransformInfo {
  const ARM64TargetMachine *TM;
  const ARM64Subtarget *ST;
  const ARM64TargetLowering *TLI;

  /// Estimate the overhead of scalarizing an instruction. Insert and Extract
  /// are set if the result needs to be inserted and/or extracted from vectors.
  unsigned getScalarizationOverhead(Type *Ty, bool Insert, bool Extract) const;

public:
  ARM64TTI() : ImmutablePass(ID), TM(0), ST(0), TLI(0) {
    llvm_unreachable("This pass cannot be directly constructed");
  }

  ARM64TTI(const ARM64TargetMachine *TM)
      : ImmutablePass(ID), TM(TM), ST(TM->getSubtargetImpl()),
        TLI(TM->getTargetLowering()) {
    initializeARM64TTIPass(*PassRegistry::getPassRegistry());
  }

  virtual void initializePass() {
    pushTTIStack(this);
  }

  virtual void finalizePass() {
    popTTIStack();
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    TargetTransformInfo::getAnalysisUsage(AU);
  }

  /// Pass identification.
  static char ID;

  /// Provide necessary pointer adjustments for the two base classes.
  virtual void *getAdjustedAnalysisPointer(const void *ID) {
    if (ID == &TargetTransformInfo::ID)
      return (TargetTransformInfo*)this;
    return this;
  }

  /// \name Scalar TTI Implementations
  /// @{

  virtual unsigned getIntImmCost(const APInt &Imm, Type *Ty) const;
  virtual PopcntSupportKind getPopcntSupport(unsigned TyWidth) const;

  /// @}

  /// \name Vector TTI Implementations
  /// @{

  unsigned getNumberOfRegisters(bool Vector) const {
    if (Vector)
      return 32;

    return 31;
  }

  unsigned getRegisterBitWidth(bool Vector) const {
    if (Vector)
      return 128;

    return 64;
  }

  unsigned getMaximumUnrollFactor() const {
      return 2;
  }

  unsigned getCastInstrCost(unsigned Opcode, Type *Dst, Type *Src) const;

  unsigned getVectorInstrCost(unsigned Opcode, Type *Val, unsigned Index) const;

  virtual unsigned getArithmeticInstrCost(unsigned Opcode, Type *Ty,
                                 OperandValueKind Opd1Info = OK_AnyValue,
                                 OperandValueKind Opd2Info = OK_AnyValue) const;
  /// @}

};

} // end anonymous namespace

INITIALIZE_AG_PASS(ARM64TTI, TargetTransformInfo, "arm64tti",
                   "ARM64 Target Transform Info", true, true, false)
char ARM64TTI::ID = 0;

ImmutablePass *
llvm::createARM64TargetTransformInfoPass(const ARM64TargetMachine *TM) {
  return new ARM64TTI(TM);
}

unsigned ARM64TTI::getIntImmCost(const APInt &Imm,Type *Ty) const {
  assert(Ty->isIntegerTy());

  unsigned BitSize = Ty->getPrimitiveSizeInBits();
  if (BitSize == 0)
    return ~0U;

  int64_t Val = Imm.getSExtValue();
  if (Val == 0 || ARM64_AM::isLogicalImmediate(Val, BitSize))
    return 1;

  if ((int64_t)Val < 0)
    Val = ~Val;
  if (BitSize == 32)
    Val &= (1LL << 32) - 1;

  unsigned LZ = CountLeadingZeros_64(Val);
  unsigned Shift = (63 - LZ) / 16;
  // MOVZ is free so return true for one or fewer MOVK.
  return (Shift == 0) ? 1 : Shift;
}

ARM64TTI::PopcntSupportKind ARM64TTI::getPopcntSupport(unsigned TyWidth) const {
  assert(isPowerOf2_32(TyWidth) && "Ty width must be power of 2");
  if (TyWidth == 32 || TyWidth == 64)
    return PSK_FastHardware;
  // TODO: ARM64TargetLowering::LowerCTPOP() supports 128bit popcount.
  return PSK_Software;
}

unsigned ARM64TTI::getCastInstrCost(unsigned Opcode, Type *Dst, Type *Src) const {
  int ISD = TLI->InstructionOpcodeToISD(Opcode);
  assert(ISD && "Invalid opcode");

  EVT SrcTy = TLI->getValueType(Src);
  EVT DstTy = TLI->getValueType(Dst);

  if (!SrcTy.isSimple() || !DstTy.isSimple())
    return TargetTransformInfo::getCastInstrCost(Opcode, Dst, Src);

  static const TypeConversionCostTblEntry<MVT> ConversionTbl[] = {
    // LowerVectorINT_TO_FP:
    { ISD::SINT_TO_FP,  MVT::v2f32, MVT::v2i32, 1 },
    { ISD::SINT_TO_FP,  MVT::v2f64, MVT::v2i8,  1 },
    { ISD::SINT_TO_FP,  MVT::v2f64, MVT::v2i16, 1 },
    { ISD::SINT_TO_FP,  MVT::v2f64, MVT::v2i32, 1 },
    { ISD::SINT_TO_FP,  MVT::v2f64, MVT::v2i64, 1 },
    { ISD::UINT_TO_FP,  MVT::v2f32, MVT::v2i32, 1 },
    { ISD::UINT_TO_FP,  MVT::v2f64, MVT::v2i8,  1 },
    { ISD::UINT_TO_FP,  MVT::v2f64, MVT::v2i16, 1 },
    { ISD::UINT_TO_FP,  MVT::v2f64, MVT::v2i32, 1 },
    { ISD::UINT_TO_FP,  MVT::v2f64, MVT::v2i64, 1 },
    // LowerVectorFP_TO_INT
    { ISD::FP_TO_SINT,  MVT::v4i32, MVT::v4f32,  1 },
    { ISD::FP_TO_SINT,  MVT::v2i64, MVT::v2f64,  1 },
    { ISD::FP_TO_UINT,  MVT::v4i32, MVT::v4f32,  1 },
    { ISD::FP_TO_UINT,  MVT::v2i64, MVT::v2f64,  1 },
    { ISD::FP_TO_UINT,  MVT::v2i32, MVT::v2f64,  1 },
    { ISD::FP_TO_SINT,  MVT::v2i32, MVT::v2f64,  1 },
  };

  int Idx = ConvertCostTableLookup<MVT>(ConversionTbl,
                                        array_lengthof(ConversionTbl),
                                        ISD, DstTy.getSimpleVT(),
                                        SrcTy.getSimpleVT());
  if (Idx != -1)
    return ConversionTbl[Idx].Cost;

  return TargetTransformInfo::getCastInstrCost(Opcode, Dst, Src);
}

unsigned ARM64TTI::getVectorInstrCost(unsigned Opcode, Type *Val,
                                      unsigned Index) const {
  assert(Val->isVectorTy() && "This must be a vector type");

  if (Index != -1U) {
    // Legalize the type.
    std::pair<unsigned, MVT> LT = TLI->getTypeLegalizationCost(Val);

    // This type is legalized to a scalar type.
    if (!LT.second.isVector())
      return 0;

    // The type may be split. Normalize the index to the new type.
    unsigned Width = LT.second.getVectorNumElements();
    Index = Index % Width;

    // The element at index zero is already inside the vector.
    if (Index == 0)
      return 0;
  }

  // All other insert/extracts cost this much.
  return 2;
}

unsigned ARM64TTI::getArithmeticInstrCost(unsigned Opcode, Type *Ty,
                                          OperandValueKind Opd1Info,
                                          OperandValueKind Opd2Info) const {
  // Legalize the type.
  std::pair<unsigned, MVT> LT = TLI->getTypeLegalizationCost(Ty);

  int ISD = TLI->InstructionOpcodeToISD(Opcode);

  switch (ISD) {
  default:
    return TargetTransformInfo::getArithmeticInstrCost(Opcode, Ty, Opd1Info,
                                                       Opd2Info);
  case ISD::ADD:
  case ISD::MUL:
  case ISD::XOR:
  case ISD::OR:
  case ISD::AND:
    // These nodes are marked as 'custom' for combining purposes only.
    // We know that they are legal. See LowerAdd in ISelLowering.
    return 1 * LT.first;
  }
}

