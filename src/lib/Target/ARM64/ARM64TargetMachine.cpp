//===-- ARM64TargetMachine.cpp - Define TargetMachine for ARM64 -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "ARM64.h"
#include "ARM64TargetMachine.h"
#include "llvm/PassManager.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/Scalar.h"
using namespace llvm;

static cl::opt<bool>
EnableCCMP("arm64-ccmp", cl::desc("Enable the CCMP formation pass"),
           cl::init(true));

static cl::opt<bool>
EnableStPairSuppress("arm64-stp-suppress", cl::Hidden,
                     cl::desc("Suppress STP for ARM64"),
                     cl::init(true));

// Option to override the default behavior of the compiler.
// FIXME: Should we merge the option of the ARM backend with
// this one?
static cl::opt<bool>
EnableGlobalMerge("arm64-global-merge", cl::Hidden,
                  cl::desc("Enable global merge pass for ARM64"),
                  cl::init(true));

static cl::opt<bool>
EnablePromoteConstant("arm64-promote-const", cl::Hidden,
                      cl::desc("Enable the promote constant pass"),
                      cl::init(true));

extern "C" void LLVMInitializeARM64Target() {
  // Register the target.
  RegisterTargetMachine<ARM64TargetMachine> X(TheARM64Target);
}

/// TargetMachine ctor - Create an ARM64 architecture model.
///
ARM64TargetMachine::ARM64TargetMachine(const Target &T, StringRef TT,
                                       StringRef CPU, StringRef FS,
                                       const TargetOptions &Options,
                                       Reloc::Model RM, CodeModel::Model CM,
                                       CodeGenOpt::Level OL)
  : LLVMTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL),
    Subtarget(TT, CPU, FS),
    DL(std::string("e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-"
                   "i64:64:64-f32:32:32-f64:64:64-v64:64:64-"
                   "v128:128:128-a0:0:64-n32:64-S128")),
    InstrInfo(Subtarget),
    TLInfo(*this),
    FrameLowering(Subtarget),
    TSInfo(*this) {
}

namespace {
/// ARM64 Code Generator Pass Configuration Options.
class ARM64PassConfig : public TargetPassConfig {
public:
  ARM64PassConfig(ARM64TargetMachine *TM, PassManagerBase &PM)
    : TargetPassConfig(TM, PM) {}

  ARM64TargetMachine &getARM64TargetMachine() const {
    return getTM<ARM64TargetMachine>();
  }

  virtual bool addPreISel();
  virtual bool addInstSelector();
  virtual bool addILPOpts();
  virtual bool addPreRegAlloc();
  virtual bool addPostRegAlloc();
  virtual bool addPreSched2();
  virtual bool addPreEmitPass();
};
} // namespace

void ARM64TargetMachine::addAnalysisPasses(PassManagerBase &PM) {
  // Add first the target-independent BasicTTI pass, then our ARM64 pass. This
  // allows the ARM64 pass to delegate to the target independent layer when
  // appropriate.
  PM.add(createBasicTargetTransformInfoPass(getTargetLowering()));
  PM.add(createARM64TargetTransformInfoPass(this));
}

TargetPassConfig *ARM64TargetMachine::createPassConfig(PassManagerBase &PM) {
  return new ARM64PassConfig(this, PM);
}

// Pass Pipeline Configuration
bool ARM64PassConfig::addPreISel() {
  // Change GlobalValue Private linkage to LinkerPrivate for the Darwin linker.
  // FIXME: Do we need this anymore? We should be handling this in MC via
  // sections at this point.
  addPass(createARM64SetGlobalLinkage());
  // Run promote constant before global merge, so that the promoted constants
  // get a chance to be merged
  if (TM->getOptLevel() != CodeGenOpt::None && EnablePromoteConstant)
    addPass(createARM64PromoteConstantPass());
  if (TM->getOptLevel() != CodeGenOpt::None && EnableGlobalMerge)
    addPass(createGlobalMergePass(TM->getTargetLowering()));
  if (TM->getOptLevel() != CodeGenOpt::None)
    addPass(createARM64AddressTypePromotionPass());
  return false;
}

bool ARM64PassConfig::addInstSelector() {
  addPass(createARM64ISelDag(getARM64TargetMachine(), getOptLevel()));
  return false;
}

bool ARM64PassConfig::addILPOpts() {
  if (EnableCCMP)
    addPass(createARM64ConditionalCompares());
  addPass(&EarlyIfConverterID);
  if (EnableStPairSuppress)
    addPass(createARM64StorePairSuppressPass());
  return true;
}

bool ARM64PassConfig::addPreRegAlloc() {
  // Use AdvSIMD scalar instructions whenever profitable.
  addPass(createARM64AdvSIMDScalar());
  return true;
}

bool ARM64PassConfig::addPostRegAlloc() {
  // Change dead register definitions to refer to the zero register.
  addPass(createARM64DeadRegisterDefinitions());
  return true;
}

bool ARM64PassConfig::addPreSched2() {
  // Expand some pseudo instructions to allow proper scheduling.
  addPass(createARM64ExpandPseudoPass());
  // Use load/store pair instructions when possible.
  addPass(createARM64LoadStoreOptimizationPass());
  return true;
}

bool ARM64PassConfig::addPreEmitPass() {
  // Relax conditional branch instructions if they're otherwise out of
  // range of their destination.
  addPass(createARM64BranchRelaxation());
  return true;
}
