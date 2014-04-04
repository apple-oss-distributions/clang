//===-- ARM64SetGlobalLinkage.cpp - Use LinkerPrivate linkage for globals -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// The Darwin linker needs to have symbols used with @page and @pageoff
// operands preserved in the object files.  This pass looks for global values
// with Private linkage and promotes them to use LinkerPrivate linkage so that
// those symbols will be available in the object file.  This is done as a
// separate pass since the GlobalValues are not supposed to be modified later
// during codegen.
// ===---------------------------------------------------------------------===//

#include "ARM64.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
using namespace llvm;

namespace {
  class ARM64SetGlobalLinkage : public FunctionPass {
  public:
    static char ID;             // Pass identification, replacement for typeid.
    explicit ARM64SetGlobalLinkage() : FunctionPass(ID) {}

    virtual bool doInitialization(Module &M);
    virtual bool runOnFunction(Function &F);

    const char *getPassName() const {
      return "Set global linkage";
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
      FunctionPass::getAnalysisUsage(AU);
    }
  };
} // end anonymous namespace

char ARM64SetGlobalLinkage::ID = 0;

/// doInitialization - Scan the list of globals and promote any with Private
/// linkage to use LinkerPrivate linkage instead.  Returns true if changed.
bool ARM64SetGlobalLinkage::doInitialization(Module &M) {
  bool Changed = false;

  for (Module::global_iterator I = M.global_begin(),
         E = M.global_end(); I != E; ++I) {
    if (I->hasPrivateLinkage()) {
      I->setLinkage(llvm::GlobalValue::LinkerPrivateLinkage);
      Changed = true;
    }
  }

  return Changed;
}

bool ARM64SetGlobalLinkage::runOnFunction(Function &F) {
  return false;
}

FunctionPass *llvm::createARM64SetGlobalLinkage() {
  return new ARM64SetGlobalLinkage();
}
