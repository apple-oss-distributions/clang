//===-- Obfuscation.cpp - String obfuscation helper -------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides helper classes to perform string obfuscation
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/TypeFinder.h"
#include "llvm/Transforms/Utils/Obfuscation.h"

using namespace llvm;
using namespace obfuscate;

void Rot13Obfuscator::anchor() {}

void IncrementObfuscator::anchor() {}

namespace {
/// \brief Helper class to recursively gather and modify all MDStrings from
/// given entry points
///
/// Will only ever visit a given MDNode once. User can feed in MDNodes with
/// "addEntryPoint". User can then incrementally, at any time call "run" to
/// start recursively modifying reachable MDStrings from un-visited entry
/// points.
class MDSRecursiveModify {
  SmallVector<MDNode *, 16> Worklist;
  DenseSet<const MDNode *> Visited;

  LLVMContext &Ctx;
  std::function<StringRef(StringRef)> F;

public:
  MDSRecursiveModify(LLVMContext &Context,
                     std::function<StringRef(StringRef)> NewStr)
      : Ctx(Context), F(NewStr) {}

  /// \brief Add a new entry point into the metadata graph to traverse from
  void addEntryPoint(MDNode *N) {
    if (!N || !Visited.count(N))
      Worklist.push_back(N);
  }
  void addEntryPoint(NamedMDNode *NMD) {
    for (auto I : NMD->operands())
      addEntryPoint(I);
  }
  void addEntryPoint(DbgInfoIntrinsic *DII) {
    if (auto DDI = dyn_cast<DbgDeclareInst>(DII))
      addEntryPoint(DDI->getVariable());
    else if (auto DVI = dyn_cast<DbgValueInst>(DII))
      addEntryPoint(DVI->getVariable());
    else
      llvm_unreachable("invalid debug info intrinsic");
  }

  /// \brief Recursively modify reachable, unvisited MDStrings
  inline void run();
};
}

void MDSRecursiveModify::run() {
  while (!Worklist.empty()) {
    auto N = Worklist.pop_back_val();
    if (!N || Visited.count(N))
      continue;
    Visited.insert(N);
    for (unsigned i = 0; i < N->getNumOperands(); ++i) {
      Metadata *MD = N->getOperand(i);
      if (!MD)
        continue;
      if (auto MDS = dyn_cast<MDString>(MD)) {
        assert(N->isResolved() && "Unexpected forward reference");
        N->replaceOperandWith(i, MDString::get(Ctx, F(MDS->getString())));
      } else if (auto NN = dyn_cast<MDNode>(MD))
        Worklist.push_back(NN);
    }
  }
}

void Obfuscator::obfuscateModule(Module &M,
                                 std::function<bool(Value &)> preserve) {
  // Obfuscate value names
  {
    SmallVector<GlobalValue *, 64> Worklist;
    auto hasName = [](const Value &V) { return V.getName() != ""; };
    for (auto &F : M) {
      if (hasName(F) && !preserve(F))
        Worklist.push_back(&F);
      // While we're here, drop parameter names if they exist
      for (auto &Arg : F.args())
        Arg.setName("");
    }
    for (auto &I : M.globals())
      if (hasName(I) && !preserve(I))
        Worklist.push_back(&I);
    for (auto &I : M.aliases())
      if (hasName(I) && !preserve(I))
        Worklist.push_back(&I);
    for (auto I : Worklist)
      I->setName(obfuscate(I->getName()));
  }

  // Drop type names
  TypeFinder TF;
  TF.run(M, true);
  for (auto I : TF)
    I->setName(obfuscateTypeName(I->getName()));

  // Obfuscate debug info strings
  MDSRecursiveModify Modify(M.getContext(),
                            [this](StringRef S) { return obfuscate(S); });

  // llvm.dbg.cu needs to be obfuscated
  if (auto DBG = M.getNamedMetadata("llvm.dbg.cu"))
    Modify.addEntryPoint(DBG);
  Modify.run();

  // llvm.dbg.value/declare's "variable" argument needs to be obfuscated
  auto addDbgIntrinsicCalls = [&](StringRef Name) {
    auto F = M.getFunction(Name);
    if (!F)
      return;
    for (auto Usr : F->users())
      Modify.addEntryPoint(cast<DbgInfoIntrinsic>(Usr));
  };
  addDbgIntrinsicCalls("llvm.dbg.declare");
  addDbgIntrinsicCalls("llvm.dbg.value");
  Modify.run();

  // Every insruction's DebugLoc needs to be obfuscated
  for (auto &F : M.functions())
    for (auto &BB : F)
      for (auto &I : BB) {
        // While we're here, go ahead and drop its name if it has one
        I.setName("");
        Modify.addEntryPoint(I.getDebugLoc().getAsMDNode());
      }
  Modify.run();
}

void IncrementObfuscator::writeReverseMap(raw_ostream &OS) {
  std::vector<StringRef> Strs;
  Strs.resize(reverseMap().size());
  // write an identifier on the first line.
  OS << "BCSymbolMap Version: 1.0\n";
  for (auto &STE : reverseMap())
    Strs[findIndex(STE.getKey())] = STE.getValue();
  for (auto S : Strs) {
    assert(S != "" && "failed to initialize a member");
    OS << S << "\n";
  }
}
