//===--- DebugInfo.cpp - Debug Information Helper Classes -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the helper classes used to build and interpret debug
// information in LLVM IR form.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/DebugInfo.h"
#include "LLVMContextImpl.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/GVMaterializer.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Dwarf.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;
using namespace llvm::dwarf;

DISubprogram *llvm::getDISubprogram(const MDNode *Scope) {
  if (auto *LocalScope = dyn_cast_or_null<DILocalScope>(Scope))
    return LocalScope->getSubprogram();
  return nullptr;
}

DISubprogram *llvm::getDISubprogram(const Function *F) {
  // We look for the first instr that has a debug annotation leading back to F.
  for (auto &BB : *F) {
    auto Inst = std::find_if(BB.begin(), BB.end(), [](const Instruction &Inst) {
      return Inst.getDebugLoc();
    });
    if (Inst == BB.end())
      continue;
    DebugLoc DLoc = Inst->getDebugLoc();
    const MDNode *Scope = DLoc.getInlinedAtScope();
    auto *Subprogram = getDISubprogram(Scope);
    return Subprogram->describes(F) ? Subprogram : nullptr;
  }

  return nullptr;
}

DICompositeTypeBase *llvm::getDICompositeType(DIType *T) {
  if (auto *C = dyn_cast_or_null<DICompositeTypeBase>(T))
    return C;

  if (auto *D = dyn_cast_or_null<DIDerivedTypeBase>(T)) {
    // This function is currently used by dragonegg and dragonegg does
    // not generate identifier for types, so using an empty map to resolve
    // DerivedFrom should be fine.
    DITypeIdentifierMap EmptyMap;
    return getDICompositeType(D->getBaseType().resolve(EmptyMap));
  }

  return nullptr;
}

DITypeIdentifierMap
llvm::generateDITypeIdentifierMap(const NamedMDNode *CU_Nodes) {
  DITypeIdentifierMap Map;
  for (unsigned CUi = 0, CUe = CU_Nodes->getNumOperands(); CUi != CUe; ++CUi) {
    auto *CU = cast<DICompileUnit>(CU_Nodes->getOperand(CUi));
    DINodeArray Retain = CU->getRetainedTypes();
    for (unsigned Ti = 0, Te = Retain.size(); Ti != Te; ++Ti) {
      if (!isa<DICompositeType>(Retain[Ti]))
        continue;
      auto *Ty = cast<DICompositeType>(Retain[Ti]);
      if (MDString *TypeId = Ty->getRawIdentifier()) {
        // Definition has priority over declaration.
        // Try to insert (TypeId, Ty) to Map.
        std::pair<DITypeIdentifierMap::iterator, bool> P =
            Map.insert(std::make_pair(TypeId, Ty));
        // If TypeId already exists in Map and this is a definition, replace
        // whatever we had (declaration or definition) with the definition.
        if (!P.second && !Ty->isForwardDecl())
          P.first->second = Ty;
      }
    }
  }
  return Map;
}

//===----------------------------------------------------------------------===//
// DebugInfoFinder implementations.
//===----------------------------------------------------------------------===//

void DebugInfoFinder::reset() {
  CUs.clear();
  SPs.clear();
  GVs.clear();
  TYs.clear();
  Scopes.clear();
  NodesSeen.clear();
  TypeIdentifierMap.clear();
  TypeMapInitialized = false;
}

void DebugInfoFinder::InitializeTypeMap(const Module &M) {
  if (!TypeMapInitialized)
    if (NamedMDNode *CU_Nodes = M.getNamedMetadata("llvm.dbg.cu")) {
      TypeIdentifierMap = generateDITypeIdentifierMap(CU_Nodes);
      TypeMapInitialized = true;
    }
}

void DebugInfoFinder::processModule(const Module &M) {
  InitializeTypeMap(M);
  if (NamedMDNode *CU_Nodes = M.getNamedMetadata("llvm.dbg.cu")) {
    for (unsigned i = 0, e = CU_Nodes->getNumOperands(); i != e; ++i) {
      auto *CU = cast<DICompileUnit>(CU_Nodes->getOperand(i));
      addCompileUnit(CU);
      for (auto *DIG : CU->getGlobalVariables()) {
        if (addGlobalVariable(DIG)) {
          processScope(DIG->getScope());
          processType(DIG->getType().resolve(TypeIdentifierMap));
        }
      }
      for (auto *SP : CU->getSubprograms())
        processSubprogram(SP);
      for (auto *ET : CU->getEnumTypes())
        processType(ET);
      for (auto *RT : CU->getRetainedTypes())
        processType(RT);
      for (auto *Import : CU->getImportedEntities()) {
        auto *Entity = Import->getEntity().resolve(TypeIdentifierMap);
        if (auto *T = dyn_cast<DIType>(Entity))
          processType(T);
        else if (auto *SP = dyn_cast<DISubprogram>(Entity))
          processSubprogram(SP);
        else if (auto *NS = dyn_cast<DINamespace>(Entity))
          processScope(NS->getScope());
        else if (auto *M = dyn_cast<DIModule>(Entity))
          processScope(M->getScope());
      }
    }
  }
}

void DebugInfoFinder::processLocation(const Module &M, const DILocation *Loc) {
  if (!Loc)
    return;
  InitializeTypeMap(M);
  processScope(Loc->getScope());
  processLocation(M, Loc->getInlinedAt());
}

void DebugInfoFinder::processType(DIType *DT) {
  if (!addType(DT))
    return;
  processScope(DT->getScope().resolve(TypeIdentifierMap));
  if (auto *DCT = dyn_cast<DICompositeTypeBase>(DT)) {
    processType(DCT->getBaseType().resolve(TypeIdentifierMap));
    if (auto *ST = dyn_cast<DISubroutineType>(DCT)) {
      for (DITypeRef Ref : ST->getTypeArray())
        processType(Ref.resolve(TypeIdentifierMap));
      return;
    }
    for (Metadata *D : DCT->getElements()) {
      if (auto *T = dyn_cast<DIType>(D))
        processType(T);
      else if (auto *SP = dyn_cast<DISubprogram>(D))
        processSubprogram(SP);
    }
  } else if (auto *DDT = dyn_cast<DIDerivedTypeBase>(DT)) {
    processType(DDT->getBaseType().resolve(TypeIdentifierMap));
  }
}

void DebugInfoFinder::processScope(DIScope *Scope) {
  if (!Scope)
    return;
  if (auto *Ty = dyn_cast<DIType>(Scope)) {
    processType(Ty);
    return;
  }
  if (auto *CU = dyn_cast<DICompileUnit>(Scope)) {
    addCompileUnit(CU);
    return;
  }
  if (auto *SP = dyn_cast<DISubprogram>(Scope)) {
    processSubprogram(SP);
    return;
  }
  if (!addScope(Scope))
    return;
  if (auto *LB = dyn_cast<DILexicalBlockBase>(Scope)) {
    processScope(LB->getScope());
  } else if (auto *NS = dyn_cast<DINamespace>(Scope)) {
    processScope(NS->getScope());
  } else if (auto *M = dyn_cast<DIModule>(Scope)) {
    processScope(M->getScope());
  }
}

void DebugInfoFinder::processSubprogram(DISubprogram *SP) {
  if (!addSubprogram(SP))
    return;
  processScope(SP->getScope().resolve(TypeIdentifierMap));
  processType(SP->getType());
  for (auto *Element : SP->getTemplateParams()) {
    if (auto *TType = dyn_cast<DITemplateTypeParameter>(Element)) {
      processType(TType->getType().resolve(TypeIdentifierMap));
    } else if (auto *TVal = dyn_cast<DITemplateValueParameter>(Element)) {
      processType(TVal->getType().resolve(TypeIdentifierMap));
    }
  }
}

void DebugInfoFinder::processDeclare(const Module &M,
                                     const DbgDeclareInst *DDI) {
  auto *N = dyn_cast<MDNode>(DDI->getVariable());
  if (!N)
    return;
  InitializeTypeMap(M);

  auto *DV = dyn_cast<DILocalVariable>(N);
  if (!DV)
    return;

  if (!NodesSeen.insert(DV).second)
    return;
  processScope(DV->getScope());
  processType(DV->getType().resolve(TypeIdentifierMap));
}

void DebugInfoFinder::processValue(const Module &M, const DbgValueInst *DVI) {
  auto *N = dyn_cast<MDNode>(DVI->getVariable());
  if (!N)
    return;
  InitializeTypeMap(M);

  auto *DV = dyn_cast<DILocalVariable>(N);
  if (!DV)
    return;

  if (!NodesSeen.insert(DV).second)
    return;
  processScope(DV->getScope());
  processType(DV->getType().resolve(TypeIdentifierMap));
}

bool DebugInfoFinder::addType(DIType *DT) {
  if (!DT)
    return false;

  if (!NodesSeen.insert(DT).second)
    return false;

  TYs.push_back(const_cast<DIType *>(DT));
  return true;
}

bool DebugInfoFinder::addCompileUnit(DICompileUnit *CU) {
  if (!CU)
    return false;
  if (!NodesSeen.insert(CU).second)
    return false;

  CUs.push_back(CU);
  return true;
}

bool DebugInfoFinder::addGlobalVariable(DIGlobalVariable *DIG) {
  if (!DIG)
    return false;

  if (!NodesSeen.insert(DIG).second)
    return false;

  GVs.push_back(DIG);
  return true;
}

bool DebugInfoFinder::addSubprogram(DISubprogram *SP) {
  if (!SP)
    return false;

  if (!NodesSeen.insert(SP).second)
    return false;

  SPs.push_back(SP);
  return true;
}

bool DebugInfoFinder::addScope(DIScope *Scope) {
  if (!Scope)
    return false;
  // FIXME: Ocaml binding generates a scope with no content, we treat it
  // as null for now.
  if (Scope->getNumOperands() == 0)
    return false;
  if (!NodesSeen.insert(Scope).second)
    return false;
  Scopes.push_back(Scope);
  return true;
}

bool llvm::stripDebugInfo(Function &F) {
  bool Changed = false;
  for (BasicBlock &BB : F) {
    for (Instruction &I : BB) {
      if (I.getDebugLoc()) {
        Changed = true;
        I.setDebugLoc(DebugLoc());
      }
    }
  }
  return Changed;
}

bool llvm::StripDebugInfo(Module &M) {
  bool Changed = false;

  // Remove all of the calls to the debugger intrinsics, and remove them from
  // the module.
  if (Function *Declare = M.getFunction("llvm.dbg.declare")) {
    while (!Declare->use_empty()) {
      CallInst *CI = cast<CallInst>(Declare->user_back());
      CI->eraseFromParent();
    }
    Declare->eraseFromParent();
    Changed = true;
  }

  if (Function *DbgVal = M.getFunction("llvm.dbg.value")) {
    while (!DbgVal->use_empty()) {
      CallInst *CI = cast<CallInst>(DbgVal->user_back());
      CI->eraseFromParent();
    }
    DbgVal->eraseFromParent();
    Changed = true;
  }

  for (Module::named_metadata_iterator NMI = M.named_metadata_begin(),
         NME = M.named_metadata_end(); NMI != NME;) {
    NamedMDNode *NMD = NMI;
    ++NMI;
    if (NMD->getName().startswith("llvm.dbg.")) {
      NMD->eraseFromParent();
      Changed = true;
    }
  }

  for (Function &F : M)
    Changed |= stripDebugInfo(F);

  if (GVMaterializer *Materializer = M.getMaterializer())
    Materializer->setStripDebugInfo();

  return Changed;
}

/// \brief Helper class to map -g metadata to -gline-tables-only metadata
///
class GLTOMapper {
  DenseMap<Metadata *, Metadata *> Replacements;

public:
  // Empty nodes
  MDNode *EmptyNode;
  MDNode *EmptySubroutineType;
private:

  // Remember what linkage name we originally had before stripping. If we end up
  // making two subprograms identical who originally had different linkage
  // names, then we need to make one of them distinct, to avoid them getting
  // uniqued. Maps the new node to the old linkage name.
  DenseMap<DISubprogram *, StringRef> NewToLinkageName;

  // TODO: Remember the distinct subprogram we created for a given linkage name,
  // so that we can continue to unique whenever possible. Map <newly created
  // node, old linkage name> to the first (possibly distinct) mdsubprogram
  // created for that combination. This is not strictly needed for correctness,
  // but can cut down on the number of MDNodes and let us diff cleanly with the
  // output of -gline-tables-only.

public:
  GLTOMapper(LLVMContext &C)
      : EmptyNode(MDNode::get(C, {})),
        EmptySubroutineType(DISubroutineType::get(C, 0, EmptyNode)) {}

  Metadata *map(Metadata *M) {
    if (!M)
      return nullptr;
    if (Replacements.count(M))
      return Replacements.lookup(M);

    return M;
  }
  MDNode *mapNode(Metadata *N) { return dyn_cast_or_null<MDNode>(map(N)); }

  // Recursively remap N and all its referenced children. Does a DF post-order
  // traversal, so as to remap bottoms up.
  void traverseAndRemap(MDNode *N) { traverse(N); }

private:
  // Create a new DISubprogram, to replace the one given
  DISubprogram *getReplacementSubprogram(DISubprogram *MDS) {
    auto FileAndScope = cast_or_null<DIFile>(map(MDS->getFile()));
    auto LinkageName = MDS->getName().empty() ? MDS->getLinkageName() : "";
    DISubprogram *Declaration = nullptr;
    auto Type = cast_or_null<DISubroutineType>(map(MDS->getType()));
    auto ContainingType = map(MDS->getContainingType());
    auto Variables = cast<MDTuple>(EmptyNode);
    auto Function = MDS->getFunction();
    auto TemplateParams = nullptr;

    // Make a distinct DISubprogram, for situations that warrent it.
    auto distinctMDSubprogram = [&]() {
      return DISubprogram::getDistinct(
          MDS->getContext(), FileAndScope->getRef(), MDS->getName(),
          LinkageName, FileAndScope, MDS->getLine(), Type, MDS->isLocalToUnit(),
          MDS->isDefinition(), MDS->getScopeLine(), DITypeRef(ContainingType),
          MDS->getVirtuality(), MDS->getVirtualIndex(), MDS->getFlags(),
          MDS->isOptimized(), Function, TemplateParams, Declaration, Variables);
    };

    if (MDS->isDistinct())
      return distinctMDSubprogram();

    auto NewMDS = DISubprogram::get(
        MDS->getContext(), FileAndScope->getRef(), MDS->getName(), LinkageName,
        FileAndScope, MDS->getLine(), Type, MDS->isLocalToUnit(),
        MDS->isDefinition(), MDS->getScopeLine(), DITypeRef(ContainingType),
        MDS->getVirtuality(), MDS->getVirtualIndex(), MDS->getFlags(),
        MDS->isOptimized(), Function, TemplateParams, Declaration, Variables);

    auto OldLinkageName = MDS->getLinkageName();

    // See if we need to make a distinct one
    if (NewToLinkageName.count(NewMDS)) {
      if (NewToLinkageName.lookup(NewMDS) == OldLinkageName)
        // We're good
        return NewMDS;

      // Otherwise, need to make a distinct one
      // TODO: Query the map to see if we already have one
      return distinctMDSubprogram();
    }

    NewToLinkageName.insert({NewMDS, MDS->getLinkageName()});
    return NewMDS;
  }

  // Create a new compile unit, to replace the one given
  DICompileUnit *getReplacementCU(DICompileUnit *CU) {
    // Drop skeleton CUs.
    if (CU->getDWOId())
      return nullptr;

    auto EmissionKind = 2; // -g-line-tables-only
    auto *File = cast_or_null<DIFile>(map(CU->getFile()));
    auto *EnumTypes = cast<MDTuple>(EmptyNode);
    auto *RetainedTypes = cast<MDTuple>(EmptyNode);
    auto *Subprograms = cast_or_null<MDTuple>(map(CU->getSubprograms().get()));
    auto *GlobalVariables = cast<MDTuple>(EmptyNode);
    auto *ImportedEntities = cast<MDTuple>(EmptyNode);
    return DICompileUnit::getDistinct(
        CU->getContext(), CU->getSourceLanguage(), File, CU->getProducer(),
        CU->isOptimized(), CU->getFlags(), CU->getRuntimeVersion(),
        CU->getSplitDebugFilename(), EmissionKind, EnumTypes, RetainedTypes,
        Subprograms, GlobalVariables, ImportedEntities, CU->getDWOId());
  }

  DILocation *getReplacementMDLocation(DILocation *MLD) {
    auto Scope = map(MLD->getScope());
    auto InlinedAt = map(MLD->getInlinedAt());
    if (MLD->isDistinct())
      return DILocation::getDistinct(MLD->getContext(), MLD->getLine(),
                                     MLD->getColumn(), Scope, InlinedAt);
    return DILocation::get(MLD->getContext(), MLD->getLine(), MLD->getColumn(),
                           Scope, InlinedAt);
  }

  // Create a new generic MDNode, to replace the one given
  MDNode *getReplacementMDNode(MDNode *N) {
    SmallVector<Metadata *, 8> Ops;
    Ops.reserve(N->getNumOperands());
    for (auto &I : N->operands())
      if (I)
        Ops.push_back(map(I));
    auto Ret = MDNode::get(N->getContext(), Ops);
    return Ret;
  }

  // TODO: Any other subclass of node that we care about?

  // Attempt to re-map N to a newly created node
  void remap(MDNode *N) {
    if (Replacements.count(N))
      return;
    auto tryRemap = [&](MDNode * N) -> MDNode * {
      if (!N)
        return nullptr;
      if (auto MDSub = dyn_cast<DISubprogram>(N))
        return getReplacementSubprogram(MDSub);
      if (isa<DISubroutineType>(N))
        return EmptySubroutineType;
      if (auto CU = dyn_cast<DICompileUnit>(N))
        return getReplacementCU(CU);
      if (isa<DIFile>(N))
        return N;
      if (auto MDLB = dyn_cast<DILexicalBlockBase>(N))
        // Remap to our referenced scope (recursively)
        return mapNode(MDLB->getScope());
      if (auto MLD = dyn_cast<DILocation>(N))
        return getReplacementMDLocation(MLD);

      // Otherwise, if we see these, just drop them now. Not strictly necessary,
      // but this speeds things up a little
      if (isa<DINode>(N))
        return EmptyNode;

      return getReplacementMDNode(N);
    };
    if (auto NewN = tryRemap(N))
      Replacements[N] = NewN;
  }

  // Do the re-mapping traversal
  void traverse(MDNode *);
};

void GLTOMapper::traverse(MDNode *N) {
  if (!N || Replacements.count(N))
    return;

  // To avoid cycles, as well as for efficiency sake, we will sometimes prune
  // parts of the graph
  auto prune = [](MDNode *Parent, MDNode *Child) {
    if (auto MDS = dyn_cast<DISubprogram>(Parent))
      return Child == MDS->getVariables().get();
    return false;
  };

  SmallVector<MDNode *, 16> ToVisit;
  DenseSet<MDNode *> Opened;

  // Visit each node starting at N in post order, and map them
  ToVisit.push_back(N);
  while (!ToVisit.empty()) {
    auto N = ToVisit.back();
    if (Opened.count(N)) {
      // Close it
      remap(N);
      ToVisit.pop_back();
      continue;
    }
    Opened.insert(N);
    for (auto &I : N->operands())
      if (auto MDN = dyn_cast_or_null<MDNode>(I))
        if (!Opened.count(MDN) && !Replacements.count(MDN) && !prune(N, MDN))
          ToVisit.push_back(MDN);
  }
}

bool llvm::convertDebugInfoToLineTables(Module &M) {
  // In order to convert debug info to what -gline-tables-only would of created,
  // we need to do the following:
  //   1) Delete all debug intrinsics
  //   2) Delete all non-CU named metadata debug infos
  //   3) Create new DebugLocs for each instruction
  //   4) Create a new CU debug info, and similarly for every metadata node
  //      that's reachable from the CU debug info

  bool Changed = false;

  // 1) First off, delete the debug intrinsics
  if (Function *Declare = M.getFunction("llvm.dbg.declare")) {
    while (!Declare->use_empty()) {
      auto *DDI = cast<DbgDeclareInst>(Declare->user_back());
      DDI->eraseFromParent();
    }
    Declare->eraseFromParent();
    Changed = true;
  }
  if (Function *DbgVal = M.getFunction("llvm.dbg.value")) {
    while (!DbgVal->use_empty()) {
      auto *DVI = cast<DbgValueInst>(DbgVal->user_back());
      DVI->eraseFromParent();
    }
    DbgVal->eraseFromParent();
    Changed = true;
  }

  // 2) Delete non-cu debug infos
  for (Module::named_metadata_iterator NMI = M.named_metadata_begin(),
                                       NME = M.named_metadata_end();
       NMI != NME;) {
    NamedMDNode *NMD = NMI;
    ++NMI;
    // Specifically keep dbg.cu around
    if (NMD->getName() == "llvm.dbg.cu")
      continue;

    if (NMD->getName().startswith("llvm.dbg.")) {
      NMD->eraseFromParent();
      Changed = true;
    }
  }

  GLTOMapper Mapper(M.getContext());

  // 3) Rewrite the DebugLocs to be equivalent to what -gline-tables-only would
  // of created
  for (auto &F : M) {
    for (auto &BB : F) {
      for (auto &I : BB) {
        if (I.getDebugLoc() == DebugLoc())
          continue;

        // Make a replacement
        auto &DL = I.getDebugLoc();
        auto Scope = DL.getScope();
        auto InlinedAt = DL.getInlinedAt();

        // remap scope
        if (Scope) {
          Mapper.traverseAndRemap(Scope);
          auto NewScope = Mapper.mapNode(Scope);
          Changed |= Scope != NewScope;
          Scope = NewScope;
        }

        // remap inlinedAt
        if (InlinedAt) {
          Mapper.traverseAndRemap(InlinedAt);
          auto NewIA = Mapper.mapNode(InlinedAt);
          Changed |= InlinedAt != NewIA;
          InlinedAt = cast_or_null<DILocation>(NewIA);
        }

        I.setDebugLoc(
            DebugLoc::get(DL.getLine(), DL.getCol(), Scope, InlinedAt));
      }
    }
  }

  // 4) Create a new llvm.dbg.cu, which is equivalent to the one
  // -gline-tables-only would have created.
  for (auto &NMD : M.getNamedMDList()) {
    SmallVector<MDNode *, 8> Ops;
    for (MDNode *Op : NMD.operands()) {
      Mapper.traverseAndRemap(Op);
      auto *NewOp = Mapper.mapNode(Op);
      Changed |= NewOp != Op;
      Ops.push_back(NewOp);
    }

    if (Changed) {
      NMD.dropAllReferences();
      for (auto *Op : Ops)
        if (Op)
          NMD.addOperand(Op);
    }
  }

  return Changed;
}

unsigned llvm::getDebugMetadataVersionFromModule(const Module &M) {
  if (auto *Val = mdconst::dyn_extract_or_null<ConstantInt>(
          M.getModuleFlag("Debug Info Version")))
    return Val->getZExtValue();
  return 0;
}

DenseMap<const llvm::Function *, DISubprogram *>
llvm::makeSubprogramMap(const Module &M) {
  DenseMap<const Function *, DISubprogram *> R;

  NamedMDNode *CU_Nodes = M.getNamedMetadata("llvm.dbg.cu");
  if (!CU_Nodes)
    return R;

  for (MDNode *N : CU_Nodes->operands()) {
    auto *CUNode = cast<DICompileUnit>(N);
    for (auto *SP : CUNode->getSubprograms()) {
      if (Function *F = SP->getFunction())
        R.insert(std::make_pair(F, SP));
    }
  }
  return R;
}
