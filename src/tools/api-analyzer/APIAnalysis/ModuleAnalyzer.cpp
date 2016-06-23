//===- APIUsageAnalysis.cpp - API Usage Analysis --------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the API usage analysis
//
//===----------------------------------------------------------------------===//

#include "ModuleAnalyzer.h"

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCTargetAsmParser.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"

#include <vector>
using namespace llvm;

//===----------------------------------------------------------------------===//
//  APIUsageAnalysis Implementation
//===----------------------------------------------------------------------===//

int AnalyzeModuleImpl(Module *M, APIAnalysisIntermediateResult &result,
                      const APIAnalysisOptions &options);

int AnalyzeModule(StringRef &filePath, APIAnalysisIntermediateResult &result,
                  const APIAnalysisOptions &options) {
  LLVMContext &Context = getGlobalContext();

  SMDiagnostic Err;
  std::unique_ptr<Module> M = parseIRFile(filePath, Err, Context);

  if (!M) {
    errs() << "Could not load module: " << Err.getMessage() << "\n";
    return 1;
  }

  return AnalyzeModuleImpl(M.get(), result, options);
}

int AnalyzeModule(llvm::MemoryBufferRef &data,
                  APIAnalysisIntermediateResult &result,
                  const APIAnalysisOptions &options) {
  LLVMContext &Context = getGlobalContext();

  SMDiagnostic Err;
  std::unique_ptr<Module> M = parseIR(data, Err, Context);

  if (!M) {
    errs() << "Could not load module: " << Err.getMessage() << "\n";
    return 1;
  }

  return AnalyzeModuleImpl(M.get(), result, options);
}

template <class ContainerType>
void InsertUnique(ContainerType &container, StringRef key) {
  auto I = container.find(key);
  if (container.end() == I)
    container.insert(std::make_pair(key, false));
}

template <> void InsertUnique(StringSet<> &container, StringRef key) {
  auto I = container.find(key);
  if (container.end() == I)
    container.insert(key);
}

StringRef TrimNull(StringRef str) {
  if (str.endswith(StringRef("\0")))
    return str.drop_back(1);
  return str;
}

User *GlobalOpToUser(Value *v) {
  auto base_user = dyn_cast<User>(v);
  assert(base_user && "Provided value is not a user!");
  auto op0 = dyn_cast<User>(base_user->getOperand(0));
  assert(op0 && "User doesn't have User operands!");
  return op0;
}

void HandleObjCMethods(Module *M, User *objc_meth_list,
                       StringMap<bool> &methods) {
  for (auto &op : objc_meth_list->operands()) {
    User *objc_meth = dyn_cast<User>(op.get());
    if (!objc_meth)
      continue;
    auto name =
        GetUnderlyingObject(objc_meth->getOperand(0), M->getDataLayout(), 2);
    auto func =
        GetUnderlyingObject(objc_meth->getOperand(2), M->getDataLayout(), 2);
    GlobalVariable *nameVar = dyn_cast<GlobalVariable>(name);
    if (!nameVar)
      continue;
    ConstantDataSequential *CD =
        dyn_cast<ConstantDataSequential>(nameVar->getInitializer());
    if (!CD)
      continue;
    auto methName = TrimNull(CD->getAsString());
    auto methodIt = methods.find(methName);
    if (methodIt == methods.end()) {
      methods.insert(std::make_pair(methName, false));
      methodIt = methods.find(methName);
    }
    methodIt->second = methodIt->second || !isa<ConstantPointerNull>(func);
  }
}

class SymbolStreamer : public MCStreamer {
  APIAnalysisIntermediateResult &result;

  void visitUsedSymbol(const MCSymbol &Sym) override {
    if (Sym.isUndefined())
      InsertUnique(result.asmSymbols, Sym.getName());
  }

public:
  SymbolStreamer(MCContext &Context, APIAnalysisIntermediateResult &res)
      : MCStreamer(Context), result(res) {}

  // required methods I don't care about.
  bool EmitSymbolAttribute(MCSymbol *, MCSymbolAttr) override { return true; }
  void EmitZerofill(MCSection *, MCSymbol *, uint64_t, unsigned) override {}
  void EmitCommonSymbol(MCSymbol *, uint64_t, unsigned) override {}
};

void HandleInlineAsm(Module *M, Function &F,
                     APIAnalysisIntermediateResult &result) {
  const DataLayout &DL = M->getDataLayout();

  StringRef TT(M->getTargetTriple());
  std::string Err;
  const Target *T = TargetRegistry::lookupTarget(TT.str(), Err);
  if (!T)
    return;

  std::unique_ptr<MCRegisterInfo> MRI(T->createMCRegInfo(TT.str()));
  if (!MRI)
    return;

  std::unique_ptr<MCAsmInfo> MAI(T->createMCAsmInfo(*MRI, TT.str()));
  if (!MAI)
    return;

  std::unique_ptr<MCSubtargetInfo> STI(
      T->createMCSubtargetInfo(TT.str(), "", ""));
  if (!STI)
    return;

  std::unique_ptr<MCInstrInfo> MCII(T->createMCInstrInfo());
  if (!MCII)
    return;

  MCObjectFileInfo MOFI;
  MCContext MCCtx(MAI.get(), MRI.get(), &MOFI);
  MOFI.InitMCObjectFileInfo(Triple(TT), Reloc::Default, CodeModel::Default, MCCtx);
  std::unique_ptr<SymbolStreamer> Streamer(new SymbolStreamer(MCCtx, result));
  T->createNullTargetStreamer(*Streamer);

  for (auto &bb : F) {
    for (auto &i : bb) {
      auto callInst = dyn_cast<CallInst>(&i);
      if (!callInst || !callInst->isInlineAsm())
        continue;
      auto str = GetUnderlyingObject(callInst->getOperand(0), DL, 2);
      auto *asmInst = dyn_cast<InlineAsm>(str);
      if (!asmInst)
        continue;
      std::unique_ptr<MemoryBuffer> Buffer(
          MemoryBuffer::getMemBuffer(asmInst->getAsmString()));
      SourceMgr SrcMgr;
      SrcMgr.AddNewSourceBuffer(std::move(Buffer), SMLoc());
      std::unique_ptr<MCAsmParser> Parser(
          createMCAsmParser(SrcMgr, MCCtx, *Streamer, *MAI));

      MCTargetOptions MCOptions;
      std::unique_ptr<MCTargetAsmParser> TAP(
          T->createMCAsmParser(*STI, *Parser, *MCII, MCOptions));
      if (!TAP)
        continue;

      Parser->setTargetParser(*TAP);
      if (Parser->Run(false))
        continue;
    }
  }

  const std::string &InlineAsm = M->getModuleInlineAsm();
  if (InlineAsm.empty())
    return;

  std::unique_ptr<MemoryBuffer> Buffer(MemoryBuffer::getMemBuffer(InlineAsm));
  SourceMgr SrcMgr;
  SrcMgr.AddNewSourceBuffer(std::move(Buffer), SMLoc());
  std::unique_ptr<MCAsmParser> Parser(
      createMCAsmParser(SrcMgr, MCCtx, *Streamer, *MAI));

  MCTargetOptions MCOptions;
  std::unique_ptr<MCTargetAsmParser> TAP(
      T->createMCAsmParser(*STI, *Parser, *MCII, MCOptions));
  if (!TAP)
    return;

  Parser->setTargetParser(*TAP);
  if (Parser->Run(false))
    return;
}

bool IsUsed(GlobalVariable &GV) {
  SmallVector<User *, 8> worklist;
  SmallPtrSet<User *, 8> visited;
  worklist.push_back(&GV);
  visited.insert(&GV);
  while (!worklist.empty()) {
    User *item = worklist.back();
    if (isa<Instruction>(item))
      return true;
    worklist.pop_back();
    for (User *user : item->users())
      if (visited.insert(user).second)
        worklist.push_back(user);
  }
  return false;
}

void TraverseObjCClassList(GlobalVariable &GV, Module *M,
                           APIAnalysisIntermediateResult &result) {
  auto classList = dyn_cast<User>(GlobalOpToUser(GV.getInitializer()));
  assert(classList &&
         "__objc_classlist should be a User because it is an array");
  for (auto &op : classList->operands()) {
    auto class_t = GlobalOpToUser(op);
    auto class_ro = GlobalOpToUser(class_t->getOperand(4));

    // read the class name
    auto class_name_global = dyn_cast<GlobalVariable>(
        GetUnderlyingObject(class_ro->getOperand(4), M->getDataLayout(), 2));
    assert(class_name_global && "Class Name must be a global");
    ConstantDataSequential *CD =
        dyn_cast<ConstantDataSequential>(class_name_global->getInitializer());
    assert(CD && "Class Name must be a ConstantDataSequential");
    StringRef className = TrimNull(CD->getAsString());

    // read instance methods
    auto method_list_global = dyn_cast<User>(
        GetUnderlyingObject(class_ro->getOperand(5), M->getDataLayout(), 2));
    if (method_list_global->getNumOperands() != 0) {
      auto objc_meth_list =
          dyn_cast<User>(GlobalOpToUser(method_list_global)->getOperand(2));
      HandleObjCMethods(M, objc_meth_list, result.instanceMethods[className]);
    }

    // read class methods
    auto metaclass_t = GlobalOpToUser(class_t->getOperand(0));
    auto metaclass_ro = GlobalOpToUser(metaclass_t->getOperand(4));

    method_list_global = dyn_cast<User>(GetUnderlyingObject(
        metaclass_ro->getOperand(5), M->getDataLayout(), 2));
    if (method_list_global->getNumOperands() != 0) {
      auto objc_meth_list =
          dyn_cast<User>(GlobalOpToUser(method_list_global)->getOperand(2));
      HandleObjCMethods(M, objc_meth_list, result.classMethods[className]);
    }

    // Get super class name
    auto superclass_t_global = dyn_cast<GlobalVariable>(
        GetUnderlyingObject(class_t->getOperand(1), M->getDataLayout(), 2));
    StringRef superClassName;
    if (superclass_t_global->hasExternalLinkage()) {
      // Will match OBJC_CLASS_$_*
      auto symbol_name = superclass_t_global->getName();
      auto start = symbol_name.find('$') + 2;
      superClassName = symbol_name.substr(start);
    } else {
      auto superclass_t = GlobalOpToUser(superclass_t_global->getOperand(0));
      auto superclass_ro = GlobalOpToUser(superclass_t->getOperand(4));
      // read the class name
      auto superclass_name_global =
          dyn_cast<GlobalVariable>(GetUnderlyingObject(
              superclass_ro->getOperand(4), M->getDataLayout(), 2));
      assert(superclass_name_global && "Class Name must be a global");
      CD = dyn_cast<ConstantDataSequential>(
          superclass_name_global->getInitializer());
      assert(CD && "Class Name must be a ConstantDataSequential");
      superClassName = TrimNull(CD->getAsString());
    }
    result.superClasses.insert(std::make_pair(className, superClassName));
  }
}

void TraverseObjCProtoList(GlobalVariable &GV, Module *M,
                           APIAnalysisIntermediateResult &result) {
  auto protocol_t = GlobalOpToUser(
      GetUnderlyingObject(GV.getInitializer(), M->getDataLayout(), 2));

  // read the protocol name
  auto protocol_name_global = dyn_cast<GlobalVariable>(
      GetUnderlyingObject(protocol_t->getOperand(1), M->getDataLayout(), 2));
  assert(protocol_name_global && "Protocol Name must be a global");
  ConstantDataSequential *CD =
      dyn_cast<ConstantDataSequential>(protocol_name_global->getInitializer());
  assert(CD && "Protocol Name must be a ConstantDataSequential");
  StringRef protocolName = TrimNull(CD->getAsString());

  auto method_list_global = dyn_cast<User>(
      GetUnderlyingObject(protocol_t->getOperand(3), M->getDataLayout(), 2));
  if (method_list_global->getNumOperands() != 0) {
    auto objc_meth_list =
        dyn_cast<User>(GlobalOpToUser(method_list_global)->getOperand(2));
    HandleObjCMethods(M, objc_meth_list,
                      result.protocolInstanceMethods[protocolName]);
  }

  method_list_global = dyn_cast<User>(
      GetUnderlyingObject(protocol_t->getOperand(4), M->getDataLayout(), 2));
  if (method_list_global->getNumOperands() != 0) {
    auto objc_meth_list =
        dyn_cast<User>(GlobalOpToUser(method_list_global)->getOperand(2));
    HandleObjCMethods(M, objc_meth_list,
                      result.protocolClassMethods[protocolName]);
  }
}

void TraverseObjCCategoryList(GlobalVariable &GV, Module *M,
                              APIAnalysisIntermediateResult &result) {
  auto categoryList = dyn_cast<User>(GV.getInitializer());
  assert(categoryList &&
         "__objc_classlist should be a User because it is an array");
  for (auto &op : categoryList->operands()) {
    auto category_t_global = GlobalOpToUser(op);
    auto category_t = GlobalOpToUser(category_t_global);

    ConstantDataSequential *CD;

    // read the category name
    auto cat_name_global = dyn_cast<GlobalVariable>(
        GetUnderlyingObject(category_t->getOperand(0), M->getDataLayout(), 2));
    assert(cat_name_global && "Category name must be a global");
    CD = dyn_cast<ConstantDataSequential>(cat_name_global->getInitializer());
    assert(CD && "Category name must be a ConstantDataSequential");
    StringRef catName = TrimNull(CD->getAsString());

    // Get extended class name
    auto class_t_global = dyn_cast<GlobalVariable>(
        GetUnderlyingObject(category_t->getOperand(1), M->getDataLayout(), 2));
    StringRef className;
    if (class_t_global->hasExternalLinkage()) {
      // Will match OBJC_CLASS_$_*
      auto symbol_name = class_t_global->getName();
      auto start = symbol_name.find('$') + 2;
      className = symbol_name.substr(start);
    } else {
      auto class_t = GlobalOpToUser(class_t_global->getOperand(0));
      auto class_ro = GlobalOpToUser(class_t->getOperand(4));
      // read the class name
      auto class_name_global = dyn_cast<GlobalVariable>(
          GetUnderlyingObject(class_ro->getOperand(4), M->getDataLayout(), 2));
      assert(class_name_global && "Class Name must be a global");
      CD =
          dyn_cast<ConstantDataSequential>(class_name_global->getInitializer());
      assert(CD && "Class Name must be a ConstantDataSequential");
      className = TrimNull(CD->getAsString());
    }

    auto method_list_global = dyn_cast<User>(
        GetUnderlyingObject(category_t->getOperand(2), M->getDataLayout(), 2));
    if (method_list_global->getNumOperands() != 0) {
      auto objc_meth_list =
          dyn_cast<User>(GlobalOpToUser(method_list_global)->getOperand(2));
      HandleObjCMethods(M, objc_meth_list,
                        result.categoryInstanceMethods[className][catName]);
    }

    method_list_global = dyn_cast<User>(
        GetUnderlyingObject(category_t->getOperand(3), M->getDataLayout(), 2));
    if (method_list_global->getNumOperands() != 0) {
      auto objc_meth_list =
          dyn_cast<User>(GlobalOpToUser(method_list_global)->getOperand(2));
      HandleObjCMethods(M, objc_meth_list,
                        result.categoryClassMethods[className][catName]);
    }
  }
}

int AnalyzeModuleImpl(Module *M, APIAnalysisIntermediateResult &result,
                      const APIAnalysisOptions &options) {
  // get selectors called via normal obj-c syntax
  for (auto &GV : M->globals()) {
    if (GV.hasSection()) {
      StringRef Section = GV.getSection();
      if (Section == "__TEXT,__objc_methname,cstring_literals") {
        assert(!GV.isExternallyInitialized() &&
               "objc_methname values can't be externally initialized");
        if (ConstantDataSequential *CD =
                dyn_cast<ConstantDataSequential>(GV.getInitializer())) {
          // if (IsUsed(GV)) {
          result.messageNames.insert(TrimNull(CD->getAsString()));
          //}
        }
      } else if (Section == "__TEXT,__objc_classname,cstring_literals") {
        assert(!GV.isExternallyInitialized() &&
               "objc_classrefs values can't be externally initialized");
        if (ConstantDataSequential *CD =
                dyn_cast<ConstantDataSequential>(GV.getInitializer())) {
          StringRef className = TrimNull(CD->getAsString());
          auto I = result.classNames.find(className);
          if (result.classNames.end() == I)
            result.classNames.insert(std::make_pair(className, true));
          else
            I->second = true;
        }
      } else if (Section ==
                 "__DATA, __objc_classlist, regular, no_dead_strip") {
        TraverseObjCClassList(GV, M, result);
      } else if (Section ==
                 "__DATA, __objc_protolist, coalesced, no_dead_strip") {
        TraverseObjCProtoList(GV, M, result);
      } else if (Section ==
                 "__DATA, __objc_catlist, regular, no_dead_strip") {
        TraverseObjCCategoryList(GV, M, result);
      }
      continue;
    }
    StringRef variableName;
    if (GV.getName().startswith("OBJC_CLASS_$_")) {
      variableName = GV.getName().substr(StringRef("OBJC_CLASS_$_").size());
      InsertUnique(result.classNames, variableName);
    } else if (GV.getName().startswith("OBJC_METACLASS_$_")) {
      variableName = GV.getName().substr(StringRef("OBJC_METACLASS_$_").size());
      InsertUnique(result.classNames, variableName);
    } else if (GV.hasName() && !GV.hasHiddenVisibility()) {
      variableName = GV.getName();
      InsertUnique(result.globals, variableName);
    }
  }

  Mangler mangler;

  // get function names
  for (auto &F : M->functions()) {
    StringRef Name;
    if (options.demangle)
      Name = F.getName();
    else {
      SmallString<256> NameSS;
      mangler.getNameWithPrefix(NameSS, &F, true);
      Name = NameSS;
      /*if (NameSS[0] == '_')
        Name = NameSS.substr(1);*/
    }

    auto I = result.functionNames.find(Name);

    // Keep track of functions that are declared vs defined. Declared
    // functions
    // that are not defined anywhere will become unresolved external
    // references.
    if (result.functionNames.end() == I) {
      result.functionNames.insert(std::make_pair(Name, !F.isDeclaration()));
    } else {
      I->second = I->second || !F.isDeclaration();
    }

    // Track NSSelectorFromString calls to find selectors referenced by
    // NSString
    // literals. This is a bit fragile based on the CFString ABI.
    if (options.scanSelFromString && "NSSelectorFromString" == F.getName()) {
      for (auto &u : F.uses()) {
        auto op = u.getUser()->getOperand(0)->stripPointerCasts();
        if (GlobalValue *GV = dyn_cast<GlobalValue>(op)) {
          if (ConstantStruct *CS =
                  dyn_cast<ConstantStruct>(GV->getOperand(0))) {
            auto str =
                GetUnderlyingObject(CS->getOperand(2), M->getDataLayout(), 2);
            if (GlobalVariable *GV = dyn_cast<GlobalVariable>(str))
              if (ConstantDataSequential *CD =
                      dyn_cast<ConstantDataSequential>(GV->getInitializer()))
                result.messageNames.insert(TrimNull(CD->getAsString()));
          }
        }
      }
    }

    if (options.scanDLSym && "dlsym" == F.getName()) {
      for (auto &u : F.uses()) {
        auto str = GetUnderlyingObject(u.getUser()->getOperand(1),
                                       M->getDataLayout(), 2);
        if (GlobalVariable *GV = dyn_cast<GlobalVariable>(str))
          if (ConstantDataSequential *CD =
                  dyn_cast<ConstantDataSequential>(GV->getInitializer()))
            result.functionNames.insert(
                std::make_pair(TrimNull(CD->getAsString()), false));
      }
    }

    // Handle inline asm
    HandleInlineAsm(M, F, result);
  }
  return 0;
}
