//===-LTOCodeGenerator.h - LLVM Link Time Optimizer -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the LTOCodeGenerator class.
//
//   LTO compilation consists of three phases: Pre-IPO, IPO and Post-IPO.
//
//   The Pre-IPO phase compiles source code into bitcode file. The resulting
// bitcode files, along with object files and libraries, will be fed to the
// linker to through the IPO and Post-IPO phases. By using obj-file extension,
// the resulting bitcode file disguises itself as an object file, and therefore
// obviates the need of writing a special set of the make-rules only for LTO
// compilation.
//
//   The IPO phase perform inter-procedural analyses and optimizations, and
// the Post-IPO consists two sub-phases: intra-procedural scalar optimizations
// (SOPT), and intra-procedural target-dependent code generator (CG).
//
//   As of this writing, we don't separate IPO and the Post-IPO SOPT. They
// are intermingled together, and are driven by a single pass manager (see
// PassManagerBuilder::populateLTOPassManager()).
//
//   The "LTOCodeGenerator" is the driver for the IPO and Post-IPO stages.
// The "CodeGenerator" here is bit confusing. Don't confuse the "CodeGenerator"
// with the machine specific code generator.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LTO_LTOCODEGENERATOR_H
#define LLVM_LTO_LTOCODEGENERATOR_H

#include "llvm-c/lto.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Support/Obfuscation.h"
#include "llvm/Target/TargetOptions.h"
#include <string>
#include <vector>

namespace llvm {
  class LLVMContext;
  class DiagnosticInfo;
  class GlobalValue;
  class Mangler;
  class MemoryBuffer;
  class TargetLibraryInfo;
  class TargetMachine;
  class raw_ostream;
  class raw_pwrite_stream;

//===----------------------------------------------------------------------===//
/// C++ class which implements the opaque lto_code_gen_t type.
///
struct LTOCodeGenerator {
  static const char *getVersionString();

  LTOCodeGenerator();
  LTOCodeGenerator(std::unique_ptr<LLVMContext> Context);
  ~LTOCodeGenerator();

  /// Merge given module.  Return true on success.
  bool addModule(struct LTOModule *);

  /// Set the destination module.
  void setModule(std::unique_ptr<LTOModule> M);

  void setTargetOptions(TargetOptions Options);
  void setDebugInfo(lto_debug_model);
  void setCodePICModel(Reloc::Model Model) { RelocModel = Model; }

  void setCpu(const char *MCpu) { this->MCpu = MCpu; }
  void setAttr(const char *MAttr) { this->MAttr = MAttr; }
  void setOptLevel(unsigned OptLevel);

  void setShouldInternalize(bool Value) { ShouldInternalize = Value; }
  void setShouldEmbedUselists(bool Value) { ShouldEmbedUselists = Value; }

  void addMustPreserveSymbol(StringRef Sym) { MustPreserveSymbols[Sym] = 1; }

  /// Pass options to the driver and optimization passes.
  ///
  /// These options are not necessarily for debugging purpose (the function
  /// name is misleading).  This function should be called before
  /// LTOCodeGenerator::compilexxx(), and
  /// LTOCodeGenerator::writeMergedModules().
  void setCodeGenDebugOptions(const char *Opts);

  /// Parse the options set in setCodeGenDebugOptions.
  ///
  /// Like \a setCodeGenDebugOptions(), this must be called before
  /// LTOCodeGenerator::compilexxx() and
  /// LTOCodeGenerator::writeMergedModules().
  void parseCodeGenDebugOptions();

  /// Write the merged module to the file specified by the given path.  Return
  /// true on success.
  bool writeMergedModules(const char *Path, std::string &ErrMsg);

  /// Compile the merged module into a *single* object file; the path to object
  /// file is returned to the caller via argument "name". Return true on
  /// success.
  ///
  /// \note It is up to the linker to remove the intermediate object file.  Do
  /// not try to remove the object file in LTOCodeGenerator's destructor as we
  /// don't who (LTOCodeGenerator or the obj file) will last longer.
  bool compile_to_file(const char **Name, bool DisableVerify,
                       bool DisableInline, bool DisableGVNLoadPRE,
                       bool DisableVectorization, std::string &ErrMsg);

  /// As with compile_to_file(), this function compiles the merged module into
  /// single object file. Instead of returning the object-file-path to the
  /// caller (linker), it brings the object to a buffer, and return the buffer
  /// to the caller. This function should delete intermediate object file once
  /// its content is brought to memory. Return NULL if the compilation was not
  /// successful.
  std::unique_ptr<MemoryBuffer> compile(bool DisableVerify, bool DisableInline,
                                        bool DisableGVNLoadPRE,
                                        bool DisableVectorization,
                                        std::string &errMsg);

  /// Optimizes the merged module.  Returns true on success.
  bool optimize(bool DisableVerify, bool DisableInline, bool DisableGVNLoadPRE,
                bool DisableVectorization, std::string &ErrMsg);

  /// Compiles the merged optimized module into a single object file. It brings
  /// the object to a buffer, and returns the buffer to the caller. Return NULL
  /// if the compilation was not successful.
  std::unique_ptr<MemoryBuffer> compileOptimized(std::string &ErrMsg);

  // reset the codegen context
  void resetContext();

  // Hide all non-external symbols from the bitcode.
  bool hideSymbols();

  // Lookup obfuscation for a symbol
  const char* lookupHiddenName(const char* SymbolName);

  // Write the reverse mapping of hidden symbols from the stripped bitcode
  bool writeReverseMap(const char *Path);

  /// Compile the merged optimized module into out.size() object files each
  /// representing a linkable partition of the module. If out contains more
  /// than one element, code generation is done in parallel with out.size()
  /// threads.  Object files will be written to members of out. Returns true on
  /// success.
  bool compileOptimized(ArrayRef<raw_pwrite_stream *> Out, std::string &ErrMsg);

  void setDiagnosticHandler(lto_diagnostic_handler_t, void *);

  LLVMContext &getContext() { return *Context; }

private:
  void initializeLTOPasses();

  bool compileOptimizedToFile(const char **Name, std::string &ErrMsg);
  void applyScopeRestrictions();
  void applyRestriction(GlobalValue &GV, ArrayRef<StringRef> Libcalls,
                        std::vector<const char *> &MustPreserveList,
                        SmallPtrSetImpl<GlobalValue *> &AsmUsed,
                        Mangler &Mangler);
  bool determineTarget(std::string &ErrMsg);

  static void DiagnosticHandler(const DiagnosticInfo &DI, void *Context);

  void DiagnosticHandler2(const DiagnosticInfo &DI);

  typedef StringMap<uint8_t> StringSet;

  std::unique_ptr<LLVMContext> OwnedContext;
  LLVMContext *Context;
  std::unique_ptr<Module> MergedModule;
  Linker IRLinker;
  std::unique_ptr<TargetMachine> TargetMach;
  bool EmitDwarfDebugInfo = false;
  bool ScopeRestrictionsDone = false;
  Reloc::Model RelocModel = Reloc::Default;
  StringSet MustPreserveSymbols;
  StringSet AsmUndefinedRefs;
  std::vector<std::string> CodegenOptions;
  std::string FeatureStr;
  std::string MCpu;
  std::string MAttr;
  std::string NativeObjectPath;
  TargetOptions Options;
  CodeGenOpt::Level CGOptLevel = CodeGenOpt::Default;
  unsigned OptLevel = 2;
  lto_diagnostic_handler_t DiagHandler = nullptr;
  void *DiagContext = nullptr;
  bool ShouldInternalize = true;
  bool ShouldEmbedUselists = false;

  typedef StringMap<StringRef, BumpPtrAllocator&> MangleNameMap;
  std::unique_ptr<MangleNameMap> MangledNames;

  // For symbol hiding/obfuscation
  IncrementObfuscator IncrObfuscate;
};
}
#endif
