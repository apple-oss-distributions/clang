
#include "BinaryAnalyzer.h"
#include "XarAnalyzer.h"
#include "ObjCStructs.h"

#include "llvm/Object/MachO.h"
#include "llvm/Object/MachOUniversal.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
using namespace object;

// Assume that __cxa_demangle is provided by libcxxabi (except for Windows).
extern "C" char *__cxa_demangle(const char *mangled_name, char *output_buffer,
                                size_t *length, int *status);

std::string DemangleName(const std::string &Name) {
  // We can spoil names of symbols with C linkage, so use an heuristic
  // approach to check if the name should be demangled.
  if (Name.substr(0, 3) != "__Z") {
    if (Name.substr(0, 1) == "_")
      return Name.substr(1);
    else
      return Name;
  }
  int status = 0;
  char *DemangledName =
      __cxa_demangle(Name.c_str() + 1, nullptr, nullptr, &status);
  if (status != 0)
    return Name;
  std::string Result = DemangledName;
  free(DemangledName);
  return Result;
}

int AnalyzeBinary(StringRef &filePath, APIAnalysisIntermediateResult &result,
                  const APIAnalysisOptions &options) {
  ErrorOr<OwningBinary<Binary>> BinaryOrErr = createBinary(filePath);
  if (std::error_code EC = BinaryOrErr.getError()) {
    errs() << "unable to read input: '" << EC.message() << "'\n";
    return 1;
  }
  Binary &binary = *BinaryOrErr.get().getBinary();
  return AnalyzeBinaryImpl(binary, result, options);
}

int AnalyzeBinary(llvm::MemoryBufferRef &fileData,
                  APIAnalysisIntermediateResult &result,
                  const APIAnalysisOptions &options) {
  ErrorOr<std::unique_ptr<Binary>> BinaryOrErr = createBinary(fileData);
  if (std::error_code EC = BinaryOrErr.getError()) {
    errs() << "unable to read input: '" << EC.message() << "'\n";
    return 1;
  }
  Binary &binary = *BinaryOrErr.get();
  return AnalyzeBinaryImpl(binary, result, options);
}

const char *TranslateOffset(uint64_t offset,
                            const MachOObjectFile *InputObject) {
  for (auto section : InputObject->sections()) {
    uint64_t sectAddress = section.getAddress();
    uint64_t sectSize = section.getSize();
    if (offset >= sectAddress && offset < sectAddress + sectSize) {
      auto sectOffset = offset - sectAddress;
      StringRef sectContents;
      section.getContents(sectContents);
      return sectContents.data() + sectOffset;
    }
  }
  return nullptr;
}

StringRef FindSymbol(uint64_t offset, const MachOObjectFile *InputObject) {
  for (auto section : InputObject->sections()) {
    uint64_t sectAddress = section.getAddress();
    for (auto bindEntry : InputObject->bindTable()) {
      uint64_t bindOffset = bindEntry.segmentOffset() + sectAddress;
      if (bindOffset == offset) {
        return bindEntry.symbolName();
      }
    }
  }
  return StringRef();
}

template <typename _uintptr_t>
void HandleObjCMethods(const MachOObjectFile *InputObject,
                       _uintptr_t method_list_offset,
                       StringMap<bool> &methods) {
  auto method_list_bytes = TranslateOffset(method_list_offset, InputObject);
  if (!method_list_bytes)
    return;
  auto method_list =
      reinterpret_cast<const _method_list_t<_uintptr_t> *>(method_list_bytes);

  auto method_list_start =
      method_list_bytes + sizeof(_method_list_t<_uintptr_t>);
  auto method_itr =
      reinterpret_cast<const _objc_method<_uintptr_t> *>(method_list_start);
  for (uint32_t idx = 0; idx < method_list->method_count; ++idx, ++method_itr) {
    const char *meth_name = TranslateOffset(method_itr->_cmd, InputObject);
    StringRef methName = StringRef(meth_name);
    auto methodIt = methods.find(methName);
    if (methodIt == methods.end()) {
      methodIt = methods.insert(std::make_pair(methName, false)).first;
    }
    methodIt->second = methodIt->second || (0 != method_itr->_imp);
  }
}

template <typename _uintptr_t>
void TraverseObjCClassList(uint64_t offset, const MachOObjectFile *InputObject,
                           APIAnalysisIntermediateResult &result) {
  auto class_t_bytes = TranslateOffset(offset, InputObject);
  auto class_t = reinterpret_cast<const _class_t<_uintptr_t> *>(class_t_bytes);

  auto class_ro_bytes = TranslateOffset(class_t->ro, InputObject);
  auto class_ro =
      reinterpret_cast<const _class_ro_t<_uintptr_t> *>(class_ro_bytes);

  const char *name = TranslateOffset(class_ro->name, InputObject);
  StringRef className = StringRef(name);

  HandleObjCMethods(InputObject, class_ro->baseMethods,
                    result.instanceMethods[className]);

  auto metaclass_t_bytes = TranslateOffset(class_t->isa, InputObject);
  auto metaclass_t =
      reinterpret_cast<const _class_t<_uintptr_t> *>(metaclass_t_bytes);

  auto metaclass_ro_bytes = TranslateOffset(metaclass_t->ro, InputObject);
  auto metaclass_ro =
      reinterpret_cast<const _class_ro_t<_uintptr_t> *>(metaclass_ro_bytes);

  HandleObjCMethods(InputObject, metaclass_ro->baseMethods,
                    result.classMethods[className]);

  StringRef superClassName;
  if (class_t->superclass) {
    auto superclass_t_bytes = TranslateOffset(class_t->superclass, InputObject);
    auto superclass_t =
        reinterpret_cast<const _class_t<_uintptr_t> *>(superclass_t_bytes);

    auto superclass_ro_bytes = TranslateOffset(superclass_t->ro, InputObject);
    auto superclass_ro =
        reinterpret_cast<const _class_ro_t<_uintptr_t> *>(superclass_ro_bytes);

    const char *superClsName =
        TranslateOffset(superclass_ro->name, InputObject);
    superClassName = StringRef(superClsName);
  } else {
    auto superclass_offset =
        offset + offsetof(_class_t<_uintptr_t>, superclass);
    auto symbolName = FindSymbol(superclass_offset, InputObject);
    auto start = symbolName.find('$') + 2;
    superClassName = symbolName.substr(start);
  }
  result.superClasses.insert(std::make_pair(className, superClassName));
}

template <typename _uintptr_t>
void TraverseObjCProtoList(uint64_t offset, const MachOObjectFile *InputObject,
                           APIAnalysisIntermediateResult &result) {
  auto protocol_t_bytes = TranslateOffset(offset, InputObject);
  auto protocol_t =
      reinterpret_cast<const _protocol_t<_uintptr_t> *>(protocol_t_bytes);

  const char *name = TranslateOffset(protocol_t->protocol_name, InputObject);
  StringRef protocolName = StringRef(name);

  HandleObjCMethods(InputObject, protocol_t->instance_methods,
                    result.protocolInstanceMethods[protocolName]);
  HandleObjCMethods(InputObject, protocol_t->class_methods,
                    result.protocolClassMethods[protocolName]);
}

template <typename _uintptr_t>
void TraverseObjCCatList(uint64_t offset, const MachOObjectFile *InputObject,
                         APIAnalysisIntermediateResult &result) {
  auto category_t_bytes = TranslateOffset(offset, InputObject);
  auto category_t =
      reinterpret_cast<const _category_t<_uintptr_t> *>(category_t_bytes);

  const char *name = TranslateOffset(category_t->name, InputObject);
  StringRef catName = StringRef(name);

  StringRef className;

  if (category_t->cls) {
    auto class_t_bytes = TranslateOffset(category_t->cls, InputObject);
    auto class_t =
        reinterpret_cast<const _class_t<_uintptr_t> *>(class_t_bytes);

    auto class_ro_bytes = TranslateOffset(class_t->ro, InputObject);
    auto class_ro =
        reinterpret_cast<const _class_ro_t<_uintptr_t> *>(class_ro_bytes);

    const char *clsName = TranslateOffset(class_ro->name, InputObject);
    className = StringRef(clsName);
  } else {
    auto class_offset = offset + offsetof(_category_t<_uintptr_t>, cls);
    auto symbolName = FindSymbol(class_offset, InputObject);
    auto start = symbolName.find('$') + 2;
    className = symbolName.substr(start);
  }

  HandleObjCMethods(InputObject, category_t->instance_methods,
                    result.categoryInstanceMethods[className][catName]);
  HandleObjCMethods(InputObject, category_t->class_methods,
                    result.categoryClassMethods[className][catName]);
}

int AnalyzeBinaryImpl(const MachOObjectFile *InputObject,
                      APIAnalysisIntermediateResult &result,
                      const APIAnalysisOptions &options) {
  // check to see if there is a bitcode section
  bool hasBitcode = false;
  for (auto s : InputObject->sections()) {
    StringRef name;
    s.getName(name);
    if (name == "__bundle")
      hasBitcode = true;
  }

  // if there is no bitcode section, this may be from an assembly file that was
  // hand coded, and since we support that we need to ignore bitcodeOnly
  bool bitcodeOnly = options.bitcodeOnly && hasBitcode;
  for (auto s : InputObject->symbols()) {
    section_iterator sec = s.getSection().get();

    // If the symbol has a section, and that section is not text or the symbol
    // has no flags -- skip it.
    if (sec != InputObject->section_end() &&
        (!sec->isText() || BasicSymbolRef::SF_None == s.getFlags()))
      continue;

    // If the symbol is format specific, skip it
    if (BasicSymbolRef::SF_FormatSpecific == s.getFlags())
      continue;

    auto type = s.getType();
    StringRef name = s.getName().get();
    bool declared = s.getFlags() & BasicSymbolRef::SF_Undefined;

    if (SymbolRef::ST_Function & type) {
      if (options.demangle)
        result.functionNames.insert(
            std::make_pair(DemangleName(name), !declared));
      else {
        result.functionNames.insert(std::make_pair(name, !declared));
      }
      continue;
    }
  }

  for (auto s : InputObject->sections()) {
    StringRef name;
    s.getName(name);

    if (name == "__bundle") {
      StringRef contents;
      if (std::error_code EC = s.getContents(contents)) {
        errs() << "Could not get section data: '" << EC.message() << "'";
        return 1;
      }
      MemoryBufferRef xarRef =
          MemoryBuffer::getMemBuffer(contents)->getMemBufferRef();
      AnalyzeXar(xarRef, result, options);
    }

    // If only looking at bitcode skip everything else.
    if (bitcodeOnly)
      continue;

    if (name == "__objc_methname") {
      StringRef contents;
      InputObject->getSectionContents(s.getRawDataRefImpl(), contents);

      size_t start = 0;
      size_t current = 0;
      size_t end = contents.size();
      for (; current < end; start = current + 1) {
        current = contents.find('\0', start);
        StringRef selector = contents.substr(start, current - start);
        if (0 < selector.size())
          result.messageNames.insert(selector);
      }
      continue;
    }
    if (name == "__objc_classname") {
      StringRef contents;
      InputObject->getSectionContents(s.getRawDataRefImpl(), contents);

      size_t start = 0;
      size_t current = 0;
      size_t end = contents.size();
      for (; current < end; start = current + 1) {
        current = contents.find('\0', start);
        StringRef className = contents.substr(start, current - start);
        if (className.empty())
          continue;
        auto I = result.classNames.find(className);
        if (result.classNames.end() == I)
          result.classNames.insert(std::make_pair(className, true));
        else
          I->second = true;
      }
      continue;
    }
    if (name == "__objc_classlist") {
      StringRef contents;
      InputObject->getSectionContents(s.getRawDataRefImpl(), contents);

      auto *itr = contents.bytes_begin();
      auto *end = contents.bytes_end();
      if (InputObject->is64Bit())
        for (; itr < end; itr += sizeof(uint64_t))
          TraverseObjCClassList<uint64_t>(
              *reinterpret_cast<const uint64_t *>(itr), InputObject, result);
      else
        for (; itr < end; itr += sizeof(uint32_t))
          TraverseObjCClassList<uint32_t>(
              *reinterpret_cast<const uint32_t *>(itr), InputObject, result);
      continue;
    }
    if (name == "__objc_protolist") {
      StringRef contents;
      InputObject->getSectionContents(s.getRawDataRefImpl(), contents);

      auto *itr = contents.bytes_begin();
      auto *end = contents.bytes_end();
      if (InputObject->is64Bit())
        for (; itr < end; itr += sizeof(uint64_t))
          TraverseObjCProtoList<uint64_t>(
              *reinterpret_cast<const uint64_t *>(itr), InputObject, result);
      else
        for (; itr < end; itr += sizeof(uint32_t))
          TraverseObjCProtoList<uint32_t>(
              *reinterpret_cast<const uint32_t *>(itr), InputObject, result);
      continue;
    }
    if (name == "__objc_catlist") {
      StringRef contents;
      InputObject->getSectionContents(s.getRawDataRefImpl(), contents);

      auto *itr = contents.bytes_begin();
      auto *end = contents.bytes_end();
      if (InputObject->is64Bit())
        for (; itr < end; itr += sizeof(uint64_t))
          TraverseObjCCatList<uint64_t>(
              *reinterpret_cast<const uint64_t *>(itr), InputObject, result);
      else
        for (; itr < end; itr += sizeof(uint32_t))
          TraverseObjCCatList<uint32_t>(
              *reinterpret_cast<const uint32_t *>(itr), InputObject, result);
      continue;
    }
  }

  // If only looking at bitcode skip everything else.
  if (bitcodeOnly)
    return 0;

  for (size_t i = 0; i < InputObject->getNumLibraries(); ++i) {
    StringRef name;
    if (std::error_code EC = InputObject->getLibraryNameByIndex(i, name)) {
      errs() << "Failed to read library name: " << EC.message();
      continue;
    }
    StringRef Suffix;
    bool isFramework;
    StringRef shortName =
        InputObject->guessLibraryShortName(name, isFramework, Suffix);
    if (name.endswith(".dylib"))
      Suffix = ".dylib";
    result.linkedLibraries.insert((shortName + Suffix).str());
  }
  return 0;
}

int AnalyzeBinaryImpl(Binary &binary, APIAnalysisIntermediateResult &result,
                      const APIAnalysisOptions &options) {
  const MachOUniversalBinary *UniversalBinary =
      dyn_cast<MachOUniversalBinary>(&binary);
  if (UniversalBinary) {
    int retCode = 0;
    for (auto &obj : UniversalBinary->objects()) {
      ErrorOr<std::unique_ptr<MachOObjectFile>> errorOrSlice =
          obj.getAsObjectFile();
      if (std::error_code EC = errorOrSlice.getError()) {
        outs() << "Could not read slice: '" << EC.message() << "'\n";
        retCode &= 1;
        continue;
      }
      retCode &= AnalyzeBinaryImpl(errorOrSlice.get().get(), result, options);
    }
    return retCode;
  }

  const MachOObjectFile *InputObject = dyn_cast<MachOObjectFile>(&binary);
  if (InputObject) {
    return AnalyzeBinaryImpl(InputObject, result, options);
  }

  errs() << "Unrecognized binary format!\n";
  return 1;
}
