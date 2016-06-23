#include "llvm/APIAnalysis.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"

#include "ArchiveAnalyzer.h"
#include "BinaryAnalyzer.h"
#include "ModuleAnalyzer.h"
#include "XarAnalyzer.h"

#include <unistd.h>

using namespace llvm;

bool isXar(llvm::MemoryBufferRef &fileData) {
  return fileData.getBufferSize() > 4 && fileData.getBufferStart()[0] == 'x' &&
         fileData.getBufferStart()[1] == 'a' &&
         fileData.getBufferStart()[2] == 'r' &&
         fileData.getBufferStart()[3] == '!';
}

void PopulateResult(APIAnalysisIntermediateResult &intRes,
                    APIAnalysisResult &result) {
  for (auto &d : intRes.messageNames)
    result.messageNames.insert(d.first());

  for (auto &d : intRes.classNames) {
    if (d.second)
      result.internalClassNames.insert(d.first());
    else
      result.externalClassNames.insert(d.first());
  }

  for (auto &d : intRes.functionNames) {
    if (d.second)
      result.internalFunctions.insert(d.first());
    else
      result.externalFunctions.insert(d.first());
  }

  for (auto &d : intRes.globals) {
    if (d.second)
      result.internalGlobals.insert(d.first());
    else
      result.externalGlobals.insert(d.first());
  }

  for (auto &d : intRes.linkedLibraries)
    result.linkedLibraries.insert(d.first());

  for (auto &c : intRes.instanceMethods) {
    auto classInfo = result.objCClasses.find(c.first());
    if (classInfo == result.objCClasses.end()) {
      classInfo =
          result.objCClasses.insert(std::make_pair(c.first(), ObjCClassInfo()))
              .first;
    }
    for (auto &m : c.second) {
      if (m.second)
        classInfo->second.internalInstanceMethods.push_back(m.first());
      else
        classInfo->second.externalInstanceMethods.push_back(m.first());
    }
  }

  for (auto &c : intRes.classMethods) {
    auto classInfo = result.objCClasses.find(c.first());
    if (classInfo == result.objCClasses.end()) {
      classInfo =
          result.objCClasses.insert(std::make_pair(c.first(), ObjCClassInfo()))
              .first;
    }
    for (auto &m : c.second) {
      if (m.second)
        classInfo->second.internalClassMethods.push_back(m.first());
      else
        classInfo->second.externalClassMethods.push_back(m.first());
    }
  }

  for (auto &c : intRes.superClasses) {
    auto classInfo = result.objCClasses.find(c.first());
    if (classInfo == result.objCClasses.end()) {
      classInfo =
          result.objCClasses.insert(std::make_pair(c.first(), ObjCClassInfo()))
              .first;
    }
    classInfo->second.parentClass = c.second;
  }

  for (auto &c : intRes.protocolInstanceMethods) {
    auto classInfo = result.objCProtocols.find(c.first());
    if (classInfo == result.objCProtocols.end()) {
      classInfo = result.objCProtocols.insert(std::make_pair(c.first(),
                                                             ObjCClassInfo()))
                      .first;
    }
    for (auto &m : c.second) {
      if (m.second)
        classInfo->second.internalInstanceMethods.push_back(m.first());
      else
        classInfo->second.externalInstanceMethods.push_back(m.first());
    }
  }

  for (auto &c : intRes.protocolClassMethods) {
    auto classInfo = result.objCProtocols.find(c.first());
    if (classInfo == result.objCProtocols.end()) {
      classInfo = result.objCProtocols.insert(std::make_pair(c.first(),
                                                             ObjCClassInfo()))
                      .first;
    }
    for (auto &m : c.second) {
      if (m.second)
        classInfo->second.internalClassMethods.push_back(m.first());
      else
        classInfo->second.externalClassMethods.push_back(m.first());
    }
  }

  for (auto &cl : intRes.categoryInstanceMethods) {
    StringRef className = cl.first();
    for (auto &ca : cl.second) {
      StringRef catName = ca.first();
      auto pair = std::make_pair(className.str(), catName.str());
      auto classInfo = result.objCCategories.find(pair);
      if (classInfo == result.objCCategories.end()) {
        classInfo =
            result.objCCategories.insert(std::make_pair(pair, ObjCClassInfo()))
                .first;
        classInfo->second.parentClass = className.str();
      }
      for (auto &m : ca.second) {
        if (m.second)
          classInfo->second.internalInstanceMethods.push_back(m.first());
        else
          classInfo->second.externalInstanceMethods.push_back(m.first());
      }
    }
  }

  for (auto &cl : intRes.categoryClassMethods) {
    StringRef className = cl.first();
    for (auto &ca : cl.second) {
      StringRef catName = ca.first();
      auto pair = std::make_pair(className.str(), catName.str());
      auto classInfo = result.objCCategories.find(pair);
      if (classInfo == result.objCCategories.end()) {
        classInfo =
            result.objCCategories.insert(std::make_pair(pair, ObjCClassInfo()))
                .first;
        classInfo->second.parentClass = className.str();
      }
      for (auto &m : ca.second) {
        if (m.second)
          classInfo->second.internalClassMethods.push_back(m.first());
        else
          classInfo->second.externalClassMethods.push_back(m.first());
      }
    }
  }

  result.orderedLibraries = intRes.orderedLibraries;

  for (auto &s : intRes.asmSymbols)
    result.asmSymbols.push_back(s.first());
}

int AnalyzeFile(llvm::MemoryBufferRef &fileData, APIAnalysisResult &result,
                const APIAnalysisOptions &options) {
  APIAnalysisIntermediateResult intRes;
  int retVal = -1;
  if (isBitcode(fileData.getBuffer().bytes_begin(),
                fileData.getBuffer().bytes_end()))
    retVal = AnalyzeModule(fileData, intRes, options);
  else if (isXar(fileData))
    retVal = AnalyzeXar(fileData, intRes, options);
  else
    retVal = AnalyzeBinary(fileData, intRes, options);
  PopulateResult(intRes, result);
  return retVal;
}

int AnalyzeFile(StringRef &filePath, APIAnalysisResult &result,
                const APIAnalysisOptions &options) {
  APIAnalysisIntermediateResult intRes;
  int retVal = AnalyzeFileImpl(filePath, intRes, options);
  PopulateResult(intRes, result);
  return retVal;
}

int AnalyzeFileImpl(StringRef &filePath, APIAnalysisIntermediateResult &result,
                    const APIAnalysisOptions &options) {
  if (filePath.endswith(".ll") || filePath.endswith(".bc"))
    return AnalyzeModule(filePath, result, options);
  if (filePath.endswith(".a"))
    return AnalyzeArchive(filePath, result, options);
  if (filePath.endswith(".xar"))
    return AnalyzeXar(filePath, result, options);

  int TmpFD = -1;
  if (std::error_code EC = sys::fs::openFileForRead(filePath, TmpFD)) {
    errs() << "unable to open file: '" << EC.message() << "'";
    return 1;
  }
  unsigned char Buf[4];
  read(TmpFD, &Buf[0], 4);
  close(TmpFD);
  if (isBitcode(&Buf[0], &Buf[3]))
    return AnalyzeModule(filePath, result, options);
  return AnalyzeBinary(filePath, result, options);
}

int InitializeAnalyzer() {
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  return 0;
}
