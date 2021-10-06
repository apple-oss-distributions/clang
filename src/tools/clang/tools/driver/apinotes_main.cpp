//===-- api_notes.cpp - API Notes Driver ----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file 
/// This file provides conversion between the YAML (source) and binary forms 
/// of API notes.
///
//===----------------------------------------------------------------------===//
#include "clang/APINotes/APINotesYAMLCompiler.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/Triple.h"

using namespace llvm;
namespace api_notes = clang::api_notes;

int cc1apinotes_main(ArrayRef<const char *> Argv, const char *Argv0,
                     void *MainAddr) {

  // Mark all our options with this category, everything else (except for
  // -version and -help) will be hidden.
  static cl::OptionCategory APINotesCategory("API Notes options");

  static cl::opt<api_notes::ActionType>
  Action(cl::desc("Mode:"), cl::init(api_notes::ActionType::None),
         cl::values(
                    clEnumValN(api_notes::ActionType::YAMLToBinary,
                               "yaml-to-binary", 
                               "Convert YAML to binary format"),
                    clEnumValN(api_notes::ActionType::BinaryToYAML,
                               "binary-to-yaml",
                               "Convert binary format to YAML"),
                    clEnumValN(api_notes::ActionType::Dump,
                               "dump", 
                               "Parse and dump the output"),
                    clEnumValEnd),
         cl::cat(APINotesCategory));

  static cl::opt<std::string>
  InputFilename(cl::Positional, cl::desc("<input file>"),
                cl::Required, cl::cat(APINotesCategory));

  static cl::opt<std::string>
  Target("target", cl::desc("Generate binary format for the given target"),
                   cl::cat(APINotesCategory));

  static cl::opt<std::string>
  OutputFilename("o", cl::desc("Output file name"), cl::cat(APINotesCategory));

  cl::HideUnrelatedOptions(APINotesCategory);

  SmallVector<const char *, 4> Args;
  Args.push_back(Argv0);
  Args.append(Argv.begin(), Argv.end());
  cl::ParseCommandLineOptions(Args.size(),
                              Args.data(),
                              "Clang API Notes Tool\n");

  if (Action == clang::api_notes::ActionType::None) {
    errs() << "action required\n";
    cl::PrintHelpMessage();
    return 1;
  }

  auto fileBufOrErr = MemoryBuffer::getFile(InputFilename);
  if (std::error_code EC = fileBufOrErr.getError()) {
    llvm::errs() << "\n Could not open input file: " + EC.message() << '\n';
    return true;
  }
  StringRef input = fileBufOrErr.get()->getBuffer();

  switch (Action) {
  case api_notes::ActionType::None:
    llvm_unreachable("handled above");

  case api_notes::ActionType::YAMLToBinary: {
    if (OutputFilename.empty()) {
      errs() << "output file is required\n";
      cl::PrintHelpMessage();
      return 1;
    }

    api_notes::OSType targetOS = api_notes::OSType::Absent;
    // TODO: Check that we've specified the target.
    if (!Target.empty()) {
      llvm::Triple target(llvm::Triple::normalize(Target));
      switch (target.getOS()) {
        case llvm::Triple::Darwin:
        case llvm::Triple::MacOSX:
          targetOS = api_notes::OSType::OSX;
          break;
        case llvm::Triple::IOS:
          targetOS = api_notes::OSType::IOS;
          break;
        case llvm::Triple::WatchOS:
          targetOS = api_notes::OSType::WatchOS;
          break;
        case llvm::Triple::TvOS:
          targetOS = api_notes::OSType::TvOS;
          break;
        default:
          errs() << "target is not supported\n";
          return 1;
      }
    }
    std::error_code EC;
    llvm::raw_fd_ostream os(OutputFilename, EC,
                            llvm::sys::fs::OpenFlags::F_None);

    if (api_notes::compileAPINotes(input, os, targetOS))
      return 1;
    
    os.flush();

    return os.has_error();
  }

  case api_notes::ActionType::BinaryToYAML: {
    if (OutputFilename.empty()) {
      errs() << "output file required\n";
      cl::PrintHelpMessage();
      return 1;
    }

    std::error_code EC;
    llvm::raw_fd_ostream os(OutputFilename, EC,
                            llvm::sys::fs::OpenFlags::F_None);

    if (api_notes::decompileAPINotes(std::move(fileBufOrErr.get()), os))
      return 1;
    
    os.flush();

    return os.has_error();
  }

  case api_notes::ActionType::Dump:
    return api_notes::parseAndDumpAPINotes(input);
  }

  return 1;
}

