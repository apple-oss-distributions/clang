//===-- dsymutil.cpp - Debug info dumping utility for llvm ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This program is a utility that aims to be a dropin replacement for
// Darwin's dsymutil.
//
//===----------------------------------------------------------------------===//

#include "BinaryHolder.h"
#include "DebugMap.h"
#include "dsymutil.h"
#include "MachOUtils.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Config/config.h"
#include "llvm/Object/MachO.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileUtilities.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Options.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include <list>
#include <string>
// FIXME: Remove this and thread permissions through llvm's API.
#include <sys/stat.h>

#ifdef __APPLE__ // Used to get bundle info and some path utilities
  #include <CoreFoundation/CoreFoundation.h>
  #include <mach-o/dyld.h>
  #include <uuid/uuid.h>
  #include "CFStringUtils.h"
  #include "CFUtils.h"
  #include "CFBundle.h"
  #include "CFString.h"
#endif

#define DEBUG_TYPE "dsymutil"

using namespace llvm::dsymutil;

namespace {
using namespace llvm::cl;

OptionCategory DsymCategory("Specific Options");
static opt<bool> Help("h", desc("Alias for -help"), Hidden);
static opt<bool> Version("v", desc("Alias for -version"), Hidden);

static list<std::string> InputFiles(Positional, ZeroOrMore,
    desc("<input files>"), cat(DsymCategory));

static opt<std::string> OutputFileOpt("o",
    desc("Specifies an alternate path to place the .dSYM bundle. The default\n"
         "dSYM bundle path is created by appending '.dSYM' to the executable\n"
         "name."), value_desc("filename"), cat(DsymCategory));
static alias OutputFileOptA("out",
    desc("Alias for -o"), aliasopt(OutputFileOpt));

static opt<std::string> OsoPrependPath("oso-prepend-path",
    desc("Specifies a path to prepend to all debug symbol object file paths."),
    value_desc("path"), cat(DsymCategory));

static opt<bool> DumpStab("symtab",
    desc("Dumps the symbol table found in executable or object file(s) and\n"
         "exits."), init(false), cat(DsymCategory));
static alias DumpStabA("s", desc("Alias for --symtab"), aliasopt(DumpStab));

static opt<bool> FlatOut("flat",
    desc("Produce a flat dSYM file (not a bundle)."), init(false),
    cat(DsymCategory));
static alias FlatOutA("f", desc("Alias for --flat"), aliasopt(FlatOut));

static opt<bool> Minimize("minimize",
    desc("When used when creating a dSYM file, this option will suppress\n"
         "the emission of the .debug_inlines, .debug_pubnames, and\n"
         ".debug_pubtypes sections since dsymutil currently has better\n"
         "equivalents: .apple_names and .apple_types. When used in\n"
         "conjunction with --update option, this option will cause redundant\n"
         "accelerator tables to be removed."), init(false), cat(DsymCategory));
static alias MinimizeA("z", desc("Alias for --minimize"), aliasopt(Minimize));

static opt<bool> Update("update",
    desc("Updates existing dSYM files to contain the latest accelerator\n"
         "tables and other DWARF optimizations. This option will currently\n"
         "add the new .apple_names and .apple_types hashed accelerator\n"
         "tables."), init(false), cat(DsymCategory));
static alias UpdateA("u", desc("Alias for --update"), aliasopt(Update));

static opt<std::string> SymbolMap("symbol-map",
    desc("Updates the existing dsyms inplace using symbol map specified."),
    value_desc("bcsymbolmap"), cat(DsymCategory));

static opt<unsigned> Threads("threads",
    desc("Specifies the maximum number (n) of simultaneous threads to use\n"
         "when linking multiple architectures."), init(0), cat(DsymCategory));
static alias ThreadsA("t", desc("Alias for --threads"), aliasopt(Threads));

static opt<bool> Verbose("verbose",
    desc("Display verbose information when linking."), init(false),
    cat(DsymCategory));

static opt<bool> NoOutput("no-output",
    desc("Do the link in memory, but do not emit the result file."),
    init(false), cat(DsymCategory));

static list<std::string> ArchFlags("arch",
    desc("Link DWARF debug information only for specified CPU architecture\n"
         "types.  Architectures may be specified by name or by number. When\n"
         "using this option, an error will be returned if any architectures\n"
         "can not be properly linked.  This option can be specified multiple\n"
         "times, once for each desired architecture.  All cpu architectures\n"
         "will be linked by default and any architectures that can't be\n"
         "properly linked will not cause dsymutil to return an error as long\n"
         "as at least one architecture was able to link successfully."),
         ZeroOrMore, cat(DsymCategory));

static opt<bool> NoODR("no-odr",
    desc("Do not use ODR (One Definition Rule) for type uniquing."),
    init(false), cat(DsymCategory));

static opt<bool> ParseOnly("parse-only",
    desc("Only parse the debug map, do not actaully link the DWARF."),
    init(false), cat(DsymCategory));
}

static void getBundleInfo(llvm::StringRef ExePath,
                          std::string &bundleVersionStr,
                          std::string &bundleShortVersionStr,
                          std::string &bundleIDStr,
                          bool &OmitShortVersion) {
#ifdef __APPLE__
	CFTypeRef cf = NULL;
	CFTypeID cf_type_id = 0;

  // Try and find the original executable's Info.plist information using
  // CoreFoundation calls by creating a URL for the executable and chopping off
  // the last path component. The CFBundle can then get the identifier and grab
  // any needed information from it directly.
	if (!ExePath.empty() && llvm::sys::fs::exists(ExePath)) {
    const char *exe_path = ExePath.data();
		CFBundle bundle(exe_path);
		CFStringRef bundleID = bundle.GetIdentifier();
		if (bundleID != NULL)
		{
			CFString::UTF8(bundleID, bundleIDStr);
			cf = bundle.GetValueForInfoDictionaryKey(CFSTR("CFBundleVersion"));
			if (cf != NULL)
			{
				cf_type_id = ::CFGetTypeID(cf);
				if (cf_type_id == ::CFStringGetTypeID())
					CFString::UTF8 ((CFStringRef)cf, bundleVersionStr);
				else
				{
					CFString cf_type_id_cfstr(::CFCopyTypeIDDescription (cf_type_id));
					std::string cf_type_id_str;
					llvm::errs() << "The Info.plist key \"CFBundleVersion\" is a "
                       << cf_type_id_cfstr.UTF8(cf_type_id_str)
                       << "but it should be a string in: "
                       << exe_path << ".\n";
				}
			}
			cf = bundle.GetValueForInfoDictionaryKey(
                                        CFSTR("CFBundleShortVersionString"));
			if (cf != NULL)
			{
				cf_type_id = ::CFGetTypeID(cf);
				if (::CFGetTypeID(cf) == ::CFStringGetTypeID())
					CFString::UTF8 ((CFStringRef)cf, bundleShortVersionStr);
				else
				{
					CFString cf_type_id_cfstr(::CFCopyTypeIDDescription (cf_type_id));
					std::string cf_type_id_str;
          llvm::errs() << "The Info.plist key \"CFBundleShortVersionString\" is"
                       << "a " << cf_type_id_cfstr.UTF8(cf_type_id_str)
                       << ", but it should be a string in: " << exe_path
                       << ".\n";
				}
			}
			OmitShortVersion = bundleShortVersionStr.empty();
		}
	}

#endif
	if (bundleVersionStr.empty())
		bundleVersionStr = "1";

	if (bundleShortVersionStr.empty() && !OmitShortVersion)
		bundleShortVersionStr = "1.0";
}

static bool createPlistFile(llvm::StringRef Bin, llvm::StringRef BundleRoot) {
  // Create plist file to write to.
  llvm::SmallString<128> InfoPlist(BundleRoot);
  llvm::sys::path::append(InfoPlist, "Contents/Info.plist");
  std::error_code EC;
  llvm::raw_fd_ostream PL(InfoPlist, EC, llvm::sys::fs::F_Text);
  if (EC) {
    llvm::errs() << "warning: error creating plist file "
                 << InfoPlist << " -- " << EC.message() << '\n';
    return false;
  }

  // Use Apple specific bundle handling, which may find version and bundle
  // information in case there's a bundle for the original input binary.
  std::string bundleVersionStr, bundleShortVersionStr, bundleIDStr;
  bool OmitShortVersion = false;
  getBundleInfo(Bin, bundleVersionStr, bundleShortVersionStr, bundleIDStr,
                OmitShortVersion);

  // If not found any bundle ID, fallback to the output name without the
  // .dSYM extension.
  if (bundleIDStr.empty()) {
    llvm::StringRef BundleID = *llvm::sys::path::rbegin(BundleRoot);
    if (llvm::sys::path::extension(BundleRoot).endswith(".dSYM"))
      bundleIDStr.assign(BundleID.drop_back(5).str());
    else
      bundleIDStr.assign(BundleID.str());
  }

  // Print out information to the plist file.
  PL << "<?xml version=\"1.0\" encoding=\"UTF-8\"\?>\n"
     << "<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" "
     << "\"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
     << "<plist version=\"1.0\">\n"
     << "\t<dict>\n"
     << "\t\t<key>CFBundleDevelopmentRegion</key>\n"
     << "\t\t<string>English</string>\n"
     << "\t\t<key>CFBundleIdentifier</key>\n"
     << "\t\t<string>com.apple.xcode.dsym." << bundleIDStr << "</string>\n"
     << "\t\t<key>CFBundleInfoDictionaryVersion</key>\n"
     << "\t\t<string>6.0</string>\n"
     << "\t\t<key>CFBundlePackageType</key>\n"
     << "\t\t<string>dSYM</string>\n"
     << "\t\t<key>CFBundleSignature</key>\n"
     << "\t\t<string>\?\?\?\?</string>\n";

  if (!OmitShortVersion)
     PL << "\t\t<key>CFBundleShortVersionString</key>\n"
        << "\t\t<string>" << bundleShortVersionStr << "</string>\n";

  PL << "\t\t<key>CFBundleVersion</key>\n"
     << "\t\t<string>" << bundleVersionStr << "</string>\n"
     << "\t</dict>\n"
     << "</plist>\n";

  PL.close();
  return true;
}

static std::error_code create_directory(const llvm::Twine &path,
                                        bool IgnoreExisting = true) {
  llvm::SmallString<128> path_storage;
  llvm::StringRef p = path.toNullTerminatedStringRef(path_storage);

  if (::mkdir(p.begin(), S_IRWXU | S_IRWXG | S_IRWXO) == -1) {
    if (errno != EEXIST || !IgnoreExisting)
      return std::error_code(errno, std::generic_category());
  }

  return std::error_code();
}

static std::error_code create_directories(const llvm::Twine &Path,
                                          bool IgnoreExisting = true) {
  llvm::SmallString<128> PathStorage;
  llvm::StringRef P = Path.toStringRef(PathStorage);

  // Be optimistic and try to create the directory
  std::error_code EC = create_directory(P, IgnoreExisting);
  // If we succeeded, or had any error other than the parent not existing, just
  // return it.
  if (EC != std::errc::no_such_file_or_directory)
    return EC;

  // We failed because of a no_such_file_or_directory, try to create the
  // parent.
  llvm::StringRef Parent = llvm::sys::path::parent_path(P);
  if (Parent.empty())
    return EC;

  if ((EC = create_directories(Parent)))
      return EC;

  return create_directory(P, IgnoreExisting);
}

static bool createBundleDir(llvm::StringRef BundleBase,
                            std::string &BundleLeafDir) {
  llvm::SmallString<128> Bundle(BundleBase.data());
  llvm::sys::path::append(Bundle, "Contents" ,"Resources", "DWARF");
  if (std::error_code EC = create_directories(Bundle.str())) {
    llvm::errs() << "Cannot create directory " << Bundle
                 << ": " << EC.message() << "\n";
    return false;
  }

  DEBUG(
    llvm::errs() << "Created bundle at " << Bundle << "\n";
  );

  BundleLeafDir.assign(Bundle.c_str());
  return true;
}

static bool getOutputDwarfPath(const std::string &InputFile,
                               std::string &Output,
                               bool IsTmpDwarf = false) {
  if (OutputFileOpt == "-") {
    Output = OutputFileOpt;
    return true;
  }

  bool UseDwarfFileOnly = FlatOut | IsTmpDwarf;
  Output = InputFile == "-" ? "a.out" : InputFile.c_str();
  std::string InputFileName = llvm::sys::path::filename(Output);

  if (!OutputFileOpt.empty()) {
    Output = OutputFileOpt;
  } else {
    if (FlatOut)
      Output += ".dwarf";
    else
      Output += ".dSYM";
  }

  // Use .dwarf and .dSYM extension if no user specified output dir or if
  // we're generating a temporary dwarf file.
  if (IsTmpDwarf) {
    if (!FlatOut && llvm::sys::fs::is_directory(Output))
      Output += (llvm::Twine("/") + InputFileName).str();

    Output += ".tmp%%%%%%.dwarf";
    int FD;
    llvm::SmallString<128> UniqueFile;
    // We use the version of createUniqueFile that actually opens the
    // file because the whole point is to prevent other processes from
    // creating the same file. The version of createUniqueFile that
    // only returns a unique name doesn't create the file, thus
    // making this logic racy. To be honest, this other version seems
    // pretty useless in a real world usage.
    if (auto EC = llvm::sys::fs::createUniqueFile(Output, FD, UniqueFile)) {
      llvm::errs() << "Failed to create temporary outfile '" << Output << "': "
                   << EC.message() << '\n';
      return false;
    }
    Output = UniqueFile.str();
    llvm::sys::RemoveFileOnSignal(Output);
    // Close the file immediately. We know it is unique. It will be
    // reopened and renamed later.
    llvm::raw_fd_ostream CloseImmediately(FD, true /* shouldClose */, true);
  }
  // Let names be properly formed before exiting since temporary output
  // files are automatically removed and we can avoid special handling
  // in such cases.
  if (NoOutput || ParseOnly)
    return true;

  // Mimic behavior from the old driver and warn the user in case there's
  // no .dSYM extension in the output file path. FIXME: disabled for now
  // since it fail tests...
  //if (!UseDwarfFileOnly && !OutputFileOpt.empty() &&
  //    !llvm::sys::path::extension(Output).endswith(".dSYM"))
  //  llvm::errs() << "warning: output path specified does not have a '.dSYM' "
  //               << "extension and may not found by lldb: '"
  //               << Output << "'\n";

  if (!IsTmpDwarf && OutputFileOpt.empty() && (Update || !SymbolMap.empty())) {
    // When updating, do in place replacement.
    Output = InputFile;
  } else if (!UseDwarfFileOnly) {
    // Create dSYM bundle and Info.plist file.
    std::string BundleLeafDir;
    if (!createBundleDir(Output, BundleLeafDir))
      return false;

    if (!createPlistFile(InputFile, Output))
      return false;

    // The output location for the dwarf in the bundle
    Output.assign(BundleLeafDir + "/" + InputFileName);
  }

  DEBUG(
    llvm::errs() << "Output dwarf path at " << Output << "\n"
  );

  return true;
}

static void getSDKPath(char **argv, std::string &SDKPath) {
  std::string Path = llvm::sys::fs::getMainExecutable(argv[0],
                                                      (void *)(uintptr_t)&getSDKPath);
  SDKPath = llvm::sys::path::parent_path(Path);
}


static void loadSymbolMap(std::string SymbolMapFilename,
                          LinkOptions &Options, llvm::StringRef InputFile,
                          const DebugMap &Map) {
  static std::vector<std::string> UnobfuscatedStrings;
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> ErrOrMemBuffer(nullptr);
#if __APPLE__
  // Look through the UUID Map
  if (llvm::sys::fs::is_directory(SymbolMapFilename) &&
      !Map.getUUID().empty()) {
    uuid_string_t UUIDString;
    uuid_unparse_upper((uint8_t*)Map.getUUID().data(), UUIDString);
    std::string plistFilename = (llvm::sys::path::parent_path(InputFile) +
      llvm::Twine("/../") + UUIDString + ".plist").str();

    CFStringRef plistFile = CFStringCreateWithCString(kCFAllocatorDefault,
                                                      plistFilename.c_str(),
                                                      kCFStringEncodingUTF8);
    CFURLRef fileURL = CFURLCreateWithFileSystemPath(kCFAllocatorDefault,
                           plistFile, kCFURLPOSIXPathStyle, false);
    CFReadStreamRef resourceData =
                      CFReadStreamCreateWithFile(kCFAllocatorDefault, fileURL);
    if (resourceData) {
      CFReadStreamOpen(resourceData);
      CFDictionaryRef plist = (CFDictionaryRef)
          CFPropertyListCreateWithStream(kCFAllocatorDefault, resourceData, 0,
                                         kCFPropertyListImmutable, NULL, NULL);

      if (plist) {
        if (CFDictionaryContainsKey(plist, CFSTR("DBGOriginalUUID"))) {
          CFStringRef OldUUID = (CFStringRef)
                CFDictionaryGetValue(plist, CFSTR("DBGOriginalUUID"));
          SymbolMapFilename += (llvm::Twine("/") +
                                llvm::StringRef(
                                  CFStringGetCStringPtr(OldUUID,
                                                  kCFStringEncodingUTF8)) +
                                ".bcsymbolmap").str();
        }
        CFRelease(plist);
      }
      CFReadStreamClose(resourceData);
      CFRelease(resourceData);
    }
    CFRelease(fileURL);
    CFRelease(plistFile);
  }
#endif

  // If the SymbolMap is not a directory, use the file as symbolmap.
  if (!llvm::sys::fs::is_directory(SymbolMapFilename)) {
    ErrOrMemBuffer = llvm::MemoryBuffer::getFile(SymbolMapFilename);
  } else {
    SymbolMapFilename +=
      (llvm::Twine("/") + llvm::sys::path::filename(InputFile) +
       "-" + getArchName(Map.getTriple()) + ".bcsymbolmap").str();
    ErrOrMemBuffer = llvm::MemoryBuffer::getFile(SymbolMapFilename);
  }

  UnobfuscatedStrings.clear();
  if (auto EC = ErrOrMemBuffer.getError()) {
    llvm::errs() << "warning: " << SymbolMapFilename << ": "
                 << EC.message() << ". Not unobfuscating.\n";
    return;
  }

  auto &MemBuf = **ErrOrMemBuffer;
  llvm::StringRef Data(MemBuf.getBufferStart(),
                       MemBuf.getBufferEnd() - MemBuf.getBufferStart());
  llvm::StringRef LHS;
  // Check version string first.
  std::tie(LHS, Data) = Data.split('\n');
  if (!LHS.startswith("BCSymbolMap Version:")) {
    // Version string not present, warns but try to parse it.
    llvm::errs() << "warning: " << SymbolMapFilename
                 << " is missing version string. Assuming 1.0.\n";
    UnobfuscatedStrings.emplace_back(LHS);
  } else if (!LHS.equals("BCSymbolMap Version: 1.0")) {
    llvm::StringRef VersionNum;
    std::tie(LHS, VersionNum) = LHS.split(':');
    llvm::errs() << "warning: " << SymbolMapFilename
                 << " has unsupported symbol map version"
                 << VersionNum << ". Not unobfuscating.\n";
    return;
  }
  while (!Data.empty()) {
    std::tie(LHS, Data) = Data.split('\n');
    UnobfuscatedStrings.emplace_back(LHS);
  }

  if (Options.Translator)
    return;

  Options.Translator = [](llvm::StringRef Input) -> llvm::StringRef {
    if (!Input.startswith("__hidden#") && !Input.startswith("___hidden#"))
        return Input;
    unsigned LineNumber = UINT_MAX;
    bool MightNeedUnderscore = false;
    llvm::StringRef Line = Input.drop_front(sizeof("__hidden#") - 1);
    if (Line[0] == '#') {
      Line = Line.drop_front();
      MightNeedUnderscore = true;
    }
    Line.split('_').first.getAsInteger(10, LineNumber);
    if (LineNumber >= UnobfuscatedStrings.size()) {
      llvm::errs() << "warning: reference to a unexisting unobfuscated string "
                   << Input.str() << ". "
                   << "Symbol map mismatch?\n"
                   << Line << '\n';

      return Input;
    }

    const std::string &Translation = UnobfuscatedStrings[LineNumber];
    if (!MightNeedUnderscore)
      return Translation;
    // Objective C symbols for the Macho symbol table start with that
    // weird \1 character. Do not preprend an underscore to these and
    // drop that initial \1.
    if (Translation[0] == 1)
      return llvm::StringRef(Translation).drop_front();

    // We need permanent storage for the string we are about to
    // create. Just append it to the vector containing
    // translations. This should only happen during MachO symbol table
    // translation, thus there should be no risk on exponential growth.
    UnobfuscatedStrings.emplace_back("_" + Translation);
    return UnobfuscatedStrings.back();
  };
}

void llvm::dsymutil::exitDsymutil(int ExitStatus) {
  llvm::sys::RunInterruptHandlers();
  exit(ExitStatus);
}

int main(int argc, char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram StackPrinter(argc, argv);
  llvm::llvm_shutdown_obj Shutdown;
  LinkOptions Options;
  std::string ErrMsg, SDKPath;
  getSDKPath(argv, SDKPath);

  HideUnrelatedOptions(DsymCategory);

  ParseCommandLineOptions(
    argc, argv,
    "manipulate archived DWARF debug symbol files.\n\n"
    "dsymutil links the DWARF debug information found in the object files\n"
    "for an executable in <input files> by using debug symbols information\n"
    "contained in its symbol table.\n");

  if (Help)
    PrintHelpMessage();

  if (Version) {
    llvm::cl::PrintVersionMessage();
    return 0;
  }

  Options.Verbose = Verbose;
  Options.NoOutput = NoOutput;
  Options.NoODR = NoODR;
  Options.Minimize = Minimize;
  Options.Update = Update;
  Options.PrependPath = OsoPrependPath;

  if (!SymbolMap.empty())
    Options.Update = true;

  if (Options.Update &&
      std::find(InputFiles.begin(), InputFiles.end(), "-") != InputFiles.end()) {
    // FIXME: We cannot use stdin for an update because stdin will be
    // consumed by the BinaryHolder during the debugmap parsing, and
    // then we will want to consume it again in DwarfLinker. If we
    // used a unique BinaryHolder object that could cache multiple
    // binaries this restriction would go away.
    llvm::errs() << "error: standard input cannot be used as input for a "
                 << "dSYM update.\n";
    exitDsymutil(1);
  }

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllTargets();
  llvm::InitializeAllAsmPrinters();

  if (InputFiles.empty())
    InputFiles.push_back("a.out");
  else if (Options.Update) {
    // If we are updating, we might get dSYM bundles as input.
    std::vector<std::string> OrigInputFiles = InputFiles;
    InputFiles.clear();
    for (const auto &Input : OrigInputFiles) {
      if (!llvm::sys::fs::is_directory(Input)) {
        InputFiles.push_back(Input);
        continue;
      }
      std::string dSYMDir = Input + "/Contents/Resources/DWARF";
      if (!llvm::sys::fs::is_directory(dSYMDir)) {
        llvm::errs() << "error: " << Input << " is a directory, but doesn't "
                     << "look like a dSYM bundle.\n";
        exitDsymutil(1);
      }
      std::error_code EC;
      llvm::sys::fs::directory_iterator DirIt(dSYMDir, EC);
      llvm::sys::fs::directory_iterator DirEnd;
      if (EC) {
        llvm::errs() << "error: " << dSYMDir << ": " << EC.message() << '\n';
        exitDsymutil(1);
      }

      while (DirIt != DirEnd) {
        InputFiles.push_back(DirIt->path());
        DirIt.increment(EC);
        if (EC) {
          llvm::errs() << "error: " << dSYMDir << ": " << EC.message() << '\n';
          exitDsymutil(1);
        }
      }
    }
  }

  if (InputFiles.size() > 1 && !SymbolMap.empty() &&
      !llvm::sys::fs::is_directory(SymbolMap)) {
    llvm::errs() << "error: when unobfuscating multiple files, --symbol-map "
                 << "needs to point to a directory.\n";
    exitDsymutil(1);
  }

  // Don't support multiple input files with the -f option.
  if (FlatOut && InputFiles.size() > 1 && !OutputFileOpt.empty()) {
    llvm::errs() << "Unsupported options: multiple input files"
      " with both -o and -f" << "\n";
    return 1;
  }

  for (const auto &Arch : ArchFlags)
    if (Arch != "*" && Arch != "all" &&
        !llvm::object::MachOObjectFile::isValidArch(Arch)) {
      llvm::errs() << "error: Unsupported cpu architecture: '"
                   << Arch << "'\n";
      exitDsymutil(1);
    }

  for (auto &InputFile : InputFiles) {
    // Dump the symbol table for each input file and requested arch
    if (DumpStab) {
      if (!dumpStab(InputFile, ArchFlags, OsoPrependPath))
        exitDsymutil(1);
      continue;
    }

    auto ErrOrDebugMaps = parseDebugMap(InputFile, ArchFlags, OsoPrependPath,
                                        Verbose);
    if (auto EC = ErrOrDebugMaps.getError()) {
      llvm::errs() << "error: cannot parse the debug map for \"" << InputFile
                   << "\": " << EC.message() << '\n';
      exitDsymutil(1);
    }

    if (Options.Update) {
      // The debug map should be empty. Add one object file
      // corresponding to the input file.
      for (auto &Map : *ErrOrDebugMaps)
        Map->addDebugMapObject(InputFile, llvm::sys::TimeValue::MinTime());

      if (InputFiles.size() > 1 && !SymbolMap.empty() &&
          !llvm::sys::fs::is_directory(SymbolMap)) {
        llvm::errs() << "error: when unobfuscating fat binaries, --symbol-map "
                     << "needs to point to a directory.\n";
        exitDsymutil(1);
      }
    }

    // Link each separated architecture into a separated thin file.
    typedef llvm::dsymutil::MachOUtils::ArchAndFilename ArchAndFilename;
    llvm::SmallVector<ArchAndFilename, 4> ThinDwarfList;
    for (auto &Map : *ErrOrDebugMaps) {
      if (Verbose)
        Map->print(llvm::outs());

      if (ParseOnly)
        continue;

      if (!SymbolMap.empty())
        loadSymbolMap(SymbolMap, Options, InputFile, *Map);

      std::string ThinMachO, ThinMachODwarf;

      if (!getOutputDwarfPath(InputFile, ThinMachODwarf, true) ||
          !linkDwarf(ThinMachODwarf, *Map, Options))
        exitDsymutil(1);

      ThinDwarfList.push_back({ getArchName(Map->getTriple()),
                                   ThinMachODwarf });
    }

    if (NoOutput || ParseOnly || DumpStab)
      continue;

    if (ThinDwarfList.empty()) {
      llvm::errs() << "warning: no link output for " << InputFile << '\n';
      continue;
    }

    // Merge thin linked dwarf files into a final .dSYM or .dwarf
    std::string FinalLinkedOut;
    if (!getOutputDwarfPath(InputFile, FinalLinkedOut) ||
        !MachOUtils::mergeThinFiles(SDKPath, ThinDwarfList, FinalLinkedOut))
      exitDsymutil(1);
  }

  exitDsymutil(0);
  llvm_unreachable("exitDsymutil didn't exit");
}
