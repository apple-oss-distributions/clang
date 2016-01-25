//===- tools/dsymutil/MachODebugMapParser.cpp - Parse STABS debug maps ----===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "BinaryHolder.h"
#include "DebugMap.h"
#include "dsymutil.h"
#include "llvm/Object/MachO.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

namespace {
using namespace llvm;
using namespace llvm::dsymutil;
using namespace llvm::object;

class MachODebugMapParser {
public:
  MachODebugMapParser(StringRef BinaryPath,
                      ArrayRef<std::string> Archs,
                      StringRef PathPrefix = "", bool Verbose = false)
    : BinaryPath(BinaryPath), Archs(Archs.begin(), Archs.end()),
      PathPrefix(PathPrefix), MainBinaryHolder(Verbose),
      CurrentObjectHolder(Verbose), CurrentDebugMapObject(nullptr) {}

  /// \brief Parses and returns the DebugMap of the input binary.
  /// \returns an error in case the provided BinaryPath doesn't exist
  /// or isn't of a supported type.
  ErrorOr<std::vector<std::unique_ptr<DebugMap>>> parse();

  /// \brief Walk the symbol table and dump it.
  bool dumpStab();

private:
  std::string BinaryPath;
  SmallVector<StringRef, 1> Archs;
  std::string PathPrefix;

  /// Owns the MemoryBuffer for the main binary.
  BinaryHolder MainBinaryHolder;
  /// Map of the binary symbol addresses.
  StringMap<uint64_t> MainBinarySymbolAddresses;
  StringRef MainBinaryStrings;
  /// The constructed DebugMap.
  std::unique_ptr<DebugMap> Result;

  /// Owns the MemoryBuffer for the currently handled object file.
  BinaryHolder CurrentObjectHolder;

  /// Map of the currently processed object file symbol addresses.
  /// The associated StringRef is the name of the symbol section if it
  /// is section based.
  StringMap<std::pair<uint64_t, StringRef>> CurrentObjectAddresses;
  /// Element of the debug map corresponfing to the current object file.
  DebugMapObject *CurrentDebugMapObject;

  /// Holds function info while function scope processing.
  const char *CurrentFunctionName;
  uint64_t CurrentFunctionAddress;

  std::unique_ptr<DebugMap> parseOneBinary(const MachOObjectFile &MainBinary,
                                           StringRef BinaryPath);

  void switchToNewDebugMapObject(StringRef Filename, sys::TimeValue Timestamp);
  void resetParserState();
  uint64_t getMainBinarySymbolAddress(StringRef Name);
  void loadMainBinarySymbols(const MachOObjectFile &MainBinary);
  void loadCurrentObjectFileSymbols(const MachOObjectFile &Obj);
  void handleStabSymbolTableEntry(uint32_t StringIndex, uint8_t Type,
                                  uint8_t SectionIndex, uint16_t Flags,
                                  uint64_t Value);

  template <typename STEType> void handleStabDebugMapEntry(const STEType &STE) {
    handleStabSymbolTableEntry(STE.n_strx, STE.n_type, STE.n_sect, STE.n_desc,
                               STE.n_value);
  }

  /// \brief Dump the symbol table output header.
  void dumpSymTabHeader(raw_ostream &OS, StringRef Arch);

  /// \brief Dump the contents of nlist entries.
  void dumpSymTabEntry(raw_ostream &OS, uint64_t Index, uint32_t StringIndex,
                       uint8_t Type, uint8_t SectionIndex, uint16_t Flags,
                       uint64_t Value);

  template <typename STEType> void dumpSymTabEntry(raw_ostream &OS,
                                                   uint64_t Index,
                                                   const STEType &STE) {
    dumpSymTabEntry(OS, Index, STE.n_strx, STE.n_type, STE.n_sect, STE.n_desc,
                    STE.n_value);
  }
  void dumpOneBinaryStab(const MachOObjectFile &MainBinary,
                         StringRef BinaryPath);

  void Warning(const Twine &Msg, StringRef File = StringRef()) {
    errs() << "warning: (" << getArchName(Result->getTriple()) << ") "
           << File << " " << Msg << "\n";

    if (getenv("RC_DEBUG_OPTIONS")) {
      if (!File.empty())
        Result->addDebugMapObject(File, sys::TimeValue::ZeroTime());
      Result->back().addWarning(Msg.str());
    }
  }
};

}

/// Reset the parser state coresponding to the current object
/// file. This is to be called after an object file is finished
/// processing.
void MachODebugMapParser::resetParserState() {
  CurrentObjectAddresses.clear();
  CurrentDebugMapObject = nullptr;
}

/// Create a new DebugMapObject. This function resets the state of the
/// parser that was referring to the last object file and sets
/// everything up to add symbols to the new one.
void MachODebugMapParser::switchToNewDebugMapObject(StringRef Filename,
                                                    sys::TimeValue Timestamp) {
  resetParserState();

  SmallString<80> Path(PathPrefix);
  sys::path::append(Path, Filename);

  auto MachOOrError =
    CurrentObjectHolder.GetFilesAs<MachOObjectFile>(Path, Timestamp);
  if (auto Error = MachOOrError.getError()) {
    Warning("unable to open object file: " + Error.message(), Path.str());
    return;
  }

  auto ErrOrAchObj =
      CurrentObjectHolder.GetAs<MachOObjectFile>(Result->getTriple());
  if (auto Err = ErrOrAchObj.getError()) {
    return Warning(Twine("cannot open debug object \"") + Path.str() + "\": " +
                   Err.message() + "\n", Path.str());
  }

  CurrentDebugMapObject = &Result->addDebugMapObject(Path, Timestamp);
  loadCurrentObjectFileSymbols(*ErrOrAchObj);
}

static Triple getTriple(const object::MachOObjectFile &Obj) {
  Triple ThumbTriple;
  Triple T = Obj.getArch(nullptr, &ThumbTriple);
  return ThumbTriple.str().empty() ? T : ThumbTriple;
}

static std::string getArchName(const object::MachOObjectFile &Obj) {
  Triple ThumbTriple;
  Triple T = Obj.getArch(nullptr, &ThumbTriple);
  return T.getArchName();
}

std::unique_ptr<DebugMap>
MachODebugMapParser::parseOneBinary(const MachOObjectFile &MainBinary,
                                    StringRef BinaryPath) {
  loadMainBinarySymbols(MainBinary);
  ArrayRef<uint8_t> UUID = MainBinary.getUuid();
  Result = make_unique<DebugMap>(getTriple(MainBinary), BinaryPath, UUID);
  MainBinaryStrings = MainBinary.getStringTableData();
  for (const SymbolRef &Symbol : MainBinary.symbols()) {
    const DataRefImpl &DRI = Symbol.getRawDataRefImpl();
    if (MainBinary.is64Bit())
      handleStabDebugMapEntry(MainBinary.getSymbol64TableEntry(DRI));
    else
      handleStabDebugMapEntry(MainBinary.getSymbolTableEntry(DRI));
  }

  resetParserState();
  return std::move(Result);
}

// FIXME: llvm-mc.cpp has very similar code, move it to a common place.
// Table that maps Darwin's Mach-O stab constants to strings to allow printing.
struct DarwinStabName {
  uint8_t NType;
  const char *Name;
};

static const struct DarwinStabName DarwinStabNames[] = {
    { MachO::N_GSYM,    "N_GSYM" },
    { MachO::N_FNAME,   "N_FNAME" },
    { MachO::N_FUN,     "N_FUN" },
    { MachO::N_STSYM,   "N_STSYM" },
    { MachO::N_LCSYM,   "N_LCSYM" },
    { MachO::N_BNSYM,   "N_BNSYM" },
    { MachO::N_PC,      "N_PC" },
    { MachO::N_AST,     "N_AST" },
    { MachO::N_OPT,     "N_OPT" },
    { MachO::N_RSYM,    "N_RSYM" },
    { MachO::N_SLINE,   "N_SLINE" },
    { MachO::N_ENSYM,   "N_ENSYM" },
    { MachO::N_SSYM,    "N_SSYM" },
    { MachO::N_SO,      "N_SO" },
    { MachO::N_OSO,     "N_OSO" },
    { MachO::N_LSYM,    "N_LSYM" },
    { MachO::N_BINCL,   "N_BINCL" },
    { MachO::N_SOL,     "N_SOL" },
    { MachO::N_PARAMS,  "N_PARAM" },
    { MachO::N_VERSION, "N_VERS" },
    { MachO::N_OLEVEL,  "N_OLEV" },
    { MachO::N_PSYM,    "N_PSYM" },
    { MachO::N_EINCL,   "N_EINCL" },
    { MachO::N_ENTRY,   "N_ENTRY" },
    { MachO::N_LBRAC,   "N_LBRAC" },
    { MachO::N_EXCL,    "N_EXCL" },
    { MachO::N_RBRAC,   "N_RBRAC" },
    { MachO::N_BCOMM,   "N_BCOMM" },
    { MachO::N_ECOMM,   "N_ECOMM" },
    { MachO::N_ECOML,   "N_ECOML" },
    { MachO::N_LENG,    "N_LENG" },
    {0, 0}};

static const char *getDarwinStabString(uint8_t NType) {
  for (unsigned i = 0; DarwinStabNames[i].Name; i++) {
    if (DarwinStabNames[i].NType == NType)
      return DarwinStabNames[i].Name;
  }
  return 0;
}

void MachODebugMapParser::dumpSymTabHeader(raw_ostream &OS, StringRef Arch) {
  OS << "-----------------------------------"
        "-----------------------------------\n";
  OS << "Symbol table for: '" << BinaryPath << "' (" << Arch.data() << ")\n";
  OS << "-----------------------------------"
        "-----------------------------------\n";
  OS << "Index    n_strx   n_type             n_sect n_desc n_value\n";
  OS << "======== -------- ------------------ ------ ------ ----------------\n";
}

void MachODebugMapParser::dumpSymTabEntry(raw_ostream &OS, uint64_t Index,
                      uint32_t StringIndex, uint8_t Type, uint8_t SectionIndex,
                      uint16_t Flags, uint64_t Value) {

     // Index
  OS << '[' << format_decimal(Index, 6) << "] "
     // n_strx
     << format_hex_no_prefix(StringIndex, 8) << ' '
     // n_type...
     << format_hex_no_prefix(Type, 2) << " (";

  if (Type & MachO::N_STAB)
    OS << left_justify(getDarwinStabString(Type), 13);
  else {
    if (Type & MachO::N_PEXT)
      OS << "PEXT ";
    else
      OS << "     ";
    switch (Type & MachO::N_TYPE) {
      case MachO::N_UNDF: // 0x0 undefined, n_sect == NO_SECT
        OS << "UNDF"; break;
      case MachO::N_ABS : // 0x2 absolute, n_sect == NO_SECT
        OS << "ABS "; break;
      case MachO::N_SECT: // 0xe defined in section number n_sect
        OS << "SECT"; break;
      case MachO::N_PBUD: // 0xc prebound undefined (defined in a dylib)
        OS << "PBUD"; break;
      case MachO::N_INDR: // 0xa indirect
        OS << "INDR"; break;
      default: OS << format_hex_no_prefix(Type, 2) << "    "; break;
    }
    if (Type & MachO::N_EXT)
      OS << " EXT";
    else
      OS << "    ";
  }

  OS << ") "
     // n_sect
     << format_hex_no_prefix(SectionIndex, 2) << "     "
     // n_desc
     << format_hex_no_prefix(Flags, 4) << "   "
     // n_value
     << format_hex_no_prefix(Value, 16);

  const char *Name = &MainBinaryStrings.data()[StringIndex];
  if (Name && Name[0])
    OS << " '" << Name << "'";

  OS << "\n";
}

void MachODebugMapParser::dumpOneBinaryStab(const MachOObjectFile &MainBinary,
                                            StringRef BinaryPath) {
  loadMainBinarySymbols(MainBinary);
  MainBinaryStrings = MainBinary.getStringTableData();
  raw_ostream &OS(llvm::outs());

  StringRef ArchName = getArchName(MainBinary);
  dumpSymTabHeader(OS, ArchName);
  uint64_t Idx = 0;
  for (const SymbolRef &Symbol : MainBinary.symbols()) {
    const DataRefImpl &DRI = Symbol.getRawDataRefImpl();
    if (MainBinary.is64Bit())
      dumpSymTabEntry(OS, Idx, MainBinary.getSymbol64TableEntry(DRI));
    else
      dumpSymTabEntry(OS, Idx, MainBinary.getSymbolTableEntry(DRI));
    Idx++;
  }

  OS << "\n\n";
  resetParserState();
}

static bool shouldLinkArch(SmallVectorImpl<StringRef> &Archs,
                           StringRef Arch) {
  if (Archs.empty() ||
      std::find(Archs.begin(), Archs.end(), "all") != Archs.end() ||
      std::find(Archs.begin(), Archs.end(), "*") != Archs.end())
    return true;

  if (std::find(Archs.begin(), Archs.end(), "arm") != Archs.end() &&
      Arch.startswith("arm"))
    return true;

  return std::find(Archs.begin(), Archs.end(), Arch) != Archs.end();
}

bool MachODebugMapParser::dumpStab() {
  auto MainBinOrError = MainBinaryHolder.GetFilesAs<MachOObjectFile>(BinaryPath);
  if (auto Error = MainBinOrError.getError()) {
    llvm::errs() << "Cannot get '" << BinaryPath << "' as MachO file: "
                 << Error.message() << "\n";
    return false;
  }

  Triple T;
  for (const auto *Binary : *MainBinOrError)
    if (shouldLinkArch(Archs, Binary->getArch(nullptr, &T).getArchName()))
      dumpOneBinaryStab(*Binary, BinaryPath);

  return true;
}

/// This main parsing routine tries to open the main binary and if
/// successful iterates over the STAB entries. The real parsing is
/// done in handleStabSymbolTableEntry.
ErrorOr<std::vector<std::unique_ptr<DebugMap>>> MachODebugMapParser::parse() {
  auto MainBinOrError =
      MainBinaryHolder.GetFilesAs<MachOObjectFile>(BinaryPath);
  if (auto Error = MainBinOrError.getError())
    return Error;

  std::vector<std::unique_ptr<DebugMap>> Results;
  Triple T;
  for (const auto *Binary : *MainBinOrError)
    if (shouldLinkArch(Archs, Binary->getArch(nullptr, &T).getArchName()))
      Results.push_back(parseOneBinary(*Binary, BinaryPath));

  return std::move(Results);
}

/// Interpret the STAB entries to fill the DebugMap.
void MachODebugMapParser::handleStabSymbolTableEntry(uint32_t StringIndex,
                                                     uint8_t Type,
                                                     uint8_t SectionIndex,
                                                     uint16_t Flags,
                                                     uint64_t Value) {
  if (!(Type & MachO::N_STAB))
    return;

  const char *Name = &MainBinaryStrings.data()[StringIndex];

  // An N_OSO entry represents the start of a new object file description.
  if (Type == MachO::N_OSO) {
    sys::TimeValue Timestamp;
    Timestamp.fromEpochTime(Value);
    return switchToNewDebugMapObject(Name, Timestamp);
  }

  // An N_AST entry is a pointer to a Swift module.
  if (Type == MachO::N_AST) {
    sys::TimeValue Timestamp;
    Timestamp.fromEpochTime(Value);
    Result->addDebugMapObject(Name, Timestamp, /* SwiftModule */ true);
    return;
  }
 
  // If the last N_OSO object file wasn't found,
  // CurrentDebugMapObject will be null. Do not update anything
  // until we find the next valid N_OSO entry.
  if (!CurrentDebugMapObject)
    return;

  uint32_t Size = 0;
  switch (Type) {
  case MachO::N_GSYM:
    // This is a global variable. We need to query the main binary
    // symbol table to find its address as it might not be in the
    // debug map (for common symbols).
    Value = getMainBinarySymbolAddress(Name);
    if (Value == UnknownAddressOrSize)
      return;
    break;
  case MachO::N_FUN:
    // Functions are scopes in STABS. They have an end marker that
    // contains the function size.
    if (Name[0] == '\0') {
      Size = Value;
      Value = CurrentFunctionAddress;
      Name = CurrentFunctionName;
      // Drop debug map entries that have zero size, such as end labels of
      // functions.
      break;
    } else {
      CurrentFunctionName = Name;
      CurrentFunctionAddress = Value;
      return;
    }
  case MachO::N_STSYM:
    break;
  default:
    return;
  }

  auto ObjectSymIt = CurrentObjectAddresses.find(Name);
  if (ObjectSymIt == CurrentObjectAddresses.end()) {
    if (Name[0] == '_')
      Warning("could not find object file symbol for symbol " +
              Twine(Name));
    return;
  }

  if (!CurrentDebugMapObject->addSymbol(Name, ObjectSymIt->getValue().first, Value,
                                        Size, ObjectSymIt->getValue().second))
    return Warning(Twine("failed to insert symbol '") + Name +
                   "' in the debug map.");
}

/// Load the current object file symbols into CurrentObjectAddresses.
void MachODebugMapParser::loadCurrentObjectFileSymbols(
    const object::MachOObjectFile &Obj) {
  CurrentObjectAddresses.clear();

  for (auto Sym : Obj.symbols()) {
    StringRef Name;
    uint64_t Addr = UnknownAddressOrSize;
    SymbolRef::Type Type;
    StringRef SecName;

    // Undefined symbols won't have an address, but we still want
    // them, because they might be referenced in the debug information.
    Sym.getAddress(Addr);

    object::section_iterator Section = Obj.section_end();
    if (!Sym.getSection(Section) && Section != Obj.section_end())
      Section->getName(SecName);

    if (Sym.getName(Name) || Name.empty() ||
        Sym.getType(Type) || Type == SymbolRef::ST_Other)
      continue;

    CurrentObjectAddresses.insert(std::make_pair(Name, std::make_pair(Addr, SecName)));
    uint32_t Flags;

    Flags = Sym.getFlags();
    uint64_t Value;
    // FIXME: We put every symbol in the debug map, because of the
    // behavior expected from lookupObjectAddress.
      Value = getMainBinarySymbolAddress(Name);

    // Symbols with no known address on either side are of absolutely
    // no use.
    if (Addr != UnknownAddressOrSize || Value != UnknownAddressOrSize)
      CurrentDebugMapObject->addSymbol(Name, Addr, Value, 0, SecName, true);
  }
}

/// Lookup a symbol address in the main binary symbol table. The
/// parser only needs to query common symbols, thus not every symbol's
/// address is available through this function.
uint64_t MachODebugMapParser::getMainBinarySymbolAddress(StringRef Name) {
  auto Sym = MainBinarySymbolAddresses.find(Name);
  if (Sym == MainBinarySymbolAddresses.end())
    return UnknownAddressOrSize;
  return Sym->second;
}

/// Load the interesting main binary symbols' addresses into
/// MainBinarySymbolAddresses.
void MachODebugMapParser::loadMainBinarySymbols(
    const MachOObjectFile &MainBinary) {
  MainBinarySymbolAddresses.clear();
  for (const auto &Sym : MainBinary.symbols()) {
    SymbolRef::Type Type;
    // Skip undefined and STAB entries.
    if (Sym.getType(Type) || (Type & SymbolRef::ST_Debug) ||
        (Type & SymbolRef::ST_Unknown))
      continue;
    StringRef Name;
    uint64_t Addr;

    if (Sym.getAddress(Addr) || Addr == UnknownAddressOrSize ||
        Sym.getName(Name) || Name.size() == 0 || Name[0] == '\0') {
      continue;
    }

    // FIXME: dsymutil-classic ompatibility: retain the first
    // symbol with a given name. We should check why we get duplicated
    // symbols with different addresses instead. (emacs binary has 2
    // Qwindow symbols)
    uint64_t &AddrInMap = MainBinarySymbolAddresses[Name];
    if (!AddrInMap)
      AddrInMap = Addr;
  }
}

namespace llvm {
namespace dsymutil {

llvm::ErrorOr<std::vector<std::unique_ptr<DebugMap>>>
parseDebugMap(StringRef InputFile, ArrayRef<std::string> Archs,
              StringRef PrependPath, bool Verbose) {
  MachODebugMapParser Parser(InputFile, Archs, PrependPath, Verbose);
  return Parser.parse();
}

bool dumpStab(StringRef InputFile, ArrayRef<std::string> Archs,
              StringRef PrependPath) {
  MachODebugMapParser Parser(InputFile, Archs, PrependPath, false);
  return Parser.dumpStab();
}

}
}
