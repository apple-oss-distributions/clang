//===- tools/dsymutil/DebugMap.cpp - Generic debug map representation -----===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "DebugMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>

namespace llvm {
namespace dsymutil {

using namespace llvm::object;

DebugMapObject::DebugMapObject(StringRef ObjectFilename,
                               sys::TimeValue Timestamp,
                               bool SwiftModule)
    : Filename(ObjectFilename), Timestamp(Timestamp),
      SwiftModule(SwiftModule) {}

bool DebugMapObject::addSymbol(StringRef Name, uint64_t ObjectAddress,
                               uint64_t LinkedAddress, uint32_t Size,
                               StringRef Section, bool FromAnotherObjectFile) {
  auto InsertResult = Symbols.insert(
      std::make_pair(Name, SymbolMapping(ObjectAddress, LinkedAddress, Section,
                                         Size, FromAnotherObjectFile)));

  DebugMapEntry &Entry = *InsertResult.first;

  // If the symbol is section based, then its address is
  // meaningful. As we put every symbol in the debug map, we might
  // have multiple aliases though (this happens with asm labels or C
  // aliases). Prefer to store one that has a linked address.
  if (!Section.empty()) {
    DebugMapEntry *&MapEntry = AddressToMapping[ObjectAddress];
    if (!MapEntry ||
        (LinkedAddress != object::UnknownAddressOrSize &&
         MapEntry->getValue().BinaryAddress == object::UnknownAddressOrSize))
      MapEntry = &Entry;
  }

  if (!InsertResult.second) {
    if (Size)
      Entry.getValue().Size = Size;
    if (!FromAnotherObjectFile && Entry.getValue().FromAnotherObjectFile) {
      Entry.getValue().ObjectAddress = ObjectAddress;
      Entry.getValue().BinaryAddress = LinkedAddress;
    }
    Entry.getValue().FromAnotherObjectFile = FromAnotherObjectFile;
    return Entry.getValue().ObjectAddress == ObjectAddress &&
           Entry.getValue().BinaryAddress == LinkedAddress;
  }

  return true;
}

void DebugMapObject::print(raw_ostream &OS) const {
  OS << getObjectFilename() << ":\n";
  // Sort the symbols in alphabetical order, like llvm-nm (and to get
  // deterministic output for testing).
  typedef std::pair<StringRef, SymbolMapping> Entry;
  std::vector<Entry> Entries;
  Entries.reserve(Symbols.getNumItems());
  for (const auto &Sym : make_range(Symbols.begin(), Symbols.end()))
    Entries.push_back(std::make_pair(Sym.getKey(), Sym.getValue()));
  std::sort(
      Entries.begin(), Entries.end(),
      [](const Entry &LHS, const Entry &RHS) { return LHS.first < RHS.first; });
  for (const auto &Sym : Entries) {
    OS << format("\t%016" PRIx64 " => %016" PRIx64,
                 uint64_t(Sym.second.ObjectAddress),
                 uint64_t(Sym.second.BinaryAddress))
       <<  "+" << format_hex(uint32_t(Sym.second.Size), 3)
       << format("\t%s %s",
                 Sym.first.data(),
                 Sym.second.FromAnotherObjectFile ? "(other object)" : "");
    if (!Sym.second.Section.empty())
      OS << "\tin " << Sym.second.Section;
    OS << '\n';
  }
  OS << '\n';
}

#ifndef NDEBUG
void DebugMapObject::dump() const { print(errs()); }
#endif

DebugMapObject &DebugMap::addDebugMapObject(StringRef ObjectFilePath,
                                            sys::TimeValue Timestamp,
                                            bool SwiftModule) {
  Objects.emplace_back(new DebugMapObject(ObjectFilePath, Timestamp,
                                          SwiftModule));
  return *Objects.back();
}

const DebugMapObject::DebugMapEntry *
DebugMapObject::lookupSymbol(StringRef SymbolName) const {
  StringMap<SymbolMapping>::const_iterator Sym = Symbols.find(SymbolName);
  if (Sym == Symbols.end())
    return nullptr;
  return &*Sym;
}

const DebugMapObject::DebugMapEntry *
DebugMapObject::lookupObjectAddress(uint64_t Address,
                                    StringRef Section) const {
  // FIXME: We allow to return a unexact match is the entry right
  // before the address is in the same section. This handle some cases
  // where the relocation in a variable location isn't mentioned in
  // the debug map because it has somehow been coalesced with a
  // neighbor value. This looks like a debug info bug (Only example in
  // the OpenGL project). We don't want to return that if the address
  // had a valid symbol in the object file. To do this, we fill the
  // map with every symbol that was present in the object even if they
  // weren't linked. With this setup, only addresses whose symbol
  // 'disapeared' will get the closest match treatment.
  auto Mapping = AddressToMapping.upper_bound(Address);
  if (Mapping != AddressToMapping.begin())
    --Mapping;

  // If the section is empty, this is a scattered relocation. Return
  // the closest match in that case, it should be the base symbol of
  // the scattered reloc.
  if (Mapping == AddressToMapping.end() ||
      Mapping->second->getValue().ObjectAddress > Address ||
      (!Section.empty() && Mapping->second->getValue().Section != Section))
    return nullptr;
  return Mapping->second;
}

void DebugMap::print(raw_ostream &OS) const {
  // yaml::Output yout(OS);
  // yout << const_cast<DebugMap&>(*this);
  OS << "DEBUG MAP: " << BinaryTriple.getTriple()
     << "\n\tobject addr =>  executable addr\tsymbol name\n";
  for (const auto &Obj : objects())
    Obj->print(OS);
  OS << "END DEBUG MAP\n";
}

#ifndef NDEBUG
void DebugMap::dump() const { print(errs()); }
#endif
}
}
