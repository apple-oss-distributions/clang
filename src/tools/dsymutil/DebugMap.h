//===--- dsymutil/DebugMap.h - Generic debug map representation -*- C++ -*-===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This file contains the class declaration of the DebugMap
/// entity. A DebugMap lists all the object files linked together to
/// produce an executable along with the linked address of all the
/// atoms used in these object files.
/// The DebugMap is an input to the DwarfLinker class that will
/// extract the Dwarf debug information from the referenced object
/// files and link their usefull debug info together.
///
//===----------------------------------------------------------------------===//
#ifndef LLVM_TOOLS_DSYMUTIL_DEBUGMAP_H
#define LLVM_TOOLS_DSYMUTIL_DEBUGMAP_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/Triple.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/YAMLTraits.h"
#include <vector>

namespace llvm {
class raw_ostream;

namespace dsymutil {
class DebugMapObject;

/// \brief The DebugMap object stores the list of object files to
/// query for debug information along with the mapping between the
/// symbols' addresses in the object file to their linked address in
/// the linked binary.
///
/// A DebugMap producer could look like this:
/// DebugMap *DM = new DebugMap();
/// for (const auto &Obj: LinkedObjects) {
///     DebugMapObject &DMO = DM->addDebugMapObject(Obj.getPath());
///     for (const auto &Sym: Obj.getLinkedSymbols())
///         DMO.addSymbol(Sym.getName(), Sym.getObjectFileAddress(),
///                       Sym.getBinaryAddress());
/// }
///
/// A DebugMap consumer can then use the map to link the debug
/// information. For example something along the lines of:
/// for (const auto &DMO: DM->objects()) {
///     auto Obj = createBinary(DMO.getObjectFilename());
///     for (auto &DIE: Obj.getDwarfDIEs()) {
///         if (SymbolMapping *Sym = DMO.lookup(DIE.getName()))
///             DIE.relocate(Sym->ObjectAddress, Sym->BinaryAddress);
///         else
///             DIE.discardSubtree();
///     }
/// }
class DebugMap {
  Triple BinaryTriple;
  std::string BinaryPath;
  std::vector<uint8_t> BinaryUUID;
  typedef std::vector<std::unique_ptr<DebugMapObject>> ObjectContainer;
  ObjectContainer Objects;

  // For YAML IO support.
  friend yaml::MappingTraits<DebugMap>;
  std::vector<std::unique_ptr<DebugMapObject>> &getObjects() { return Objects; }
  //  DebugMap() {}

public:
  DebugMap(const Triple &BinaryTriple, StringRef BinaryPath = "",
           ArrayRef<uint8_t> BinaryUUID = ArrayRef<uint8_t>())
      : BinaryTriple(BinaryTriple), BinaryPath(BinaryPath),
        BinaryUUID(BinaryUUID.begin(), BinaryUUID.end()) {}

  DebugMap() {}

  typedef ObjectContainer::const_iterator const_iterator;

  iterator_range<const_iterator> objects() const {
    return make_range(begin(), end());
  }

  const_iterator begin() const { return Objects.begin(); }

  const_iterator end() const { return Objects.end(); }

  DebugMapObject &back() { return *Objects.back(); }

  /// This function adds an DebugMapObject to the list owned by this
  /// debug map.
  DebugMapObject &addDebugMapObject(StringRef ObjectFilePath,
                                    sys::TimeValue Timestamp,
                                    bool SwiftModule = false);

  void addSwiftModule(StringRef ModulePath, sys::TimeValue Timestamp);

  const Triple &getTriple() const { return BinaryTriple; }

  const ArrayRef<uint8_t> getUUID() const {
    return ArrayRef<uint8_t>(BinaryUUID);
  }

  StringRef getBinaryPath() const { return BinaryPath; }

  void print(raw_ostream &OS) const;

#ifndef NDEBUG
  void dump() const;
#endif
};

/// \brief The DebugMapObject represents one object file described by
/// the DebugMap. It contains a list of mappings between addresses in
/// the object file and in the linked binary for all the linked atoms
/// in this object file.
class DebugMapObject {
public:
  struct SymbolMapping {
    yaml::Hex64 ObjectAddress;
    yaml::Hex64 BinaryAddress;
    std::string Section;
    yaml::Hex32 Size;
    bool FromAnotherObjectFile;
    SymbolMapping(uint64_t ObjectAddress, uint64_t BinaryAddress,
                  StringRef Section, uint32_t Size,
                  bool FromAnotherObjectFile)
        : ObjectAddress(ObjectAddress), BinaryAddress(BinaryAddress),
          Section(Section), Size(Size),
          FromAnotherObjectFile(FromAnotherObjectFile) {}
    SymbolMapping() {}
  };

  typedef StringMapEntry<SymbolMapping> DebugMapEntry;

  /// \brief Adds a symbol mapping to this DebugMapObject.
  /// \returns false if the symbol was already registered. The request
  /// is discarded in this case.
  bool addSymbol(llvm::StringRef SymName, uint64_t ObjectAddress,
                 uint64_t LinkedAddress, uint32_t Size, StringRef Section,
                 bool FromAnotherObjectFile = false);

  /// \brief Lookup a symbol mapping.
  /// \returns null if the symbol isn't found.
  const DebugMapEntry *lookupSymbol(StringRef SymbolName) const;

  /// \brief Lookup an objectfile address.
  /// \returns null if the address isn't found.
  const DebugMapEntry *lookupObjectAddress(uint64_t Address,
                                           StringRef Section) const;

  llvm::StringRef getObjectFilename() const { return Filename; }
  sys::TimeValue getTimestamp() const { return Timestamp; }
  bool isSwiftModule() const { return SwiftModule; }

  iterator_range<StringMap<SymbolMapping>::const_iterator> symbols() const {
    return make_range(Symbols.begin(), Symbols.end());
  }

  bool empty() { return Symbols.empty(); }

  void print(raw_ostream &OS) const;
#ifndef NDEBUG
  void dump() const;
#endif

  void addWarning(StringRef Warning) { Warnings.push_back(Warning); }
  const std::vector<std::string> &getWarnings() const { return Warnings; }
private:
  friend class DebugMap;
  /// DebugMapObjects can only be constructed by the owning DebugMap.
  DebugMapObject(StringRef ObjectFilename, sys::TimeValue Timestamp,
                 bool SwiftModule);

  std::string Filename;
  sys::TimeValue Timestamp;
  bool SwiftModule;
  StringMap<SymbolMapping> Symbols;
  std::map<uint64_t, DebugMapEntry *> AddressToMapping;

  std::vector<std::string> Warnings;

  // For YAML IO support.
  friend yaml::MappingTraits<dsymutil::DebugMapObject>;
  friend yaml::SequenceTraits<std::vector<std::unique_ptr<DebugMapObject>>>;
  DebugMapObject() {}
};
}
}

namespace llvm {
namespace yaml {

template <>
struct MappingTraits<
    std::pair<std::string, dsymutil::DebugMapObject::SymbolMapping>> {

  static void
  mapping(IO &io,
          std::pair<std::string, dsymutil::DebugMapObject::SymbolMapping> &s) {
    io.mapRequired("sym", s.first);
    io.mapRequired("objAddr", s.second.ObjectAddress);
    io.mapRequired("binAddr", s.second.BinaryAddress);
  }
};

template <>
struct SequenceTraits<std::vector<
    std::pair<std::string, dsymutil::DebugMapObject::SymbolMapping>>> {

  static size_t
  size(IO &io,
       std::vector<std::pair<std::string,
                             dsymutil::DebugMapObject::SymbolMapping>> &seq) {
    return seq.size();
  }
  static std::pair<std::string, dsymutil::DebugMapObject::SymbolMapping> &
  element(IO &,
          std::vector<std::pair<std::string,
                                dsymutil::DebugMapObject::SymbolMapping>> &seq,
          size_t index) {
    if (index >= seq.size())
      seq.resize(index + 1);
    return seq[index];
  }
};

template <> struct MappingTraits<dsymutil::DebugMapObject> {

  struct SequencedStringMap {
  public:
    SequencedStringMap(IO &io) {}
    SequencedStringMap(
        IO &io, StringMap<dsymutil::DebugMapObject::SymbolMapping> &Map) {
      Entries.reserve(Map.size());
      for (auto &Entry : Map)
        Entries.push_back(std::make_pair(Entry.getKey(), Entry.getValue()));
    }
    StringMap<dsymutil::DebugMapObject::SymbolMapping> denormalize(IO &) {
      StringMap<dsymutil::DebugMapObject::SymbolMapping> Res;

      for (auto &Entry : Entries)
        Res[Entry.first] = Entry.second;

      return std::move(Res);
    }
    std::vector<std::pair<std::string, dsymutil::DebugMapObject::SymbolMapping>>
        Entries;
  };

  static void mapping(IO &io, dsymutil::DebugMapObject &s) {
    MappingNormalization<SequencedStringMap,
                         StringMap<dsymutil::DebugMapObject::SymbolMapping>>
        seq(io, s.Symbols);
    io.mapRequired("filename", s.Filename);
    io.mapRequired("symbols", seq->Entries);
  }
};

template <> struct ScalarTraits<Triple> {
  static void output(const Triple &val, void *, llvm::raw_ostream &out) {
    out << val.str();
  }
  static StringRef input(StringRef scalar, void *, Triple &value) {
    value = Triple(scalar);
    return value.str();
  }
  static bool mustQuote(StringRef) { return true; }
};

template <>
struct SequenceTraits<std::vector<std::unique_ptr<dsymutil::DebugMapObject>>> {
  static size_t
  size(IO &io, std::vector<std::unique_ptr<dsymutil::DebugMapObject>> &seq) {
    return seq.size();
  }
  static dsymutil::DebugMapObject &
  element(IO &, std::vector<std::unique_ptr<dsymutil::DebugMapObject>> &seq,
          size_t index) {
    if (index >= seq.size()) {
      seq.resize(index + 1);
      seq[index].reset(new dsymutil::DebugMapObject);
    }
    return *seq[index];
  }
};

template <> struct MappingTraits<dsymutil::DebugMap> {
  static void mapping(IO &io, dsymutil::DebugMap &DM) {
    io.mapRequired("triple", DM.BinaryTriple);
    io.mapRequired("binary", DM.BinaryPath);
    io.mapOptional("objects", DM.Objects);
  }
};
}
}

#endif // LLVM_TOOLS_DSYMUTIL_DEBUGMAP_H
