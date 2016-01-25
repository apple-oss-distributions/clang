//===- tools/dsymutil/DwarfLinker.cpp - Dwarf debug info linker -----------===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "DebugMap.h"

#include "BinaryHolder.h"
#include "DebugMap.h"
#include "dsymutil.h"
#include "llvm/ADT/IntervalMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/DIE.h"
#include "llvm/Config/config.h"
#include "../../lib/CodeGen/AsmPrinter/DwarfAccelTable.h"
#include "llvm/DebugInfo/DWARF/DWARFAcceleratorTable.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "llvm/DebugInfo/DWARF/DWARFDebugInfoEntry.h"
#include "llvm/DebugInfo/DWARF/DWARFFormValue.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCDwarf.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCMachObjectWriter.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCObjectStreamer.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSectionMachO.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Object/MachO.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Dwarf.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/LEB128.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <cstdlib>
#include <string>
#include <tuple>

#define DEBUG_TYPE "dsymutil-dwarf-linker"

namespace llvm {
namespace dsymutil {

namespace {

void warn(const Twine &Warning, const Twine &Context) {
  errs() << Twine("while processing ") + Context + ":\n";
  errs() << Twine("warning: ") + Warning + "\n";
}

bool error(const Twine &Error, const Twine &Context) {
  errs() << Twine("while processing ") + Context + ":\n";
  errs() << Twine("error: ") + Error + "\n";
  return false;
}

/// \brief Retrieve the section named \a SecName in \a Obj. To
/// accomodate for platform discrenpancies, the name passed should be
/// eg. 'debug_info' to match either '__debug_info' or
/// '.debug_info'. The logic here will strip the initial
/// platform-specific characters.
Optional<object::SectionRef>
getSectionByName(const object::ObjectFile &Obj, StringRef SecName) {
  for (const object::SectionRef &Section : Obj.sections()) {
    StringRef SectionName;
    Section.getName(SectionName);
    SectionName = SectionName.substr(SectionName.find_first_not_of("._"));
    if (SectionName != SecName)
      continue;
    return Section;
  }
  return None;
}

template <typename KeyT, typename ValT>
using HalfOpenIntervalMap =
    IntervalMap<KeyT, ValT, IntervalMapImpl::NodeSizer<KeyT, ValT>::LeafSize,
                IntervalMapHalfOpenInfo<KeyT>>;

typedef HalfOpenIntervalMap<uint64_t, int64_t> FunctionIntervals;

class CompileUnit;

class DeclContext {
  //  SmallString<96> FullName;
  //  StringRef Key;
  StringRef Name;
  DeclContext *Parent;
  DeclContext *Collision;
  // Discriminators:
  StringRef File;
  uint32_t Line;
  uint32_t ByteSize;
  //  StringMap<std::unique_ptr<DeclContext>> Children;
  uint32_t LastSeenCompileUnitID;
  const DWARFDebugInfoEntryMinimal *LastSeenDIE;
  uint32_t QualifiedNameHash;
  uint32_t CanonicalDIEOffset;
  uint16_t Tag;
  bool Valid;

public:
  //  typedef StringMap<DeclContext, BumpPtrAllocator> Map;
  typedef DenseMap<uint32_t, DeclContext *> Map;

  DeclContext()
      : Parent(nullptr), LastSeenCompileUnitID(0), QualifiedNameHash(5381),
        CanonicalDIEOffset(0), Tag(dwarf::DW_TAG_compile_unit), Valid(true) {}

  DeclContext(StringRef Name, uint32_t Hash, DeclContext *Parent,
              StringRef File, uint32_t Line, uint32_t ByteSize, uint16_t Tag,
              unsigned CUId)
      : Name(Name), Parent(Parent), Collision(nullptr), File(File), Line(Line),
        ByteSize(ByteSize), LastSeenCompileUnitID(CUId), LastSeenDIE(nullptr),
        QualifiedNameHash(Hash), CanonicalDIEOffset(0), Tag(Tag), Valid(true) {
    // if (!Parent->Parent)
    //   QualifiedNameHash = 5381; // DJB hash initial seed.
    // else
    //   QualifiedNameHash = DwarfAccelTable::HashDJB(Name,
    //   DwarfAccelTable::HashDJB("::", Parent->QualifiedNameHash));
  }

  uint32_t getQualifiedNameHash() const { return QualifiedNameHash; }

  void setLastSeenDIE(const DWARFDebugInfoEntryMinimal *Die) {
    LastSeenDIE = Die;
  }
  void resetLastSeen(CompileUnit &U);
  
  std::pair<DeclContext *, DeclContext *>
  getChild(DeclContext::Map &Contexts, StringRef ShortName,
           StringRef Name, StringRef File, unsigned Line,
           unsigned ByteSize, uint16_t Tag, CompileUnit &U);

  uint32_t getCanonicalDIEOffset() const { return CanonicalDIEOffset; }

  void setCanonicalDIEOffset(uint32_t Offset) { CanonicalDIEOffset = Offset; }

  uint16_t getTag() const { return Tag; }

  DeclContext *getParent() const { return Parent; }

  void invalidate() { Valid = false; }

  bool isValid() const { return Valid; }
};

/// \brief Stores all information relating to a compile unit, be it in
/// its original instance in the object file to its brand new cloned
/// and linked DIE tree.
class CompileUnit {
public:
  /// \brief Information gathered about a DIE in the object file.
  struct DIEInfo {
    int64_t AddrAdjust; ///< Address offset to apply to the described entity.
    DeclContext *Ctxt;  ///< Declaration context.
    DIE *Clone;         ///< Cloned version of that DIE.
    uint32_t ParentIdx; ///< The index of this DIE's parent.
    bool Keep;          ///< Is the DIE part of the linked output?
    bool InDebugMap;    ///< Was this DIE's entity found in the map?
  };

  CompileUnit(DWARFUnit &OrigUnit, unsigned ID, bool CanUseODR,
              const DebugMapObject &Obj)
      : OrigUnit(OrigUnit), ID(ID), CUDie(nullptr), StartOffset(0),
        NextUnitOffset(0), LowPc(UINT64_MAX), HighPc(0), RangeAlloc(),
        Ranges(RangeAlloc), UnitRangeAttribute(nullptr),
        HasInterestingContent(false) {
    Info.resize(OrigUnit.getNumDIEs());

    const auto *CUDie = OrigUnit.getCompileUnitDIE(false);
    unsigned Lang = CUDie->getAttributeValueAsUnsignedConstant(
        &OrigUnit, dwarf::DW_AT_language, 0);
    HasODR = CanUseODR && (Lang == dwarf::DW_LANG_C_plus_plus ||
                           Lang == dwarf::DW_LANG_C_plus_plus_03 ||
                           Lang == dwarf::DW_LANG_C_plus_plus_11 ||
                           Lang == dwarf::DW_LANG_C_plus_plus_14 ||
                           Lang == dwarf::DW_LANG_ObjC_plus_plus);
  }

  CompileUnit(CompileUnit &&RHS)
      : OrigUnit(RHS.OrigUnit), Info(std::move(RHS.Info)),
        CUDie(std::move(RHS.CUDie)), StartOffset(RHS.StartOffset),
        NextUnitOffset(RHS.NextUnitOffset), RangeAlloc(), Ranges(RangeAlloc) {
    // The CompileUnit container has been 'reserve()'d with the right
    // size. We cannot move the IntervalMap anyway.
    llvm_unreachable("CompileUnits should not be moved.");
  }

  DWARFUnit &getOrigUnit() const { return OrigUnit; }

  unsigned getUniqueID() const { return ID; }

  DIE *getOutputUnitDIE() const { return CUDie; }
  void setOutputUnitDIE(DIE *Die) { CUDie = Die; }

  bool hasODR() const { return HasODR; }

  DIEInfo &getInfo(unsigned Idx) { return Info[Idx]; }
  const DIEInfo &getInfo(unsigned Idx) const { return Info[Idx]; }

  uint64_t getStartOffset() const { return StartOffset; }
  uint64_t getNextUnitOffset() const { return NextUnitOffset; }
  void setStartOffset(uint64_t DebugInfoSize) { StartOffset = DebugInfoSize; }

  uint64_t getLowPc() const { return LowPc; }
  uint64_t getHighPc() const { return HighPc; }
  bool hasLabelAt(uint64_t Addr) const { return Labels.count(Addr); }

  DIEInteger *getUnitRangesAttribute() const { return UnitRangeAttribute; }
  const FunctionIntervals &getFunctionRanges() const { return Ranges; }
  const std::vector<DIEInteger *> &getRangesAttributes() const {
    return RangeAttributes;
  }

  const std::vector<std::pair<DIEInteger *, int64_t>> &
  getLocationAttributes() const {
    return LocationAttributes;
  }

  void setHasInterestingContent() { HasInterestingContent = true; }
  bool hasInterestingContent() { return HasInterestingContent; }

  /// \brief Mark every DIE in this unit as kept. This function also
  /// marks variables as InDebugMap so that they appear in the
  /// reconstructed accelerator tables.
  void markEverythingAsKept();

  /// \brief Compute the end offset for this unit. Must be
  /// called after the CU's DIEs have been cloned.
  /// \returns the next unit offset (which is also the current
  /// debug_info section size).
  uint64_t computeNextUnitOffset();

  /// \brief Keep track of a forward reference to DIE \p Die in \p
  /// RefUnit by \p Attr. The attribute should be fixed up later to
  /// point to the absolute offset of \p Die in the debug_info section.
  void noteForwardReference(DIE *Die, const CompileUnit *RefUnit,
                            DeclContext *Ctxt, DIEInteger *Attr);

  /// \brief Apply all fixups recored by noteForwardReference().
  void fixupForwardReferences();

  /// \brief Add the low_pc of a label that is relocatad by applying
  /// offset \p PCOffset.
  void addLabelLowPc(uint64_t LabelLowPc, int64_t PcOffset);

  uint64_t getMinLabelInCU() const { return LowPc; }

  /// \brief Add a function range [\p LowPC, \p HighPC) that is
  /// relocated by applying offset \p PCOffset.
  void addFunctionRange(uint64_t LowPC, uint64_t HighPC, int64_t PCOffset);

  /// \brief Keep track of a DW_AT_range attribute that we will need to
  /// patch up later.
  void noteRangeAttribute(const DIE &Die, DIEInteger *Attr);

  /// \brief Keep track of a location attribute pointing to a location
  /// list in the debug_loc section.
  void noteLocationAttribute(DIEInteger *Attr, int64_t PcOffset);

  /// \brief Add various accelerator entries for \p Die with \p Name
  /// which is stored in the string table at \p Offset. \p Name must
  /// be an Objective-C selector.
  void addObjCAccelerator(const DIE *Die, const char *Name, uint32_t Offset,
                          bool SkipPubnamesSection = false);

  /// \brief Add a name accelerator entry for \p Die with \p Name
  /// which is stored in the string table at \p Offset.
  void addNameAccelerator(const DIE *Die, const char *Name, uint32_t Offset,
                          bool SkipPubnamesSection = false);

  /// \brief Add a type accelerator entry for \p Die with \p Name
  /// which is stored in the string table at \p Offset.
  void addTypeAccelerator(const DIE *Die, const char *Name, uint32_t Offset,
                          bool ObjCClassIsImplementation,
                          uint32_t QualifiedNameHash);

  /// \brief Add a namespace accelerator entry for \p Die with \p Name
  /// which is stored in the string table at \p Offset.
  void addNamespaceAccelerator(const DIE *Die, const char *Name,
                               uint32_t Offset);

  struct AccelInfo {
    StringRef Name;
    const DIE *Die;
    uint32_t NameOffset;
    uint32_t QualifiedNameHash;
    bool SkipPubSection : 1;
    bool ObjCClassIsImplementation : 1;

    AccelInfo(StringRef Name, const DIE *Die, uint32_t NameOffset,
              bool SkipPubSection = false,
              bool ObjCClassIsImplementation = false, uint32_t Hash = 0)
        : Name(Name), Die(Die), NameOffset(NameOffset), QualifiedNameHash(Hash),
          SkipPubSection(SkipPubSection),
          ObjCClassIsImplementation(ObjCClassIsImplementation) {}
  };

  const std::vector<AccelInfo> &getPubnames() const { return Pubnames; }
  const std::vector<AccelInfo> &getPubtypes() const { return Pubtypes; }
  const std::vector<AccelInfo> &getNamespaces() const { return Namespaces; }
  const std::vector<AccelInfo> &getObjC() const { return ObjC; }

private:
  DWARFUnit &OrigUnit;
  unsigned ID;
  std::vector<DIEInfo> Info; ///< DIE info indexed by DIE index.
  DIE *CUDie;                ///< Root of the linked DIE tree.

  uint64_t StartOffset;
  uint64_t NextUnitOffset;

  uint64_t LowPc;
  uint64_t HighPc;

  /// \brief A list of attributes to fixup with the absolute offset of
  /// a DIE in the debug_info section.
  ///
  /// The offsets for the attributes in this array couldn't be set while
  /// cloning because for cross-cu forward refences the target DIE's
  /// offset isn't known you emit the reference attribute.
  std::vector<std::tuple<DIE *, const CompileUnit *, DeclContext *,
                         DIEInteger *>> ForwardDIEReferences;

  FunctionIntervals::Allocator RangeAlloc;
  /// \brief The ranges in that interval map are the PC ranges for
  /// functions in this unit, associated with the PC offset to apply
  /// to the addresses to get the linked address.
  FunctionIntervals Ranges;

  /// \brief The DW_AT_low_pc of each DW_TAG_label.
  SmallDenseMap<uint64_t, uint64_t, 1> Labels;

  /// \brief DW_AT_ranges attributes to patch after we have gathered
  /// all the unit's function addresses.
  /// @{
  std::vector<DIEInteger *> RangeAttributes;
  DIEInteger *UnitRangeAttribute;
  /// @}

  /// \brief Location attributes that need to be transfered from th
  /// original debug_loc section to the liked one. They are stored
  /// along with the PC offset that is to be applied to their
  /// function's address.
  std::vector<std::pair<DIEInteger *, int64_t>> LocationAttributes;

  /// \brief Accelerator entries for the unit, both for the pub*
  /// sections and the apple* ones.
  /// @{
  std::vector<AccelInfo> Namespaces;
  std::vector<AccelInfo> Pubnames;
  std::vector<AccelInfo> Pubtypes;
  std::vector<AccelInfo> ObjC;
  /// @}
public:
  const DWARFDebugInfoEntryMinimal *
  getParent(const DWARFDebugInfoEntryMinimal *DIE) {
    unsigned Idx = OrigUnit.getDIEIndex(DIE);
    if (Idx == 0)
      return nullptr;
    return OrigUnit.getDIEAtIndex(Info[Idx].ParentIdx);
  }

  const char *getResolvedPath(unsigned FileNum) {
    if (FileNum >= ResolvedPaths.size())
      return nullptr;
    return ResolvedPaths[FileNum].size() ? ResolvedPaths[FileNum].c_str()
                                         : nullptr;
  }

  void setResolvedPath(unsigned FileNum, const std::string &Path) {
    if (ResolvedPaths.size() <= FileNum)
      ResolvedPaths.resize(FileNum + 1);
    ResolvedPaths[FileNum] = Path;
  }

  std::vector<std::string> ResolvedPaths;

  bool HasODR;
  bool HasInterestingContent; ///< Did a DIE actually contain a valid reloc?
};

static bool inFunctionScope(CompileUnit &U, unsigned Idx) {
  if (Idx == 0)
    return false;
  if (U.getOrigUnit().getDIEAtIndex(Idx)->getTag() == dwarf::DW_TAG_subprogram)
    return true;
  return inFunctionScope(U, U.getInfo(Idx).ParentIdx);
}

/// \brief Mark every DIE in this unit as kept. This function also
/// marks variables as InDebugMap so that they appear in the
/// reconstructed accelerator tables.
void CompileUnit::markEverythingAsKept() {
  unsigned Idx = 0;

  for (auto &I : Info) {
    I.Keep = true;
    const auto *DIE = OrigUnit.getDIEAtIndex(Idx++);

    // Try to guess which DIEs must go to the accelerator tables. We
    // do that just for variables, because functions will be handled
    // depending on wether they carry a DW_AT_low_pc attribute or not.
    if (DIE->getTag() != dwarf::DW_TAG_variable &&
         DIE->getTag() != dwarf::DW_TAG_constant)
      continue;
    DWARFFormValue Value;
    if (!DIE->getAttributeValue(&OrigUnit, dwarf::DW_AT_location, Value)) {
      if (DIE->getAttributeValue(&OrigUnit, dwarf::DW_AT_const_value, Value) &&
          !inFunctionScope(*this, I.ParentIdx))
        I.InDebugMap = true;
      continue;
    }
    if (auto Block = Value.getAsBlock()) {
      if (Block->size() >= OrigUnit.getAddressByteSize() + 1 &&
          (*Block)[0] == dwarf::DW_OP_addr)
        I.InDebugMap = true;
    }
  }
}

uint64_t CompileUnit::computeNextUnitOffset() {
  NextUnitOffset = StartOffset + 11 /* Header size */;
  // The root DIE might be null, meaning that the Unit had nothing to
  // contribute to the linked output. In that case, we will emit the
  // unit header without any actual DIE.
  if (CUDie)
    NextUnitOffset += CUDie->getSize();
  return NextUnitOffset;
}

/// \brief Keep track of a forward cross-cu reference from this unit
/// to \p Die that lives in \p RefUnit.
void CompileUnit::noteForwardReference(DIE *Die, const CompileUnit *RefUnit,
                                       DeclContext *Ctxt, DIEInteger *Attr) {
  ForwardDIEReferences.emplace_back(Die, RefUnit, Ctxt, Attr);
}

/// \brief Apply all fixups recorded by noteForwardReference().
void CompileUnit::fixupForwardReferences() {
  for (const auto &Ref : ForwardDIEReferences) {
    DIE *RefDie;
    const CompileUnit *RefUnit;
    DIEInteger *Attr;
    DeclContext *Ctxt;
    std::tie(RefDie, RefUnit, Ctxt, Attr) = Ref;
    if (Ctxt && Ctxt->getCanonicalDIEOffset())
      Attr->setValue(Ctxt->getCanonicalDIEOffset());
    else
      Attr->setValue(RefDie->getOffset() + RefUnit->getStartOffset());
  }
}

void CompileUnit::addLabelLowPc(uint64_t LabelLowPc, int64_t PcOffset) {
  Labels.insert({LabelLowPc, PcOffset});
  //  this->LowPc = std::min(this->LowPc, LabelLowPc + PcOffset);
}

void CompileUnit::addFunctionRange(uint64_t FuncLowPc, uint64_t FuncHighPc,
                                   int64_t PcOffset) {
  Ranges.insert(FuncLowPc, FuncHighPc, PcOffset);
  this->LowPc = std::min(LowPc, FuncLowPc + PcOffset);
  this->HighPc = std::max(HighPc, FuncHighPc + PcOffset);
}

void CompileUnit::noteRangeAttribute(const DIE &Die, DIEInteger *Attr) {
  if (Die.getTag() != dwarf::DW_TAG_compile_unit)
    RangeAttributes.push_back(Attr);
  else
    UnitRangeAttribute = Attr;
}

void CompileUnit::noteLocationAttribute(DIEInteger *Attr, int64_t PcOffset) {
  LocationAttributes.emplace_back(Attr, PcOffset);
}

void CompileUnit::addObjCAccelerator(const DIE *Die, const char *Name,
                                     uint32_t Offset, bool SkipPubSection) {
  ObjC.emplace_back(Name, Die, Offset, SkipPubSection);
}

/// \brief Add a name accelerator entry for \p Die with \p Name
/// which is stored in the string table at \p Offset.
void CompileUnit::addNameAccelerator(const DIE *Die, const char *Name,
                                     uint32_t Offset, bool SkipPubSection) {
  Pubnames.emplace_back(Name, Die, Offset, SkipPubSection);
}

/// \brief Add a type accelerator entry for \p Die with \p Name
/// which is stored in the string table at \p Offset.
void CompileUnit::addTypeAccelerator(const DIE *Die, const char *Name,
                                     uint32_t Offset,
                                     bool ObjCClassIsImplementation,
                                     uint32_t FullyQualifiedNameHash) {
  Pubtypes.emplace_back(Name, Die, Offset, false, ObjCClassIsImplementation,
                        FullyQualifiedNameHash);
}

/// \brief Add a namespace accelerator entry for \p Die with \p Name
/// which is stored in the string table at \p Offset.
void CompileUnit::addNamespaceAccelerator(const DIE *Die, const char *Name,
                                          uint32_t Offset) {
  Namespaces.emplace_back(Name, Die, Offset);
}

/// \brief A string table that doesn't need relocations.
///
/// We are doing a final link, no need for a string table that
/// has relocation entries for every reference to it. This class
/// provides this ablitity by just associating offsets with
/// strings.
class NonRelocatableStringpool {
public:
  /// \brief Entries are stored into the StringMap and simply linked
  /// together through the second element of this pair in order to
  /// keep track of insertion order.
  typedef StringMap<std::pair<uint32_t, StringMapEntryBase *>, BumpPtrAllocator>
      MapTy;

  typedef std::function<StringRef (StringRef)> TranslatorTy;

  NonRelocatableStringpool(TranslatorTy T = TranslatorTy())
    : CurrentEndOffset(0), Sentinel(0), Last(&Sentinel), Translator(T) {
    // Legacy dsymutil puts an empty string at the start of the line
    // table.
    getStringOffset("");
  }

  /// \brief Get the offset of string \p S in the string table. This
  /// can insert a new element or return the offset of a preexisitng
  /// one.
  uint32_t getStringOffset(StringRef S);

  /// \brief Get the offset of string \p S in the string table along
  /// with a StringRef that points to this stored string. This
  /// can insert a new element or return the offset of a preexisitng
  /// one.
  std::pair<const char *, uint32_t> getStringAndOffset(StringRef S);

  /// \brief Get permanent storage for \p S (but do not necessarily
  /// emit \p S in the output section).
  /// \returns The StringRef that points to permanent storage to use
  /// in place of \p S.
  StringRef internString(StringRef S);

  // \brief Return the first entry of the string table.
  const MapTy::MapEntryTy *getFirstEntry() const {
    return getNextEntry(&Sentinel);
  }

  // \brief Get the entry following \p E in the string table or null
  // if \p E was the last entry.
  const MapTy::MapEntryTy *getNextEntry(const MapTy::MapEntryTy *E) const {
    return static_cast<const MapTy::MapEntryTy *>(E->getValue().second);
  }

  uint64_t getSize() { return CurrentEndOffset; }

private:
  MapTy Strings;
  uint32_t CurrentEndOffset;
  MapTy::MapEntryTy Sentinel, *Last;
  TranslatorTy Translator;
};

/// \brief Get the offset of string \p S in the string table along
/// with a StringRef that points to this stored string. This
/// can insert a new element or return the offset of a preexisitng
/// one.
std::pair<const char *, uint32_t>
NonRelocatableStringpool::getStringAndOffset(StringRef S) {
  if (S.empty() && !Strings.empty())
    return std::make_pair("", 0);

  std::pair<uint32_t, StringMapEntryBase *> Entry(0, nullptr);
  MapTy::iterator It;
  bool Inserted;

  if (Translator)
    S = Translator(S);

  // A non-empty string can't be at offset 0, so if we have an entry
  // with a 0 offset, it must be a previously interned string.
  std::tie(It, Inserted) = Strings.insert(std::make_pair(S, Entry));
  if (Inserted || It->getValue().first == 0) {
    // Set offset and chain at the end of the entries list.
    It->getValue().first = CurrentEndOffset;
    CurrentEndOffset += S.size() + 1; // +1 for the '\0'.
    Last->getValue().second = &*It;
    Last = &*It;
  }
  return std::make_pair(It->getKey().data(), It->getValue().first);
};

/// \brief Get the offset of string \p S in the string table. This
/// can insert a new element or return the offset of a preexisitng
/// one.
uint32_t NonRelocatableStringpool::getStringOffset(StringRef S) {
  return getStringAndOffset(S).second;
};

/// \brief Put \p S into the StringMap so that it gets permanent
/// storage, but do not actually link it in the chain of elements
/// that go into the output section. A latter call to
/// getStringOffset() with the same string will chain it though.
StringRef NonRelocatableStringpool::internString(StringRef S) {
  std::pair<uint32_t, StringMapEntryBase *> Entry(0, nullptr);
  if (Translator)
    S = Translator(S);
  auto InsertResult = Strings.insert(std::make_pair(S, Entry));
  return InsertResult.first->getKey();
};

/// \brief The Dwarf streaming logic
///
/// All interactions with the MC layer that is used to build the debug
/// information binary representation are handled in this class.
class DwarfStreamer {
  /// \defgroup MCObjects MC layer objects constructed by the streamer
  /// @{
  std::unique_ptr<MCRegisterInfo> MRI;
  std::unique_ptr<MCAsmInfo> MAI;
  std::unique_ptr<MCObjectFileInfo> MOFI;
  std::unique_ptr<MCContext> MC;
  MCAsmBackend *MAB; // Owned by MCStreamer
  std::unique_ptr<MCInstrInfo> MII;
  std::unique_ptr<MCSubtargetInfo> MSTI;
  MCCodeEmitter *MCE; // Owned by MCStreamer
  MCStreamer *MS;     // Owned by AsmPrinter
  std::unique_ptr<TargetMachine> TM;
  std::unique_ptr<AsmPrinter> Asm;
  /// @}

  /// \brief the file we stream the linked Dwarf to.
  std::unique_ptr<raw_fd_ostream> OutFile;

  uint32_t RangesSectionSize;
  uint32_t LocSectionSize;
  uint32_t LineSectionSize;
  uint32_t FrameSectionSize;

  /// \brief Emit the pubnames or pubtypes section contribution for \p
  /// Unit into \p Sec. The data is provided in \p Names.
  void emitPubSectionForUnit(const MCSection *Sec, StringRef Name,
                             const CompileUnit &Unit,
                             const std::vector<CompileUnit::AccelInfo> &Names);

public:
  /// \brief Actually create the streamer and the ouptut file.
  ///
  /// This could be done directly in the constructor, but it feels
  /// more natural to handle errors through return value.
  bool init(Triple TheTriple, StringRef OutputFilename, bool Minimize);

  /// \brief Dump the file to the disk.
  bool finish(const DebugMap &Map, NonRelocatableStringpool::TranslatorTy T);

  AsmPrinter &getAsmPrinter() const { return *Asm; }

  /// \brief Set the current output section to debug_info and change
  /// the MC Dwarf version to \p DwarfVersion.
  void switchToDebugInfoSection(unsigned DwarfVersion);

  /// \brief Emit the compilation unit header for \p Unit in the
  /// debug_info section.
  ///
  /// As a side effect, this also switches the current Dwarf version
  /// of the MC layer to the one of U.getOrigUnit().
  void emitCompileUnitHeader(CompileUnit &Unit);

  /// \brief Recursively emit the DIE tree rooted at \p Die.
  void emitDIE(DIE &Die);

  /// \brief Emit the abbreviation table \p Abbrevs to the
  /// debug_abbrev section.
  void emitAbbrevs(const std::vector<DIEAbbrev *> &Abbrevs);

  /// \brief Emit the string table described by \p Pool.
  void emitStrings(const NonRelocatableStringpool &Pool);

  /// \brief Emit debug_ranges for \p FuncRange by translating the
  /// original \p Entries.
  void emitRangesEntries(
      int64_t UnitPcOffset, uint64_t OrigLowPc,
      FunctionIntervals::const_iterator FuncRange,
      const std::vector<DWARFDebugRangeList::RangeListEntry> &Entries,
      unsigned AddressSize);

  /// \brief Emit debug_aranges entries for \p Unit and if \p
  /// DoRangesSection is true, also emit the debug_ranges entries for
  /// the DW_TAG_compile_unit's DW_AT_ranges attribute.
  void emitUnitRangesEntries(CompileUnit &Unit, bool DoRangesSection);

  uint32_t getRangesSectionSize() const { return RangesSectionSize; }

  /// \brief Emit the debug_loc contribution for \p Unit by copying
  /// the entries from \p Dwarf and offseting them. Update the
  /// location attributes to point to the new entries.
  void emitLocationsForUnit(const CompileUnit &Unit, DWARFContext &Dwarf);

  /// \brief Emit the line table described in \p Rows into the
  /// debug_line section.
  void emitLineTableForUnit(MCDwarfLineTableParameters Params,
                            StringRef PrologueBytes, unsigned MinInstLength,
                            std::vector<DWARFDebugLine::Row> &Rows,
                            unsigned AdddressSize);

  void copyInvariantDebugSection(const object::ObjectFile &Obj,
                                 LinkOptions &);
  void translateLineTable(DataExtractor LineData, uint32_t Offset,
                          LinkOptions &Options);

  uint32_t getLineSectionSize() const { return LineSectionSize; }

  /// \brief Emit the .debug_pubnames contribution for \p Unit.
  void emitPubNamesForUnit(const CompileUnit &Unit);

  /// \brief Emit the .debug_pubtypes contribution for \p Unit.
  void emitPubTypesForUnit(const CompileUnit &Unit);

  /// \brief Emit a CIE.
  void emitCIE(StringRef CIEBytes);

  /// \brief Emit an FDE with data \p Bytes.
  void emitFDE(uint32_t CIEOffset, uint32_t AddrSize, uint64_t Address,
               StringRef Bytes);

  void emitAppleNamespaces(DwarfAccelTable &Table);
  void emitAppleNames(DwarfAccelTable &Table);
  void emitAppleTypes(DwarfAccelTable &Table);
  void emitAppleObjc(DwarfAccelTable &Table);
  void emitDebugInlined(
      const std::map<std::pair<uint32_t, uint32_t>,
                     std::vector<std::pair<uint32_t, uint64_t>>> &Inlined);
  void emitSwiftModule(StringRef Buffer);

  uint32_t getFrameSectionSize() const { return FrameSectionSize; }
};

bool DwarfStreamer::init(Triple TheTriple, StringRef OutputFilename,
                         bool Minimize) {
  std::string ErrorStr;
  std::string TripleName;
  StringRef Context = "dwarf streamer init";

  // Get the target.
  const Target *TheTarget =
      TargetRegistry::lookupTarget(TripleName, TheTriple, ErrorStr);
  if (!TheTarget)
    return error(ErrorStr, Context);
  TripleName = TheTriple.getTriple();

  // Create all the MC Objects.
  MRI.reset(TheTarget->createMCRegInfo(TripleName));
  if (!MRI)
    return error(Twine("no register info for target ") + TripleName, Context);

  MAI.reset(TheTarget->createMCAsmInfo(*MRI, TripleName));
  if (!MAI)
    return error("no asm info for target " + TripleName, Context);

  MOFI.reset(new MCObjectFileInfo);
  MC.reset(new MCContext(MAI.get(), MRI.get(), MOFI.get()));
  MOFI->InitMCObjectFileInfo(TripleName, Reloc::Default, CodeModel::Default,
                             *MC);

  MAB = TheTarget->createMCAsmBackend(*MRI, TripleName, "");
  if (!MAB)
    return error("no asm backend for target " + TripleName, Context);

  MII.reset(TheTarget->createMCInstrInfo());
  if (!MII)
    return error("no instr info info for target " + TripleName, Context);

  MSTI.reset(TheTarget->createMCSubtargetInfo(TripleName, "", ""));
  if (!MSTI)
    return error("no subtarget info for target " + TripleName, Context);

  MCE = TheTarget->createMCCodeEmitter(*MII, *MRI, *MC);
  if (!MCE)
    return error("no code emitter for target " + TripleName, Context);

  // Create the output file.
  std::error_code EC;
  OutFile =
      llvm::make_unique<raw_fd_ostream>(OutputFilename, EC, sys::fs::F_None);
  if (EC)
    return error(Twine(OutputFilename) + ": " + EC.message(), Context);

  MS = TheTarget->createMCObjectStreamer(TripleName, *MC, *MAB, *OutFile, MCE,
                                         *MSTI, false);
  if (!MS)
    return error("no object streamer for target " + TripleName, Context);

  // Finally create the AsmPrinter we'll use to emit the DIEs.
  TM.reset(TheTarget->createTargetMachine(TripleName, "", "", TargetOptions()));
  if (!TM)
    return error("no target machine for target " + TripleName, Context);

  Asm.reset(TheTarget->createAsmPrinter(*TM, std::unique_ptr<MCStreamer>(MS)));
  if (!Asm)
    return error("no asm printer for target " + TripleName, Context);

  LineSectionSize = 0;
  RangesSectionSize = 0;
  LocSectionSize = 0;
  LineSectionSize = 0;
  FrameSectionSize = 0;

  // Get ouptut sections in dsymutil's order. This isn't functionally important.
  MS->SwitchSection(MOFI->getDwarfAbbrevSection());
  MS->SwitchSection(MOFI->getDwarfARangesSection());
  MS->SwitchSection(MOFI->getDwarfFrameSection());
  MS->SwitchSection(MOFI->getDwarfInfoSection());

  if (!Minimize)
    Asm->OutStreamer.SwitchSection(
        MC->getMachOSection("__DWARF", "__debug_inlined", MachO::S_ATTR_DEBUG,
                            SectionKind::getMetadata()));

  MS->SwitchSection(MOFI->getDwarfLineSection());
  MS->SwitchSection(MOFI->getDwarfLocSection());

  if (!Minimize) {
    MS->SwitchSection(MOFI->getDwarfPubNamesSection());
    MS->SwitchSection(MOFI->getDwarfPubTypesSection());
  }

  MS->SwitchSection(MOFI->getDwarfRangesSection());
  MS->SwitchSection(MOFI->getDwarfStrSection());
  MS->SwitchSection(MOFI->getDwarfAccelNamesSection());
  MS->SwitchSection(MOFI->getDwarfAccelTypesSection());
  MS->SwitchSection(MOFI->getDwarfAccelNamespaceSection());
  MS->SwitchSection(MOFI->getDwarfAccelObjCSection());

  return true;
}

static MachO::mach_header getMachOHeader(const object::MachOObjectFile &Obj) {
  if (!Obj.is64Bit())
    return Obj.getHeader();

  MachO::mach_header_64 Header64 = Obj.getHeader64();
  MachO::mach_header Header;

  memcpy(&Header, &Header64, sizeof(Header));
  return Header;
}

template <typename FunctionTy>
static void iterateOnSegments(const object::MachOObjectFile &Obj,
                              const MachO::mach_header &Header,
                              FunctionTy Handler) {
  object::MachOObjectFile::LoadCommandInfo LCI = Obj.getFirstLoadCommandInfo();
  for (unsigned i = 0;; ++i) {
    if (LCI.C.cmd == MachO::LC_SEGMENT) {
      auto Segment = Obj.getSegmentLoadCommand(LCI);
      Handler(LCI, Segment.segname, Segment.nsects);
    } else if (LCI.C.cmd == MachO::LC_SEGMENT_64) {
      auto Segment = Obj.getSegment64LoadCommand(LCI);
      Handler(LCI, Segment.segname, Segment.nsects);
    }
    if (i == Header.ncmds - 1)
      break;
    LCI = Obj.getNextLoadCommandInfo(LCI);
  }
}

template <typename NListTy>
static bool transferSymbol(NListTy NList, bool IsLittleEndian,
                           StringRef Strings, SmallVectorImpl<char> &NewSymtab,
                           NonRelocatableStringpool &NewStrings,
                           bool &InDebugNote) {
  if ((NList.n_type & MachO::N_TYPE) == MachO::N_UNDF)
    return false;

  StringRef Name = StringRef(Strings.begin() + NList.n_strx);
  if (InDebugNote) {
    InDebugNote =
        (NList.n_type != MachO::N_SO) || (!Name.empty() && Name[0] != '\0');
    return false;
  } else if (NList.n_type == MachO::N_SO) {
    InDebugNote = true;
    return false;
  }

  // The + 1 is here to mimic dsymutil.
  NList.n_strx = NewStrings.getStringOffset(Name) + 1;
  if (IsLittleEndian != sys::IsLittleEndianHost)
    MachO::swapStruct(NList);

  NewSymtab.append((char *)&NList, (char *)(&NList + 1));
  return true;
}

static unsigned transferSymbols(const object::MachOObjectFile &Obj,
                                SmallVectorImpl<char> &NewSymtab,
                                NonRelocatableStringpool &NewStrings) {
  unsigned Syms = 0;
  StringRef Strings = Obj.getStringTableData();
  bool IsLittleEndian = Obj.isLittleEndian();
  bool InDebugNote = false;

  if (Obj.is64Bit()) {
    for (const object::SymbolRef &Symbol : Obj.symbols()) {
      object::DataRefImpl DRI = Symbol.getRawDataRefImpl();
      if (transferSymbol(Obj.getSymbol64TableEntry(DRI), IsLittleEndian,
                         Strings, NewSymtab, NewStrings, InDebugNote))
        ++Syms;
    }
  } else {
    for (const object::SymbolRef &Symbol : Obj.symbols()) {
      object::DataRefImpl DRI = Symbol.getRawDataRefImpl();
      if (transferSymbol(Obj.getSymbolTableEntry(DRI), IsLittleEndian, Strings,
                         NewSymtab, NewStrings, InDebugNote))
        ++Syms;
    }
  }
  return Syms;
}

static MachO::section
getSection(const object::MachOObjectFile &Obj,
           const MachO::segment_command &Seg,
           const object::MachOObjectFile::LoadCommandInfo &LCI, unsigned Idx) {
  return Obj.getSection(LCI, Idx);
}

static MachO::section_64
getSection(const object::MachOObjectFile &Obj,
           const MachO::segment_command_64 &Seg,
           const object::MachOObjectFile::LoadCommandInfo &LCI, unsigned Idx) {
  return Obj.getSection64(LCI, Idx);
}

template <typename SegmentTy>
static void transferSegmentAndSections(
    const object::MachOObjectFile::LoadCommandInfo &LCI, SegmentTy Segment,
    const object::MachOObjectFile &Obj, MCObjectWriter &Writer,
    uint64_t LinkeditOffset, uint64_t LinkeditSize, uint64_t DwarfSegmentSize,
    uint64_t &GapForDwarf, uint64_t &EndAddress) {
  if (StringRef("__DWARF") == Segment.segname)
    return;

  unsigned NumSections = Segment.nsects;
  Segment.fileoff = Segment.filesize = 0;

  if (StringRef("__LINKEDIT") == Segment.segname) {
    Segment.fileoff = LinkeditOffset;
    Segment.filesize = LinkeditSize;
  }

  // Check if the end address of the last segment and our current
  // start address leave a sufficient gap to store the __DWARF
  // segment.
  uint64_t PrevEndAddress = EndAddress;
  EndAddress = RoundUpToAlignment(EndAddress, 0x1000);
  if (GapForDwarf == UINT64_MAX && Segment.vmaddr > EndAddress &&
      Segment.vmaddr - EndAddress >= DwarfSegmentSize)
    GapForDwarf = EndAddress;

  // The segments are not necessarily sorted by their vmaddr.
  EndAddress = std::max<uint64_t>(PrevEndAddress,
                                  Segment.vmaddr + Segment.vmsize);
  if (Obj.isLittleEndian() != sys::IsLittleEndianHost)
    MachO::swapStruct(Segment);
  Writer.WriteBytes(StringRef((char *)&Segment, sizeof(Segment)));
  for (unsigned i = 0; i < NumSections; ++i) {
    auto Sect = getSection(Obj, Segment, LCI, i);
    Sect.offset = Sect.reloff = Sect.nreloc = 0;
    if (Obj.isLittleEndian() != sys::IsLittleEndianHost)
      MachO::swapStruct(Sect);
    Writer.WriteBytes(StringRef((char *)&Sect, sizeof(Sect)));
  }

  return;
}

template <typename SegmentTy, typename SectionTy>
static void createDwarfSegment(uint64_t VMAddr, uint64_t FileOffset,
                               uint64_t FileSize, unsigned NumSections,
                               MCAsmLayout &Layout, MachObjectWriter &Writer) {
  SegmentTy Segment;
  memset(&Segment, 0, sizeof(Segment));

  memcpy(Segment.segname, "__DWARF", sizeof("__DWARF"));
  Segment.cmd = sizeof(Segment) == sizeof(MachO::segment_command)
                    ? MachO::LC_SEGMENT
                    : MachO::LC_SEGMENT_64;
  Segment.cmdsize = sizeof(SegmentTy) + NumSections * sizeof(SectionTy);
  Segment.nsects = NumSections;
  Segment.fileoff = FileOffset;
  Segment.filesize = FileSize;
  Segment.vmsize = RoundUpToAlignment(FileSize, 0x1000);
  Segment.vmaddr = VMAddr;
  Segment.maxprot = 7;
  Segment.initprot = 3;
  if (Writer.isLittleEndian() != sys::IsLittleEndianHost)
    MachO::swapStruct(Segment);
  Writer.WriteBytes(StringRef((char *)&Segment, sizeof(Segment)));
  for (unsigned int i = 0, n = Layout.getSectionOrder().size(); i != n; ++i) {
    MCSectionData *SD = Layout.getSectionOrder()[i];
    if (SD->empty())
      continue;

    uint64_t Size = Layout.getSectionFileSize(SD);
    if (!Size)
      continue;

    SectionTy Section;
    memset(&Section, 0, sizeof(SectionTy));
    const MCSectionMachO &MachOSec = cast<MCSectionMachO>(SD->getSection());
    StringRef Name = MachOSec.getSectionName();
    unsigned Align = SD->getAlignment();
    if (Align > 1) {
      VMAddr = RoundUpToAlignment(VMAddr, Align);
      FileOffset = RoundUpToAlignment(FileOffset, Align);
    }
    // memcpy(Section.sectname, Name.data(), Name.size());
    // memcpy(Section.segname, "__DWARF", sizeof("__DWARF"));

    Writer.WriteBytes(Name, 16);
    Writer.WriteBytes("__DWARF", 16);

    if (Writer.is64Bit()) {
      Writer.Write64(VMAddr); // address
      Writer.Write64(Size);   // size
    } else {
      Writer.Write32(VMAddr); // address
      Writer.Write32(Size);   // size
    }
    Writer.Write32(FileOffset);
    Writer.Write32(Log2_32(Align));
    Writer.Write32(0); // reloc start
    Writer.Write32(0); // num relocs
    Writer.Write32(0); // Flags
    Writer.Write32(0); // reserved1
    Writer.Write32(0); // reserved2
    if (Writer.is64Bit())
      Writer.Write32(0); // reserved3

    // Writer.WriteSection(Layout.getAssembler(), Layout, *SD,
    //                     VMAddr, FileOffset, 0, 0);
    FileOffset += Layout.getSectionAddressSize(SD);
    VMAddr += Layout.getSectionAddressSize(SD);
  }
}

static bool isKext(const object::MachOObjectFile &Obj) {
  if (Obj.is64Bit())
    return Obj.getHeader64().filetype == MachO::MH_OBJECT;
  else
    return Obj.getHeader().filetype == MachO::MH_OBJECT;
}

static bool hasLinkEditSegment(const object::MachOObjectFile &Obj) {
  bool Result = false;
  iterateOnSegments(Obj, getMachOHeader(Obj),
                    [&](const object::MachOObjectFile::LoadCommandInfo &LCI,
                        const char *Name, uint32_t NumSections) {
                      if (StringRef("__LINKEDIT") == Name)
                        Result = true;
                    });
  return Result;
}

bool DwarfStreamer::finish(const DebugMap &DM,
                           NonRelocatableStringpool::TranslatorTy T) {
  // MS->Finish();
  // return true;
  auto &ObjectStreamer = *static_cast<MCObjectStreamer *>(MS);
  MCAssembler &MCAsm = ObjectStreamer.getAssembler();
  auto &Writer = static_cast<MachObjectWriter &>(MCAsm.getWriter());
  MCAsmLayout Layout(MCAsm);

  for (unsigned int i = 0, n = Layout.getSectionOrder().size(); i != n; ++i) {
    MCSectionData *SD = Layout.getSectionOrder()[i];
    unsigned FragmentIndex = 0;
    for (auto &Fragment : make_range(SD->begin(), SD->end()))
      Fragment.setLayoutOrder(FragmentIndex++);

    if (!SD->empty())
      Layout.getFragmentOffset(&*Layout.getSectionOrder()[i]->rbegin());
  }

  MCAsm.fixup(Layout);
  BinaryHolder InputBinaryHolder(false);
  auto ErrOrObjs = InputBinaryHolder.GetObjectFiles(DM.getBinaryPath());
  if (auto Error = ErrOrObjs.getError())
    return error(Twine("opening ") + DM.getBinaryPath() + ": " +
                     Error.message(),
                 "output file streaming");

  auto ErrOrInputBinary =
      InputBinaryHolder.GetAs<object::MachOObjectFile>(DM.getTriple());
  if (auto Error = ErrOrInputBinary.getError())
    return error(Twine("opening ") + DM.getBinaryPath() + ": " +
                     Error.message(),
                 "output file streaming");
  auto &InputBinary = *ErrOrInputBinary;

  bool Is64Bit = InputBinary.is64Bit();

  object::MachOObjectFile::LoadCommandInfo LCI =
      InputBinary.getFirstLoadCommandInfo();

  MachO::symtab_command SymtabCmd = InputBinary.getSymtabLoadCommand();
  MachO::uuid_command UUIDCmd;
  memset(&UUIDCmd, 0, sizeof(UUIDCmd));
  UUIDCmd.cmd = MachO::LC_UUID;
  UUIDCmd.cmdsize = 24;

  MachO::mach_header Header = getMachOHeader(InputBinary);
  for (unsigned i = 0;; ++i) {
    if (LCI.C.cmd == MachO::LC_UUID) {
      UUIDCmd = InputBinary.getUuidCommand(LCI);
      break;
    }
    if (i == Header.ncmds - 1)
      break;
    LCI = InputBinary.getNextLoadCommandInfo(LCI);
  }

  unsigned LoadCommandSize = 0;
  unsigned NumLoadCommands = 0;
  // We will copy the UUID if there is one.
  if (UUIDCmd.cmd != 0) {
    ++NumLoadCommands;
    LoadCommandSize += sizeof(MachO::uuid_command);
  }

  // Unless this is a kext, we will copy the symtab.
  // NOTE: dsymutil-classic compatibility: For information hiding
  // purposes, dsymutil does not emit symbol tables for kernel
  // extensions.
  bool ShouldEmitSymtab =
      !isKext(InputBinary) && hasLinkEditSegment(InputBinary);
  if (ShouldEmitSymtab) {
    LoadCommandSize += sizeof(MachO::symtab_command);
    ++NumLoadCommands;
  }

  unsigned HeaderSize =
      Is64Bit ? sizeof(MachO::mach_header_64) : sizeof(MachO::mach_header);
  // We will copy every segment that isn't __DWARF.
  iterateOnSegments(InputBinary, Header,
                    [&](const object::MachOObjectFile::LoadCommandInfo &LCI,
                        const char *Name, uint32_t NumSections) {
                      if (StringRef("__DWARF") == Name)
                        return;

                      ++NumLoadCommands;
                      if (Is64Bit) {
                        LoadCommandSize += sizeof(MachO::segment_command_64);
                        LoadCommandSize +=
                            NumSections * sizeof(MachO::section_64);
                      } else {
                        LoadCommandSize += sizeof(MachO::segment_command);
                        LoadCommandSize += NumSections * sizeof(MachO::section);
                      }
                    });

  // We will add our own brand new __DWARF segment if we have debug
  // info.
  unsigned NumDwarfSections = 0;
  uint64_t DwarfSegmentSize = 0;

  for (unsigned int i = 0, n = Layout.getSectionOrder().size(); i != n; ++i) {
    MCSectionData *SD = Layout.getSectionOrder()[i];
    if (SD->empty())
      continue;

    if (uint64_t Size = Layout.getSectionFileSize(SD)) {
      DwarfSegmentSize =
        RoundUpToAlignment(DwarfSegmentSize, SD->getAlignment());
      DwarfSegmentSize += Size;
      ++NumDwarfSections;
    }
  }

  if (NumDwarfSections) {
    ++NumLoadCommands;
    if (Is64Bit) {
      LoadCommandSize += sizeof(MachO::segment_command_64);
      LoadCommandSize += NumDwarfSections * sizeof(MachO::section_64);
    } else {
      LoadCommandSize += sizeof(MachO::segment_command);
      LoadCommandSize += NumDwarfSections * sizeof(MachO::section);
    }
  }

  SmallString<0> NewSymtab;
  NonRelocatableStringpool NewStrings(T);
  unsigned NListSize = Is64Bit ? sizeof(MachO::nlist_64) : sizeof(MachO::nlist);
  unsigned NumSyms = 0;
  uint64_t NewStringsSize = 0;
  if (ShouldEmitSymtab) {
    NewSymtab.reserve(SymtabCmd.nsyms * NListSize / 2);
    NumSyms = transferSymbols(InputBinary, NewSymtab, NewStrings);
    NewStringsSize = NewStrings.getSize() + 1;
  }

  uint64_t SymtabStart = LoadCommandSize;
  SymtabStart += HeaderSize;

  SymtabStart = RoundUpToAlignment(SymtabStart, 0x1000);

  Writer.WriteHeader(MachO::MH_DSYM, NumLoadCommands, LoadCommandSize, false);

  assert(OutFile->tell() == HeaderSize);
  // UUID Load Command
  if (UUIDCmd.cmd != 0) {
    Writer.Write32(UUIDCmd.cmd);
    Writer.Write32(UUIDCmd.cmdsize);
    Writer.WriteBytes(StringRef((const char *)UUIDCmd.uuid, 16));
    assert(OutFile->tell() == HeaderSize + sizeof(UUIDCmd));
  }

  // Symtab
  assert(SymtabCmd.cmd && "No symbol table.");
  uint64_t StringStart = SymtabStart + NumSyms * NListSize;
  if (ShouldEmitSymtab)
    Writer.WriteSymtabLoadCommand(SymtabStart, NumSyms, StringStart,
                                  NewStringsSize);

  uint64_t DwarfSegmentStart = StringStart + NewStringsSize;
  DwarfSegmentStart = RoundUpToAlignment(DwarfSegmentStart, 0x1000);

  // Imported segments and sections
  LCI = InputBinary.getFirstLoadCommandInfo();
  uint64_t EndAddress = 0;
  uint64_t GapForDwarf = UINT64_MAX;
  for (unsigned i = 0;; ++i) {
    if (LCI.C.cmd == MachO::LC_SEGMENT)
      transferSegmentAndSections(LCI, InputBinary.getSegmentLoadCommand(LCI),
                                 InputBinary, Writer, SymtabStart,
                                 StringStart + NewStringsSize - SymtabStart,
                                 DwarfSegmentSize, GapForDwarf, EndAddress);
    else if (LCI.C.cmd == MachO::LC_SEGMENT_64)
      transferSegmentAndSections(LCI, InputBinary.getSegment64LoadCommand(LCI),
                                 InputBinary, Writer, SymtabStart,
                                 StringStart + NewStringsSize - SymtabStart,
                                 DwarfSegmentSize, GapForDwarf, EndAddress);
    if (i == Header.ncmds - 1)
      break;
    LCI = InputBinary.getNextLoadCommandInfo(LCI);
  }

  uint64_t DwarfVMAddr = RoundUpToAlignment(EndAddress, 0x1000);
  uint64_t DwarfVMMax = Is64Bit ? UINT64_MAX : UINT32_MAX;
  if (DwarfVMAddr + DwarfSegmentSize > DwarfVMMax ||
      DwarfVMAddr + DwarfSegmentSize < DwarfVMAddr /* Overflow */) {
    // There is no room for the __DWARF segment at the end of the
    // address space. Look trhough segments to find a gap.
    DwarfVMAddr = GapForDwarf;
    if (DwarfVMAddr == UINT64_MAX)
      warn("not enough VM space for the __DWARF segment.",
           "output file streaming");
  }

  // Dwarf segment
  if (Is64Bit)
    createDwarfSegment<MachO::segment_command_64, MachO::section_64>(
        DwarfVMAddr, DwarfSegmentStart, DwarfSegmentSize, NumDwarfSections,
        Layout, Writer);
  else
    createDwarfSegment<MachO::segment_command, MachO::section>(
        DwarfVMAddr, DwarfSegmentStart, DwarfSegmentSize, NumDwarfSections,
        Layout, Writer);

  assert(OutFile->tell() == LoadCommandSize + HeaderSize);

  Writer.WriteZeros(SymtabStart - (LoadCommandSize + HeaderSize));

  assert(OutFile->tell() == SymtabStart);

  // Transfer symbols.
  if (ShouldEmitSymtab) {
    Writer.WriteBytes(NewSymtab.str());
    assert(OutFile->tell() == StringStart);

    // Transfer string table.
    Writer.WriteZeros(1);
    typedef NonRelocatableStringpool::MapTy MapTy;
    for (auto *Entry = NewStrings.getFirstEntry(); Entry;
         Entry = static_cast<MapTy::MapEntryTy *>(Entry->getValue().second))
      Writer.WriteBytes(
          StringRef(Entry->getKey().data(), Entry->getKey().size() + 1));
  }

  assert(OutFile->tell() == StringStart + NewStringsSize);

  // Pad till the Dwarf segment start
  Writer.WriteZeros(DwarfSegmentStart - (StringStart + NewStringsSize));
  assert(OutFile->tell() == DwarfSegmentStart);

  for (const MCSectionData &SD : make_range(MCAsm.begin(), MCAsm.end())) {
    if (SD.empty())
      continue;

    uint64_t Pos = OutFile->tell();
    Writer.WriteZeros(RoundUpToAlignment(Pos, SD.getAlignment()) - Pos);
    MCAsm.writeSectionData(&SD, Layout);
  }

  return true;
}

/// \brief Set the current output section to debug_info and change
/// the MC Dwarf version to \p DwarfVersion.
void DwarfStreamer::switchToDebugInfoSection(unsigned DwarfVersion) {
  MS->SwitchSection(MOFI->getDwarfInfoSection());
  MC->setDwarfVersion(DwarfVersion);
}

/// \brief Emit the compilation unit header for \p Unit in the
/// debug_info section.
///
/// A Dwarf scetion header is encoded as:
///  uint32_t   Unit length (omiting this field)
///  uint16_t   Version
///  uint32_t   Abbreviation table offset
///  uint8_t    Address size
///
/// Leading to a total of 11 bytes.
void DwarfStreamer::emitCompileUnitHeader(CompileUnit &Unit) {
  unsigned Version = Unit.getOrigUnit().getVersion();
  switchToDebugInfoSection(Version);

  // Emit size of content not including length itself. The size has
  // already been computed in CompileUnit::computeOffsets(). Substract
  // 4 to that size to account for the length field.
  Asm->EmitInt32(Unit.getNextUnitOffset() - Unit.getStartOffset() - 4);
  Asm->EmitInt16(Version);
  // We share one abbreviations table across all units so it's always at the
  // start of the section.
  Asm->EmitInt32(0);
  Asm->EmitInt8(Unit.getOrigUnit().getAddressByteSize());
}

/// \brief Emit the \p Abbrevs array as the shared abbreviation table
/// for the linked Dwarf file.
void DwarfStreamer::emitAbbrevs(const std::vector<DIEAbbrev *> &Abbrevs) {
  MS->SwitchSection(MOFI->getDwarfAbbrevSection());
  Asm->emitDwarfAbbrevs(Abbrevs);
}

/// \brief Recursively emit the DIE tree rooted at \p Die.
void DwarfStreamer::emitDIE(DIE &Die) {
  MS->SwitchSection(MOFI->getDwarfInfoSection());
  Asm->emitDwarfDIE(Die);
}

/// \brief Emit the debug_str section stored in \p Pool.
void DwarfStreamer::emitStrings(const NonRelocatableStringpool &Pool) {
  Asm->OutStreamer.SwitchSection(MOFI->getDwarfStrSection());
  for (auto *Entry = Pool.getFirstEntry(); Entry;
       Entry = Pool.getNextEntry(Entry))
    Asm->OutStreamer.EmitBytes(
        StringRef(Entry->getKey().data(), Entry->getKey().size() + 1));
}

void DwarfStreamer::emitAppleNamespaces(DwarfAccelTable &Table) {
  Asm->OutStreamer.SwitchSection(MOFI->getDwarfAccelNamespaceSection());
  Table.FinalizeTable(Asm.get(), "namespac");
  auto *SectionBegin = Asm->GetTempSymbol("namespac_begin");
  Asm->OutStreamer.EmitLabel(SectionBegin);
  Table.emit(Asm.get(), SectionBegin, nullptr);
}

void DwarfStreamer::emitAppleNames(DwarfAccelTable &Table) {
  Asm->OutStreamer.SwitchSection(MOFI->getDwarfAccelNamesSection());
  Table.FinalizeTable(Asm.get(), "names");
  auto *SectionBegin = Asm->GetTempSymbol("names_begin");
  Asm->OutStreamer.EmitLabel(SectionBegin);
  Table.emit(Asm.get(), SectionBegin, nullptr);
}

void DwarfStreamer::emitAppleTypes(DwarfAccelTable &Table) {
  Asm->OutStreamer.SwitchSection(MOFI->getDwarfAccelTypesSection());
  Table.FinalizeTable(Asm.get(), "types");
  auto *SectionBegin = Asm->GetTempSymbol("types_begin");
  Asm->OutStreamer.EmitLabel(SectionBegin);
  Table.emit(Asm.get(), SectionBegin, nullptr);
}

void DwarfStreamer::emitAppleObjc(DwarfAccelTable &Table) {
  Asm->OutStreamer.SwitchSection(MOFI->getDwarfAccelObjCSection());
  Table.FinalizeTable(Asm.get(), "objc");
  auto *SectionBegin = Asm->GetTempSymbol("objc_begin");
  Asm->OutStreamer.EmitLabel(SectionBegin);
  Table.emit(Asm.get(), SectionBegin, nullptr);
}

void DwarfStreamer::emitDebugInlined(
    const std::map<std::pair<uint32_t, uint32_t>,
                   std::vector<std::pair<uint32_t, uint64_t>>> &Inlined) {
  if (Inlined.empty())
    return;

  // Start the dwarf pubnames section.
  Asm->OutStreamer.SwitchSection(
      MC->getMachOSection("__DWARF", "__debug_inlined", MachO::S_ATTR_DEBUG,
                          SectionKind::getMetadata()));
  // Emit the header.
  MCSymbol *BeginLabel = Asm->GetTempSymbol("inlined_begin");
  MCSymbol *EndLabel = Asm->GetTempSymbol("inlined_end");
  Asm->EmitLabelDifference(EndLabel, BeginLabel, 4);

  Asm->OutStreamer.EmitLabel(BeginLabel);
  Asm->EmitInt16(2);
  int AddressSize = Asm->getDataLayout().getPointerSize();
  Asm->EmitInt8(AddressSize);

  for (const auto &InlineFunction : Inlined) {
    Asm->EmitInt32(InlineFunction.first.second);
    Asm->EmitInt32(InlineFunction.first.first);
    Asm->EmitULEB128(InlineFunction.second.size());

    for (const auto &InlinePoint : InlineFunction.second) {
      Asm->EmitInt32(InlinePoint.first);
      MS->EmitIntValue(InlinePoint.second, AddressSize);
    }
  }

  Asm->OutStreamer.EmitLabel(EndLabel);
}

/// \brief Emit the debug_range section contents for \p FuncRange by
/// translating the original \p Entries. The debug_range section
/// format is totally trivial, consisting just of pairs of address
/// sized addresses describing the ranges.
void DwarfStreamer::emitRangesEntries(
    int64_t UnitPcOffset, uint64_t OrigLowPc,
    FunctionIntervals::const_iterator FuncRange,
    const std::vector<DWARFDebugRangeList::RangeListEntry> &Entries,
    unsigned AddressSize) {
  MS->SwitchSection(MC->getObjectFileInfo()->getDwarfRangesSection());

  // Offset each range by the right amount.
  int64_t PcOffset = Entries.empty() ? 0 : FuncRange.value() + UnitPcOffset;
  for (const auto &Range : Entries) {
    if (Range.isBaseAddressSelectionEntry(AddressSize)) {
      warn("unsupported base address selection operation",
           "emitting debug_ranges");
      break;
    }
    // Do not emit empty ranges.
    if (Range.StartAddress == Range.EndAddress)
      continue;

    // All range entries should lie in the function range.
    if (!(Range.StartAddress + OrigLowPc >= FuncRange.start() &&
          Range.EndAddress + OrigLowPc <= FuncRange.stop()))
      warn("inconsistent range data.", "emitting debug_ranges");
    MS->EmitIntValue(Range.StartAddress + PcOffset, AddressSize);
    MS->EmitIntValue(Range.EndAddress + PcOffset, AddressSize);
    RangesSectionSize += 2 * AddressSize;
  }

  // Add the terminator entry.
  MS->EmitIntValue(0, AddressSize);
  MS->EmitIntValue(0, AddressSize);
  RangesSectionSize += 2 * AddressSize;
}

/// \brief Emit the debug_aranges contribution of a unit and
/// if \p DoDebugRanges is true the debug_range contents for a
/// compile_unit level DW_AT_ranges attribute (Which are basically the
/// same thing with a different base address).
/// Just aggregate all the ranges gathered inside that unit.
void DwarfStreamer::emitUnitRangesEntries(CompileUnit &Unit,
                                          bool DoDebugRanges) {
  unsigned AddressSize = Unit.getOrigUnit().getAddressByteSize();
  // Gather the ranges in a vector, so that we can simplify them. The
  // IntervalMap will have coalesced the non-linked ranges, but here
  // we want to coalesce the linked addresses.
  std::vector<std::pair<uint64_t, uint64_t>> Ranges;
  const auto &FunctionRanges = Unit.getFunctionRanges();
  for (auto Range = FunctionRanges.begin(), End = FunctionRanges.end();
       Range != End; ++Range)
    Ranges.push_back(std::make_pair(Range.start() + Range.value(),
                                    Range.stop() + Range.value()));

  // The object addresses where sorted, but again, the linked
  // addresses might end up in a different order.
  std::sort(Ranges.begin(), Ranges.end());

  if (!Ranges.empty()) {
    MS->SwitchSection(MC->getObjectFileInfo()->getDwarfARangesSection());

    MCSymbol *BeginLabel = Asm->GetTempSymbol("Barange", Unit.getUniqueID());
    MCSymbol *EndLabel = Asm->GetTempSymbol("Earange", Unit.getUniqueID());

    unsigned HeaderSize =
        sizeof(int32_t) + // Size of contents (w/o this field
        sizeof(int16_t) + // DWARF ARange version number
        sizeof(int32_t) + // Offset of CU in the .debug_info section
        sizeof(int8_t) +  // Pointer Size (in bytes)
        sizeof(int8_t);   // Segment Size (in bytes)

    unsigned TupleSize = AddressSize * 2;
    unsigned Padding = OffsetToAlignment(HeaderSize, TupleSize);

    Asm->EmitLabelDifference(EndLabel, BeginLabel, 4); // Arange length
    Asm->OutStreamer.EmitLabel(BeginLabel);
    Asm->EmitInt16(dwarf::DW_ARANGES_VERSION); // Version number
    Asm->EmitInt32(Unit.getStartOffset());     // Corresponding unit's offset
    Asm->EmitInt8(AddressSize);                // Address size
    Asm->EmitInt8(0);                          // Segment size

    Asm->OutStreamer.EmitFill(Padding, 0x0);

    for (auto Range = Ranges.begin(), End = Ranges.end(); Range != End;
         ++Range) {
      uint64_t RangeStart = Range->first;
      MS->EmitIntValue(RangeStart, AddressSize);
      while ((Range + 1) != End && Range->second == (Range + 1)->first)
        ++Range;
      MS->EmitIntValue(Range->second - RangeStart, AddressSize);
    }

    // Emit terminator
    Asm->OutStreamer.EmitIntValue(0, AddressSize);
    Asm->OutStreamer.EmitIntValue(0, AddressSize);
    Asm->OutStreamer.EmitLabel(EndLabel);
  }

  if (!DoDebugRanges)
    return;

  MS->SwitchSection(MC->getObjectFileInfo()->getDwarfRangesSection());
  // Offset each range by the right amount.
  int64_t PcOffset = -Unit.getLowPc();
  // Emit coalesced ranges.
  for (auto Range = Ranges.begin(), End = Ranges.end(); Range != End; ++Range) {
    MS->EmitIntValue(Range->first + PcOffset, AddressSize);
    while (Range + 1 != End && Range->second == (Range + 1)->first)
      ++Range;
    MS->EmitIntValue(Range->second + PcOffset, AddressSize);
    RangesSectionSize += 2 * AddressSize;
  }

  // Add the terminator entry.
  MS->EmitIntValue(0, AddressSize);
  MS->EmitIntValue(0, AddressSize);
  RangesSectionSize += 2 * AddressSize;
}

/// \brief Emit location lists for \p Unit and update attribtues to
/// point to the new entries.
void DwarfStreamer::emitLocationsForUnit(const CompileUnit &Unit,
                                         DWARFContext &Dwarf) {
  const std::vector<std::pair<DIEInteger *, int64_t>> &Attributes =
      Unit.getLocationAttributes();

  if (Attributes.empty())
    return;

  MS->SwitchSection(MC->getObjectFileInfo()->getDwarfLocSection());

  unsigned AddressSize = Unit.getOrigUnit().getAddressByteSize();
  const DWARFSection &InputSec = Dwarf.getLocSection();
  DataExtractor Data(InputSec.Data, Dwarf.isLittleEndian(), AddressSize);
  DWARFUnit &OrigUnit = Unit.getOrigUnit();
  const auto *OrigUnitDie = OrigUnit.getCompileUnitDIE(false);
  int64_t UnitPcOffset = 0;
  uint64_t OrigLowPc = OrigUnitDie->getAttributeValueAsAddress(
      &OrigUnit, dwarf::DW_AT_low_pc, -1ULL);
  if (OrigLowPc != -1ULL)
    UnitPcOffset = int64_t(OrigLowPc) - Unit.getLowPc();

  for (const auto &Attr : Attributes) {
    uint32_t Offset = Attr.first->getValue();
    Attr.first->setValue(LocSectionSize);
    // This is the quantity to add to the old location address to get
    // the correct address for the new one.
    int64_t LocPcOffset = Attr.second + UnitPcOffset;
    while (Data.isValidOffset(Offset)) {
      uint64_t Low = Data.getUnsigned(&Offset, AddressSize);
      uint64_t High = Data.getUnsigned(&Offset, AddressSize);
      if (Low == 0 && High == 0) {
        LocSectionSize += 2 * AddressSize;
        Asm->OutStreamer.EmitIntValue(0, AddressSize);
        Asm->OutStreamer.EmitIntValue(0, AddressSize);
        break;
      }
      uint64_t Length = Data.getU16(&Offset);
      if (Low != High) {
        Asm->OutStreamer.EmitIntValue(Low + LocPcOffset, AddressSize);
        Asm->OutStreamer.EmitIntValue(High + LocPcOffset, AddressSize);
        Asm->OutStreamer.EmitIntValue(Length, 2);
        // Just copy the bytes over.
        Asm->OutStreamer.EmitBytes(
            StringRef(InputSec.Data.substr(Offset, Length)));
        LocSectionSize += 2 * AddressSize + Length + 2;
      }
      Offset += Length;
    }
  }
}

void DwarfStreamer::emitLineTableForUnit(MCDwarfLineTableParameters Params,
                                         StringRef PrologueBytes,
                                         unsigned MinInstLength,
                                         std::vector<DWARFDebugLine::Row> &Rows,
                                         unsigned PointerSize) {
  // Switch to the section where the table will be emitted into.
  MS->SwitchSection(MC->getObjectFileInfo()->getDwarfLineSection());
  MCSymbol *LineStartSym = MC->CreateTempSymbol();
  MCSymbol *LineEndSym = MC->CreateTempSymbol();

  // The first 4 bytes is the tota-l length of the information for this
  // compilation unit (not including these 4 bytes for the length).
  Asm->EmitLabelDifference(LineEndSym, LineStartSym, 4);
  Asm->OutStreamer.EmitLabel(LineStartSym);
  // Copy Prologue.
  MS->EmitBytes(PrologueBytes);
  LineSectionSize += PrologueBytes.size() + 4;

  SmallString<128> EncodingBuffer;
  raw_svector_ostream EncodingOS(EncodingBuffer);

  if (Rows.empty()) {
    // We only have the dummy entry, dsymutil emits an entry with a 0
    // address in that case.
    MCDwarfLineAddr::Encode(*MC, Params, INT64_MAX, 0, EncodingOS);
    MS->EmitBytes(EncodingOS.str());
    LineSectionSize += EncodingBuffer.size();
    MS->EmitLabel(LineEndSym);
    return;
  }

  // Line table state machine fields
  unsigned FileNum = 1;
  unsigned LastLine = 1;
  unsigned Column = 0;
  unsigned IsStatement = 1;
  unsigned Isa = 0;
  uint64_t Address = -1ULL;

  unsigned RowsSinceLastSequence = 0;

  for (unsigned Idx = 0; Idx < Rows.size(); ++Idx) {
    auto &Row = Rows[Idx];

    int64_t AddressDelta;
    if (Address == -1ULL) {
      MS->EmitIntValue(dwarf::DW_LNS_extended_op, 1);
      MS->EmitULEB128IntValue(PointerSize + 1);
      MS->EmitIntValue(dwarf::DW_LNE_set_address, 1);
      MS->EmitIntValue(Row.Address, PointerSize);
      LineSectionSize += 2 + PointerSize + getULEB128Size(PointerSize + 1);
      AddressDelta = 0;
    } else {
      AddressDelta = (Row.Address - Address) / MinInstLength;
    }

    // FIXME: code copied and transformed from
    // MCDwarf.cpp::EmitDwarfLineTable. We should find a way to share
    // this code, but the current compatibility requirement with
    // classic dsymutil makes it hard. Revisit that once this
    // requirement is dropped.

    if (FileNum != Row.File) {
      FileNum = Row.File;
      MS->EmitIntValue(dwarf::DW_LNS_set_file, 1);
      MS->EmitULEB128IntValue(FileNum);
      LineSectionSize += 1 + getULEB128Size(FileNum);
    }
    if (Column != Row.Column) {
      Column = Row.Column;
      MS->EmitIntValue(dwarf::DW_LNS_set_column, 1);
      MS->EmitULEB128IntValue(Column);
      LineSectionSize += 1 + getULEB128Size(Column);
    }

    // FIXME: We should handle the discriminator here, but dsymutil
    // doesn't consider it, thus ignore it for now.

    if (Isa != Row.Isa) {
      Isa = Row.Isa;
      MS->EmitIntValue(dwarf::DW_LNS_set_isa, 1);
      MS->EmitULEB128IntValue(Isa);
      LineSectionSize += 1 + getULEB128Size(Isa);
    }
    if (IsStatement != Row.IsStmt) {
      IsStatement = Row.IsStmt;
      MS->EmitIntValue(dwarf::DW_LNS_negate_stmt, 1);
      LineSectionSize += 1;
    }
    if (Row.BasicBlock) {
      MS->EmitIntValue(dwarf::DW_LNS_set_basic_block, 1);
      LineSectionSize += 1;
    }

    if (Row.PrologueEnd) {
      MS->EmitIntValue(dwarf::DW_LNS_set_prologue_end, 1);
      LineSectionSize += 1;
    }

    if (Row.EpilogueBegin) {
      MS->EmitIntValue(dwarf::DW_LNS_set_epilogue_begin, 1);
      LineSectionSize += 1;
    }

    int64_t LineDelta = int64_t(Row.Line) - LastLine;
    if (!Row.EndSequence) {
      MCDwarfLineAddr::Encode(*MC, Params, LineDelta, AddressDelta, EncodingOS);
      MS->EmitBytes(EncodingOS.str());
      LineSectionSize += EncodingBuffer.size();
      EncodingBuffer.resize(0);
      EncodingOS.resync();
      Address = Row.Address;
      LastLine = Row.Line;
      RowsSinceLastSequence++;
    } else {
      if (LineDelta) {
        MS->EmitIntValue(dwarf::DW_LNS_advance_line, 1);
        MS->EmitSLEB128IntValue(LineDelta);
        LineSectionSize += 1 + getSLEB128Size(LineDelta);
      }
      if (AddressDelta) {
        MS->EmitIntValue(dwarf::DW_LNS_advance_pc, 1);
        MS->EmitULEB128IntValue(AddressDelta);
        LineSectionSize += 1 + getULEB128Size(AddressDelta);
      }
      MCDwarfLineAddr::Encode(*MC, Params, INT64_MAX, 0, EncodingOS);
      MS->EmitBytes(EncodingOS.str());
      LineSectionSize += EncodingBuffer.size();
      EncodingBuffer.resize(0);
      EncodingOS.resync();
      Address = -1ULL;
      LastLine = FileNum = IsStatement = 1;
      RowsSinceLastSequence = Column = Isa = 0;
    }
  }

  if (RowsSinceLastSequence) {
    MCDwarfLineAddr::Encode(*MC, Params, INT64_MAX, 0, EncodingOS);
    MS->EmitBytes(EncodingOS.str());
    LineSectionSize += EncodingBuffer.size();
    EncodingBuffer.resize(0);
    EncodingOS.resync();
  }

  MS->EmitLabel(LineEndSym);
}

static void emitSectionContents(const object::ObjectFile &Obj, StringRef SecName,
                                MCStreamer *MS) {
  StringRef Contents;
  if (auto Sec = getSectionByName(Obj, SecName))
    if (!Sec->getContents(Contents))
      MS->EmitBytes(Contents);
}

/// \brief copy the debug_line over to the updated binary while
/// unobfuscating the file names and directories.
void DwarfStreamer::translateLineTable(DataExtractor Data, uint32_t Offset,
                                       LinkOptions &Options) {
  MS->SwitchSection(MC->getObjectFileInfo()->getDwarfLineSection());
  StringRef Contents = Data.getData();
  // We have to deconstruct the line table header, because it
  // contains to length fields that will need to be updated when we
  // change the length of the files and directories in there.
  unsigned UnitLength = Data.getU32(&Offset);
  unsigned UnitEnd = Offset + UnitLength;
  MCSymbol *BeginLabel = MC->CreateTempSymbol();
  MCSymbol *EndLabel = MC->CreateTempSymbol();
  unsigned Version = Data.getU16(&Offset);

  if (Version > 5) {
    warn("Unsupported line table version. Dropping contents.",
         "Unobfsucating line table");
    return;
  }

  Asm->EmitLabelDifference(EndLabel, BeginLabel, 4);
  Asm->OutStreamer.EmitLabel(BeginLabel);
  Asm->EmitInt16(Version);
  LineSectionSize += 6;

  MCSymbol *HeaderBeginLabel = MC->CreateTempSymbol();
  MCSymbol *HeaderEndLabel = MC->CreateTempSymbol();
  Asm->EmitLabelDifference(HeaderEndLabel, HeaderBeginLabel, 4);
  Asm->OutStreamer.EmitLabel(HeaderBeginLabel);
  Offset += 4;
  LineSectionSize += 4;

  uint32_t AfterHeaderLengthOffset = Offset;
  // Skip to the directories.
  Offset += (Version >= 4) ? 5 : 4;
  unsigned OpcodeBase = Data.getU8(&Offset);
  Offset += OpcodeBase - 1;
  Asm->OutStreamer.EmitBytes(Contents.slice(AfterHeaderLengthOffset, Offset));
  LineSectionSize += Offset - AfterHeaderLengthOffset;

  // Offset points to the first directory.
  while (const char *Dir = Data.getCStr(&Offset)) {
    if (Dir[0] == 0)
      break;

    StringRef Translated = Options.Translator(Dir);
    Asm->OutStreamer.EmitBytes(Translated);
    Asm->EmitInt8(0);
    LineSectionSize += Translated.size() + 1;
  }
  Asm->EmitInt8(0);
  LineSectionSize += 1;

  while (const char *File = Data.getCStr(&Offset)) {
    if (File[0] == 0)
      break;

    StringRef Translated = Options.Translator(File);
    Asm->OutStreamer.EmitBytes(Translated);
    Asm->EmitInt8(0);
    LineSectionSize += Translated.size() + 1;

    uint32_t OffsetBeforeLEBs = Offset;
    Asm->EmitULEB128(Data.getULEB128(&Offset));
    Asm->EmitULEB128(Data.getULEB128(&Offset));
    Asm->EmitULEB128(Data.getULEB128(&Offset));
      LineSectionSize += Offset - OffsetBeforeLEBs;
  }
  Asm->EmitInt8(0);
  LineSectionSize += 1;

  Asm->OutStreamer.EmitLabel(HeaderEndLabel);
  // Copy the actual line table program over.
  Asm->OutStreamer.EmitBytes(Contents.slice(Offset, UnitEnd));
  LineSectionSize += UnitEnd - Offset;
  Asm->OutStreamer.EmitLabel(EndLabel);
  Offset = UnitEnd;
}

void DwarfStreamer::copyInvariantDebugSection(const object::ObjectFile &Obj,
                                              LinkOptions &Options) {
  if (!Options.Translator) {
    MS->SwitchSection(MC->getObjectFileInfo()->getDwarfLineSection());
    emitSectionContents(Obj, "debug_line", MS);
  }
  MS->SwitchSection(MC->getObjectFileInfo()->getDwarfLocSection());
  emitSectionContents(Obj, "debug_loc", MS);
  MS->SwitchSection(MC->getObjectFileInfo()->getDwarfRangesSection());
  emitSectionContents(Obj, "debug_ranges", MS);
  MS->SwitchSection(MC->getObjectFileInfo()->getDwarfFrameSection());
  emitSectionContents(Obj, "debug_frame", MS);
  MS->SwitchSection(MC->getObjectFileInfo()->getDwarfARangesSection());
  emitSectionContents(Obj, "debug_aranges", MS);

  if (!Options.Minimize && isa<object::MachOObjectFile>(Obj)) {
    Asm->OutStreamer.SwitchSection(
        MC->getMachOSection("__DWARF", "__debug_inlined", MachO::S_ATTR_DEBUG,
                            SectionKind::getMetadata()));
    emitSectionContents(Obj, "debug_inlined", MS);
  }
}


/// \brief Emit the pubnames or pubtypes section contribution for \p
/// Unit into \p Sec. The data is provided in \p Names.
void DwarfStreamer::emitPubSectionForUnit(
    const MCSection *Sec, StringRef SecName, const CompileUnit &Unit,
    const std::vector<CompileUnit::AccelInfo> &Names) {
  if (Names.empty())
    return;

  // Start the dwarf pubnames section.
  Asm->OutStreamer.SwitchSection(Sec);
  MCSymbol *BeginLabel =
      Asm->GetTempSymbol("pub" + SecName + "_begin", Unit.getUniqueID());
  MCSymbol *EndLabel =
      Asm->GetTempSymbol("pub" + SecName + "_end", Unit.getUniqueID());

  bool HeaderEmitted = false;
  // Emit the pubnames for this compilation unit.
  for (const auto &Name : Names) {
    if (Name.SkipPubSection)
      continue;

    if (!HeaderEmitted) {
      // Emit the header.
      Asm->EmitLabelDifference(EndLabel, BeginLabel, 4); // Length
      Asm->OutStreamer.EmitLabel(BeginLabel);
      Asm->EmitInt16(dwarf::DW_PUBNAMES_VERSION); // Version
      Asm->EmitInt32(Unit.getStartOffset());      // Unit offset
      Asm->EmitInt32(Unit.getNextUnitOffset() - Unit.getStartOffset()); // Size
      HeaderEmitted = true;
    }
    Asm->EmitInt32(Name.Die->getOffset());
    Asm->OutStreamer.EmitBytes(
        StringRef(Name.Name.data(), Name.Name.size() + 1));
  }

  if (!HeaderEmitted)
    return;
  Asm->EmitInt32(0); // End marker.
  Asm->OutStreamer.EmitLabel(EndLabel);
}

/// \brief Emit .debug_pubnames for \p Unit.
void DwarfStreamer::emitPubNamesForUnit(const CompileUnit &Unit) {
  emitPubSectionForUnit(MC->getObjectFileInfo()->getDwarfPubNamesSection(),
                        "names", Unit, Unit.getPubnames());
}

/// \brief Emit .debug_pubtypes for \p Unit.
void DwarfStreamer::emitPubTypesForUnit(const CompileUnit &Unit) {
  emitPubSectionForUnit(MC->getObjectFileInfo()->getDwarfPubTypesSection(),
                        "types", Unit, Unit.getPubtypes());
}

/// \brief Emit a CIE into the debug_frame section.
void DwarfStreamer::emitCIE(StringRef CIEBytes) {
  MS->SwitchSection(MC->getObjectFileInfo()->getDwarfFrameSection());

  MS->EmitBytes(CIEBytes);
  FrameSectionSize += CIEBytes.size();
}

/// \brief Emit a FDE into the debug_frame section. \p FDEByted
/// contains the FDE data without the length, CIE offset and address
/// which will be replaced with the paramter values.
void DwarfStreamer::emitFDE(uint32_t CIEOffset, uint32_t AddrSize,
                            uint64_t Address, StringRef FDEBytes) {
  MS->SwitchSection(MC->getObjectFileInfo()->getDwarfFrameSection());

  MS->EmitIntValue(FDEBytes.size() + 4 + AddrSize, 4);
  MS->EmitIntValue(CIEOffset, 4);
  MS->EmitIntValue(Address, AddrSize);
  MS->EmitBytes(FDEBytes);
  FrameSectionSize += FDEBytes.size() + 8 + AddrSize;
}

void DwarfStreamer::emitSwiftModule(StringRef Buffer) {
  auto Section = MC->getMachOSection("__DWARF", "__swift_ast",
                                     MachO::S_ATTR_DEBUG,
                                     SectionKind::getMetadata());
  auto &ObjectStreamer = *static_cast<MCObjectStreamer *>(MS);
  MCSectionData &SectData =
    ObjectStreamer.getAssembler().getOrCreateSectionData(*Section);
  SectData.setAlignment(1<<5);
  MS->SwitchSection(Section);
  MS->EmitBytes(Buffer);
}

static LLVM_CONSTEXPR DwarfAccelTable::Atom TypeAtoms[] = {
    DwarfAccelTable::Atom(dwarf::DW_ATOM_die_offset, dwarf::DW_FORM_data4),
    DwarfAccelTable::Atom(dwarf::DW_ATOM_die_tag, dwarf::DW_FORM_data2),
    DwarfAccelTable::Atom(5, dwarf::DW_FORM_data1),
    DwarfAccelTable::Atom(6, dwarf::DW_FORM_data4)};

/// \brief The core of the Dwarf linking logic.
///
/// The link of the dwarf information from the object files will be
/// driven by the selection of 'root DIEs', which are DIEs that
/// describe variables or functions that are present in the linked
/// binary (and thus have entries in the debug map). All the debug
/// information that will be linked (the DIEs, but also the line
/// tables, ranges, ...) is derived from that set of root DIEs.
///
/// The root DIEs are identified because they contain relocations that
/// correspond to a debug map entry at specific places (the low_pc for
/// a function, the location for a variable). These relocations are
/// called ValidRelocs in the DwarfLinker and are gathered as a very
/// first step when we start processing a DebugMapObject.
class DwarfLinker {
public:
  DwarfLinker(StringRef OutputFilename, const LinkOptions &Options)
      : OutputFilename(OutputFilename), Options(Options),
        BinHolder(Options.Verbose), CurrentDebugObject(nullptr),
        StringPool(Options.Translator),
        AppleNames(DwarfAccelTable::Atom(dwarf::DW_ATOM_die_offset,
                                         dwarf::DW_FORM_data4),
                   true, true),
        AppleNamespaces(DwarfAccelTable::Atom(dwarf::DW_ATOM_die_offset,
                                              dwarf::DW_FORM_data4),
                        true, true),
        AppleTypes(TypeAtoms, true, true),
        AppleObjc(DwarfAccelTable::Atom(dwarf::DW_ATOM_die_offset,
                                        dwarf::DW_FORM_data4),
                  true, true), LastCIEOffset(0) {}

  ~DwarfLinker() {
    for (auto *Abbrev : Abbreviations)
      delete Abbrev;
  }

  /// \brief Link the contents of the DebugMap.
  bool link(const DebugMap &);

private:
  /// \brief Called at the start of a debug object link.
  void startDebugObject(DWARFContext &, DebugMapObject &);

  /// \brief Called at the end of a debug object link.
  void endDebugObject();

  const DebugMapObject &getCurrentDMO() const { return *CurrentDebugObject; }

  /// \brief Keeps track of relocations.
  class RelocationManager {
    struct ValidReloc {
      uint32_t Offset;
      uint32_t Size;
      uint64_t Addend;
      const DebugMapObject::DebugMapEntry *Mapping;
      // FIXME: We store the FromDebugMapAttribute here, because we
      // don't want the to consider this as a 'root' relocation if it
      // came out of a partial address match. If partial address
      // matches weren't needed, we could remove that and use the
      // value in Mapping.
      bool FromDebugMap;

      ValidReloc(uint32_t Offset, uint32_t Size, uint64_t Addend,
                 const DebugMapObject::DebugMapEntry *Mapping,
                 bool FromDebugMap)
          : Offset(Offset), Size(Size), Addend(Addend), Mapping(Mapping),
            FromDebugMap(FromDebugMap) {}

      bool operator<(const ValidReloc &RHS) const {
        return Offset < RHS.Offset;
      }
    };

    DwarfLinker &Linker;

    /// \brief The valid relocations for the current DebugMapObject.
    /// This vector is sorted by relocation offset.
    std::vector<ValidReloc> ValidRelocs;

    /// \brief Index into ValidRelocs of the next relocation to
    /// consider. As we walk the DIEs in acsending file offset and as
    /// ValidRelocs is sorted by file offset, keeping this index
    /// uptodate is all we have to do to have a cheap lookup during the
    /// root DIE selection and during DIE cloning.
    unsigned NextValidReloc;

  public:
    RelocationManager(DwarfLinker &Linker)
        : Linker(Linker), NextValidReloc(0) {}

    bool hasValidRelocs() const { return !ValidRelocs.empty(); }
    /// \brief Reset the NextValidReloc counter.
    void resetValidRelocs() { NextValidReloc = 0; }

    /// \defgroup FindValidRelocations Translate debug map into a list
    /// of relevant relocations
    ///
    /// @{
    bool findValidRelocsInDebugInfo(const object::ObjectFile &Obj,
                                    const DebugMapObject &DMO);

    bool findValidRelocs(const object::SectionRef &Section,
                         const object::ObjectFile &Obj,
                         const DebugMapObject &DMO);

    bool findValidRelocsMachO(const object::SectionRef &Section,
                              const object::MachOObjectFile &Obj,
                              const DebugMapObject &DMO);
    /// @}

    bool hasValidRelocation(uint32_t StartOffset, uint32_t EndOffset,
                            CompileUnit::DIEInfo &Info);

    bool applyValidRelocs(MutableArrayRef<char> Data, uint32_t BaseOffset,
                          bool isLittleEndian);
  };

  /// \defgroup FindRootDIEs Find DIEs corresponding to debug map entries.
  ///
  /// @{
  /// \brief Recursively walk the \p DIE tree and look for DIEs to
  /// keep. Store that information in \p CU's DIEInfo.
  void lookForDIEsToKeep(RelocationManager &RelocMgr,
                         const DWARFDebugInfoEntryMinimal &DIE,
                         const DebugMapObject &DMO, CompileUnit &CU,
                         unsigned Flags);

  /// \brief If this CU is really a skeleton clang module reference,
  /// register it in ClangModules and return true.
  bool registerModuleReference(const DWARFDebugInfoEntryMinimal &CUDie,
                               const DWARFUnit &Unit, DebugMap &ModuleMap,
                               unsigned Indent = 0);

  /// \brief Recursively add the debug info in this clang module .pcm
  /// file (and all the modules imported by it in a bottom-up fashion)
  /// to Units.
  void loadClangModule(StringRef Filename, StringRef ModulePath,
                       DebugMap &ModuleMap, unsigned Indent = 0);

  /// \brief Attempt to load a debug object from disk.
  ErrorOr<const object::ObjectFile &> loadObject(BinaryHolder &BinaryHolder,
                                                 DebugMapObject &Obj,
                                                 const DebugMap &Map);

  /// \brief Flags passed to DwarfLinker::lookForDIEsToKeep
  enum TravesalFlags {
    TF_Keep = 1 << 0,            ///< Mark the traversed DIEs as kept.
    TF_InFunctionScope = 1 << 1, ///< Current scope is a fucntion scope.
    TF_DependencyWalk = 1 << 2,  ///< Walking the dependencies of a kept DIE.
    TF_ParentWalk = 1 << 3,      ///< Walking up the parents of a kept DIE.
    TF_SkipPC = 1 << 4,          ///< Do not clone location attributes.
    TF_ODR = 1 << 5,             ///< Use the ODR whhile keeping dependants.
    TF_SkipSubprograms = 1 << 6, ///< Skip non declaration functions.
  };

  /// \brief Mark the passed DIE as well as all the ones it depends on
  /// as kept.
  void keepDIEAndDependencies(RelocationManager &RelocMgr,
                              const DWARFDebugInfoEntryMinimal &DIE,
                              CompileUnit::DIEInfo &MyInfo,
                              const DebugMapObject &DMO, CompileUnit &CU,
                              bool UseODR);

  unsigned shouldKeepDIE(RelocationManager &RelocMgr,
                         const DWARFDebugInfoEntryMinimal &DIE,
                         CompileUnit &Unit, CompileUnit::DIEInfo &MyInfo,
                         unsigned Flags);

  unsigned shouldKeepVariableDIE(RelocationManager &RelocMgr,
                                 const DWARFDebugInfoEntryMinimal &DIE,
                                 CompileUnit &Unit,
                                 CompileUnit::DIEInfo &MyInfo, unsigned Flags);

  unsigned shouldKeepSubprogramDIE(RelocationManager &RelocMgr,
                                   const DWARFDebugInfoEntryMinimal &DIE,
                                   CompileUnit &Unit,
                                   CompileUnit::DIEInfo &MyInfo,
                                   unsigned Flags);

  unsigned shouldKeepLabelDIE(RelocationManager &RelocMgr,
                              const DWARFDebugInfoEntryMinimal &DIE,
                              CompileUnit &Unit, CompileUnit::DIEInfo &MyInfo,
                              unsigned Flags);
  /// @}

  /// \defgroup Linking Methods used to link the debug information
  ///
  /// @{

  class DIECloner {
    DwarfLinker &Linker;
    RelocationManager &RelocMgr;
    /// \brief Allocator used for all the DIEValue objects.
    BumpPtrAllocator &DIEAlloc;
    /// The .apple_exttypes index form the curent object file.
    std::unique_ptr<DWARFAcceleratorTable> ExtTypes;
    MutableArrayRef<CompileUnit> CompileUnits;
    LinkOptions Options;

  public:
    DIECloner(DwarfLinker &Linker, RelocationManager &RelocMgr,
              BumpPtrAllocator &DIEAlloc,
              MutableArrayRef<CompileUnit> CompileUnits,
              LinkOptions &Options)
        : Linker(Linker), RelocMgr(RelocMgr), DIEAlloc(DIEAlloc),
          CompileUnits(CompileUnits), Options(Options) {}

    /// \brief Recursively clone \p InputDIE into an tree of DIE objects
    /// where useless (as decided by lookForDIEsToKeep()) bits have been
    /// stripped out and addresses have been rewritten according to the
    /// debug map.
    ///
    /// \param OutOffset is the offset the cloned DIE in the output
    /// compile unit.
    /// \param PCOffset (while cloning a function scope) is the offset
    /// applied to the entry point of the function to get the linked address.
    ///
    /// \returns the root of the cloned tree.
    DIE *cloneDIE(const DWARFDebugInfoEntryMinimal &InputDIE, CompileUnit &U,
                  int64_t PcOffset, uint32_t OutOffset, unsigned Flags);

    /// \brief Construct the output DIE tree by cloning the DIEs we
    /// chose to keep above. If there are no valid relocs, then there's
    /// nothing to clone/emit.
    void cloneAllCompileUnits(DWARFContextInMemory &DwarfContext);

  private:
    /// \brief Helper for cloneDIE.
    void addObjCAccelerator(CompileUnit &Unit, const DIE *Die, const char *Name,
                            bool SkipPubSection);

    typedef DWARFAbbreviationDeclaration::AttributeSpec AttributeSpec;

    /// \brief Information gathered and exchanged between the various
    /// clone*Attributes helpers about the attributes of a particular DIE.
    struct AttributesInfo {
      const char *Name, *MangledName;         ///< Names.
      uint32_t NameOffset, MangledNameOffset; ///< Offsets in the string pool.

      uint64_t LowPc;      ///< Value of low_pc for this DIE.
      uint64_t OrigLowPc;  ///< Value of AT_low_pc in the input DIE
      uint64_t OrigHighPc; ///< Value of AT_high_pc in the input DIE
      int64_t PCOffset; ///< Offset to apply to PC addresses inside a function.

      StringRef NameWithoutTemplate;
      uint32_t NameWithoutTemplateOffset;

      bool HasLowPc;      ///< Does the DIE have a low_pc attribute?
      bool IsDeclaration; ///< Is this DIE only a declaration?

      AttributesInfo()
          : Name(nullptr), MangledName(nullptr), NameOffset(0),
            MangledNameOffset(0), LowPc(0), OrigLowPc(UINT64_MAX),
            OrigHighPc(0), PCOffset(0), HasLowPc(false), IsDeclaration(false) {}
    };

    /// \brief Helper for cloneDIE.
    unsigned cloneAttribute(DIE &Die,
                            const DWARFDebugInfoEntryMinimal &InputDIE,
                            CompileUnit &U, const DWARFFormValue &Val,
                            const AttributeSpec AttrSpec, unsigned AttrSize,
                            AttributesInfo &AttrInfo);

    /// \brief Helper for cloneDIE.
    unsigned cloneStringAttribute(DIE &Die, AttributeSpec AttrSpec,
                                  const DWARFFormValue &Val, const DWARFUnit &U,
                                  AttributesInfo &Info);

    /// \brief Helper for cloneDIE.
    unsigned cloneExtTypeRef(DIE &Die,
                             const DWARFDebugInfoEntryMinimal &InputDIE,
                             AttributeSpec AttrSpec, const DWARFFormValue &Val,
                             const DWARFUnit &U, AttributesInfo &Info);

    /// \brief Helper for cloneDIE.
    unsigned
    cloneDieReferenceAttribute(DIE &Die,
                               const DWARFDebugInfoEntryMinimal &InputDIE,
                               AttributeSpec AttrSpec, unsigned AttrSize,
                               const DWARFFormValue &Val, CompileUnit &Unit);

    /// \brief Helper for cloneDIE.
    unsigned cloneBlockAttribute(DIE &Die, AttributeSpec AttrSpec,
                                 const DWARFFormValue &Val, unsigned AttrSize);

    /// \brief Helper for cloneDIE.
    unsigned cloneAddressAttribute(DIE &Die, AttributeSpec AttrSpec,
                                   const DWARFFormValue &Val, CompileUnit &Unit,
                                   AttributesInfo &Info);

    /// \brief Helper for cloneDIE.
    unsigned cloneScalarAttribute(DIE &Die,
                                  const DWARFDebugInfoEntryMinimal &InputDIE,
                                  CompileUnit &U, AttributeSpec AttrSpec,
                                  const DWARFFormValue &Val, unsigned AttrSize,
                                  AttributesInfo &Info);

    /// \brief Helper for cloneDIE.
    bool getDIENames(const DWARFDebugInfoEntryMinimal &Die, DWARFUnit &U,
                     AttributesInfo &Info, bool StripTemplate = false);

    void copyAbbrev(const DWARFAbbreviationDeclaration &Abbrev, bool hasODR);

    uint32_t hashFullyQualifiedName(const DWARFDebugInfoEntryMinimal *DIE,
                                    CompileUnit &U, int RecurseDepth = 0);
    unsigned resolveExtType(StringRef UID, const DWARFUnit &U);
  };

  /// \brief Assign an abbreviation number to \p Abbrev
  void AssignAbbrev(DIEAbbrev &Abbrev);

  /// \brief FoldingSet that uniques the abbreviations.
  FoldingSet<DIEAbbrev> AbbreviationsSet;
  /// \brief Storage for the unique Abbreviations.
  /// This is passed to AsmPrinter::emitDwarfAbbrevs(), thus it cannot
  /// be changed to a vecot of unique_ptrs.
  std::vector<DIEAbbrev *> Abbreviations;

  /// \brief Compute and emit debug_ranges section for \p Unit, and
  /// patch the attributes referencing it.
  void patchRangesForUnit(const CompileUnit &Unit, DWARFContext &Dwarf) const;

  /// \brief Generate and emit the DW_AT_ranges attribute for a
  /// compile_unit if it had one.
  void generateUnitRanges(CompileUnit &Unit) const;

  /// \brief Extract the line tables fromt he original dwarf, extract
  /// the relevant parts according to the linked function ranges and
  /// emit the result in the debug_line section.
  void patchLineTableForUnit(CompileUnit &Unit, const DebugMapObject &DMO,
                             DWARFContext &OrigDwarf);

  /// \brief Emit the accelerator entries for \p Unit.
  void emitAcceleratorEntriesForUnit(CompileUnit &Unit);

  /// \brief DIELoc objects that need to be destructed (but not freed!).
  std::vector<DIELoc *> DIELocs;
  /// \brief DIEBlock objects that need to be destructed (but not freed!).
  std::vector<DIEBlock *> DIEBlocks;
  /// \brief Allocator used for all the DIEValue objects.
  BumpPtrAllocator DIEAlloc;
  /// @}

  /// \defgroup Helpers Various helper methods.
  ///
  /// @{
public:
  void reportWarning(const Twine &Warning, const DWARFUnit *Unit = nullptr,
                     const DWARFDebugInfoEntryMinimal *DIE = nullptr) const;

  bool createStreamer(Triple TheTriple, StringRef InputFileName,
                      StringRef OutputFileName);

protected:
  /// @}

  void patchFrameInfoForObject(const DebugMapObject &, DWARFContext &,
                               unsigned AddrSize);

  uint64_t SizeUnit(CompileUnit *Unit);

private:
  std::string OutputFilename;
  LinkOptions Options;
  BinaryHolder BinHolder;
  std::unique_ptr<DwarfStreamer> Streamer;
  uint64_t OutputDebugInfoSize = 0;
  unsigned UnitID; ///< A unique ID that identifies each compile unit.

  DenseMap<DIE *, DeclContext *> DIEToDeclContext;

  /// The units of the current debug map object.
  SmallVector<CompileUnit, 1> Units;

  /// The debug map object curently under consideration.
  DebugMapObject *CurrentDebugObject;

  /// \brief The Dwarf string pool
  NonRelocatableStringpool StringPool;

  /// \brief This map is keyed by the entry PC of functions in that
  /// debug object and the associated value is a pair storing the
  /// corresponding end PC and the offset to apply to get the linked
  /// address.
  ///
  /// See startDebugObject() for a more complete description of its use.
  std::map<uint64_t, std::pair<uint64_t, int64_t>> Ranges;

  DeclContext RootDeclContext;
  DeclContext::Map DeclContexts;

  std::vector<DIE *> DIEsToDelete;

  StringMap<uint32_t> EmittedCIEs;
  DwarfAccelTable AppleNames;
  DwarfAccelTable AppleNamespaces;
  DwarfAccelTable AppleTypes;
  DwarfAccelTable AppleObjc;
  uint32_t LastCIEOffset;

  typedef std::pair<uint32_t, uint32_t> InlinedKey;
  typedef std::pair<uint32_t, uint64_t> InlinedEntry;
  typedef std::map<InlinedKey, std::vector<InlinedEntry>> InlinedMap;
  InlinedMap Inlined;

  struct ClangModule {
    std::unique_ptr<BinaryHolder> BinaryHolder;
    std::unique_ptr<DWARFContext> DwarfContext;
    std::unique_ptr<CompileUnit> Unit;
    std::unique_ptr<DWARFAcceleratorTable> AppleTypes;
  };
  /// FIXME: We may need to use something more resilient than the PCM filename.
  StringMap<ClangModule> ClangModules;
  BumpPtrAllocator ModuleDIEAlloc;
};
}

/// \brief Similar to DWARFUnitSection::getUnitForOffset(), but
/// returning our CompileUnit object instead.
static CompileUnit *getUnitForOffset(MutableArrayRef<CompileUnit> &Units,
                                     unsigned Offset) {
  auto CU =
      std::upper_bound(Units.begin(), Units.end(), Offset,
                       [](uint32_t LHS, const CompileUnit &RHS) {
                         return LHS < RHS.getOrigUnit().getNextUnitOffset();
                       });
  return CU != Units.end() ? &*CU : nullptr;
}

static const DWARFDebugInfoEntryMinimal *
resolveDIEReference(MutableArrayRef<CompileUnit> Units, uint64_t RefOffset,
                    const DWARFUnit &Unit,
                    const DWARFDebugInfoEntryMinimal &DIE, CompileUnit *&RefCU,
                    const DwarfLinker &Linker) {
  if ((RefCU = getUnitForOffset(Units, RefOffset)))
    if (const auto *RefDie = RefCU->getOrigUnit().getDIEForOffset(RefOffset))
      return RefDie;

  Linker.reportWarning("could not find referenced DIE", &Unit, &DIE);
  return nullptr;
}

/// \brief Resolve the DIE attribute reference that has been
/// extracted in \p RefValue. The resulting DIE migh be in another
/// CompileUnit which is stored into \p ReferencedCU.
/// \returns null if resolving fails for any reason.
static const DWARFDebugInfoEntryMinimal *
resolveDIEReference(MutableArrayRef<CompileUnit> Units,
                    const DWARFFormValue &RefValue, const DWARFUnit &Unit,
                    const DWARFDebugInfoEntryMinimal &DIE, CompileUnit *&RefCU,
                    const DwarfLinker &Linker) {
  assert(RefValue.isFormClass(DWARFFormValue::FC_Reference));
  uint64_t RefOffset = *RefValue.getAsReference(&Unit);

  return resolveDIEReference(Units, RefOffset, Unit, DIE, RefCU, Linker);
}

static std::pair<DeclContext *, DeclContext *>
updateDeclContext(DeclContext *Context,
                  const DWARFDebugInfoEntryMinimal *DIE,
                  CompileUnit &U,
                  NonRelocatableStringpool &StringPool,
                  DeclContext::Map &Contexts) {
  if (!Context)
    return std::make_pair(nullptr, nullptr);

  // FIXME: dsymutil-classic compat: We should bail out here if we
  // have a specification or an abstract_origin. We will get the
  // parent context wrong here.

  switch (DIE->getTag()) {
  default:
    return std::make_pair(nullptr, nullptr);
  case dwarf::DW_TAG_compile_unit:
    return std::make_pair(Context, Context);
  case dwarf::DW_TAG_subprogram:
    if ((Context->getTag() == dwarf::DW_TAG_namespace ||
         Context->getTag() == dwarf::DW_TAG_compile_unit) &&
        !DIE->getAttributeValueAsUnsignedConstant(&U.getOrigUnit(),
                                                  dwarf::DW_AT_external, 0))
      return std::make_pair(nullptr, nullptr);
  case dwarf::DW_TAG_member:
  case dwarf::DW_TAG_namespace:
  case dwarf::DW_TAG_structure_type:
  case dwarf::DW_TAG_class_type:
  case dwarf::DW_TAG_union_type:
  case dwarf::DW_TAG_enumeration_type:
  case dwarf::DW_TAG_typedef:
  case dwarf::DW_TAG_base_type:
  case dwarf::DW_TAG_pointer_type:
  case dwarf::DW_TAG_unspecified_type:
    if (DIE->getAttributeValueAsUnsignedConstant(&U.getOrigUnit(),
                                                 dwarf::DW_AT_artificial, 0))
      return std::make_pair(nullptr, nullptr);
    break;
  }

  const char *Name = DIE->getName(&U.getOrigUnit(), DINameKind::LinkageName);
  const char *ShortName = DIE->getName(&U.getOrigUnit(), DINameKind::ShortName);
  StringRef NameRef;
  StringRef ShortNameRef;
  StringRef FileRef;

  if (Name)
    NameRef = StringPool.internString(Name);
  else if (DIE->getTag() == dwarf::DW_TAG_namespace)
    // FIXME: I think uniquing within anonymous namespaces is
    // wrong. There is no ODR guarantee there
    NameRef = StringPool.internString("(anonymous namespace)");

  if (ShortName && ShortName != Name)
    ShortNameRef = StringPool.internString(ShortName);
  else
    ShortNameRef = NameRef;
  // if (DIE->getTag() != dwarf::DW_TAG_subprogram &&
  //     DIE->getAttributeValueAsUnsignedConstant(&U.getOrigUnit(),
  //                                              dwarf::DW_AT_declaration, 0))
  //   return &Context->getChild((Twine(".fwd.") + Name).str(),
  //     DIE->getTag(), U.ID);
  if (DIE->getTag() != dwarf::DW_TAG_class_type &&
      DIE->getTag() != dwarf::DW_TAG_structure_type &&
      DIE->getTag() != dwarf::DW_TAG_union_type &&
      DIE->getTag() != dwarf::DW_TAG_enumeration_type && NameRef.empty())
    return std::make_pair(nullptr, nullptr);

  std::string File;
  unsigned Line = 0;
  unsigned ByteSize = 0;

  if (U.hasODR()) {
    ByteSize = DIE->getAttributeValueAsUnsignedConstant(
        &U.getOrigUnit(), dwarf::DW_AT_byte_size, UINT64_MAX);
    if (DIE->getTag() != dwarf::DW_TAG_namespace || !Name) {
      if (unsigned FileNum = DIE->getAttributeValueAsUnsignedConstant(
              &U.getOrigUnit(), dwarf::DW_AT_decl_file, 0))
        if (const auto *LT = U.getOrigUnit().getContext().getLineTableForUnit(
                &U.getOrigUnit())) {
          // FIXME: dsymutil-classic compat. -classic tries to use the CU file
          // for anonymous namespaces and not the namespace's
          // file. Not sure which is better. Neither I guess,
          // anonymous namespaces shouldn't be uniqued. Moreover, to
          // do so, classic uses the file at index one, which I'm
          // pretty sure has not much to do with the CU file...
          // The rest of this discriming logic is in getChild().
          if (!Name && DIE->getTag() == dwarf::DW_TAG_namespace)
            FileNum = 1;

          // FIXME: Passing U.getOrigUnit().getCompilationDir()
          // instead of "" would allow more uniquing, but for now, do
          // it this way to match dsymutil-classic.
          if (LT->getFileNameByIndex(
                  FileNum, "",
                  DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath,
                  File)) {
            Line = DIE->getAttributeValueAsUnsignedConstant(
                &U.getOrigUnit(), dwarf::DW_AT_decl_line, 0);
#ifdef HAVE_REALPATH
            if (const char *ResolvedPath = U.getResolvedPath(FileNum)) {
              File = ResolvedPath;
            } else {
              char RealPath[PATH_MAX + 1];
              RealPath[PATH_MAX] = 0;
              if (::realpath(File.c_str(), RealPath))
                File = RealPath;
              U.setResolvedPath(FileNum, File);
            }
#endif
            FileRef = StringPool.internString(File);
          }
        }
    }
  }

  if (Line || !NameRef.empty()) {
    auto Res =
        Context->getChild(Contexts, ShortNameRef, NameRef, FileRef, Line,
                          ByteSize, DIE->getTag(), U);
    if (!Res.first)
      return Res;
    if ((DIE->getTag() == dwarf::DW_TAG_member ||
         DIE->getTag() == dwarf::DW_TAG_subprogram) &&
        Context->getTag() != dwarf::DW_TAG_structure_type &&
        Context->getTag() != dwarf::DW_TAG_class_type)
      Res.first->invalidate();
    if (DIE->getTag() == dwarf::DW_TAG_union_type ||
        DIE->getTag() == dwarf::DW_TAG_base_type ||
        DIE->getTag() == dwarf::DW_TAG_pointer_type ||
        DIE->getTag() == dwarf::DW_TAG_unspecified_type)
      Res.first->invalidate();

    return Res;
  }

  return std::make_pair(nullptr, nullptr);
}

void DeclContext::resetLastSeen(CompileUnit &U) {
  DWARFUnit &OrigUnit = U.getOrigUnit();
  uint32_t FirstIdx = OrigUnit.getDIEIndex(LastSeenDIE);
  U.getInfo(FirstIdx).Ctxt = nullptr;
}

static bool isODRAttribute(uint16_t Attr) {
  switch (Attr) {
  default:
    return false;
  case dwarf::DW_AT_type:
  case dwarf::DW_AT_containing_type:
  case dwarf::DW_AT_specification:
  case dwarf::DW_AT_abstract_origin:
  case dwarf::DW_AT_import:
    return true;
  }
  llvm_unreachable("Improper attribute.");
}

std::pair<DeclContext *, DeclContext *>
DeclContext::getChild(DeclContext::Map &Contexts, StringRef ShortName,
                                   StringRef Name, StringRef File, unsigned Line,
                                   unsigned ByteSize, uint16_t Tag, CompileUnit &U) {
  uint32_t Hash = QualifiedNameHash;

  // FIXME: dsymutil-classic compat.
  Hash += Tag;

  if (Parent)
    Hash = DwarfAccelTable::HashDJB("::", Hash);
  // dsymutil-classic compatibility: Use the mangled name so
  // "constIntPtr<void () __attribute__((noreturn))>" and
  // "constIntPtr<void ()>" hash to the same value.
  // rdar://problem/20228310
  Hash = DwarfAccelTable::HashDJB(/*Short*/ Name, Hash);

  // FIXME: dsymutil-classic compat.
  if (Tag == dwarf::DW_TAG_namespace && Name == "(anonymous namespace)")
    Hash = DwarfAccelTable::HashDJB(File, Hash);

  DeclContext *&Res = Contexts[Hash];
  DeclContext *Ctxt = Res;
  while (Ctxt &&
         (Ctxt->Parent->QualifiedNameHash != QualifiedNameHash ||
          Ctxt->Line != Line || Ctxt->ByteSize != ByteSize ||
          Ctxt->Name.data() != Name.data() || Ctxt->File.data() != File.data()))
    Ctxt = Ctxt->Collision;

  if (!Ctxt) {
    Ctxt = new DeclContext(Name, Hash, this, File, Line, ByteSize, Tag,
                           U.getUniqueID());
    Ctxt->Collision = Res;
    Res = Ctxt;
  } else if (Ctxt->LastSeenCompileUnitID == U.getUniqueID() &&
             Tag != dwarf::DW_TAG_namespace) {
    Ctxt->resetLastSeen(U);
    return std::make_pair(nullptr, Ctxt);
  }

  Ctxt->LastSeenCompileUnitID = U.getUniqueID();

  return std::make_pair(Ctxt, Ctxt);
}

/// \brief Get the potential name and mangled name for the entity
/// described by \p Die and store them in \Info if they are not
/// already there.
/// \returns is a name was found.
bool DwarfLinker::DIECloner::getDIENames(const DWARFDebugInfoEntryMinimal &Die,
                                         DWARFUnit &U, AttributesInfo &Info,
                                         bool StripTemplate) {
  // FIXME: the logi here is a bit convoluted for compatibility with
  // dsymutil-classic.
  bool GetNameOffset = false;
  // FIXME: a bit wasteful as the first getName might return the
  // short name.
  if (!Info.MangledName &&
      (Info.MangledName = Die.getName(&U, DINameKind::LinkageName)))
    std::tie(Info.MangledName, Info.MangledNameOffset) =
        Linker.StringPool.getStringAndOffset(Info.MangledName);

  if (!Info.Name && (Info.Name = Die.getName(&U, DINameKind::ShortName))) {
    Info.Name = Linker.StringPool.internString(Info.Name).data();
    GetNameOffset = true;
  }

  if (StripTemplate) {
    if (Info.Name && Info.MangledName != Info.Name)
      // FIXME: dsymutil compatibility. This is wrong for operator<
      if (char *LessThan = strchr(Info.Name, '<')) {
        Info.NameWithoutTemplate = StringRef(Info.Name, LessThan - Info.Name);
        Info.NameWithoutTemplate =
            Linker.StringPool.internString(Info.NameWithoutTemplate);
        Info.NameWithoutTemplateOffset =
            Linker.StringPool.getStringOffset(Info.NameWithoutTemplate);
      }
  }

  if (GetNameOffset)
    std::tie(Info.Name, Info.NameOffset) =
      Linker.StringPool.getStringAndOffset(Info.Name);

  return Info.Name || Info.MangledName;
}

/// \brief Report a warning to the user, optionaly including
/// information about a specific \p DIE related to the warning.
void DwarfLinker::reportWarning(const Twine &Warning, const DWARFUnit *Unit,
                                const DWARFDebugInfoEntryMinimal *DIE) const {
  StringRef Context = "<debug map>";
  if (CurrentDebugObject)
    Context = CurrentDebugObject->getObjectFilename();
  warn(Warning, Context);

  if (!Options.Verbose || !DIE)
    return;

  errs() << "    in DIE:\n";
  DIE->dump(errs(), const_cast<DWARFUnit *>(Unit), 0 /* RecurseDepth */,
            6 /* Indent */);
}

bool DwarfLinker::createStreamer(Triple TheTriple, StringRef InputFilename,
                                 StringRef OutputFilename) {
  if (Options.NoOutput)
    return true;

  Streamer = make_unique<DwarfStreamer>();
  return Streamer->init(TheTriple, OutputFilename, Options.Minimize);
}

/// \brief Recursive helper to gather the child->parent relationships in the
/// original compile unit.
static void gatherDIEParents(const DWARFDebugInfoEntryMinimal *DIE,
                             unsigned ParentIdx, CompileUnit &CU,
                             DeclContext *CurrentDeclContext,
                             NonRelocatableStringpool &StringPool,
                             DeclContext::Map &Contexts) {
  unsigned MyIdx = CU.getOrigUnit().getDIEIndex(DIE);
  CompileUnit::DIEInfo &Info = CU.getInfo(MyIdx);
  Info.ParentIdx = ParentIdx;
  if (CU.hasODR()) {
    auto Ctxt = updateDeclContext(CurrentDeclContext, DIE, CU, StringPool, Contexts);
    if (Ctxt.first && Ctxt.first != CurrentDeclContext)
      Ctxt.first->setLastSeenDIE(DIE);
    Info.Ctxt = Ctxt.first;
    CurrentDeclContext = Ctxt.second;
  }
  if (DIE->hasChildren())
    for (auto *Child = DIE->getFirstChild(); Child && !Child->isNULL();
         Child = Child->getSibling())
      gatherDIEParents(Child, MyIdx, CU, CurrentDeclContext, StringPool,
                       Contexts);
}

static bool dieNeedsChildrenToBeMeaningful(uint32_t Tag) {
  switch (Tag) {
  default:
    return false;
  case dwarf::DW_TAG_subprogram:
  case dwarf::DW_TAG_lexical_block:
  case dwarf::DW_TAG_subroutine_type:
  case dwarf::DW_TAG_structure_type:
  case dwarf::DW_TAG_class_type:
  case dwarf::DW_TAG_union_type:
    return true;
  }
  llvm_unreachable("Invalid Tag");
}

void DwarfLinker::startDebugObject(DWARFContext &Dwarf, DebugMapObject &Obj) {
  Units.reserve(Dwarf.getNumCompileUnits());
  // Iterate over the debug map entries and put all the ones that are
  // functions (because they have a size) into the Ranges map. This
  // map is very similar to the FunctionRanges that are stored in each
  // unit, with 2 notable differences:
  //  - obviously this one is global, while the other ones are per-unit.
  //  - this one contains not only the functions described in the DIE
  // tree, but also the ones that are only in the debug map.
  // The latter information is required to reproduce dsymutil's logic
  // while linking line tables. The cases where this information
  // matters look like bugs that need to be investigated, but for now
  // we need to reproduce dsymutil's behavior.
  // FIXME: Once we understood exactly if that information is needed,
  // maybe totally remove this (or try to use it to do a real
  // -gline-tables-only on Darwin.
  for (const auto &Entry : Obj.symbols()) {
    const auto &Mapping = Entry.getValue();
    if (Mapping.Size && !Mapping.FromAnotherObjectFile)
      Ranges[Mapping.ObjectAddress] = std::make_pair(
          Mapping.ObjectAddress + Mapping.Size,
          int64_t(Mapping.BinaryAddress) - Mapping.ObjectAddress);
  }
}

void DwarfLinker::endDebugObject() {
  Units.clear();
  Ranges.clear();

  // for (auto *Block : DIEBlocks)
  //   Block->~DIEBlock();
  // for (auto *Loc : DIELocs)
  //   Loc->~DIELoc();

  DIEBlocks.clear();
  DIELocs.clear();
  DIEAlloc.Reset();
}

/// \brief Iterate over the relocations of the given \p Section and
/// store the ones that correspond to debug map entries into the
/// ValidRelocs array.
bool DwarfLinker::RelocationManager::findValidRelocsMachO(
    const object::SectionRef &Section, const object::MachOObjectFile &Obj,
    const DebugMapObject &DMO) {
  StringRef Contents;
  Section.getContents(Contents);
  DataExtractor Data(Contents, Obj.isLittleEndian(), 0);
  bool FoundInterstingReloc = false;

  for (const object::RelocationRef &Reloc : Section.relocations()) {
    object::DataRefImpl RelocDataRef = Reloc.getRawDataRefImpl();
    MachO::any_relocation_info MachOReloc = Obj.getRelocation(RelocDataRef);
    unsigned RelocSize = 1 << Obj.getAnyRelocationLength(MachOReloc);
    uint64_t Offset64;
    if ((RelocSize != 4 && RelocSize != 8) || Reloc.getOffset(Offset64)) {
      Linker.reportWarning(" unsupported relocation in debug_info section.");
      continue;
    }
    uint32_t Offset = Offset64;
    // Mach-o uses REL relocations, the addend is at the relocation offset.
    uint64_t Addend = Data.getUnsigned(&Offset, RelocSize);

    auto Sym = Reloc.getSymbol();
    auto Section = Obj.getRelocationSection(Obj.getRelocation(Reloc.getRawDataRefImpl()));
    StringRef SecName;
    if (object::section_iterator(Section) != Obj.section_end())
      Section.getName(SecName);

    if (Sym != Obj.symbol_end()) {
      StringRef SymbolName;
      if (Sym->getName(SymbolName)) {
        Linker.reportWarning("error getting relocation symbol name.");
        continue;
      }
      if (const auto *Mapping = DMO.lookupSymbol(SymbolName)) {
        if (Mapping->getValue().BinaryAddress == object::UnknownAddressOrSize)
          continue;
        ValidRelocs.emplace_back(Offset64, RelocSize, Addend, Mapping,
                                 !Mapping->getValue().FromAnotherObjectFile);
        FoundInterstingReloc |= !Mapping->getValue().FromAnotherObjectFile;
      }
    } else if (const auto *Mapping = DMO.lookupObjectAddress(Addend, SecName)) {
      if (Mapping->getValue().BinaryAddress == object::UnknownAddressOrSize)
        continue;
      if (Mapping->getValue().ObjectAddress == Addend) {
        // Do not store the addend. The addend was the address of the
        // symbol in the object file, the address in the binary that is
        // stored in the debug map doesn't need to be offseted.
        ValidRelocs.emplace_back(Offset64, RelocSize, 0, Mapping,
                                 !Mapping->getValue().FromAnotherObjectFile);
        FoundInterstingReloc |= !Mapping->getValue().FromAnotherObjectFile;
      } else {
        // FIXME: dsymutil-classic compat.
        // Store closest matches in the debug map. In the OpenGL
        // project, -classic gets some addresses right that we
        // don't wihtout this. It looks more like a debug info
        // generation bug than a feature dsymutil should have.
        // This is the right thing to do for scattered relocations though.
        ValidRelocs.emplace_back(Offset64, RelocSize,
                                 Addend - Mapping->getValue().ObjectAddress,
                                 Mapping, false);
      }
    }
  }

  return FoundInterstingReloc;
}

bool DwarfLinker::RelocationManager::findValidRelocs(
    const object::SectionRef &Section, const object::ObjectFile &Obj,
    const DebugMapObject &DMO) {
  bool FoundInterestingReloc = false;
  // Dispatch to the right handler depending on the file type.
  if (auto *MachOObj = dyn_cast<object::MachOObjectFile>(&Obj))
    FoundInterestingReloc = findValidRelocsMachO(Section, *MachOObj, DMO);
  else
    Linker.reportWarning(Twine("unsupported object file type: ") +
                         Obj.getFileName());

  if (!FoundInterestingReloc)
    return false;

  // Sort the relocations by offset. We will walk the DIEs linearly in
  // the file, this allows us to just keep an index in the relocation
  // array that we advance during our walk, rather than resorting to
  // some associative container. See DwarfLinker::NextValidReloc.
  std::sort(ValidRelocs.begin(), ValidRelocs.end());
  return true;
}

/// \brief Look for relocations in the debug_info section that match
/// entries in the debug map. These relocations will drive the Dwarf
/// link by indicating which DIEs refer to symbols present in the
/// linked binary.
/// \returns wether there are any valid relocations in the debug info.
bool DwarfLinker::RelocationManager::findValidRelocsInDebugInfo(
    const object::ObjectFile &Obj, const DebugMapObject &DMO) {
  // Find the debug_info section.
  Optional<object::SectionRef> Section = getSectionByName(Obj, "debug_info");
  if (Section)
    return findValidRelocs(*Section, Obj, DMO);
  return false;
}

/// \brief Checks that there is a relocation against an actual debug
/// map entry between \p StartOffset and \p NextOffset.
///
/// This function must be called with offsets in strictly ascending
/// order because it never looks back at relocations it already 'went past'.
/// \returns true and sets Info.InDebugMap if it is the case.
bool DwarfLinker::RelocationManager::hasValidRelocation(
    uint32_t StartOffset, uint32_t EndOffset, CompileUnit::DIEInfo &Info) {
  assert(NextValidReloc == 0 ||
         StartOffset > ValidRelocs[NextValidReloc - 1].Offset);
  if (NextValidReloc >= ValidRelocs.size())
    return false;

  uint64_t RelocOffset = ValidRelocs[NextValidReloc].Offset;

  // We might need to skip some relocs that we didn't consider. For
  // example the high_pc of a discarded DIE might contain a reloc that
  // is in the list because it actually corresponds to the start of a
  // function that is in the debug map.
  while (RelocOffset < StartOffset && NextValidReloc < ValidRelocs.size() - 1)
    RelocOffset = ValidRelocs[++NextValidReloc].Offset;

  if (RelocOffset < StartOffset || RelocOffset >= EndOffset)
    return false;

  const auto &ValidReloc = ValidRelocs[NextValidReloc++];
  if (!ValidReloc.FromDebugMap)
    return false;

  if (Linker.Options.Verbose)
    outs() << "Found valid debug map entry: " << ValidReloc.Mapping->getKey()
           << " "
           << format("\t%016" PRIx64 " => %016" PRIx64,
                     uint64_t(ValidReloc.Mapping->getValue().ObjectAddress),
                     uint64_t(ValidReloc.Mapping->getValue().BinaryAddress));

  Info.AddrAdjust = int64_t(ValidReloc.Mapping->getValue().BinaryAddress) +
                    ValidReloc.Addend -
                    ValidReloc.Mapping->getValue().ObjectAddress;
  Info.InDebugMap = true;
  return true;
}

/// \brief Get the starting and ending (exclusive) offset for the
/// attribute with index \p Idx descibed by \p Abbrev. \p Offset is
/// supposed to point to the position of the first attribute described
/// by \p Abbrev.
/// \return [StartOffset, EndOffset) as a pair.
static std::pair<uint32_t, uint32_t>
getAttributeOffsets(const DWARFAbbreviationDeclaration *Abbrev, unsigned Idx,
                    unsigned Offset, const DWARFUnit &Unit) {
  DataExtractor Data = Unit.getDebugInfoExtractor();

  for (unsigned i = 0; i < Idx; ++i)
    DWARFFormValue::skipValue(Abbrev->getFormByIndex(i), Data, &Offset, &Unit);

  uint32_t End = Offset;
  DWARFFormValue::skipValue(Abbrev->getFormByIndex(Idx), Data, &End, &Unit);

  return std::make_pair(Offset, End);
}

/// \brief Check if a variable describing DIE should be kept.
/// \returns updated TraversalFlags.
unsigned DwarfLinker::shouldKeepVariableDIE(
    RelocationManager &RelocMgr, const DWARFDebugInfoEntryMinimal &DIE,
    CompileUnit &Unit, CompileUnit::DIEInfo &MyInfo, unsigned Flags) {
  const auto *Abbrev = DIE.getAbbreviationDeclarationPtr();

  // Global variables with constant value can always be kept.
  if (!(Flags & TF_InFunctionScope) &&
      Abbrev->findAttributeIndex(dwarf::DW_AT_const_value) != -1U) {
    MyInfo.InDebugMap = true;
    return Flags | TF_Keep;
  }

  uint32_t LocationIdx = Abbrev->findAttributeIndex(dwarf::DW_AT_location);
  if (LocationIdx == -1U)
    return Flags;

  uint32_t Offset = DIE.getOffset() + getULEB128Size(Abbrev->getCode());
  const DWARFUnit &OrigUnit = Unit.getOrigUnit();
  uint32_t LocationOffset, LocationEndOffset;
  std::tie(LocationOffset, LocationEndOffset) =
      getAttributeOffsets(Abbrev, LocationIdx, Offset, OrigUnit);

  LocationEndOffset = LocationOffset;
  DWARFFormValue Value(Abbrev->getFormByIndex(LocationIdx));
  if ((!Value.isFormClass(DWARFFormValue::FC_Block) &&
       !Value.isFormClass(DWARFFormValue::FC_Exprloc)) ||
      !Value.extractValue(OrigUnit.getDebugInfoExtractor(), &LocationEndOffset,
                          &OrigUnit))
    return Flags;

  // FIXME: ignore tls variables for now. It seems the debug info (at
  // least on Darwin) isn't exactly what we'd expect.
  auto Loc = *Value.getAsBlock();
  if ((Loc.size() == 10 && Loc[0] == dwarf::DW_OP_const8u &&
       Loc[9] == dwarf::DW_OP_GNU_push_tls_address) ||
      (Loc.size() == 6 && Loc[0] == dwarf::DW_OP_const4u &&
       Loc[5] == dwarf::DW_OP_GNU_push_tls_address))
    return Flags;

  // See if there is a relocation to a valid debug map entry inside
  // this variable's location. The order is important here. We want to
  // always check in the variable has a valid relocation, so that the
  // DIEInfo is filled. However, we don't want a static variable in a
  // function to force us to keep the enclosing function.
  if (!RelocMgr.hasValidRelocation(LocationOffset, LocationEndOffset, MyInfo) ||
      (Flags & TF_InFunctionScope))
    return Flags;

  if (Options.Verbose)
    DIE.dump(outs(), const_cast<DWARFUnit *>(&OrigUnit), 0, 8 /* Indent */);

  Unit.setHasInterestingContent();
  return Flags | TF_Keep;
}

/// \brief Check if a function describing DIE should be kept.
/// \returns updated TraversalFlags.
unsigned DwarfLinker::shouldKeepSubprogramDIE(
    RelocationManager &RelocMgr, const DWARFDebugInfoEntryMinimal &DIE,
    CompileUnit &Unit, CompileUnit::DIEInfo &MyInfo, unsigned Flags) {
  const auto *Abbrev = DIE.getAbbreviationDeclarationPtr();

  Flags |= TF_InFunctionScope;

  uint32_t LowPcIdx = Abbrev->findAttributeIndex(dwarf::DW_AT_low_pc);
  if (LowPcIdx == -1U)
    return Flags;

  uint32_t Offset = DIE.getOffset() + getULEB128Size(Abbrev->getCode());
  DWARFUnit &OrigUnit = Unit.getOrigUnit();
  uint32_t LowPcOffset, LowPcEndOffset;
  std::tie(LowPcOffset, LowPcEndOffset) =
      getAttributeOffsets(Abbrev, LowPcIdx, Offset, OrigUnit);

  uint64_t LowPc =
      DIE.getAttributeValueAsAddress(&OrigUnit, dwarf::DW_AT_low_pc, -1ULL);
  assert(LowPc != -1ULL && "low_pc attribute is not an address.");
  if (LowPc == -1ULL ||
      !RelocMgr.hasValidRelocation(LowPcOffset, LowPcEndOffset, MyInfo))
    return Flags;

  if (Options.Verbose)
    DIE.dump(outs(), const_cast<DWARFUnit *>(&OrigUnit), 0, 8 /* Indent */);

  if (DIE.getTag() == dwarf::DW_TAG_label) {
    if (Unit.hasLabelAt(LowPc))
      return Flags;
    // FIXME: dsymutil-classic compat. dsymutil-classic doesn't
    // consider labels that don't fall into the CU's aranges. This is
    // wrong IMO. Debug info generation bugs pu aside, this is really
    // wrong in the case of labels, where a label marking the end of a
    // function will have a PC == CU's high_pc.
    if (OrigUnit.getCompileUnitDIE()->getAttributeValueAsAddress(&OrigUnit,
                                                                 dwarf::DW_AT_high_pc,
                                                                 UINT64_MAX) <= LowPc)
      return Flags;
    Unit.setHasInterestingContent();
    Unit.addLabelLowPc(LowPc, MyInfo.AddrAdjust);
    return Flags | TF_Keep;
  }

  Unit.setHasInterestingContent();
  Flags |= TF_Keep;

  DWARFFormValue HighPcValue;
  if (!DIE.getAttributeValue(&OrigUnit, dwarf::DW_AT_high_pc, HighPcValue)) {
    reportWarning("Function without high_pc. Range will be discarded.\n",
                  &OrigUnit, &DIE);
    return Flags;
  }

  uint64_t HighPc;
  if (HighPcValue.isFormClass(DWARFFormValue::FC_Address)) {
    HighPc = *HighPcValue.getAsAddress(&OrigUnit);
  } else {
    assert(HighPcValue.isFormClass(DWARFFormValue::FC_Constant));
    HighPc = LowPc + *HighPcValue.getAsUnsignedConstant();
  }

  // Replace the debug map range with a more accurate one.
  Ranges[LowPc] = std::make_pair(HighPc, MyInfo.AddrAdjust);
  Unit.addFunctionRange(LowPc, HighPc, MyInfo.AddrAdjust);
  return Flags;
}

/// \brief Check if a DIE should be kept.
/// \returns updated TraversalFlags.
unsigned DwarfLinker::shouldKeepDIE(RelocationManager &RelocMgr,
                                    const DWARFDebugInfoEntryMinimal &DIE,
                                    CompileUnit &Unit,
                                    CompileUnit::DIEInfo &MyInfo,
                                    unsigned Flags) {
  switch (DIE.getTag()) {
  case dwarf::DW_TAG_constant:
  case dwarf::DW_TAG_variable:
    return shouldKeepVariableDIE(RelocMgr, DIE, Unit, MyInfo, Flags);
  case dwarf::DW_TAG_label:
  case dwarf::DW_TAG_subprogram:
    return shouldKeepSubprogramDIE(RelocMgr, DIE, Unit, MyInfo, Flags);
  case dwarf::DW_TAG_module:
  case dwarf::DW_TAG_imported_module:
  case dwarf::DW_TAG_imported_declaration:
  case dwarf::DW_TAG_imported_unit:
    // We always want to keep these.
    return Flags | TF_Keep;
  }

  return Flags;
}

/// \brief Mark the passed DIE as well as all the ones it depends on
/// as kept.
///
/// This function is called by lookForDIEsToKeep on DIEs that are
/// newly discovered to be needed in the link. It recursively calls
/// back to lookForDIEsToKeep while adding TF_DependencyWalk to the
/// TraversalFlags to inform it that it's not doing the primary DIE
/// tree walk.
void DwarfLinker::keepDIEAndDependencies(RelocationManager &RelocMgr,
                                         const DWARFDebugInfoEntryMinimal &DIE,
                                         CompileUnit::DIEInfo &MyInfo,
                                         const DebugMapObject &DMO,
                                         CompileUnit &CU, bool UseODR) {
  const DWARFUnit &Unit = CU.getOrigUnit();
  MyInfo.Keep = true;

  // First mark all the parent chain as kept.
  unsigned AncestorIdx = MyInfo.ParentIdx;
  while (!CU.getInfo(AncestorIdx).Keep) {
    unsigned ODRFlag = UseODR ? TF_ODR : 0;
    lookForDIEsToKeep(RelocMgr, *Unit.getDIEAtIndex(AncestorIdx), DMO, CU,
                      TF_ParentWalk | TF_Keep | TF_DependencyWalk | ODRFlag);
    AncestorIdx = CU.getInfo(AncestorIdx).ParentIdx;
  }

  // Then we need to mark all the DIEs referenced by this DIE's
  // attributes as kept.
  DataExtractor Data = Unit.getDebugInfoExtractor();
  const auto *Abbrev = DIE.getAbbreviationDeclarationPtr();
  uint32_t Offset = DIE.getOffset() + getULEB128Size(Abbrev->getCode());

  // Mark all DIEs referenced through attributes as kept.
  for (const auto &AttrSpec : Abbrev->attributes()) {
    DWARFFormValue Val(AttrSpec.Form);

    if (!Val.isFormClass(DWARFFormValue::FC_Reference)) {
      DWARFFormValue::skipValue(AttrSpec.Form, Data, &Offset, &Unit);
      continue;
    }

    Val.extractValue(Data, &Offset, &Unit);
    CompileUnit *ReferencedCU;
    if (const auto *RefDIE =
            resolveDIEReference(Units, Val, Unit, DIE, ReferencedCU, *this)) {
      uint32_t RefIdx = ReferencedCU->getOrigUnit().getDIEIndex(RefDIE);
      CompileUnit::DIEInfo &Info = ReferencedCU->getInfo(RefIdx);
      // FIXME: compatibility with dsymutil-classic. UseODR shouldn't
      // be necessary and could be advantageously replaced by
      // ReferencedCU->hasODR() && CU.hasODR().
      // FIXME: for compatibility with dsymutil-classic. There is no
      // reason not to unique ref_addr references.
      if (AttrSpec.Form != dwarf::DW_FORM_ref_addr && UseODR && Info.Ctxt &&
          Info.Ctxt != ReferencedCU->getInfo(Info.ParentIdx).Ctxt &&
          Info.Ctxt->getCanonicalDIEOffset() && Info.Ctxt->isValid() &&
          isODRAttribute(AttrSpec.Attr))
        continue;

      unsigned ODRFlag = UseODR ? TF_ODR : 0;
      lookForDIEsToKeep(RelocMgr, *RefDIE, DMO, *ReferencedCU,
                        TF_Keep | TF_DependencyWalk | ODRFlag);
    }
  }
}

/// \brief Recursively walk the \p DIE tree and look for DIEs to
/// keep. Store that information in \p CU's DIEInfo.
///
/// This function is the entry point of the DIE selection
/// algorithm. It is expected to walk the DIE tree in file order and
/// (though the mediation of its helper) call hasValidRelocation() on
/// each DIE that might be a 'root DIE' (See DwarfLinker class
/// comment).
/// While walking the dependencies of root DIEs, this function is
/// also called, but during these dependency walks the file order is
/// not respected. The TF_DependencyWalk flag tells us which kind of
/// traversal we are currently doing.
void DwarfLinker::lookForDIEsToKeep(RelocationManager &RelocMgr,
                                    const DWARFDebugInfoEntryMinimal &DIE,
                                    const DebugMapObject &DMO, CompileUnit &CU,
                                    unsigned Flags) {
  unsigned Idx = CU.getOrigUnit().getDIEIndex(&DIE);
  CompileUnit::DIEInfo &MyInfo = CU.getInfo(Idx);
  bool AlreadyKept = MyInfo.Keep;

  if (153508 == DIE.getOffset())
    (void)"SDFsdfsdfsdfsdfsdf";
  
  if (Flags & TF_DependencyWalk) {
    // If we are walking dependencies and our target is already marked
    // as kept, we're all set.
    if (AlreadyKept)
      return;
  }
  if (Flags & TF_SkipSubprograms) {
    // FIXME: dsymutil-classic compatibility. This seems wrong. The
    // goal is to not include any stripped out function instance, but
    // a subprogram could be a declaration without having the
    // attribute (by abstract_origin or such). Might be better off
    // chacking for a low_pc. (The only example of that is in some
    // unavailable GraphcsDrivers code).
    if (DIE.getTag() == dwarf::DW_TAG_subprogram &&
        !DIE.getAttributeValueAsUnsignedConstant(&CU.getOrigUnit(),
                                                 dwarf::DW_AT_declaration, 0))
      return;
    Flags &= ~TF_SkipSubprograms;
  }

  // We must not call shouldKeepDIE while called from keepDIEAndDependencies,
  // because it would screw up the relocation finding logic.
  if (!(Flags & TF_DependencyWalk))
    Flags = shouldKeepDIE(RelocMgr, DIE, CU, MyInfo, Flags);

  // If it is a newly kept DIE mark it as well as all its dependencies as kept.
  if (!AlreadyKept && (Flags & TF_Keep)) {
    bool UseOdr = (Flags & TF_DependencyWalk) ? (Flags & TF_ODR) : CU.hasODR();
    keepDIEAndDependencies(RelocMgr, DIE, MyInfo, DMO, CU, UseOdr);
  }
  // The TF_ParentWalk flag tells us that we are currently walking up
  // the parent chain of a required DIE, and we don't want to mark all
  // the children of the parents as kept (consider for example a
  // DW_TAG_namespace node in the parent chain). There are however a
  // set of DIE types for which we want to ignore that directive and still
  // walk their children.
  if (dieNeedsChildrenToBeMeaningful(DIE.getTag())) {
    if (Flags & TF_ParentWalk &&
        (DIE.getTag() == dwarf::DW_TAG_structure_type ||
         DIE.getTag() == dwarf::DW_TAG_class_type ||
         DIE.getTag() == dwarf::DW_TAG_union_type))
      Flags |= TF_SkipSubprograms;
    Flags &= ~TF_ParentWalk;
  }

  if (!DIE.hasChildren() || (Flags & TF_ParentWalk))
    return;

  for (auto *Child = DIE.getFirstChild(); Child && !Child->isNULL();
       Child = Child->getSibling())
    lookForDIEsToKeep(RelocMgr, *Child, DMO, CU, Flags);
}

/// \brief Assign an abbreviation numer to \p Abbrev.
///
/// Our DIEs get freed after every DebugMapObject has been processed,
/// thus the FoldingSet we use to unique DIEAbbrevs cannot refer to
/// the instances hold by the DIEs. When we encounter an abbreviation
/// that we don't know, we create a permanent copy of it.
void DwarfLinker::AssignAbbrev(DIEAbbrev &Abbrev) {
  // Check the set for priors.
  FoldingSetNodeID ID;
  Abbrev.Profile(ID);
  void *InsertToken;
  DIEAbbrev *InSet = AbbreviationsSet.FindNodeOrInsertPos(ID, InsertToken);

  // If it's newly added.
  if (InSet) {
    // Assign existing abbreviation number.
    Abbrev.setNumber(InSet->getNumber());
  } else {
    // Add to abbreviation list.
    Abbreviations.push_back(
        new DIEAbbrev(Abbrev.getTag(), Abbrev.hasChildren()));
    for (const auto &Attr : Abbrev.getData())
      Abbreviations.back()->AddAttribute(Attr.getAttribute(), Attr.getForm());
    AbbreviationsSet.InsertNode(Abbreviations.back(), InsertToken);
    // Assign the unique abbreviation number.
    Abbrev.setNumber(Abbreviations.size());
    Abbreviations.back()->setNumber(Abbreviations.size());
  }
}

/// \brief Clone a string attribute described by \p AttrSpec and add
/// it to \p Die.
/// \returns the size of the new attribute.
unsigned DwarfLinker::DIECloner::cloneStringAttribute(DIE &Die,
                                                      AttributeSpec AttrSpec,
                                                      const DWARFFormValue &Val,
                                                      const DWARFUnit &U,
                                                      AttributesInfo &Info) {
  // Switch everything to out of line strings.
  const char *String = *Val.getAsCString(&U);
  if (AttrSpec.Form == dwarf::DW_FORM_string)
    String = Linker.StringPool.internString(String).data();
  if (AttrSpec.Form == dwarf::DW_FORM_strp || strlen(String) >= 3) {
    unsigned Offset;
    std::tie(String, Offset) = Linker.StringPool.getStringAndOffset(String);
    if (AttrSpec.Attr == dwarf::DW_AT_name) {
      Info.Name = String;
      Info.NameOffset = Offset;
    } else if (AttrSpec.Attr == dwarf::DW_AT_MIPS_linkage_name ||
               AttrSpec.Attr == dwarf::DW_AT_linkage_name) {
      Info.MangledName = String;
      Info.MangledNameOffset = Offset;
    }
    Die.addValue(dwarf::Attribute(AttrSpec.Attr), dwarf::DW_FORM_strp,
                 new (DIEAlloc) DIEInteger(Offset), &Linker.DIEsToDelete);
    return 4;
  } else {
    assert(AttrSpec.Form == dwarf::DW_FORM_string);
    DIEBlock *StringBlock = new (DIEAlloc) DIEBlock();
    unsigned Length = 0;
    Linker.DIEBlocks.push_back(StringBlock);
    while (*String) {
      StringBlock->addValue(dwarf::Attribute(0), dwarf::DW_FORM_data1,
                            new (DIEAlloc) DIEInteger(*String++));
      ++Length;
    }
    StringBlock->addValue(dwarf::Attribute(0), dwarf::DW_FORM_data1,
                          new (DIEAlloc) DIEInteger(0));
    Die.addValue(dwarf::Attribute(AttrSpec.Attr), dwarf::DW_FORM_string,
                 StringBlock, &Linker.DIEsToDelete);
    return Length + 1;
  }
}

static unsigned AccelLookupData4(DWARFAcceleratorTable Accel, StringRef Key) {
  auto Entry = Accel.find(Key);
  if (Entry != Accel.end())
    for (auto Data : *Entry)
      return *Data.getAsSignedConstant();
  return -1U;
}

unsigned DwarfLinker::DIECloner::resolveExtType(StringRef UID,
                                                const DWARFUnit &U) {
  bool Verbose = Linker.Options.Verbose;
  if (Verbose)
    outs() << "Looking up " << UID << " in .apple_exttypes\n";
  unsigned ModuleStr = AccelLookupData4(*ExtTypes, UID);
  if (ModuleStr != -1U) {
    if (const char *ModuleFile = U.getStringExtractor().getCStr(&ModuleStr)) {
      if (Verbose)
        outs() << "  Looking up " << UID << " in " << ModuleFile << " ... ";
      auto Module = Linker.ClangModules.find(ModuleFile);
      if (Module != Linker.ClangModules.end()) {
        unsigned Offset = AccelLookupData4(*Module->second.AppleTypes, UID);
        if (Offset != -1U) {
          // Performance note. Strictly speaking this indirection is
          // only necessary for DWARF 2 where the size of DW_FORM_strp
          // is different from that of the DW_FORM_ref_addr we're
          // replacing it with.  On DWARF 3 and up we could use Offset
          // verbatim! But we need to be careful because we are
          // occasianally dropping and rewriting attributes while
          // cloning DIEs.
          auto &RefUnit = *Module->second.Unit;
          if (auto RefDie = RefUnit.getOrigUnit().getDIEForOffset(Offset)) {
            unsigned Idx = RefUnit.getOrigUnit().getDIEIndex(RefDie);
            auto &RefInfo = RefUnit.getInfo(Idx);
            if (RefInfo.Clone) {
              unsigned CloneOffset = RefInfo.Clone->getOffset();
              if (Verbose)
                outs() << "found at offset " << CloneOffset << ".\n";
              return CloneOffset + RefUnit.getStartOffset();
            } else {
              if (Verbose)
                outs() << "Could not find cloned DIE.\n";
              assert(false && "DIE not cloned");
            }
          } else if (Verbose)
            outs() << "No index entry for type " << UID << " in " << ModuleFile
                   << ".\n";
        } else if (Verbose)
          outs() << "Could not find type " << UID << " in " << ModuleFile
                 << ".\n";
      } else {
        if (Verbose)
          outs() << "Clang module " << ModuleFile << " was not imported.\n";
        // Don't assert if the module has been deleted prematurely, just warn.
        // <rdar://problem/20819207>
        return 0;
      }
    } else if (Verbose)
      outs() << "Could not find clang module string.\n";
  } else if (Verbose)
    outs() << "Could not find external type " << UID
           << " in .apple_exttypes.\n";

  assert(false && "Unresolved external type reference. Corrupt DWARF input?");
  // Just emit the above warnings in a release build.
  return 0;
}

static unsigned getRefAddrSize(const DwarfStreamer &Streamer,
                               const DWARFUnit &U) {
  return Streamer.getAsmPrinter().OutStreamer.getContext().getDwarfVersion() ==
                 2
             ? U.getAddressByteSize()
             : 4;
}

/// \brief Clone of an external (clang module) type and turn the
/// by-name reference into a regular DW_FORM_ref_addr.
unsigned DwarfLinker::DIECloner::cloneExtTypeRef(
    DIE &Die, const DWARFDebugInfoEntryMinimal &InputDIE,
    AttributeSpec AttrSpec, const DWARFFormValue &Val, const DWARFUnit &U,
    AttributesInfo &Info) {
  StringRef UID(*Val.getAsCString(&U));
  if (unsigned Offset = resolveExtType(UID, U)) {
    Die.addValue(dwarf::Attribute(AttrSpec.Attr), dwarf::Form(AttrSpec.Form),
                 new (DIEAlloc) DIEInteger(Offset), &Linker.DIEsToDelete);
    return getRefAddrSize(*Linker.Streamer, U);
  } else {
    Linker.reportWarning(Twine("Could not resolve external type ") + UID, &U,
                         &InputDIE);
    return 0;
  }
}

/// \brief Clone an attribute referencing another DIE and add
/// it to \p Die.
/// \returns the size of the new attribute.
unsigned DwarfLinker::DIECloner::cloneDieReferenceAttribute(
    DIE &Die, const DWARFDebugInfoEntryMinimal &InputDIE,
    AttributeSpec AttrSpec, unsigned AttrSize, const DWARFFormValue &Val,
    CompileUnit &Unit) {
  const DWARFUnit &U = Unit.getOrigUnit();
  uint32_t Ref = *Val.getAsReference(&U);
  DIE *NewRefDie = nullptr;
  CompileUnit *RefUnit = nullptr;
  const DWARFDebugInfoEntryMinimal *RefDie = nullptr;
  DeclContext *Ctxt = nullptr;

  if (AttrSpec.Attr == dwarf::DW_AT_signature) {
    Die.addValue(dwarf::Attribute(AttrSpec.Attr), dwarf::Form(AttrSpec.Form),
                 new (DIEAlloc) DIEInteger(*Val.getAsReference(&U)),
                 &Linker.DIEsToDelete);
    return AttrSize;
  }
  if ( // Unit.hasODR() &&
      isODRAttribute(AttrSpec.Attr)) {
    const auto *Type =
        resolveDIEReference(CompileUnits, Val, U, InputDIE, RefUnit, Linker);
    assert(Type);
    auto &TypeInfo = RefUnit->getInfo(RefUnit->getOrigUnit().getDIEIndex(Type));
    if (TypeInfo.Ctxt && TypeInfo.Ctxt->isValid()) {
      if (uint32_t TypeOffset = TypeInfo.Ctxt->getCanonicalDIEOffset()) {
        DIEInteger *Attr = new (DIEAlloc) DIEInteger(TypeOffset);
        Die.addValue((dwarf::Attribute)AttrSpec.Attr, dwarf::DW_FORM_ref_addr,
                     Attr, &Linker.DIEsToDelete);
        return getRefAddrSize(*Linker.Streamer, U);
      }
      Ctxt = TypeInfo.Ctxt;
    }
  }

  if (!(RefUnit = getUnitForOffset(CompileUnits, Ref)) ||
      !(RefDie = RefUnit->getOrigUnit().getDIEForOffset(Ref))) {
    const char *AttributeString = dwarf::AttributeString(AttrSpec.Attr);
    if (!AttributeString)
      AttributeString = "DW_AT_???";
    Linker.reportWarning(Twine("Missing DIE for ref in attribute ") +
                             AttributeString + ". Dropping.",
                         &U, &InputDIE);
    return 0;
  }

  unsigned Idx = RefUnit->getOrigUnit().getDIEIndex(RefDie);
  CompileUnit::DIEInfo &RefInfo = RefUnit->getInfo(Idx);
  if (!RefInfo.Clone) {
    assert(Ref > InputDIE.getOffset());
    // We haven't cloned this DIE yet. Just create an empty one and
    // store it. It'll get really cloned when we process it.
    RefInfo.Clone = new DIE(dwarf::Tag(RefDie->getTag()));
  }
  NewRefDie = RefInfo.Clone;

  if (AttrSpec.Form == dwarf::DW_FORM_ref_addr ||
      (Unit.hasODR() && isODRAttribute(AttrSpec.Attr))) {
    // We cannot currently rely on a DIEEntry to emit ref_addr
    // references, because the implementation calls back to DwarfDebug
    // to find the unit offset. (We don't have a DwarfDebug)
    // FIXME: we should be able to design DIEEntry reliance on
    // DwarfDebug away.
    DIEInteger *Attr;
    if (Ref < InputDIE.getOffset()) {
      // We must have already cloned that DIE.
      uint32_t NewRefOffset =
          RefUnit->getStartOffset() + NewRefDie->getOffset();
      Attr = new (DIEAlloc) DIEInteger(NewRefOffset);
    } else {
      // A forward reference. Note and fixup later.
      Attr = new (DIEAlloc) DIEInteger(0xBADDEF);
      Unit.noteForwardReference(NewRefDie, RefUnit, Ctxt, Attr);
    }
    Die.addValue(dwarf::Attribute(AttrSpec.Attr), dwarf::DW_FORM_ref_addr, Attr,
                 &Linker.DIEsToDelete);
    return getRefAddrSize(*Linker.Streamer, U);
  }

  Die.addValue(dwarf::Attribute(AttrSpec.Attr), dwarf::Form(AttrSpec.Form),
               new (DIEAlloc) DIEEntry(*NewRefDie), &Linker.DIEsToDelete);
  return AttrSize;
}

/// \brief Clone an attribute of block form (locations, constants) and add
/// it to \p Die.
/// \returns the size of the new attribute.
unsigned DwarfLinker::DIECloner::cloneBlockAttribute(DIE &Die,
                                                     AttributeSpec AttrSpec,
                                                     const DWARFFormValue &Val,
                                                     unsigned AttrSize) {
  DIE *Attr;
  DIEValue *Value;
  DIELoc *Loc = nullptr;
  DIEBlock *Block = nullptr;
  // Just copy the block data over.
  if (AttrSpec.Form == dwarf::DW_FORM_exprloc) {
    Loc = new (DIEAlloc) DIELoc();
    Linker.DIELocs.push_back(Loc);
  } else {
    Block = new (DIEAlloc) DIEBlock();
    Linker.DIEBlocks.push_back(Block);
  }
  Attr = Loc ? static_cast<DIE *>(Loc) : static_cast<DIE *>(Block);
  Value = Loc ? static_cast<DIEValue *>(Loc) : static_cast<DIEValue *>(Block);
  ArrayRef<uint8_t> Bytes = *Val.getAsBlock();
  for (auto Byte : Bytes)
    Attr->addValue(dwarf::Attribute(0), dwarf::DW_FORM_data1,
                   new (DIEAlloc) DIEInteger(Byte), &Linker.DIEsToDelete);
  // FIXME: If DIEBlock and DIELoc just reuses the Size field of
  // the DIE class, this if could be replaced by
  // Attr->setSize(Bytes.size()).
  if (Linker.Streamer) {
    if (Loc)
      Loc->ComputeSize(&Linker.Streamer->getAsmPrinter());
    else
      Block->ComputeSize(&Linker.Streamer->getAsmPrinter());
  }
  Die.addValue(dwarf::Attribute(AttrSpec.Attr), dwarf::Form(AttrSpec.Form),
               Value, &Linker.DIEsToDelete);
  return AttrSize;
}

/// \brief Clone an address attribute and add it to \p Die.
/// \returns the size of the new attribute.
unsigned DwarfLinker::DIECloner::cloneAddressAttribute(
    DIE &Die, AttributeSpec AttrSpec, const DWARFFormValue &Val,
    CompileUnit &Unit, AttributesInfo &Info) {
  uint64_t Addr = *Val.getAsAddress(&Unit.getOrigUnit());

  if (LLVM_UNLIKELY(Linker.Options.Update)) {
    if (AttrSpec.Attr == dwarf::DW_AT_low_pc)
      Info.HasLowPc = true;
    Die.addValue(dwarf::Attribute(AttrSpec.Attr), dwarf::Form(AttrSpec.Form),
                 new (DIEAlloc) DIEInteger(Addr), &Linker.DIEsToDelete);
    return Unit.getOrigUnit().getAddressByteSize();
  }

  if (AttrSpec.Attr == dwarf::DW_AT_low_pc) {
    if (Die.getTag() == dwarf::DW_TAG_subprogram ||
        Die.getTag() == dwarf::DW_TAG_label)
      ; // Do nothing, relocations have been applied.
    else if (Die.getTag() == dwarf::DW_TAG_compile_unit) {
      // FIXME: no low_pc means we should certainly drop the attribute.
      Addr = Unit.getLowPc() == UINT64_MAX
                 ? (Info.OrigLowPc != UINT64_MAX ? Info.OrigLowPc : Addr)
                 : Unit.getLowPc();
    } else
      // FIXME: <rdar://problem/20345869>
      // The low_pc of a block or inline subroutine might get
      // relocated because it happens to match the low_pc of the
      // enclosing subprogram. To prevent issues with that, always use
      // the low_pc from the input DIE if relocations have been applied.
      Addr = (Info.OrigLowPc != UINT64_MAX ? Info.OrigLowPc : Addr) +
             Info.PCOffset;
    Info.HasLowPc = true;
    Info.LowPc = Addr;
  } else if (AttrSpec.Attr == dwarf::DW_AT_high_pc) {
    if (Die.getTag() == dwarf::DW_TAG_compile_unit)
      // FIXME: No high pc means we should certanly drop the attribute.
      Addr = Unit.getHighPc() ? Unit.getHighPc()
                              : (Info.OrigHighPc ? Info.OrigHighPc : Addr);
    else
      // If we have a high_pc recorded for the input DIE, use
      // it. Otherwise (when no relocations where applied) just use the
      // one we just decoded.
      // Use OrigLowPc to test if we had relocation. It happens that
      // we generate inlined_subroutine DIEs with low_pc == high_pc ==
      // 0, in which case the test for OrigHighPc == 0 will return the
      // wrong result.
      Addr = (Info.OrigLowPc != UINT64_MAX? Info.OrigHighPc : Addr) + Info.PCOffset;
  }
  Die.addValue(dwarf::Attribute(AttrSpec.Attr), dwarf::Form(AttrSpec.Form),
               new (DIEAlloc) DIEInteger(Addr), &Linker.DIEsToDelete);
  return Unit.getOrigUnit().getAddressByteSize();
}

/// \brief Clone a scalar attribute  and add it to \p Die.
/// \returns the size of the new attribute.
unsigned DwarfLinker::DIECloner::cloneScalarAttribute(
    DIE &Die, const DWARFDebugInfoEntryMinimal &InputDIE, CompileUnit &Unit,
    AttributeSpec AttrSpec, const DWARFFormValue &Val, unsigned AttrSize,
    AttributesInfo &Info) {
  uint64_t Value;
  if (LLVM_UNLIKELY(Linker.Options.Update)) {
    if (auto OptionalValue = Val.getAsUnsignedConstant())
      Value = *OptionalValue;
    else if (auto OptionalValue = Val.getAsSignedConstant())
      Value = *OptionalValue;
    else if (auto OptionalValue = Val.getAsSectionOffset())
      Value = *OptionalValue;
    else {
      Linker.reportWarning(
          "Unsupported scalar attribute form. Dropping attribute.",
          &Unit.getOrigUnit(), &InputDIE);
      return 0;
    }
    if (AttrSpec.Attr == dwarf::DW_AT_declaration && Value)
      Info.IsDeclaration = true;
    Die.addValue(dwarf::Attribute(AttrSpec.Attr), dwarf::Form(AttrSpec.Form),
                 new (DIEAlloc) DIEInteger(Value), &Linker.DIEsToDelete);
    return AttrSize;
  }

  if (AttrSpec.Attr == dwarf::DW_AT_high_pc &&
      Die.getTag() == dwarf::DW_TAG_compile_unit) {
    if (Unit.getLowPc() == -1ULL)
      return 0;
    // Dwarf >= 4 high_pc is an size, not an address.
    Value = Unit.getHighPc() - Unit.getLowPc();
  } else if (AttrSpec.Form == dwarf::DW_FORM_sec_offset)
    Value = *Val.getAsSectionOffset();
  else if (AttrSpec.Form == dwarf::DW_FORM_sdata)
    Value = *Val.getAsSignedConstant();
  else if (auto OptionalValue = Val.getAsUnsignedConstant())
    Value = *OptionalValue;
  else {
    Linker.reportWarning(
        "Unsupported scalar attribute form. Dropping attribute.",
        &Unit.getOrigUnit(), &InputDIE);
    return 0;
  }
  DIEInteger *Attr = new (DIEAlloc) DIEInteger(Value);
  if (AttrSpec.Attr == dwarf::DW_AT_ranges)
    Unit.noteRangeAttribute(Die, Attr);
  // A more generic way to check for location attributes would be
  // nice, but it's very unlikely that any other attribute needs a
  // location list.
  else if (AttrSpec.Attr == dwarf::DW_AT_location ||
           AttrSpec.Attr == dwarf::DW_AT_frame_base)
    Unit.noteLocationAttribute(Attr, Info.PCOffset);
  else if (AttrSpec.Attr == dwarf::DW_AT_declaration && Value)
    Info.IsDeclaration = true;

  Die.addValue(dwarf::Attribute(AttrSpec.Attr), dwarf::Form(AttrSpec.Form),
               Attr, &Linker.DIEsToDelete);
  return AttrSize;
}

/// \brief Clone \p InputDIE's attribute described by \p AttrSpec with
/// value \p Val, and add it to \p Die.
/// \returns the size of the cloned attribute.
unsigned DwarfLinker::DIECloner::cloneAttribute(
    DIE &Die, const DWARFDebugInfoEntryMinimal &InputDIE, CompileUnit &Unit,
    const DWARFFormValue &Val, const AttributeSpec AttrSpec, unsigned AttrSize,
    AttributesInfo &Info) {
  const DWARFUnit &U = Unit.getOrigUnit();

  switch (AttrSpec.Form) {
  case dwarf::DW_FORM_strp:
    // Module debugging external type references.
    if (AttrSpec.Attr == dwarf::DW_AT_type          ||
        AttrSpec.Attr == dwarf::DW_AT_specification ||
        AttrSpec.Attr == dwarf::DW_AT_import        ||
        AttrSpec.Attr == dwarf::DW_AT_friend)
      return cloneExtTypeRef(
          Die, InputDIE,
          AttributeSpec(AttrSpec.Attr, dwarf::DW_FORM_ref_addr),
          Val, U, Info);

  // fall through
  case dwarf::DW_FORM_string:
    return cloneStringAttribute(Die, AttrSpec, Val, U, Info);
  case dwarf::DW_FORM_ref_addr:
  case dwarf::DW_FORM_ref1:
  case dwarf::DW_FORM_ref2:
  case dwarf::DW_FORM_ref4:
  case dwarf::DW_FORM_ref8:
  case dwarf::DW_FORM_ref_sig8:
    return cloneDieReferenceAttribute(Die, InputDIE, AttrSpec, AttrSize, Val,
                                      Unit);
  case dwarf::DW_FORM_block:
  case dwarf::DW_FORM_block1:
  case dwarf::DW_FORM_block2:
  case dwarf::DW_FORM_block4:
  case dwarf::DW_FORM_exprloc:
    return cloneBlockAttribute(Die, AttrSpec, Val, AttrSize);
  case dwarf::DW_FORM_addr:
    return cloneAddressAttribute(Die, AttrSpec, Val, Unit, Info);
  case dwarf::DW_FORM_data1:
  case dwarf::DW_FORM_data2:
  case dwarf::DW_FORM_data4:
  case dwarf::DW_FORM_data8:
  case dwarf::DW_FORM_udata:
  case dwarf::DW_FORM_sdata:
  case dwarf::DW_FORM_sec_offset:
  case dwarf::DW_FORM_flag:
  case dwarf::DW_FORM_flag_present:
    return cloneScalarAttribute(Die, InputDIE, Unit, AttrSpec, Val, AttrSize,
                                Info);
  default:
    Linker.reportWarning(
        "Unsupported attribute form in cloneAttribute. Dropping.", &U,
        &InputDIE);
  }

  return 0;
}

/// \brief Apply the valid relocations found by findValidRelocs() to
/// the buffer \p Data, taking into account that Data is at \p BaseOffset
/// in the debug_info section.
///
/// Like for findValidRelocs(), this function must be called with
/// monotonic \p BaseOffset values.
///
/// \returns wether any reloc has been applied.
bool DwarfLinker::RelocationManager::applyValidRelocs(
    MutableArrayRef<char> Data, uint32_t BaseOffset, bool isLittleEndian) {
  assert((NextValidReloc == 0 ||
          BaseOffset > ValidRelocs[NextValidReloc - 1].Offset) &&
         "BaseOffset should only be increasing.");
  if (NextValidReloc >= ValidRelocs.size())
    return false;

  // Skip relocs that haven't been applied.
  while (NextValidReloc < ValidRelocs.size() &&
         ValidRelocs[NextValidReloc].Offset < BaseOffset)
    ++NextValidReloc;

  bool Applied = false;
  uint64_t EndOffset = BaseOffset + Data.size();
  while (NextValidReloc < ValidRelocs.size() &&
         ValidRelocs[NextValidReloc].Offset >= BaseOffset &&
         ValidRelocs[NextValidReloc].Offset < EndOffset) {
    const auto &ValidReloc = ValidRelocs[NextValidReloc++];
    assert(ValidReloc.Offset - BaseOffset < Data.size());
    assert(ValidReloc.Offset - BaseOffset + ValidReloc.Size <= Data.size());
    char Buf[8];
    uint64_t Value = ValidReloc.Mapping->getValue().BinaryAddress;
    Value += ValidReloc.Addend;
    for (unsigned i = 0; i != ValidReloc.Size; ++i) {
      unsigned Index = isLittleEndian ? i : (ValidReloc.Size - i - 1);
      Buf[i] = uint8_t(Value >> (Index * 8));
    }
    assert(ValidReloc.Size <= sizeof(Buf));
    memcpy(&Data[ValidReloc.Offset - BaseOffset], Buf, ValidReloc.Size);
    Applied = true;
  }

  return Applied;
}

static bool isTypeTag(uint16_t Tag) {
  switch (Tag) {
  case dwarf::DW_TAG_array_type:
  case dwarf::DW_TAG_class_type:
  case dwarf::DW_TAG_enumeration_type:
  case dwarf::DW_TAG_pointer_type:
  case dwarf::DW_TAG_reference_type:
  case dwarf::DW_TAG_string_type:
  case dwarf::DW_TAG_structure_type:
  case dwarf::DW_TAG_subroutine_type:
  case dwarf::DW_TAG_typedef:
  case dwarf::DW_TAG_union_type:
  case dwarf::DW_TAG_ptr_to_member_type:
  case dwarf::DW_TAG_set_type:
  case dwarf::DW_TAG_subrange_type:
  case dwarf::DW_TAG_base_type:
  case dwarf::DW_TAG_const_type:
  case dwarf::DW_TAG_constant:
  case dwarf::DW_TAG_file_type:
  case dwarf::DW_TAG_namelist:
  case dwarf::DW_TAG_packed_type:
  case dwarf::DW_TAG_volatile_type:
  case dwarf::DW_TAG_restrict_type:
  case dwarf::DW_TAG_interface_type:
  case dwarf::DW_TAG_unspecified_type:
  case dwarf::DW_TAG_shared_type:
    return true;
  default:
    break;
  }
  return false;
}

static bool isObjCSelector(const char *Name) {
  return Name && (Name[0] == '-' || Name[0] == '+') && (Name[1] == '[');
}

void DwarfLinker::DIECloner::addObjCAccelerator(CompileUnit &Unit,
                                                const DIE *Die,
                                                const char *Name,
                                                bool SkipPubSection) {
  assert(isObjCSelector(Name) && "not an objc selector");
  // Objective C method or class function.
  // "- [Class(Category) selector :withArg ...]"
  StringRef ClassNameStart(Name + 2);
  size_t FirstSpace = ClassNameStart.find(' ');
  if (FirstSpace == StringRef::npos)
    return;

  StringRef SelectorStart(ClassNameStart.data() + FirstSpace + 1);
  if (!SelectorStart.size())
    return;

  StringRef Selector(SelectorStart.data(), SelectorStart.size() - 1);
  StringRef SelectorStr = Linker.StringPool.internString(Selector);
  Unit.addNameAccelerator(Die, SelectorStr.data(),
                          Linker.StringPool.getStringOffset(SelectorStr),
                          SkipPubSection);

  // Add an entry for the class name that points to this
  // method/class function.
  StringRef ClassName(ClassNameStart.data(), FirstSpace);
  StringRef ClassNameStr = Linker.StringPool.internString(ClassName);
  Unit.addObjCAccelerator(Die, ClassNameStr.data(),
                          Linker.StringPool.getStringOffset(ClassNameStr),
                          SkipPubSection);
  if (ClassName[ClassName.size() - 1] == ')') {
    size_t OpenParens = ClassName.find('(');
    if (OpenParens != StringRef::npos) {
      StringRef ClassNameNoCategory(ClassName.data(), OpenParens);
      StringRef ClassNameNoCategoryStr =
          Linker.StringPool.internString(ClassNameNoCategory);
      Unit.addObjCAccelerator(
          Die, ClassNameNoCategoryStr.data(),
          Linker.StringPool.getStringOffset(ClassNameNoCategoryStr),
          SkipPubSection);

      std::string MethodNameNoCategory(Name, OpenParens + 2);
      // FIXME: The missing space here may be a bug, but
      //        dsymutil-classic also does it this way.
      MethodNameNoCategory.append(SelectorStart);
      StringRef MethodNameNoCategoryStr =
          Linker.StringPool.internString(MethodNameNoCategory);
      Unit.addNameAccelerator(
          Die, MethodNameNoCategoryStr.data(),
          Linker.StringPool.getStringOffset(MethodNameNoCategoryStr),
          SkipPubSection);
    }
  }
}

/// \brief Recursively clone \p InputDIE's subtrees that have been
/// selected to appear in the linked output.
///
/// \param OutOffset is the Offset where the newly created DIE will
/// lie in the linked compile unit.
///
/// \returns the cloned DIE object or null if nothing was selected.
DIE *DwarfLinker::DIECloner::cloneDIE(
    const DWARFDebugInfoEntryMinimal &InputDIE, CompileUnit &Unit,
    int64_t PCOffset, uint32_t OutOffset, unsigned Flags) {
  DWARFUnit &U = Unit.getOrigUnit();
  unsigned Idx = U.getDIEIndex(&InputDIE);
  CompileUnit::DIEInfo &Info = Unit.getInfo(Idx);

  // Should the DIE appear in the output?
  if (!Info.Keep)
    return nullptr;

  uint32_t Offset = InputDIE.getOffset();
  // The DIE might have been already created by a forward reference
  // (see cloneDieReferenceAttribute()).
  DIE *Die = Info.Clone;
  if (!Die)
    Die = Info.Clone = new (DIEAlloc) DIE(dwarf::Tag(InputDIE.getTag()));
  assert(Die->getTag() == InputDIE.getTag());
  Die->setOffset(OutOffset);
  if (Unit.hasODR() && Die->getTag() != dwarf::DW_TAG_namespace && Info.Ctxt &&
      Info.Ctxt != Unit.getInfo(Info.ParentIdx).Ctxt &&
      !Info.Ctxt->getCanonicalDIEOffset()) {
    Info.Ctxt->setCanonicalDIEOffset(OutOffset + Unit.getStartOffset());
  }

  // Extract and clone every attribute.
  DataExtractor Data = U.getDebugInfoExtractor();
  uint32_t NextOffset =
    (Idx + 1 < U.getNumDIEs())
    ? U.getDIEAtIndex(Idx + 1)->getOffset()
    : U.getNextUnitOffset();
  AttributesInfo AttrInfo;

  // We could copy the data only if we need to apply a relocation to
  // it. After testing, it seems there is no performance downside to
  // doing the copy unconditionally, and it makes the code simpler.
  SmallString<40> DIECopy(Data.getData().substr(Offset, NextOffset - Offset));
  Data = DataExtractor(DIECopy, Data.isLittleEndian(), Data.getAddressSize());
  // Modify the copy with relocated addresses.
  if (RelocMgr.applyValidRelocs(DIECopy, Offset, Data.isLittleEndian())) {
    // If we applied relocations, we store the value of high_pc that was
    // potentially stored in the input DIE. If high_pc is an address
    // (Dwarf version == 2), then it might have been relocated to a
    // totally unrelated value (because the end address in the object
    // file might be start address of another function which got moved
    // independantly by the linker). The computation of the actual
    // high_pc value is done in cloneAddressAttribute().
    AttrInfo.OrigLowPc =
        InputDIE.getAttributeValueAsAddress(&U, dwarf::DW_AT_low_pc, 0);
    AttrInfo.OrigHighPc =
        InputDIE.getAttributeValueAsAddress(&U, dwarf::DW_AT_high_pc, 0);
  }

  // Reset the Offset to 0 as we will be working on the local copy of
  // the data.
  Offset = 0;

  const auto *Abbrev = InputDIE.getAbbreviationDeclarationPtr();
  Offset += getULEB128Size(Abbrev->getCode());

  // We are entering a subprogram. Get and propagate the PCOffset.
  if (Die->getTag() == dwarf::DW_TAG_subprogram)
    PCOffset = Info.AddrAdjust;
  AttrInfo.PCOffset = PCOffset;

  if (Abbrev->getTag() == dwarf::DW_TAG_subprogram)
    Flags |= TF_InFunctionScope;

  if (Abbrev->getTag() == dwarf::DW_TAG_subprogram && !Info.InDebugMap)
    Flags |= TF_SkipPC;
  bool skipLocation =
      (!(Flags & TF_InFunctionScope) &&
       Abbrev->getTag() == dwarf::DW_TAG_variable && !Info.InDebugMap) ||
      (Flags & TF_SkipPC);

  if (LLVM_UNLIKELY(Options.Update)) {
    skipLocation = false;
    Flags &= ~TF_SkipPC;
  }

  bool Copied = false;
  for (const auto &AttrSpec : Abbrev->attributes()) {
    DWARFFormValue Val(AttrSpec.Form);
    if (((Flags & TF_SkipPC) && (AttrSpec.Attr == dwarf::DW_AT_low_pc ||
                                 AttrSpec.Attr == dwarf::DW_AT_high_pc ||
                                 AttrSpec.Attr == dwarf::DW_AT_ranges)) ||
        (skipLocation && (AttrSpec.Attr == dwarf::DW_AT_location ||
                          AttrSpec.Attr == dwarf::DW_AT_frame_base) &&
         !Val.isFormClass(DWARFFormValue::FC_Block))) {
      DWARFFormValue::skipValue(AttrSpec.Form, Data, &Offset, &U);
      // FIXME: Only necessary for bit-for-bit compatibility. Can drop as
      // soon as the transition is over.
      if (!Copied) {
        copyAbbrev(*InputDIE.getAbbreviationDeclarationPtr(), Unit.hasODR());
        Copied = true;
      }
      continue;
    }
    uint32_t AttrSize = Offset;
    Val.extractValue(Data, &Offset, &U);
    AttrSize = Offset - AttrSize;

    OutOffset +=
        cloneAttribute(*Die, InputDIE, Unit, Val, AttrSpec, AttrSize, AttrInfo);
  }

  // Look for accelerator entries.
  uint16_t Tag = InputDIE.getTag();
  // FIXME: This is slightly wrong. An inline_subroutine without a
  // low_pc, but with AT_ranges might be interesting to get into the
  // accelerator tables too. For now stick with dsymutil's behavior.
  if ((Info.InDebugMap || AttrInfo.HasLowPc) &&
      Tag != dwarf::DW_TAG_compile_unit &&
      // This condition is only here for dsymutil-classic compatibility.
      Tag != dwarf::DW_TAG_label &&
      getDIENames(InputDIE, Unit.getOrigUnit(), AttrInfo,
                  Tag != dwarf::DW_TAG_inlined_subroutine)) {
    if (AttrInfo.MangledName &&
        AttrInfo.MangledNameOffset != AttrInfo.NameOffset)
      Unit.addNameAccelerator(Die, AttrInfo.MangledName,
                              AttrInfo.MangledNameOffset,
                              Tag == dwarf::DW_TAG_inlined_subroutine);
    if (Tag == dwarf::DW_TAG_inlined_subroutine && AttrInfo.MangledName &&
        !LLVM_UNLIKELY(Options.Update)) {
      uint32_t NameOffset = AttrInfo.NameOffset;
      if (AttrInfo.NameOffset == AttrInfo.MangledNameOffset)
        NameOffset = 0;
      Linker.Inlined[std::make_pair(NameOffset, AttrInfo.MangledNameOffset)]
          .push_back(std::make_pair(Die->getOffset() + Unit.getStartOffset(),
                                    AttrInfo.LowPc));
    }

    if (AttrInfo.Name) {
      if (Tag != dwarf::DW_TAG_inlined_subroutine) {
        if (!AttrInfo.NameWithoutTemplate.empty())
          Unit.addNameAccelerator(Die, AttrInfo.NameWithoutTemplate.data(),
                                  AttrInfo.NameWithoutTemplateOffset,
                                  true /* SkipPubSection */);
      }
      Unit.addNameAccelerator(Die, AttrInfo.Name, AttrInfo.NameOffset,
                              Tag == dwarf::DW_TAG_inlined_subroutine);
    }
    if (isObjCSelector(AttrInfo.Name))
      addObjCAccelerator(Unit, Die, AttrInfo.Name, true /* SkipPubSection */);
  } else if (Tag == dwarf::DW_TAG_namespace) {
    if (!AttrInfo.Name) {
      AttrInfo.Name = "(anonymous namespace)";
      AttrInfo.NameOffset = Linker.StringPool.getStringOffset(AttrInfo.Name);
    }
    Unit.addNamespaceAccelerator(Die, AttrInfo.Name, AttrInfo.NameOffset);
  } else if (isTypeTag(Tag) && !AttrInfo.IsDeclaration &&
             getDIENames(InputDIE, Unit.getOrigUnit(), AttrInfo) &&
             AttrInfo.Name && AttrInfo.Name[0]) {
    uint32_t Hash = hashFullyQualifiedName(&InputDIE, Unit);
    uint64_t RuntimeLang = InputDIE.getAttributeValueAsUnsignedConstant(
        &U, dwarf::DW_AT_APPLE_runtime_class, 0);
    bool ObjCClassIsImplementation =
        (RuntimeLang == dwarf::DW_LANG_ObjC ||
         RuntimeLang == dwarf::DW_LANG_ObjC_plus_plus) &&
        InputDIE.getAttributeValueAsUnsignedConstant(
            &U, dwarf::DW_AT_APPLE_objc_complete_type, 0);
    Unit.addTypeAccelerator(Die, AttrInfo.Name, AttrInfo.NameOffset,
                            ObjCClassIsImplementation, Hash);
  }

  DIEAbbrev &NewAbbrev = Die->getAbbrev();
  // If a scope DIE is kept, we must have kept at least one child. If
  // it's not the case, we'll just be emitting one wasteful end of
  // children marker, but things won't break.
  if (InputDIE.hasChildren())
    NewAbbrev.setChildrenFlag(dwarf::DW_CHILDREN_yes);
  // Assign a permanent abbrev number
  Linker.AssignAbbrev(Die->getAbbrev());

  // Add the size of the abbreviation number to the output offset.
  OutOffset += getULEB128Size(Die->getAbbrevNumber());

  if (!Abbrev->hasChildren()) {
    // Update our size.
    Die->setSize(OutOffset - Die->getOffset());
    return Die;
  }

  // Recursively clone children.
  for (auto *Child = InputDIE.getFirstChild(); Child && !Child->isNULL();
       Child = Child->getSibling())
    if (DIE *Clone = cloneDIE(*Child, Unit, PCOffset, OutOffset, Flags)) {
      Die->addChild(Clone);
      OutOffset = Clone->getOffset() + Clone->getSize();
    }

  // Account for the end of children marker.
  OutOffset += sizeof(int8_t);
  // Update our size.
  Die->setSize(OutOffset - Die->getOffset());
  return Die;
}

/// \brief Patch the input object file relevant debug_ranges entries
/// and emit them in the output file. Update the relevant attributes
/// to point at the new entries.
void DwarfLinker::patchRangesForUnit(const CompileUnit &Unit,
                                     DWARFContext &OrigDwarf) const {
  DWARFDebugRangeList RangeList;
  const auto &FunctionRanges = Unit.getFunctionRanges();
  unsigned AddressSize = Unit.getOrigUnit().getAddressByteSize();
  DataExtractor RangeExtractor(OrigDwarf.getRangeSection(),
                               OrigDwarf.isLittleEndian(), AddressSize);
  auto InvalidRange = FunctionRanges.end(), CurrRange = InvalidRange;
  DWARFUnit &OrigUnit = Unit.getOrigUnit();
  const auto *OrigUnitDie = OrigUnit.getCompileUnitDIE(false);
  uint64_t OrigLowPc = OrigUnitDie->getAttributeValueAsAddress(
      &OrigUnit, dwarf::DW_AT_low_pc, -1ULL);
  // Ranges addresses are based on the unit's low_pc. Compute the
  // offset we need to apply to adapt to the the new unit's low_pc.
  int64_t UnitPcOffset = 0;
  if (OrigLowPc != -1ULL)
    UnitPcOffset = int64_t(OrigLowPc) - Unit.getLowPc();

  for (const auto &RangeAttribute : Unit.getRangesAttributes()) {
    uint32_t Offset = RangeAttribute->getValue();
    RangeAttribute->setValue(Streamer->getRangesSectionSize());
    RangeList.extract(RangeExtractor, &Offset);
    const auto &Entries = RangeList.getEntries();
    if (!Entries.empty()) {
      const DWARFDebugRangeList::RangeListEntry &First = Entries.front();

      if (CurrRange == InvalidRange ||
          First.StartAddress + OrigLowPc < CurrRange.start() ||
          First.StartAddress + OrigLowPc >= CurrRange.stop()) {
        CurrRange = FunctionRanges.find(First.StartAddress + OrigLowPc);
        if (CurrRange == InvalidRange ||
            CurrRange.start() > First.StartAddress + OrigLowPc) {
          reportWarning("no mapping for range.");
          continue;
        }
      }
    }
    Streamer->emitRangesEntries(UnitPcOffset, OrigLowPc, CurrRange, Entries,
                                AddressSize);
  }
}

/// \brief Generate the debug_aranges entries for \p Unit and if the
/// unit has a DW_AT_ranges attribute, also emit the debug_ranges
/// contribution for this attribute.
/// FIXME: this could actually be done right in patchRangesForUnit,
/// but for the sake of initial bit-for-bit compatibility with legacy
/// dsymutil, we have to do it in a delayed pass.
void DwarfLinker::generateUnitRanges(CompileUnit &Unit) const {
  DIEInteger *Attr = Unit.getUnitRangesAttribute();
  if (Attr)
    Attr->setValue(Streamer->getRangesSectionSize());
  Streamer->emitUnitRangesEntries(Unit, Attr != nullptr);
}

/// \brief Insert the new line info sequence \p Seq into the current
/// set of already linked line info \p Rows.
static void insertLineSequence(std::vector<DWARFDebugLine::Row> &Seq,
                               std::vector<DWARFDebugLine::Row> &Rows) {
  if (Seq.size() <= 1)
    return;

  if (!Rows.empty() && Rows.back().Address < Seq.front().Address) {
    Rows.insert(Rows.end(), Seq.begin(), Seq.end());
    Seq.clear();
    return;
  }

  auto InsertPoint = std::lower_bound(
      Rows.begin(), Rows.end(), Seq.front(),
      [](const DWARFDebugLine::Row &LHS, const DWARFDebugLine::Row &RHS) {
        return LHS.Address < RHS.Address;
      });

  // FIXME: this only removes the unneeded end_sequence if the
  // sequences have been inserted in order. using a global sort like
  // described in patchLineTableForUnit() and delaying the end_sequence
  // elimination to emitLineTableForUnit() we can get rid of all of them.
  if (InsertPoint != Rows.end() &&
      InsertPoint->Address == Seq.front().Address && InsertPoint->EndSequence) {
    *InsertPoint = Seq.front();
    Rows.insert(InsertPoint + 1, Seq.begin() + 1, Seq.end());
  } else {
    Rows.insert(InsertPoint, Seq.begin(), Seq.end());
  }

  Seq.clear();
}

/// \brief Extract the line table for \p Unit from \p OrigDwarf, and
/// recreate a relocated version of these for the address ranges that
/// are present in the binary.
void DwarfLinker::patchLineTableForUnit(CompileUnit &Unit,
                                        const DebugMapObject &DMO,
                                        DWARFContext &OrigDwarf) {
  const DWARFDebugInfoEntryMinimal *CUDie =
      Unit.getOrigUnit().getCompileUnitDIE();
  uint64_t StmtList = CUDie->getAttributeValueAsSectionOffset(
      &Unit.getOrigUnit(), dwarf::DW_AT_stmt_list, -1ULL);
  if (StmtList == -1ULL)
    return;

  // Update the cloned DW_AT_stmt_list with the correct debug_line offset.
  if (auto *OutputDIE = Unit.getOutputUnitDIE()) {
    const auto &Abbrev = OutputDIE->getAbbrev().getData();
    auto Stmt = std::find_if(
        Abbrev.begin(), Abbrev.end(), [](const DIEAbbrevData &AbbrevData) {
          return AbbrevData.getAttribute() == dwarf::DW_AT_stmt_list;
        });
    assert(Stmt < Abbrev.end() && "Didn't find DW_AT_stmt_list in cloned DIE!");
    DIEInteger *StmtAttr =
        cast<DIEInteger>(OutputDIE->getValues()[Stmt - Abbrev.begin()]);
    StmtAttr->setValue(Streamer->getLineSectionSize());
  }

  // Parse the original line info for the unit.
  uint32_t StmtOffset = StmtList;
  StringRef LineData = OrigDwarf.getLineSection().Data;
  DataExtractor LineExtractor(LineData, OrigDwarf.isLittleEndian(),
                              Unit.getOrigUnit().getAddressByteSize());

  if (Options.Translator)
    return Streamer->translateLineTable(LineExtractor, StmtList, Options);

  DWARFDebugLine::LineTable LineTable;
  LineTable.parse(LineExtractor, &OrigDwarf.getLineSection().Relocs,
                  &StmtOffset);

  // This vector is the output line table.
  std::vector<DWARFDebugLine::Row> NewRows;
  NewRows.reserve(LineTable.Rows.size());

  // Current sequence of rows being extracted, before being inserted
  // in NewRows.
  std::vector<DWARFDebugLine::Row> Seq;
  // auto InvalidRange = FunctionRanges.end(), CurrRange = InvalidRange;
  std::pair<uint64_t, uint64_t> InvalidRange = {-1ULL, -1ULL};
  auto CurrRange = InvalidRange;
  uint64_t PCOffset = -1ULL;

  // FIXME: This logic is meant to generate exactly the same output as
  // Darwin's classic dsymutil. There is a nicer way to implement this
  // by simply putting all the relocated line info in NewRows and simply
  // sorting NewRows before passing it to emitLineTableForUnit. This
  // should be correct as sequences for a function should stay
  // together in the sorted output. There are a few corner cases that
  // look suspicious though, and that required to implement the logic
  // this way. Revisit that once initial validation is finished.

  // Iterate over the object file line info and extract the sequences
  // that correspond to linked functions.
  for (auto &Row : LineTable.Rows) {
    // Check wether we stepped out of the range. The range is
    // half-open, but consider accept the end address of the range if
    // it is marked as end_sequence in the input (because in that
    // case, the relocation offset is accurate and that entry won't
    // serve as the start of another function).
    if (CurrRange == InvalidRange || Row.Address < CurrRange.first ||
        Row.Address > CurrRange.second ||
        (Row.Address == CurrRange.second && !Row.EndSequence)) {
      bool InPrevRangeInclusive =
          CurrRange != InvalidRange && Row.Address == CurrRange.second;
      // We just stepped out of a known range. Insert an end_sequence
      // corresponding to the end of the range.
      uint64_t StopAddress =
          CurrRange == InvalidRange ? -1ULL : CurrRange.second + PCOffset;


      bool CurrRangeValid = false;
          CurrRange = InvalidRange;
        // Try harder by looking in the DebugMapObject function
        // ranges map. There are corner cases where this finds a
        // valid entry. It's unclear if this is right or wrong, but
        // for now do as dsymutil.
        // FIXME: Understand exactly what cases this addresses and
        // potentially remove it along with the Ranges map.
        // NOTE: In rdar://problem/20228310, there is a
        // compiler-generated function that shows up in the symbol
        // table and the line table, but not in the debug info.
        auto Range = Ranges.lower_bound(Row.Address);
        if (Range != Ranges.end()) {
          if (Range != Ranges.begin() &&
              (Range->first > Row.Address ||
               (Row.EndSequence &&
                std::prev(Range)->second.first == Row.Address)))
            --Range;

          if (Range->first <= Row.Address &&
              (Range->second.first > Row.Address ||
               (Row.EndSequence && Range->second.first == Row.Address))) {
            if (Row.EndSequence || StopAddress == -1ULL)
              StopAddress = Row.Address + Range->second.second;
            CurrRange = {Range->first, Range->second.first};
            PCOffset = Range->second.second;
            CurrRangeValid = true;
          }
        }

      if (StopAddress != -1ULL && !Seq.empty()) {
        // Insert end sequence row with the computed end address, but
        // the same line as the previous one. Only push a new state if
        // the stop address comes after the last address.
        Seq.emplace_back(Seq.empty() ? Row : Seq.back());
        Seq.back().Address = StopAddress;
        Seq.back().EndSequence = 1;
        Seq.back().PrologueEnd = 0;
        Seq.back().BasicBlock = 0;
        Seq.back().EpilogueBegin = 0;
        // FIXME: dsymutil-classic compatibility. Preserving this flag
        // makes no sense if there is no range mapping to the current row.
        if (!CurrRangeValid && InPrevRangeInclusive)
          Seq.back().IsStmt = Row.IsStmt;
        insertLineSequence(Seq, NewRows);
      }
      if (!CurrRangeValid)
        continue;
      if (Row.Address == CurrRange.second)
        continue;
    }

    // Ignore empty sequences.
    if (Row.EndSequence && Seq.empty())
      continue;

    // Relocate row address and add it to the current sequence.
    Row.Address += PCOffset;
    Seq.emplace_back(Row);

    if (Row.EndSequence)
      insertLineSequence(Seq, NewRows);
  }

  // Finished extracting, now emit the line tables.
  uint32_t PrologueEnd = StmtList + 10 + LineTable.Prologue.PrologueLength;
  // FIXME: LLVM hardcodes its prologue values. We just copy the
  // prologue over and that works because we act as both producer and
  // consumer. It would be nicer to have a real configurable line
  // table emitter.
  if (LineTable.Prologue.Version != 2 ||
      LineTable.Prologue.DefaultIsStmt != DWARF2_LINE_DEFAULT_IS_STMT ||
      LineTable.Prologue.OpcodeBase > 13)
    reportWarning("line table parameters mismatch. Cannot emit.");
  else {
    MCDwarfLineTableParameters Params;
    Params.DWARF2LineOpcodeBase = LineTable.Prologue.OpcodeBase;
    Params.DWARF2LineBase = LineTable.Prologue.LineBase;
    Params.DWARF2LineRange = LineTable.Prologue.LineRange;
    Streamer->emitLineTableForUnit(Params,
                                   LineData.slice(StmtList + 4, PrologueEnd),
                                   LineTable.Prologue.MinInstLength, NewRows,
                                   Unit.getOrigUnit().getAddressByteSize());
  }
}

void DwarfLinker::emitAcceleratorEntriesForUnit(CompileUnit &Unit) {
  for (const auto &Namespace : Unit.getNamespaces())
    AppleNamespaces.AddName(Namespace.Name, Namespace.NameOffset,
                            Namespace.Die->getOffset() + Unit.getStartOffset());

  if (!Options.Minimize)
    Streamer->emitPubNamesForUnit(Unit);
  for (const auto &Pubname : Unit.getPubnames())
    AppleNames.AddName(Pubname.Name, Pubname.NameOffset,
                       Pubname.Die->getOffset() + Unit.getStartOffset());

  if (!Options.Minimize)
    Streamer->emitPubTypesForUnit(Unit);
  for (const auto &Pubtype : Unit.getPubtypes())
    AppleTypes.AddName(Pubtype.Name, Pubtype.NameOffset,
                       Pubtype.Die->getOffset() + Unit.getStartOffset())
        .addAtom<uint16_t>(Pubtype.Die->getTag())
        .addAtom<uint8_t>(Pubtype.ObjCClassIsImplementation
                              ? dwarf::DW_FLAG_type_implementation
                              : 0)
        .addAtom<uint32_t>(Pubtype.QualifiedNameHash);

  for (const auto &ObjC : Unit.getObjC())
    AppleObjc.AddName(ObjC.Name, ObjC.NameOffset,
                      ObjC.Die->getOffset() + Unit.getStartOffset());
}

void DwarfLinker::patchFrameInfoForObject(const DebugMapObject &DMO,
                                          DWARFContext &OrigDwarf,
                                          unsigned AddrSize) {
  StringRef FrameData = OrigDwarf.getDebugFrameSection();
  if (FrameData.empty())
    return;

  DataExtractor Data(FrameData, OrigDwarf.isLittleEndian(), 0);
  uint32_t InputOffset = 0;

  // Store the data of the CIEs defined in this object, keyed by their
  // offsets.
  DenseMap<uint32_t, StringRef> LocalCIES;

  bool HasInterestingContent = false;
  for (auto &Unit : Units)
    if (Unit.hasInterestingContent()) {
      HasInterestingContent = true;
      break;
    }

  if (!HasInterestingContent)
    return;

  while (Data.isValidOffset(InputOffset)) {
    uint32_t EntryOffset = InputOffset;
    uint32_t InitialLength = Data.getU32(&InputOffset);
    if (InitialLength == 0xFFFFFFFF)
      return reportWarning("Dwarf64 bits no supported");

    uint32_t CIEId = Data.getU32(&InputOffset);
    if (CIEId == 0xFFFFFFFF) {
      // This is a CIE, store it.
      StringRef CIEData = FrameData.substr(EntryOffset, InitialLength + 4);
      LocalCIES[EntryOffset] = CIEData;
      InputOffset += InitialLength - 4;
      continue;
    }

    uint64_t Loc;
    switch (AddrSize) {
    case 4:
      Loc = Data.getU32(&InputOffset);
      break;
    case 8:
      Loc = Data.getU64(&InputOffset);
      break;
    default:
      return reportWarning("Unsuported address size.");
    }

    auto Range = Ranges.upper_bound(Loc);
    if (Range != Ranges.begin())
      --Range;
    if (Range == Ranges.end() || Range->first > Loc ||
        Range->second.first <= Loc) {
      InputOffset = EntryOffset + InitialLength + 4;
      continue;
    }

    // This is an FDE, and we have a mapping.
    // Have we already emitted a corresponding CIE?
    StringRef CIEData = LocalCIES[CIEId];
    if (CIEData.empty())
      return reportWarning("Inconsistent debug_frame content. Dropping.");

    // Look if we already emitted a CIE that corresponds to the
    // referenced one (the CIE data is the key of that lookup).
    auto IteratorInserted = EmittedCIEs.insert(
        std::make_pair(CIEData, Streamer->getFrameSectionSize()));
    // If there is no CIE yet for this ID, emit it.
    if (IteratorInserted.second ||
        // FIXME: Unnecessary inefficiency to match dsymutil-classic.
        LastCIEOffset != IteratorInserted.first->getValue()) {
      IteratorInserted.first->getValue() = LastCIEOffset = Streamer->getFrameSectionSize();
      Streamer->emitCIE(CIEData);
    }

    // Emit the FDE with updated address and CIE pointer.
    Streamer->emitFDE(
        IteratorInserted.first->getValue(), AddrSize,
        Loc + Range->second.second,
        FrameData.substr(InputOffset, InitialLength - (4 + AddrSize)));
    InputOffset += InitialLength - (4 + AddrSize);
  }
}

void DwarfLinker::DIECloner::copyAbbrev(
    const DWARFAbbreviationDeclaration &Abbrev, bool hasODR) {
  DIEAbbrev Copy((dwarf::Tag)Abbrev.getTag(),
                 (dwarf::Form)Abbrev.hasChildren());

  for (const auto &Attr : Abbrev.attributes()) {
    uint16_t Form = Attr.Form;
    if (hasODR && isODRAttribute(Attr.Attr))
      Form = dwarf::DW_FORM_ref_addr;
    Copy.AddAttribute(dwarf::Attribute(Attr.Attr), dwarf::Form(Form));
  }

  Linker.AssignAbbrev(Copy);
}

uint32_t DwarfLinker::DIECloner::hashFullyQualifiedName(
    const DWARFDebugInfoEntryMinimal *DIE, CompileUnit &U, int RecurseDepth) {
  const char *Name = nullptr;
  DWARFUnit *OrigUnit = &U.getOrigUnit();
  CompileUnit *CU = &U;
  uint64_t Ref;

  do {
    if (const char *CurrentName =
            DIE->getName(&U.getOrigUnit(), DINameKind::ShortName))
      Name = CurrentName;

    Ref = DIE->getAttributeValueAsReference(&U.getOrigUnit(),
                                            dwarf::DW_AT_specification, 0);
    if (!Ref)
      Ref = DIE->getAttributeValueAsReference(&U.getOrigUnit(),
                                              dwarf::DW_AT_abstract_origin, 0);
    if (Ref) {
      CompileUnit *RefCU;
      if (auto *RefDIE = resolveDIEReference(CompileUnits, Ref, U.getOrigUnit(),
                                             *DIE, RefCU, Linker)) {
        CU = RefCU;
        OrigUnit = &RefCU->getOrigUnit();
        DIE = RefDIE;
      }
    }
  } while (Ref);

  unsigned Idx = OrigUnit->getDIEIndex(DIE);
  if (!Name && DIE->getTag() == dwarf::DW_TAG_namespace)
    Name = "(anonymous namespace)";
  
  if (CU->getInfo(Idx).ParentIdx == 0 ||
      // FIXME: dsymutil-classic compatibility. Ignore modules.
      CU->getOrigUnit().getDIEAtIndex(CU->getInfo(Idx).ParentIdx)->getTag() == dwarf::DW_TAG_module)
    return DwarfAccelTable::HashDJB(
        Name ? Name : "", DwarfAccelTable::HashDJB(RecurseDepth ? "" : "::"));

  return DwarfAccelTable::HashDJB(
      (Name ? Name : ""),
      DwarfAccelTable::HashDJB(
          (Name ? "::" : ""),
          hashFullyQualifiedName(
              OrigUnit->getDIEAtIndex(CU->getInfo(Idx).ParentIdx), *CU,
              ++RecurseDepth)));
}

static DWARFAcceleratorTable *getAccelTable(DWARFContextInMemory &DwarfContext,
                                            const DWARFSection &Section) {
  DataExtractor AccelSection(Section.Data, DwarfContext.isLittleEndian(), 0);
  DataExtractor StrData(DwarfContext.getStringSection(),
                        DwarfContext.isLittleEndian(), 0);
  auto Table = new DWARFAcceleratorTable(AccelSection, StrData, Section.Relocs);
  Table->extract();
  return Table;
}

bool DwarfLinker::registerModuleReference(
    const DWARFDebugInfoEntryMinimal &CUDie, const DWARFUnit &Unit,
    DebugMap &ModuleMap, unsigned Indent) {
  std::string PCMfile =
      CUDie.getAttributeValueAsString(&Unit, dwarf::DW_AT_GNU_dwo_name, "");
  if (PCMfile.empty())
    return false;

  // Clang module DWARF skeleton CUs abuse this for the path to the module.
  std::string PCMpath =
    CUDie.getAttributeValueAsString(&Unit, dwarf::DW_AT_comp_dir, "");

  if (Options.Verbose) {
    outs().indent(Indent);
    outs() << "Found clang module reference " << PCMfile;
  }

  if (ClangModules.count(PCMfile)) {
    if (Options.Verbose)
      outs() << " [skipping].\n";
    return true;
  }
  if (Options.Verbose)
    outs() << " [following].\n";

  // Cyclic dependencies are disallowed by Clang, but we still
  // shouldn't run into an infinite loop, so mark it as processed now.
  ClangModules.insert({PCMfile, ClangModule()});
  loadClangModule(PCMfile, PCMpath, ModuleMap, Indent + 2);
  return true;
}

ErrorOr<const object::ObjectFile &>
DwarfLinker::loadObject(BinaryHolder &BinaryHolder, DebugMapObject &Obj,
                        const DebugMap &Map) {
  auto ErrOrObjs =
      BinaryHolder.GetObjectFiles(Obj.getObjectFilename(), Obj.getTimestamp());
  if (std::error_code EC = ErrOrObjs.getError())
    reportWarning(Twine(Obj.getObjectFilename()) + ": " + EC.message());
  auto ErrOrObj = BinaryHolder.Get(Map.getTriple());
  if (std::error_code EC = ErrOrObj.getError())
    reportWarning(Twine(Obj.getObjectFilename()) + ": " + EC.message());
  return ErrOrObj;
}

void DwarfLinker::loadClangModule(StringRef Filename, StringRef ModulePath,
                                  DebugMap &ModuleMap,
                                  unsigned Indent) {
  SmallString<80> Path(Options.PrependPath);
  if (sys::path::is_relative(Filename))
    sys::path::append(Path, ModulePath, Filename);
  else
    sys::path::append(Path, Filename);
  auto *ObjHolder = new BinaryHolder(Options.Verbose);
  auto &Obj = ModuleMap.addDebugMapObject(Path, sys::TimeValue::MinTime());
  auto ErrOrObj = loadObject(*ObjHolder, Obj, ModuleMap);
  if (!ErrOrObj) {
    ClangModules.erase(ClangModules.find(Filename));
    return;
  }

  CompileUnit *Unit = nullptr;

  // Setup access to the debug info.
  auto *DwarfContext = new DWARFContextInMemory(*ErrOrObj);
  RelocationManager RelocMgr(*this);
  for (const auto &CU : DwarfContext->compile_units()) {
    auto *CUDie = CU->getCompileUnitDIE(false);
    if (false && Options.Verbose) {
      outs().indent(Indent);
      outs() << "Input compilation unit:";
      CUDie->dump(outs(), CU.get(), 0, Indent);
    }
    // Recursively get all modules imported by this one.
    if (!registerModuleReference(*CUDie, *CU, ModuleMap, Indent)) {
      // Add this module.
      if (Unit) {
        errs() << Filename << ": Clang modules are expected to have exactly"
               << " 1 compile unit.\n";
        exitDsymutil(1);
      }
      Unit = new CompileUnit(*CU, UnitID++, !Options.NoODR, Obj);
      Unit->setHasInterestingContent();
      gatherDIEParents(CUDie, 0, *Unit, &RootDeclContext, StringPool,
                       DeclContexts);
      // Keep everything.
      Unit->markEverythingAsKept();
    }
  }
  if (Options.Verbose) {
    outs().indent(Indent);
    outs() << "cloning .debug_info from " << Filename << "\n";
  }

  ClangModule &CM = ClangModules.find(Filename)->second;
  CM.BinaryHolder.reset(ObjHolder);
  CM.DwarfContext.reset(DwarfContext);
  CM.AppleTypes.reset(
      getAccelTable(*DwarfContext, DwarfContext->getAppleTypesSection()));
  CM.Unit.reset(Unit);
  // Performance note: If this is DWARF 3 or greater, we could pass in
  // the regular DIEAlloc.
  DIECloner(*this, RelocMgr, ModuleDIEAlloc,
            MutableArrayRef<CompileUnit>(*Unit), Options)
      .cloneAllCompileUnits(*DwarfContext);
}

void DwarfLinker::DIECloner::cloneAllCompileUnits(
    DWARFContextInMemory &DwarfContext) {
  if (!Linker.Streamer)
    return;

  ExtTypes.reset(
      getAccelTable(DwarfContext, DwarfContext.getAppleExternalTypesSection()));

  for (auto &CurrentUnit : CompileUnits) {
    const auto *InputDIE = CurrentUnit.getOrigUnit().getCompileUnitDIE();
    Linker.Streamer->switchToDebugInfoSection(
        CurrentUnit.getOrigUnit().getVersion());
    CurrentUnit.setStartOffset(Linker.OutputDebugInfoSize);
    DIE *OutputDIE = nullptr;

    if (CurrentUnit.hasInterestingContent() || CompileUnits.size() != 1)
      OutputDIE = cloneDIE(*InputDIE, CurrentUnit, 0 /* PC offset */,
                           11 /* Unit Header size */, 0);
    CurrentUnit.setOutputUnitDIE(OutputDIE);
    // FIXME: for compatibility with the classic dsymutil
    if (OutputDIE || CompileUnits.size() != 1)
      Linker.OutputDebugInfoSize = CurrentUnit.computeNextUnitOffset();
    if (Linker.Options.NoOutput)
      continue;
    // FIXME: for compatibility with the classic dsymutil, we emit
    // an empty line table for the unit, even if the unit doesn't
    // actually exist in the DIE tree.
    if ((!Linker.Options.Update || Linker.Options.Translator) &&
        (OutputDIE || CompileUnits.size() != 1))
      Linker.patchLineTableForUnit(CurrentUnit, Linker.getCurrentDMO(),
                                   DwarfContext);
    if (!OutputDIE)
      continue;
    Linker.emitAcceleratorEntriesForUnit(CurrentUnit);
    if (Linker.Options.Update)
      continue;
    Linker.patchRangesForUnit(CurrentUnit, DwarfContext);
    Linker.Streamer->emitLocationsForUnit(CurrentUnit, DwarfContext);
  }

  if (Linker.Options.NoOutput)
    return;

  // Emit all the compile unit's debug information.
  for (auto &CurrentUnit : CompileUnits) {
    if (!Linker.Options.Update)
      Linker.generateUnitRanges(CurrentUnit);
    CurrentUnit.fixupForwardReferences();
    // FIXME: for compatibility with the classic dsymutil, we emit
    if (CurrentUnit.getOutputUnitDIE() || CompileUnits.size() != 1)
      Linker.Streamer->emitCompileUnitHeader(CurrentUnit);
    if (!CurrentUnit.getOutputUnitDIE())
      continue;
    Linker.Streamer->emitDIE(*CurrentUnit.getOutputUnitDIE());
  }
}

bool DwarfLinker::link(const DebugMap &Map) {

  // Empty debug maps should lead to an linked dwarf MachO with an empty
  // debug info section in order to match dsymutil-classic behavior.
  if (Map.begin() == Map.end() && !Options.Update)
    errs() << "warning: no debug symbols in executable (-arch "
           << Map.getTriple().getArchName() << ")\n";

  if (!createStreamer(Map.getTriple(), Map.getBinaryPath(), OutputFilename))
    return false;

  // Size of the DIEs (and headers) generated for the linked output.
  OutputDebugInfoSize = 0;
  // A unique ID that identifies each compile unit.
  UnitID = 0;

  DebugMap ModuleMap(Map.getTriple(), Map.getBinaryPath());

  for (const auto &Obj : Map.objects()) {
    CurrentDebugObject = Obj.get();

    if (Options.Verbose)
      outs() << "DEBUG MAP OBJECT: " << Obj->getObjectFilename()
             << " (timestamp: " << Obj->getTimestamp().seconds() << ")\n";

    // Handle Swift modules.
    if (Obj->isSwiftModule()) {
      StringRef File = Obj->getObjectFilename();
      auto ErrorOrMem = MemoryBuffer::getFile(File);
      if (!ErrorOrMem) {
        errs() << "Warning: Could not open " << File << "\n";
        continue;
      }
      sys::fs::file_status Stat;
      if (auto errc = sys::fs::status(File, Stat)) {
        errs() << "Warning: " << errc.message() << "\n";
        continue;
      }
      if (Stat.getLastModificationTime() != Obj->getTimestamp()) {
        errs() << "Warning: Timestamp mismatch for " << File << "\n";
        continue;
      }

      // Copy the module into the .swift_ast section.
      Streamer->emitSwiftModule((*ErrorOrMem)->getBuffer());
      continue;
    }
    
    if (Obj->getWarnings().size() && Obj->empty()) {
      // dsymutil-classic compatibility: Emit warnings into the .dSYM
      Streamer->switchToDebugInfoSection(/* Version */ 2);
      DIE *CUDie = new (DIEAlloc) DIE(dwarf::DW_TAG_compile_unit);
      CUDie->setOffset(11);
      StringRef Producer = StringPool.internString( //"llvm-dsymutil"
          "dsymutil from dwarf_utilities-132 (Apple Inc.)");
      StringRef File = StringPool.internString(Obj->getObjectFilename());
      CUDie->addValue(
          dwarf::Attribute(dwarf::DW_AT_producer), dwarf::DW_FORM_strp,
          new (DIEAlloc) DIEInteger(StringPool.getStringOffset(Producer)),
          &DIEsToDelete);
      DIEBlock *String = new (DIEAlloc) DIEBlock();
      DIEBlocks.push_back(String);
      for (auto &C : File)
        String->addValue(dwarf::Attribute(0), dwarf::DW_FORM_data1,
                         new (DIEAlloc) DIEInteger(C));
      String->addValue(dwarf::Attribute(0), dwarf::DW_FORM_data1,
                       new (DIEAlloc) DIEInteger(0));

      CUDie->addValue(dwarf::Attribute(dwarf::DW_AT_name),
                      dwarf::DW_FORM_string, String, &DIEsToDelete);
      for (const auto &Warning : Obj->getWarnings()) {
        CUDie->addChild(new (DIEAlloc) DIE(dwarf::DW_TAG_constant));
        DIE &ConstDie = CUDie->getChildren().back();
        ConstDie.addValue(
            dwarf::Attribute(dwarf::DW_AT_name), dwarf::DW_FORM_strp,
            new (DIEAlloc)
                DIEInteger(StringPool.getStringOffset("dsymutil_warning")),
            &DIEsToDelete);
        ConstDie.addValue(dwarf::Attribute(dwarf::DW_AT_artificial),
                          dwarf::DW_FORM_flag, new (DIEAlloc) DIEInteger(1),
                          &DIEsToDelete);
        ConstDie.addValue(dwarf::Attribute(dwarf::DW_AT_const_value),
                          dwarf::DW_FORM_strp,
                          new (DIEAlloc) DIEInteger(StringPool.getStringOffset(
                              StringPool.internString(Warning))),
                          &DIEsToDelete);
      }
      unsigned Size = 4 /* FORM_strp */ + File.size() + 1 +
                      CUDie->getChildren().size() * (4 + 1 + 4) +
                      1 /* End of children */;
      AssignAbbrev(CUDie->getAbbrev());
      Size += getULEB128Size(CUDie->getAbbrev().getNumber());
      // Abbreviation ordering needed for classic compatibility.
      for (auto &Child : CUDie->getChildren()) {
        AssignAbbrev(Child.getAbbrev());
        Size += getULEB128Size(Child.getAbbrev().getNumber());
      }
      CUDie->setSize(Size);
      auto &Asm = Streamer->getAsmPrinter();
      Asm.EmitInt32(11 + CUDie->getSize() - 4);
      Asm.EmitInt16(2);
      Asm.EmitInt32(0);
      Asm.EmitInt8(Asm.getDataLayout().getPointerSize());
      Streamer->emitDIE(*CUDie);
      OutputDebugInfoSize += 11 /* Header */ + Size;
      continue;
    }

    auto ErrOrObj = loadObject(BinHolder, *Obj, Map);
    if (!ErrOrObj)
      continue;

    // Look for relocations that correspond to debug map entries.
    RelocationManager RelocMgr(*this);
    if (!Options.Update &&
        !RelocMgr.findValidRelocsInDebugInfo(*ErrOrObj, *Obj)) {
      if (Options.Verbose)
        outs() << "No valid relocations found. Skipping.\n";
      continue;
    }

    // Setup access to the debug info.
    DWARFContextInMemory DwarfContext(*ErrOrObj);
    startDebugObject(DwarfContext, *Obj);

    // In a first phase, just read in the debug info and store the DIE
    // parent links that we will use during the next phase.
    for (const auto &CU : DwarfContext.compile_units()) {
      auto *CUDie = CU->getCompileUnitDIE(false);
      if (Options.Verbose) {
        outs() << "Input compilation unit:";
        CUDie->dump(outs(), CU.get(), 0);
      }
      if (!registerModuleReference(*CUDie, *CU, ModuleMap)) {
        bool CanUseODR = !Options.NoODR && !Options.Update;
        Units.emplace_back(*CU, UnitID++, CanUseODR, *Obj);
        gatherDIEParents(CUDie, 0, Units.back(), &RootDeclContext, StringPool,
                         DeclContexts);
      }
    }

    // Then mark all the DIEs that need to be present in the linked
    // output and collect some information about them. Note that this
    // loop can not be merged with the previous one because cross-cu
    // references require the ParentIdx to be setup for every CU in
    // the object file before calling this.
    if (Options.Update) {
      for (auto &Unit : Units)
        Unit.markEverythingAsKept();
      Streamer->copyInvariantDebugSection(*ErrOrObj, Options);
    } else {
      for (auto &CurrentUnit : Units)
        lookForDIEsToKeep(RelocMgr,
                          *CurrentUnit.getOrigUnit().getCompileUnitDIE(), *Obj,
                          CurrentUnit, 0);
    }

    // The calls to applyValidRelocs inside cloneDIE will walk the
    // reloc array again (in the same way findValidRelocsInDebugInfo()
    // did). We need to reset the NextValidReloc index to the beginning.
    RelocMgr.resetValidRelocs();
    if (RelocMgr.hasValidRelocs() || Options.Update)
      DIECloner(*this, RelocMgr, DIEAlloc, Units, Options)
          .cloneAllCompileUnits(DwarfContext);

    // FIXME: dsymutil-classic compat. We only patch the frame info if
    // we applied relocs to the debug_info section. This feels wrong,
    // we could have functions described in the debug_frame section
    // that do not have counter-parts in debug_info.
    if (RelocMgr.hasValidRelocs() && !Options.NoOutput && !Units.empty() &&
        !Options.Update)
      patchFrameInfoForObject(*Obj, DwarfContext,
                              Units[0].getOrigUnit().getAddressByteSize());

    //    assert(Streamer->DIEToDIERefs.empty());
    //    DIEToDIERefs.clear();
    DIEToDeclContext.clear();
    // for (auto *Block: DIEBlocks)
    //   Block->~DIEBlock();
    // for (auto *Locs: DIELocs)
    //   Block->~DIEBlock();
    // for (auto &Unit: Streamer->Units)
    //   delete Unit.CUDie;

    //    Blocks.clear();
    Units.clear();

    for (auto *Die : DIEsToDelete) {
      Die->getChildren().clearAndLeakNodesUnsafely();
      Die->~DIE();
    }
    DIEsToDelete.resize(0);
    // DIEAlloc.PrintStats();
    // PermanentAlloc.PrintStats();
    DIEAlloc.Reset();
    if (Options.Verbose) {
      outs() << "DebugInfo is currently " << OutputDebugInfoSize / (1024 * 1024)
             << "MB\n";
    }

    // Clean-up before starting working on the next object.
    endDebugObject();
  }

  // Emit everything that's global.
  if (!Options.NoOutput) {
    Streamer->emitAbbrevs(Abbreviations);
    Streamer->emitStrings(StringPool);
    Streamer->emitAppleNames(AppleNames);
    Streamer->emitAppleNamespaces(AppleNamespaces);
    Streamer->emitAppleTypes(AppleTypes);
    Streamer->emitAppleObjc(AppleObjc);
    if (!Options.Minimize && !Options.Update)
      Streamer->emitDebugInlined(Inlined);
  }

  return Options.NoOutput ? true : Streamer->finish(Map, Options.Translator);
}

bool linkDwarf(StringRef OutputFilename, const DebugMap &DM,
               const LinkOptions &Options) {
  DwarfLinker Linker(OutputFilename, Options);
  return Linker.link(DM);
}
}
}
