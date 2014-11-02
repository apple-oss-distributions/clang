//===-- RuntimeDyldMachO.h - Run-time dynamic linker for MC-JIT ---*- C++ -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// MachO support for MC-JIT runtime dynamic linker.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_RUNTIME_DYLD_MACHO_H
#define LLVM_RUNTIME_DYLD_MACHO_H

#include "RuntimeDyldImpl.h"
#include "llvm/Object/MachO.h"
#include "llvm/Support/Format.h"

using namespace llvm;
using namespace llvm::object;

namespace llvm {
class RuntimeDyldMachO : public RuntimeDyldImpl {
  class GlobalOffsetTableManager {
    struct GlobalOffsetTable {
      SID SectionID;
      std::vector<RelocationValueRef> Entries;
      std::map<RelocationValueRef, size_t> RelocationToIndex;
      std::map<StringRef, size_t> SymbolToIndex;
      std::map<uint64_t, size_t> AddressToIndex;

      GlobalOffsetTable() : SectionID(RTDYLD_INVALID_SECTION_ID) { }

      void insert(const RelocationValueRef &RelocVal) {
        auto result = RelocationToIndex.insert(std::make_pair(RelocVal, 0));
        // Element has already been inserted, so there is nothing to do.
        if (!result.second)
          return;
        // Insert the element and update the index.
        Entries.push_back(RelocVal);
        size_t Index = Entries.size() - 1;
        result.first->second = Index;
        // Add quick lookup for symbols.
        if (RelocVal.SymbolName)
          SymbolToIndex.insert(std::make_pair(RelocVal.SymbolName, Index));
      }

      void insert(uint64_t Addr, size_t Index) {
        auto result = AddressToIndex.insert(std::make_pair(Addr, Index));
        // Check if the element has already been inserted.
        if (result.second)
          return;
        assert(result.first->second == Index && "Different indices!");
      }

      std::pair<size_t, bool> find(uint64_t Addr) const {
        auto result = AddressToIndex.find(Addr);
        if (result == AddressToIndex.end())
          return std::make_pair(-1, false);
        return std::make_pair(result->second, true);
      }

      std::pair<size_t, bool> find(StringRef Name) const {
        auto result = SymbolToIndex.find(Name);
        if (result == SymbolToIndex.end())
          return std::make_pair(-1, false);
        return std::make_pair(result->second, true);
      }

      size_t size() { return Entries.size(); }

      std::vector<RelocationValueRef>::iterator begin() {
        return Entries.begin();
      }
      std::vector<RelocationValueRef>::iterator end() {
        return Entries.end();
      }
    };

    SmallVector<GlobalOffsetTable, 4> GOTs;
  public:
    GlobalOffsetTable GOTEntries;

    void insert(const RelocationValueRef &RelocVal) {
      GOTEntries.insert(RelocVal);
    }

    void commit(SID SectionID) {
      GOTEntries.SectionID = SectionID;
      GOTs.push_back(std::move(GOTEntries));
    }

    SmallVector<GlobalOffsetTable, 4>::iterator begin() { return GOTs.begin(); }
    SmallVector<GlobalOffsetTable, 4>::iterator end() { return GOTs.end(); }
  };

  bool resolveI386Relocation(uint8_t *LocalAddress,
                             uint64_t FinalAddress,
                             uint64_t Value,
                             bool IsPCRel,
                             unsigned Type,
                             unsigned Size,
                             int64_t Addend,
                             unsigned SectionAID,
                             unsigned SectionBID);
  bool resolveX86_64Relocation(uint8_t *LocalAddress,
                               uint64_t FinalAddress,
                               uint64_t Value,
                               bool IsPCRel,
                               unsigned Type,
                               unsigned Size,
                               int64_t Addend);
  bool resolveARMRelocation(uint8_t *LocalAddress,
                            uint64_t FinalAddress,
                            uint64_t Value,
                            bool IsPCRel,
                            unsigned Type,
                            unsigned Size,
                            int64_t Addend);

  bool resolveARM64Relocation(uint8_t *LocalAddress,
                              uint64_t FinalAddress,
                              uint64_t Value,
                              bool IsPCRel,
                              unsigned Type,
                              unsigned Size,
                              int64_t Addend);

  void resolveRelocation(const SectionEntry &Section,
                         uint64_t Offset,
                         uint64_t Value,
                         uint32_t Type,
                         int64_t Addend,
                         bool IsPCRel,
                         unsigned Size,
                         unsigned SectionAID = 0,
                         unsigned SectionBID = 0);

  // Populate stubs in __jump_table section.
  void populateJumpTable(MachOObjectFile &Obj, const SectionRef &JTSection,
                         unsigned JTSectionID);

  // Populate __pointers section.
  void populatePointersSection(MachOObjectFile &Obj, const SectionRef &PTSection,
                               unsigned PTSectionID);


  unsigned getMaxStubSize() {
    if (Arch == Triple::arm || Arch == Triple::thumb)
      return 8; // 32-bit instruction and 32-bit address
    else if (Arch == Triple::x86_64)
      return 8; // GOT entry
    else
      return 0;
  }

  unsigned getStubAlignment() {
    return 1;
  }

  virtual void updateGOTEntries(StringRef Name, uint64_t Addr);

  uint64_t findGOTEntry(uint64_t Addr);
  size_t getGOTEntrySize();

  relocation_iterator processSECTDIFFRelocation(
                                             unsigned SectionID,
                                             relocation_iterator RelI,
                                             ObjectImage &ObjImg,
                                             ObjSectionToIDMap &ObjSectionToID);

  // Global Offset Table for Large Code Model
  GlobalOffsetTableManager GOTMgr;

  struct EHFrameRelatedSections {
    EHFrameRelatedSections() : EHFrameSID(RTDYLD_INVALID_SECTION_ID),
                               TextSID(RTDYLD_INVALID_SECTION_ID),
                               ExceptTabSID(RTDYLD_INVALID_SECTION_ID) {}
    EHFrameRelatedSections(SID EH, SID T, SID Ex)
      : EHFrameSID(EH), TextSID(T), ExceptTabSID(Ex) {}
    SID EHFrameSID;
    SID TextSID;
    SID ExceptTabSID;
  };

  // When a module is loaded we save the SectionID of the EH frame section
  // in a table until we receive a request to register all unregistered
  // EH frame sections with the memory manager.
  SmallVector<EHFrameRelatedSections, 2> UnregisteredEHFrameSections;
public:
  RuntimeDyldMachO(RTDyldMemoryManager *mm) : RuntimeDyldImpl(mm) {}

  virtual void resolveRelocation(const RelocationEntry &RE, uint64_t Value);
  virtual relocation_iterator
  processRelocationRef(unsigned SectionID, const section_iterator &SI,
                       relocation_iterator RelI, ObjectImage &Obj,
                       ObjSectionToIDMap &ObjSectionToID,
                       const SymbolTableMap &Symbols, StubMap &Stubs);
  virtual bool isCompatibleFormat(const ObjectBuffer *Buffer) const;
  virtual bool isCompatibleFile(const object::ObjectFile *Obj) const;
  virtual void registerEHFrames();
  virtual void finalizeLoad(ObjectImage &ObjImg,
                            ObjSectionToIDMap &SectionMap);
};

} // end namespace llvm

#endif
