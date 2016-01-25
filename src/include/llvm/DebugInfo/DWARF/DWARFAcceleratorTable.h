//===--- DWARFAcceleratorTable.h --------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallVector.h"
#include "llvm/DebugInfo/DWARF/DWARFFormValue.h"
#include "llvm/DebugInfo/DWARF/DWARFRelocMap.h"
#include <cstdint>

namespace llvm {

class DWARFAcceleratorTable {

  struct Header {
    uint32_t Magic;
    uint16_t Version;
    uint16_t HashFunction;
    uint32_t NumBuckets;
    uint32_t NumHashes;
    uint32_t HeaderDataLength;
  };

  struct HeaderData {
    typedef uint16_t AtomType;
    typedef uint16_t Form;
    uint32_t DIEOffsetBase;
    SmallVector<std::pair<AtomType, Form>, 3> Atoms;
  };

  struct Header Hdr;
  struct HeaderData HdrData;
  DataExtractor AccelSection;
  DataExtractor StringSection;
  const RelocAddrMap& Relocs;
public:
  DWARFAcceleratorTable(DataExtractor AccelSection, DataExtractor StringSection,
                        const RelocAddrMap &Relocs)
    : AccelSection(AccelSection), StringSection(StringSection), Relocs(Relocs) {}

  bool extract();
  void dump(raw_ostream &OS) const;

  class DataIterator : public std::iterator<std::input_iterator_tag,
                                            SmallVectorImpl<DWARFFormValue>> {
    const DWARFAcceleratorTable *AccelTable;
    SmallVector<DWARFFormValue, 3> AtomForms;
    unsigned DataOffset, Data, NumData;

    void next() {
      assert(NumData > 0 && "attempted to increment iterator past the end");
      auto &AccelSection = AccelTable->AccelSection;
      if (Data >= NumData ||
          !AccelSection.isValidOffsetForDataOfSize(DataOffset, 4)) {
        NumData = 0;
        return;
      }
      for (auto &Atom : AtomForms)
        Atom.extractValue(AccelSection, &DataOffset, nullptr);
      ++Data;
    }
  public:
    /// End marker.
    DataIterator() : NumData(0) {}
    DataIterator(const DWARFAcceleratorTable &AccelTable, unsigned Ofs)
      : AccelTable(&AccelTable), DataOffset(Ofs), Data(0) {
      if (!AccelTable.AccelSection.isValidOffsetForDataOfSize(DataOffset, 4)) {
        NumData = 0;
        return;
      }
      for (const auto &Atom: AccelTable.HdrData.Atoms)
        AtomForms.push_back(DWARFFormValue(Atom.second));
      // Read the first entry.
      NumData = AccelTable.AccelSection.getU32(&DataOffset);
      next();
    }
    const SmallVectorImpl<DWARFFormValue> &operator*() const {
      return AtomForms;
    }
    const SmallVectorImpl<DWARFFormValue> *operator->() const {
      return &AtomForms;
    }
    DataIterator &operator++() { next(); return *this; }
    DataIterator operator++(int) { DataIterator X(*this); next(); return X; }
    bool operator==(const DataIterator &X) const {
      if (NumData == 0)
        return X.NumData == 0;
      return NumData == X.NumData && DataOffset == X.DataOffset;
    }
    bool operator!=(const DataIterator &X) const {
      return !(*this == X);
    }
  };

  /// \brief Look up an entry in the accelerator table by its key.
  DataIterator find(StringRef Key) const;
  DataIterator end() const { return DataIterator(); }
};

}
