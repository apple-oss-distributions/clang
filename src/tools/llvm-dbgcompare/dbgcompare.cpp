#include <../dsymutil/BinaryHolder.h>
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/Options.h"


using namespace llvm;
using namespace llvm::dsymutil;

namespace {
using namespace llvm::cl;

static opt<std::string> File1(Positional, desc("<bin1>"));
static opt<std::string> File2(Positional, desc("<bin2>"));

}

BinaryHolder Bin1(false), Bin2(false);

bool compareAbbrevDeclarations(const DWARFAbbreviationDeclaration *A1,
                               const DWARFAbbreviationDeclaration *A2) {
  if (!A1 || !A2)
    return false;
  
  if (A1->getTag() != A2->getTag())
    return false;
  
  if (A1->hasChildren() != A2->hasChildren())
    return false;
  
  auto Attr2It = A2->attributes().begin();
  const auto Attr2End = A2->attributes().end();
  bool Identical = true;
  for (const auto &Attr1 : A1->attributes()) {
    if (Attr2It == Attr2End ||
        Attr1.Attr != Attr2It->Attr ||
        Attr1.Form != Attr2It->Form) {
      Identical = false;
      break;
    }
    Attr2It++;
  }

  if (Attr2It != Attr2End)
    return false;
  
  return Identical;
}

static void dumpDIEAndParents(const DWARFDebugInfoEntryMinimal *DIE,
                              DWARFUnit *Unit,
                              std::vector<const DWARFDebugInfoEntryMinimal *>& Parents) {
  unsigned incr = 0;
  for (const auto *Parent : Parents)
    Parent->dump(errs(), Unit, 0, incr++ * 4);
  errs() << " \\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/\\/";
  DIE->dump(errs(), Unit, 0, incr * 4);
}

bool compareDIEs(const DWARFDebugInfoEntryMinimal *DIE1,
                 const DWARFDebugInfoEntryMinimal *DIE2,
                 const std::vector<bool>& AbbrevsIdentical,
                 DWARFUnit *Unit1,
                 DWARFUnit *Unit2,
                 std::vector<const DWARFDebugInfoEntryMinimal *>& Parents1,
                 std::vector<const DWARFDebugInfoEntryMinimal *>& Parents2) {
  bool NeedsAttrCompare = true;
  bool IsDIE1Null = !DIE1 || DIE1->isNULL();
  bool IsDIE2Null = !DIE2 || DIE2->isNULL();

  if (IsDIE1Null && IsDIE2Null)
    return true;

  if (IsDIE1Null || IsDIE2Null) {
    errs() << "DIE STRUCTURE MISMATCH\n========================================\n";
    errs() << "Not same number of children\n";
    if (IsDIE2Null) {
      errs() << "Not in " << File2 << "\n";
      dumpDIEAndParents(DIE1, Unit1, Parents1);
    } else {
      errs() << "Not in " << File1 << "\n";
      dumpDIEAndParents(DIE2, Unit2, Parents2);
    }
    return false;
  }

  const auto *Attr1 = DIE1->getAbbreviationDeclarationPtr();
  const auto *Attr2 = DIE2->getAbbreviationDeclarationPtr();

  if (Attr1->getCode() == Attr2->getCode() && AbbrevsIdentical[Attr1->getCode()])
    NeedsAttrCompare = false;

  if (NeedsAttrCompare)
    if (!compareAbbrevDeclarations(Attr1, Attr2)) {
      errs() << "DIE STRUCTURE MISMATCH\n========================================\n";
      errs() << "In " << File1 << '\n';
      dumpDIEAndParents(DIE1, Unit1, Parents1);
      errs() << "========================================\nIn " << File2 << '\n';
      dumpDIEAndParents(DIE2, Unit2, Parents2);
      errs() << "========================================\n";
      return false;
    }

  Parents1.push_back(DIE1);
  Parents2.push_back(DIE2);
  const auto *Child1 = DIE1->getFirstChild(),
    *Child2 = DIE2->getFirstChild();
  for (;Child1 && Child2;
       Child1 = Child1->getSibling(), Child2 = Child2->getSibling()) {
    if (!compareDIEs(Child1, Child2, AbbrevsIdentical, Unit1, Unit2, Parents1, Parents2))
      return false;
  }
  Parents1.pop_back();
  Parents2.pop_back();
  
  if (Child1 != Child2) {
    errs() << "DIE STRUCTURE MISMATCH\n========================================\n";
    errs() << "Not same number of children\n";
    if (Child1) {
      errs() << "Not in " << File2 << "\n";
      dumpDIEAndParents(DIE1, Unit1, Parents1);
    } else {
      errs() << "Not in " << File1 << "\n";
      dumpDIEAndParents(DIE2, Unit2, Parents2);
    }
    return false;
  }

  return true;
}

static StringRef getSectionData(StringRef SecName, const object::ObjectFile &Obj) {
  for (const auto &Section : Obj.sections()) {
    StringRef name;
    Section.getName(name);

    if (name == SecName) {
      StringRef Data;
      Section.getContents(Data);
      return Data;
    }
  }
  return StringRef();
}

static bool compare(StringRef Data1, StringRef Data2, uint32_t *Where) {
  if (Data1.size() == 0) {
    if (Data2.size() == 0)
      return true;

    *Where = 0;
    return false;
  } else if (Data2.size() == 0) {
    *Where = 0;
    return false;
  }

  if (Data1 == Data2)
    return true;
  
  unsigned i;
  for (i = 0; i < Data1.size() && i < Data2.size(); ++i) {
    // outs() << format(" 0x%02x", (unsigned)(Data1[i]&0xFF));
    // if (i%16 == 0)
    //   outs() << "\n";
    if (Data1[i] != Data2[i])
      break;
  }
  
  *Where = i;
  return false;
}

int main(int argc, char *argv[]) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "llvm dbgcompare\n");

  bool CompareDebugInfo = false;
  
  auto ErrOrBins1 = Bin1.GetObjectFiles(File1);
  if (auto Error = ErrOrBins1.getError()) {
    errs() << "error opening '" << File1 << "': " << Error.message() << '\n';
    return 1;
  }

  auto ErrOrBins2 = Bin2.GetObjectFiles(File2);
  if (auto Error = ErrOrBins2.getError()) {
    errs() << "error opening '" << File2 << "': " << Error.message() << '\n';
    return 1;
  }

  StringRef SectionsToCompare[] = {
    "__debug_line", "__debug_aranges", "__debug_ranges", "__apple_namespac",
    "__apple_names", "__apple_types", "__debug_pubtypes", "__debug_pubnames",
    "__debug_inlined", "__debug_str", "__debug_loc", "__debug_info",
    "__debug_abbrev", "__apple_objc", "__debug_frame" };


  for (StringRef SecName : SectionsToCompare) {
    StringRef Data1 = getSectionData(SecName, *(*ErrOrBins1)[0]);
    StringRef Data2 = getSectionData(SecName, *(*ErrOrBins2)[0]);
    uint32_t Where;
    if (!compare(Data1, Data2, &Where)) {
      outs() << "Comparing " << SecName << "\n";
      outs() << "\tData1.size() == " << Data1.size() << " Data2.size() == "
             << Data2.size() << "\n";
      outs() << "\tERROR at offset " << Where << "\n";
      if (SecName == "__debug_info")
        CompareDebugInfo = true;
    }
  }

  if (!CompareDebugInfo)
    return 0;

  DWARFContextInMemory Dwarf1(*(*ErrOrBins1)[0]);
  DWARFContextInMemory Dwarf2(*(*ErrOrBins2)[0]);

  const DWARFDebugAbbrev *Abbrev1 = Dwarf1.getDebugAbbrev();
  const DWARFDebugAbbrev *Abbrev2 = Dwarf2.getDebugAbbrev();

  if (!Abbrev1 || !Abbrev2) {
    errs() << "Missing Abbrev info\n";
    return 1;
  }
  
  // We only care about the first abbrev set (there is only one in
  // in dsymutil generated Dwarf).
  const auto *AbbrevSet1 = Abbrev1->getAbbreviationDeclarationSet(0);
  const auto *AbbrevSet2 = Abbrev2->getAbbreviationDeclarationSet(0);

  if (!Abbrev1 || !Abbrev2) {
    errs() << "No Abbrev set at 0 ?!\n";
    return 1;
  }
  
  // Abbrevs start at 1.
  std::vector<bool> AbbrevsIdentical;
  AbbrevsIdentical.push_back(false);
  for (unsigned idx = 1; true; idx++) {
    const auto *A1 = AbbrevSet1->getAbbreviationDeclaration(idx);
    const auto *A2 = AbbrevSet2->getAbbreviationDeclaration(idx);
    if (!A1)
      break;

    AbbrevsIdentical.push_back(compareAbbrevDeclarations(A1, A2));
    if (!AbbrevsIdentical.back())
      errs() << "Abbrev " << idx << " differs\n";
  }

  auto CU2It = Dwarf2.compile_units().begin();
  const auto CU2End = Dwarf2.compile_units().end();

  std::vector<const DWARFDebugInfoEntryMinimal *> Parents1, Parents2;
  for (const auto &CU1 : Dwarf1.compile_units()) {
    const auto *DIE1 = CU1->getCompileUnitDIE(false);
    if (CU2It == CU2End) {
      errs() << "Not the same number of units! Not in " << File2 << '\n';
      DIE1->dump(errs(), CU1.get(), 0, 0);
      return 1;
    }
    const auto *DIE2 = (*CU2It)->getCompileUnitDIE(false);
    
    if (!compareDIEs(DIE1, DIE2, AbbrevsIdentical, CU1.get(), (*CU2It).get(), Parents1, Parents2)){
      errs() << "=============================================================\n";
      errs() << "In " << File1 << " in CU:\n";
      DIE1->dump(errs(), CU1.get(), -1U, 0);
      errs() << "=============================================================\n";
      errs() << "In " << File2 << " in CU:\n";
      DIE2->dump(errs(), (*CU2It).get(), -1U, 0);
      return 1;
    }

    // assert(Parents1.empty() && Parents2.empty());
    CU2It++;
  }

  if (CU2It != CU2End) {
      errs() << "Not the same number of units! Not in " << File1 << '\n';
      (*CU2It)->getCompileUnitDIE()->dump(errs(), (*CU2It).get(), 0, 0);
      return 1;
  }
  
  return 0;
}
