//===-- ARM64MCAsmInfo.cpp - ARM64 asm properties -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the ARM64MCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "ARM64MCAsmInfo.h"
using namespace llvm;

ARM64MCAsmInfo::ARM64MCAsmInfo() {
  PrivateGlobalPrefix = "L";
  PCSymbol=".";
  SeparatorString = "%%";
  CommentString = ";";
  PointerSize = CalleeSaveStackSlotSize = 8;

  AlignmentIsInBytes = false;
  AllowNameToStartWithDigit = true;
  UsesELFSectionDirectiveForBSS = true;
  SupportsDebugInformation = true;
  UseDataRegionDirectives = true;

  ExceptionsType = ExceptionHandling::DwarfCFI;
}

const MCExpr *
ARM64MCAsmInfo::getExprForPersonalitySymbol(const MCSymbol *Sym,
                                            unsigned Encoding,
                                            MCStreamer &Streamer) const {
  // On Darwin, we can reference dwarf symbols with foo@GOT-., which
  // is an indirect pc-relative reference. The default implementation
  // won't reference using the GOT, so we need this target-specific
  // version.
  MCContext &Context = Streamer.getContext();
  const MCExpr *Res =
    MCSymbolRefExpr::Create(Sym, MCSymbolRefExpr::VK_GOT, Context);
  MCSymbol *PCSym = Context.CreateTempSymbol();
  Streamer.EmitLabel(PCSym);
  const MCExpr *PC = MCSymbolRefExpr::Create(PCSym, Context);
  return MCBinaryExpr::CreateSub(Res, PC, Context);
}
