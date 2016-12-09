//===--- IndexDataStoreUtils.cpp - Functions/constants for the data store -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "IndexDataStoreUtils.h"
#include "llvm/Bitcode/BitstreamWriter.h"

using namespace clang;
using namespace clang::index;
using namespace clang::index::store;
using namespace llvm;

void store::emitBlockID(unsigned ID, const char *Name,
                        BitstreamWriter &Stream, RecordDataImpl &Record) {
  Record.clear();
  Record.push_back(ID);
  Stream.EmitRecord(bitc::BLOCKINFO_CODE_SETBID, Record);

  // Emit the block name if present.
  if (!Name || Name[0] == 0)
    return;
  Record.clear();
  while (*Name)
    Record.push_back(*Name++);
  Stream.EmitRecord(bitc::BLOCKINFO_CODE_BLOCKNAME, Record);
}

void store::emitRecordID(unsigned ID, const char *Name,
                         BitstreamWriter &Stream,
                         RecordDataImpl &Record) {
  Record.clear();
  Record.push_back(ID);
  while (*Name)
    Record.push_back(*Name++);
  Stream.EmitRecord(bitc::BLOCKINFO_CODE_SETRECORDNAME, Record);
}
