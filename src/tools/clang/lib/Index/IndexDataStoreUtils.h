//===--- IndexDataStoreUtils.h - Functions/constants for the data store ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_INDEX_INDEXDATASTOREUTILS_H
#define LLVM_CLANG_LIB_INDEX_INDEXDATASTOREUTILS_H

#include "llvm/Bitcode/BitCodes.h"
#include "clang/Basic/LLVM.h"

namespace llvm {
  class BitstreamWriter;
}

namespace clang {
namespace index {
namespace store {

static const unsigned STORE_FORMAT_VERSION = 2;

void makeUnitSubDir(SmallVectorImpl<char> &StorePathBuf);
void makeRecordSubDir(SmallVectorImpl<char> &StorePathBuf);

enum RecordBitRecord {
  REC_VERSION,
  REC_DECLINFO,
  REC_DECLOFFSETS,
  REC_DECLOCCURRENCE,
};

enum RecordBitBlock {
  REC_VERSION_BLOCK_ID = llvm::bitc::FIRST_APPLICATION_BLOCKID,
  REC_DECLS_BLOCK_ID,
  REC_DECLOFFSETS_BLOCK_ID,
  REC_DECLOCCURRENCES_BLOCK_ID,
};

enum UnitBitRecord {
  UNIT_VERSION,
  UNIT_INFO,
  UNIT_PATHS,
  UNIT_DEPENDENCY,
};

enum UnitBitBlock {
  UNIT_VERSION_BLOCK_ID = llvm::bitc::FIRST_APPLICATION_BLOCKID,
  UNIT_INFO_BLOCK_ID,
  UNIT_DEPENDENCIES_BLOCK_ID,
  UNIT_PATHS_BLOCK_ID,
};

enum UnitDependencyKind {
  UNIT_DEPEND_KIND_FILE = 0,
  UNIT_DEPEND_KIND_RECORD = 1,
  UNIT_DEPEND_KIND_UNIT = 2,
};
static const unsigned UnitDependencyKindBitNum = 2;

enum UnitFilePathPrefixKind {
  UNIT_PATH_PREFIX_NONE = 0,
  UNIT_PATH_PREFIX_WORKDIR = 1,
  UNIT_PATH_PREFIX_SYSROOT = 2,
};
static const unsigned UnitFilePathPrefixKindBitNum = 2;

typedef SmallVector<uint64_t, 64> RecordData;
typedef SmallVectorImpl<uint64_t> RecordDataImpl;

void emitBlockID(unsigned ID, const char *Name,
                 llvm::BitstreamWriter &Stream, RecordDataImpl &Record);

void emitRecordID(unsigned ID, const char *Name,
                  llvm::BitstreamWriter &Stream, RecordDataImpl &Record);

} // end namespace store
} // end namespace index
} // end namespace clang

#endif
