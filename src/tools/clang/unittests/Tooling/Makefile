##===- unittests/Tooling/Makefile --------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##

CLANG_LEVEL = ../..
TESTNAME = Tooling
include $(CLANG_LEVEL)/../../Makefile.config
LINK_COMPONENTS := $(TARGETS_TO_BUILD) asmparser bitreader ipo objcarcopts \
                   instrumentation bitwriter support mc option DebugInfoDWARF
USEDLIBS = clangTooling.a clangToolingCore.a clangFrontend.a \
	   clangCodeGen.a clangIndex.a clangSerialization.a clangDriver.a \
           clangParse.a clangRewrite.a clangRewriteFrontend.a \
	   clangSema.a clangAnalysis.a clangEdit.a \
           clangAST.a clangASTMatchers.a clangLex.a clangAPINotes.a clangBasic.a

include $(CLANG_LEVEL)/unittests/Makefile
