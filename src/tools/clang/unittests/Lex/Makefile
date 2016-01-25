##===- unittests/Lex/Makefile ------------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##

CLANG_LEVEL = ../..
TESTNAME = Lex
include $(CLANG_LEVEL)/../../Makefile.config
LINK_COMPONENTS := $(TARGETS_TO_BUILD) asmparser bitreader ipo objcarcopts \
                   instrumentation bitwriter support mc option DebugInfoDWARF
USEDLIBS = clangParse.a clangSema.a clangAnalysis.a clangEdit.a \
	clangSerialization.a clangAST.a clangLex.a clangAPINotes.a clangBasic.a \
        clangFrontend.a clangCodeGen.a clangDriver.a clangIndex.a 

include $(CLANG_LEVEL)/unittests/Makefile
