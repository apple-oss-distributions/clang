##===- clang-format/Makefile -------------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##

CLANG_LEVEL := ../..

TOOLNAME = clang-format

# Apple Internal: If a separate install prefix was specified for internal
# tools, use it when installing clang-format.
INTERNAL_TOOL = 1

# No plugins, optimize startup time.
TOOL_NO_EXPORTS = 1

include $(CLANG_LEVEL)/../../Makefile.config
LINK_COMPONENTS := $(TARGETS_TO_BUILD) asmparser bitreader support mc option
USEDLIBS = clangFormat.a clangToolingCore.a clangDriver.a clangRewrite.a \
           clangLex.a clangAPINotes.a clangBasic.a 

include $(CLANG_LEVEL)/Makefile
