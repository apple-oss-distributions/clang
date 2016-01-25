##===- tools/dsymutil/Makefile -----------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##

LEVEL := ../..
TOOLNAME := llvm-dsymutil
LINK_COMPONENTS := all-targets AsmPrinter DebugInfoDWARF MC Object Support

SOURCES := BinaryHolder.cpp DebugMap.cpp DwarfLinker.cpp \
					 MachODebugMapParser.cpp MachOUtils.cpp dsymutil.cpp

include $(LEVEL)/Makefile.config

ifeq ($(HOST_OS),Darwin)
	SOURCES += CFBundle.cpp CFString.cpp CFStringUtils.c
	LDFLAGS += -framework CoreFoundation
endif

# This tool has no plugins, optimize startup time.
TOOL_NO_EXPORTS := 1

include $(LEVEL)/Makefile.common
