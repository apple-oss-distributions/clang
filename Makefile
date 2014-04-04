##
# Top-level Clang B&I Makefile
##

# LLVM/Clang default configuration.

# Select default assertion mode, valid values are '0' and '1'.
Clang_Use_Assertions   := 0
# Select default optimized mode, valid values are '0' and '1'.
Clang_Use_Optimized    := 1
# Select clang driver mode, valid values are 'Production' and 'Development'.
Clang_Driver_Mode      := Production
# Select optimization flags to use.
Clang_Optimize_Option  := -O2 -fno-stack-protector
# Select linker options to use.
ifneq ($(RC_ProjectName),clang_device)
Clang_Linker_Options   := -fno-pie
endif
# Additional arbitrary compiler options.
Clang_Extra_Options    :=
# Additional arbitrary compiler options, only passed to final configure stage.
ifeq ($(RC_ProjectName),clang)
Clang_Final_Extra_Options := -flto -gline-tables-only
else
Clang_Final_Extra_Options := -g
endif
# Build all LLVM tools; not just clang?
Clang_Build_All        := 0
# Set the Clang version.
Clang_Version          := 5.0
# Enable bootstrap.
Clang_Enable_Bootstrap := 1
# Enable automatic order file generation, when using buildit.
#
# FIXME: This is currently disabled due to problems with dtrace and LTO.
ifeq ($(RC_BUILDIT),YES)
Clang_Autogenerate_Order_File := 0
else
Clang_Autogenerate_Order_File := 0
endif
# Enable building clang tools.
Clang_Build_No_Tools   := YES
# Add an extra search path to use a custom libLTO. Currently only
# affects stage-2 builds.
Clang_libLTO_SearchPath := 

##
# Include build logic.
include ClangBNI.mk


