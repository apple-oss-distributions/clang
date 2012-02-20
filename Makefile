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
Clang_Optimize_Option  := -O2
# Select linker options to use.
Clang_Linker_Options  := -fno-pie
# Additional arbitrary compiler options.
Clang_Extra_Options    := \
	-DDISABLE_DEFAULT_STRICT_ALIASING
# Additional arbitrary compiler options, only passed to final configure stage.
Clang_Final_Extra_Options := -g
# Build all LLVM tools; not just clang?
Clang_Build_All        := 0
# Set the Clang version.
Clang_Version          := 3.1
# Enable bootstrap.
Clang_Enable_Bootstrap := 1

##
# Source Info

# The name used to identify this "train".
Train_Name := palisade
# Can be one of "trunk", "branch", or "branch-llvm-only".
Source_To_Draw_From := branch

##
# Include build logic.
include ClangBNI.mk
