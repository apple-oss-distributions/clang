##
# Top-level Clang B&I Makefile
##

# LLVM/Clang default configuration.

# Select default assertion mode, valid values are '0' and '1'.
Clang_Use_Assertions   := 0
# Select clang driver mode, valid values are 'Production' and 'Development'.
Clang_Driver_Mode      := Production
# Select optimization flags to use.
Clang_Optimize_Option  := -O2
# Additional arbitrary compiler options.
Clang_Extra_Options    := -DDISABLE_SMART_POINTERS
# Additional arbitrary compiler options, only passed to final configure stage.
Clang_Final_Extra_Options := -g
# Enable use of clang++?
Clang_Enable_CXX       := 1
# Build all LLVM tools; not just clang?
Clang_Build_All        := 0
# Set the Clang version.
Clang_Version          := 2.0
# Enable bootstrap.
Clang_Enable_Bootstrap := 1

##
# Source Info

# The name used to identify this "train".
Train_Name := whitney
# Can be one of "trunk", "branch", or "branch-llvm-only".
Source_To_Draw_From := branch

##
# Include build logic.
include ClangBNI.mk
