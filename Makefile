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
Clang_Linker_Options  := -fno-pie
# Additional arbitrary compiler options.
Clang_Extra_Options    := \
	-DDISABLE_DEFAULT_STRICT_ALIASING
# Additional arbitrary compiler options, only passed to final configure stage.
Clang_Final_Extra_Options := -g
# Build all LLVM tools; not just clang?
Clang_Build_All        := 0
# Set the Clang version.
Clang_Version          := 4.0
# Enable bootstrap.
Clang_Enable_Bootstrap := 1
# Enable automatic order file generation, when using buildit.
ifeq ($(RC_BUILDIT),YES)
Clang_Autogenerate_Order_File := 1
else
Clang_Autogenerate_Order_File := 0
endif

##
# Include build logic.
include ClangBNI.mk
