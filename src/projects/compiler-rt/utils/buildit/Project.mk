BUILD_COMPILER_RT = $(shell find $(SRCROOT) -name build_compiler_rt)

compiler-rt: $(OBJROOT) $(SYMROOT) $(DSTROOT)
	cd $(OBJROOT) && \
		$(BUILD_COMPILER_RT) $(Clang_Use_Assertions) $(Clang_Use_Optimized) $(Clang_Version)
