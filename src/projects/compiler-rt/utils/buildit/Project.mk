BUILD_COMPILER_RT = $(shell find $(SRCROOT) -name build_compiler_rt)

compiler-rt: $(OBJROOT) $(SYMROOT) $(DSTROOT)
	mkdir -p $(DSTROOT)/usr/local/ && \
	echo "This file is a placeholder for the compiler-rt build" > $(DSTROOT)/usr/local/compiler-rt.placeholder

compiler-rt-real: $(OBJROOT) $(SYMROOT) $(DSTROOT)
	cd $(OBJROOT) && \
		$(BUILD_COMPILER_RT) $(Clang_Use_Assertions) $(Clang_Use_Optimized)
