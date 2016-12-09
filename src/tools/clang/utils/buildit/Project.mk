BUILD_CLANG = $(shell find $(SRCROOT) -name build_clang)

clang: $(OBJROOT) $(SYMROOT) $(DSTROOT)
	cd $(OBJROOT) && \
		export LLVM_REPOSITORY=$(LLVM_REPOSITORY) && \
		export SVN_REVISION=$(SVN_REVISION) && \
		$(BUILD_CLANG) $(Clang_Use_Assertions) $(Clang_Use_Optimized) $(Clang_Version) $(Clang_Enable_LTO)
