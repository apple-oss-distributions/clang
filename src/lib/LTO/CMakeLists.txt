add_llvm_library(LLVMLTO
  LTOModule.cpp
  LTOCodeGenerator.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/LTO

  LINK_LIBS
  LLVMTransformUtils
  )

add_dependencies(LLVMLTO intrinsics_gen)

