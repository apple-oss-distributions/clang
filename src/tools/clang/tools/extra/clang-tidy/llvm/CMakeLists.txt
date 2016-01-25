set(LLVM_LINK_COMPONENTS support)

add_clang_library(clangTidyLLVMModule
  HeaderGuardCheck.cpp
  IncludeOrderCheck.cpp
  LLVMTidyModule.cpp
  TwineLocalCheck.cpp

  LINK_LIBS
  clangAPINotes
  clangAST
  clangASTMatchers
  clangBasic
  clangLex
  clangTidy
  clangTidyReadabilityModule
  clangTidyUtils
  clangTooling
  )
