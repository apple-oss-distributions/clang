set(LLVM_LINK_COMPONENTS support)

add_clang_library(clangTidyUtils
  HeaderGuard.cpp

  LINK_LIBS
  clangAPINotes
  clangAST
  clangASTMatchers
  clangBasic
  clangLex
  clangTidy
  )
