set(LLVM_LINK_COMPONENTS support)

add_clang_library(clangTidyReadabilityModule
  BracesAroundStatementsCheck.cpp
  ContainerSizeEmpty.cpp
  ElseAfterReturnCheck.cpp
  FunctionSize.cpp
  NamespaceCommentCheck.cpp
  ReadabilityTidyModule.cpp
  RedundantSmartptrGet.cpp
  ShrinkToFitCheck.cpp

  LINK_LIBS
  clangAPINotes
  clangAST
  clangASTMatchers
  clangBasic
  clangLex
  clangTidy
  clangTooling
  )
