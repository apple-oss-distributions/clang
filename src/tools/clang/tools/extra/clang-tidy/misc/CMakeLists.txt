set(LLVM_LINK_COMPONENTS support)

add_clang_library(clangTidyMiscModule
  ArgumentCommentCheck.cpp
  BoolPointerImplicitConversion.cpp
  MiscTidyModule.cpp
  SwappedArgumentsCheck.cpp
  UndelegatedConstructor.cpp
  UnusedRAII.cpp
  UniqueptrResetRelease.cpp
  UseOverride.cpp

  LINK_LIBS
  clangAPINotes
  clangAST
  clangASTMatchers
  clangBasic
  clangLex
  clangTidy
  )
