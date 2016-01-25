set(LLVM_LINK_COMPONENTS
  Option
  Support
  )

add_clang_executable(clang-check
  ClangCheck.cpp
  )

target_link_libraries(clang-check
  clangAPINotes
  clangAST
  clangBasic
  clangDriver
  clangCodeGen
  clangFrontend
  clangRewriteFrontend
  clangStaticAnalyzerFrontend
  clangTooling
  )

install(TARGETS clang-check
  RUNTIME DESTINATION bin)
