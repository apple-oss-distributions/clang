set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_unittest(ASTMatchersTests
  ASTMatchersTest.cpp)

target_link_libraries(ASTMatchersTests
  clangAST
  clangASTMatchers
  clangBasic
  clangCodeGen
  clangFrontend
  clangTooling
  )

add_subdirectory(Dynamic)
