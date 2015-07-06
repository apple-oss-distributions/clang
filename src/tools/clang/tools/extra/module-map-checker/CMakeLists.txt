set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_executable(module-map-checker
  ModuleMapChecker.cpp
  )

target_link_libraries(module-map-checker
  clangAPINotes
  clangAST
  clangBasic
  clangFrontend
  clangLex
  clangTooling
  )
