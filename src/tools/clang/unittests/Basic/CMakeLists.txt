set(LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Support
  )

add_clang_unittest(BasicTests
  CharInfoTest.cpp
  DiagnosticTest.cpp
  FileManagerTest.cpp
  SourceManagerTest.cpp
  VirtualFileSystemTest.cpp
  )

target_link_libraries(BasicTests
  clangBasic
  clangCodeGen
  clangLex
  )
