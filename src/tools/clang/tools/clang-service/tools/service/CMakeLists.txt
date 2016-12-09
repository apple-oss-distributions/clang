create_framework_service(
  NAME
  ClangXPC

  PLIST
  Info.plist

  FMWK
  ClangService

  SOURCES
  service.cpp

  LLVM_COMPONENTS
  support

  CLANG_COMPONENTS
  libclang
  clangAST
  clangBasic
  clangFrontend
  clangIndex
)
