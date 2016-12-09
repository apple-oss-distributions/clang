add_clang_subdirectory(Core)
add_clang_subdirectory(Support)
add_clang_subdirectory(XPC)

create_framework_dylib(
  PLIST
  Info.plist

  FMWK
  ClangService

  CLANG_COMPONENTS
  clangServiceCore
  clangServiceSupport
  clangServiceXPC
)
