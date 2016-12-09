set(LLVM_LINK_COMPONENTS
  support
)

add_clang_library(clangServiceSupport
  UID.cpp
  Value.cpp
)
