set(LLVM_LINK_COMPONENTS
  support
)

add_clang_library(clangServiceCore
  C-API.cpp
  Service.cpp
)
