set(LLVM_LINK_COMPONENTS
  support
)

add_clang_library(clangServiceXPC
  Client.cpp
  Service.cpp
  XPC.cpp
)
