set(LLVM_LINK_COMPONENTS
  DebugInfoDWARF
  Object
  Support
  )

add_llvm_tool(llvm-dbgcompare
  dbgcompare.cpp
  ../dsymutil/BinaryHolder.cpp
  )
