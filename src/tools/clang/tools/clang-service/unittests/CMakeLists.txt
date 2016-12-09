add_custom_target(ClangServiceUnitTests)
set_target_properties(ClangServiceUnitTests PROPERTIES FOLDER "Clang Service tests")

function(add_clangservice_unittest test)
  add_unittest(ClangServiceUnitTests ${test} ${ARGN})
  set_target_properties(${test} PROPERTIES INSTALL_RPATH "${LLVM_LIBRARY_DIR}")
endfunction()

add_clangservice_unittest(ClangServiceTests
  C-API.cpp
  CodeCompletion.cpp
  Communication.cpp
  Indexing.cpp
  Serialization.cpp
)

target_link_libraries(ClangServiceTests
  ClangService
  libclang
)

add_dependencies(ClangServiceUnitTests ClangXPC)
