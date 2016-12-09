macro(create_framework_dylib)
  cmake_parse_arguments(DYLIB
    ""
    "PLIST;FMWK"
    "LLVM_COMPONENTS;CLANG_COMPONENTS"
    ${ARGN}
  )
  set(name ${DYLIB_FMWK})
  set(plist ${DYLIB_PLIST})
  set(llvmcom ${DYLIB_LLVM_COMPONENTS})
  set(clangcom ${DYLIB_CLANG_COMPONENTS})

  set(fmwk_dir "${LLVM_LIBRARY_DIR}/${name}.framework")
  set(fmwk_A "${fmwk_dir}/Versions/A")

  add_library(${name} SHARED "${plist}")
  target_link_libraries(${name} ${llvmcom})
  target_link_libraries(${name} ${clangcom})
  target_link_libraries(${name} ${LLVM_COMMON_LIBS})
  set_target_properties(${name} PROPERTIES FRAMEWORK TRUE)
  set_target_properties(${name} PROPERTIES LINKER_LANGUAGE CXX)
  set_target_properties(${name} PROPERTIES ARCHIVE_OUTPUT_DIRECTORY "${fmwk_A}")
  set_target_properties(${name} PROPERTIES INSTALL_RPATH "${LLVM_LIBRARY_DIR}")
  if(LLVM_COMMON_DEPENDS)
    add_dependencies(${name} ${LLVM_COMMON_DEPENDS})
  endif(LLVM_COMMON_DEPENDS)

  add_custom_command(
    TARGET ${name}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${plist}"
            "${fmwk_A}/Resources/Info.plist"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
endmacro(create_framework_dylib)

macro(create_framework_service)
  cmake_parse_arguments(XPC
    ""
    "NAME;PLIST;FMWK"
    "SOURCES;LLVM_COMPONENTS;CLANG_COMPONENTS"
    ${ARGN}
  )
  set(name ${XPC_NAME})
  set(plist ${XPC_PLIST})
  set(fmwk ${XPC_FMWK})
  set(srcs ${XPC_SOURCES})
  set(llvmcom ${XPC_LLVM_COMPONENTS})
  set(clangcom ${XPC_CLANG_COMPONENTS})

  set(fmwk_dir ${LLVM_LIBRARY_DIR}/${fmwk}.framework)
  set(fmwk_A ${fmwk_dir}/Versions/A)
  set(xpc_contents_dir "${fmwk_A}/XPCServices/${name}.xpc/Contents")
  set(xpc_bin_dir "${xpc_contents_dir}/MacOS")

  list(APPEND srcs "${plist}")

  set(LLVM_LINK_COMPONENTS ${llvmcom})
  add_clang_executable(${name} ${srcs})
  target_link_libraries(${name} ${clangcom})
  target_link_libraries(${name} ${LLVM_COMMON_LIBS})
  target_link_libraries(${name} ${fmwk})
  set_target_properties(${name} PROPERTIES LINKER_LANGUAGE CXX)
  set_target_properties(${name} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${xpc_bin_dir}")
  set_target_properties(${name} PROPERTIES INSTALL_RPATH "${LLVM_LIBRARY_DIR}")
  if(LLVM_COMMON_DEPENDS)
    add_dependencies(${name} ${LLVM_COMMON_DEPENDS})
  endif(LLVM_COMMON_DEPENDS)

  add_custom_command(
    TARGET ${name}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${plist}"
            "${xpc_contents_dir}/Info.plist"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )

  add_custom_command(
    TARGET ${name}
    POST_BUILD

    # FIXME: Install the clang-service headers into the framework. Then we can
    # avoid awkwardly creating the headers directory here.
    COMMAND ${CMAKE_COMMAND} -E make_directory ${fmwk_A}/Headers

    COMMAND ${CMAKE_COMMAND} -E create_symlink
            ${fmwk_A}
            ${fmwk_dir}/Versions/Current
    COMMAND ${CMAKE_COMMAND} -E create_symlink
            ${fmwk_A}/Headers
            ${fmwk_dir}/Headers
    COMMAND ${CMAKE_COMMAND} -E create_symlink
            ${fmwk_A}/Resources
            ${fmwk_dir}/Resources
    COMMAND ${CMAKE_COMMAND} -E create_symlink
            ${fmwk_A}/XPCServices
            ${fmwk_dir}/XPCServices
  )
endmacro(create_framework_service)

add_clang_subdirectory(lib)
add_clang_subdirectory(tools)
if(LLVM_INCLUDE_TESTS)
  add_subdirectory(unittests)
endif()
