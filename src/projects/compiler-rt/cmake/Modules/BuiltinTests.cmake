
# This function takes an OS and a list of architectures and identifies the
# subset of the architectures list that the installed toolchain can target.
function(try_compile_only output)
  set(SIMPLE_C ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/src.c)
  file(WRITE ${SIMPLE_C} "int foo(int x, int y) { return x + y; }\n")

  execute_process(
    COMMAND ${CMAKE_C_COMPILER} ${CMAKE_C_FLAGS} ${ARGN} -c ${SIMPLE_C}
    RESULT_VARIABLE result
    OUTPUT_VARIABLE TEST_OUTPUT
    ERROR_VARIABLE TEST_ERROR
  )
  if(result EQUAL 0)
    set(${output} True PARENT_SCOPE)
  else()
    file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
        "Testing compiler for supporting ${ARGN}:\n"
        "${TEST_OUTPUT}\n${TEST_ERROR}\n")
    set(${output} False PARENT_SCOPE)
  endif()
endfunction()

function(builtin_check_c_compiler_flag flag output)
  message(STATUS "Performing Test ${output}")
  try_compile_only(result ${flag})
  set(${output} ${result} PARENT_SCOPE)
  if(${result})
    message(STATUS "Performing Test ${output} - Success")
  else()
    message(STATUS "Performing Test ${output} - Failed")
  endif()
endfunction()
