# Because compiler-rt spends a lot of time setting up custom compile flags,
# define a handy helper function for it. The compile flags setting in CMake
# has serious issues that make its syntax challenging at best.
function(set_target_compile_flags target)
  set(argstring "")
  foreach(arg ${ARGN})
    set(argstring "${argstring} ${arg}")
  endforeach()
  set_property(TARGET ${target} PROPERTY COMPILE_FLAGS "${argstring}")
endfunction()

function(set_target_link_flags target)
  set(argstring "")
  foreach(arg ${ARGN})
    set(argstring "${argstring} ${arg}")
  endforeach()
  set_property(TARGET ${target} PROPERTY LINK_FLAGS "${argstring}")
endfunction()

# Set the variable var_PYBOOL to True if var holds a true-ish string,
# otherwise set it to False.
macro(pythonize_bool var)
  if (${var})
    set(${var}_PYBOOL True)
  else()
    set(${var}_PYBOOL False)
  endif()
endmacro()

# Appends value to all lists in ARGN, if the condition is true.
macro(append_list_if condition value)
  if(${condition})
    foreach(list ${ARGN})
      list(APPEND ${list} ${value})
    endforeach()
  endif()
endmacro()

# Appends value to all strings in ARGN, if the condition is true.
macro(append_string_if condition value)
  if(${condition})
    foreach(str ${ARGN})
      set(${str} "${${str}} ${value}")
    endforeach()
  endif()
endmacro()

macro(append_no_rtti_flag list)
  append_list_if(COMPILER_RT_HAS_FNO_RTTI_FLAG -fno-rtti ${list})
  append_list_if(COMPILER_RT_HAS_GR_FLAG /GR- ${list})
endmacro()

function(TO_LIST_SPACES _LIST_NAME OUTPUT_VAR)
  set(NEW_LIST_SPACE)
  foreach(ITEM ${${_LIST_NAME}})
    set(NEW_LIST_SPACE "${NEW_LIST_SPACE} ${ITEM}")
  endforeach()
  string(STRIP ${NEW_LIST_SPACE} NEW_LIST_SPACE)
  set(${OUTPUT_VAR} "${NEW_LIST_SPACE}" PARENT_SCOPE)
endfunction()

macro(darwin_drop_unsupported_arches os)
  set(SIMPLE_SOURCE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/simple.cc)
  file(WRITE ${SIMPLE_SOURCE} "#include <stdlib.h>\n#include <limits>\n#include <stdint.h>\n#include <stdio.h>\nint main() {}\n")
  set(WORKING_ARCHES)
  foreach(arch ${SANITIZER_COMMON_DARWIN_${os}_ARCHES})
    set(arch_linker_flags "-arch ${arch}" "${DARWIN_${os}_LINKFLAGS}")
    TO_LIST_SPACES(arch_linker_flags arch_linker_flags)
    try_compile(CAN_TARGET_${arch} ${CMAKE_BINARY_DIR} ${SIMPLE_SOURCE}
                COMPILE_DEFINITIONS "${TARGET_${arch}_CFLAGS} -arch ${arch}" ${DARWIN_${os}_CFLAGS}
                OUTPUT_VARIABLE TARGET_${arch}_OUTPUT
                CMAKE_FLAGS "-DCMAKE_EXE_LINKER_FLAGS=${arch_linker_flags}")
    if(${CAN_TARGET_${arch}})
      list(APPEND WORKING_ARCHES ${arch})
    else()
      message("dropping ${arch} from ${os}...")
    endif()
  endforeach()
  set(SANITIZER_COMMON_DARWIN_${os}_ARCHES ${WORKING_ARCHES})
endmacro()
