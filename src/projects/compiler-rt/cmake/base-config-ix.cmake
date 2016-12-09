# The CompilerRT build system requires CMake version 2.8.8 or higher in order
# to use its support for building convenience "libraries" as a collection of
# .o files. This is particularly useful in producing larger, more complex
# runtime libraries.

include(CheckIncludeFile)
check_include_file(unwind.h HAVE_UNWIND_H)

# Top level target used to build all compiler-rt libraries.
add_custom_target(compiler-rt ALL)

if (NOT COMPILER_RT_STANDALONE_BUILD)
  # Compute the Clang version from the LLVM version.
  # FIXME: We should be able to reuse CLANG_VERSION variable calculated
  #        in Clang cmake files, instead of copying the rules here.
  string(REGEX MATCH "[0-9]+\\.[0-9]+(\\.[0-9]+)?" CLANG_VERSION
         ${PACKAGE_VERSION})
  # Setup the paths where compiler-rt runtimes and headers should be stored.
  set(COMPILER_RT_OUTPUT_DIR ${LLVM_LIBRARY_OUTPUT_INTDIR}/clang/${CLANG_VERSION})
  set(COMPILER_RT_EXEC_OUTPUT_DIR ${LLVM_RUNTIME_OUTPUT_INTDIR})
  set(COMPILER_RT_INSTALL_PATH lib${LLVM_LIBDIR_SUFFIX}/clang/${CLANG_VERSION})
  option(COMPILER_RT_INCLUDE_TESTS "Generate and build compiler-rt unit tests."
         ${LLVM_INCLUDE_TESTS})
 option(COMPILER_RT_ENABLE_WERROR "Fail and stop if warning is triggered"
        ${LLVM_ENABLE_WERROR})
  # Use just-built Clang to compile/link tests on all platforms, except for
  # Windows where we need to use clang-cl instead.
  if(NOT MSVC)
    set(COMPILER_RT_TEST_COMPILER ${LLVM_RUNTIME_OUTPUT_INTDIR}/clang)
  else()
    set(COMPILER_RT_TEST_COMPILER ${LLVM_RUNTIME_OUTPUT_INTDIR}/clang.exe)
  endif()
else()
    # Take output dir and install path from the user.
  set(COMPILER_RT_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR} CACHE PATH
    "Path where built compiler-rt libraries should be stored.")
  set(COMPILER_RT_EXEC_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/bin CACHE PATH
    "Path where built compiler-rt executables should be stored.")
  set(COMPILER_RT_INSTALL_PATH ${CMAKE_INSTALL_PREFIX} CACHE PATH
    "Path where built compiler-rt libraries should be installed.")
  option(COMPILER_RT_INCLUDE_TESTS "Generate and build compiler-rt unit tests." OFF)
  option(COMPILER_RT_ENABLE_WERROR "Fail and stop if warning is triggered" OFF)
  # Use a host compiler to compile/link tests.
  set(COMPILER_RT_TEST_COMPILER ${CMAKE_C_COMPILER} CACHE PATH "Compiler to use for testing")
endif()

if("${COMPILER_RT_TEST_COMPILER}" MATCHES "clang[+]*$")
  set(COMPILER_RT_TEST_COMPILER_ID Clang)
elseif("${COMPILER_RT_TEST_COMPILER}" MATCHES "clang.*.exe$")
  set(COMPILER_RT_TEST_COMPILER_ID Clang)
else()
  set(COMPILER_RT_TEST_COMPILER_ID GNU)
endif()

if ("${COMPILER_RT_DEFAULT_TARGET_ABI}" STREQUAL "androideabi")
  set(ANDROID 1)
endif()

string(TOLOWER ${CMAKE_SYSTEM_NAME} COMPILER_RT_OS_DIR)
set(COMPILER_RT_LIBRARY_OUTPUT_DIR
  ${COMPILER_RT_OUTPUT_DIR}/lib/${COMPILER_RT_OS_DIR})
set(COMPILER_RT_LIBRARY_INSTALL_DIR
  ${COMPILER_RT_INSTALL_PATH}/lib/${COMPILER_RT_OS_DIR})

if(APPLE)
  option(COMPILER_RT_ENABLE_IOS "Enable building for iOS - Experimental" On)
  option(COMPILER_RT_ENABLE_WATCHOS "Enable building for watchOS - Experimental" On)
  option(COMPILER_RT_ENABLE_TVOS "Enable building for tvOS - Experimental" On)
endif()

# test_target_arch(<arch> <def> <target flags...>)
# Checks if architecture is supported: runs host compiler with provided
# flags to verify that:
#   1) <def> is defined (if non-empty)
#   2) simple file can be successfully built.
# If successful, saves target flags for this architecture.
macro(test_target_arch arch def)
  set(TARGET_${arch}_CFLAGS ${ARGN})
  set(argstring "")
  foreach(arg ${ARGN})
    set(argstring "${argstring} ${arg}")
  endforeach()
  check_compile_definition("${def}" "${argstring}" HAS_${arch}_DEF)
  if(NOT HAS_${arch}_DEF)
    set(CAN_TARGET_${arch} FALSE)
  elseif(TEST_COMPILE_ONLY)
    set(argstring "${CMAKE_EXE_LINKER_FLAGS} ${argstring}")
    try_compile_only(CAN_TARGET_${arch} ${TARGET_${arch}_CFLAGS})
  else()
    set(argstring "${CMAKE_EXE_LINKER_FLAGS} ${argstring}")
    try_compile(CAN_TARGET_${arch} ${CMAKE_BINARY_DIR} ${SIMPLE_SOURCE}
                COMPILE_DEFINITIONS "${TARGET_${arch}_CFLAGS}"
                OUTPUT_VARIABLE TARGET_${arch}_OUTPUT
                CMAKE_FLAGS "-DCMAKE_EXE_LINKER_FLAGS:STRING=${argstring}")
  endif()
  if(${CAN_TARGET_${arch}})
    list(APPEND COMPILER_RT_SUPPORTED_ARCH ${arch})
  elseif("${COMPILER_RT_DEFAULT_TARGET_ARCH}" MATCHES "${arch}" AND
         COMPILER_RT_HAS_EXPLICIT_DEFAULT_TARGET_TRIPLE)
    # Bail out if we cannot target the architecture we plan to test.
    message(FATAL_ERROR "Cannot compile for ${arch}:\n${TARGET_${arch}_OUTPUT}")
  endif()
endmacro()

macro(test_targets)
  # Generate the COMPILER_RT_SUPPORTED_ARCH list.
  if(ANDROID)
    # Examine compiler output to determine target architecture.
    detect_target_arch()
    set(COMPILER_RT_OS_SUFFIX "-android")
  elseif(NOT APPLE) # Supported archs for Apple platforms are generated later
    if("${COMPILER_RT_DEFAULT_TARGET_ARCH}" MATCHES "i[2-6]86|x86|amd64")
      if(NOT MSVC)
        test_target_arch(x86_64 "" "-m64")
        # FIXME: We build runtimes for both i686 and i386, as "clang -m32" may
        # target different variant than "$CMAKE_C_COMPILER -m32". This part should
        # be gone after we resolve PR14109.
        test_target_arch(i686 __i686__ "-m32")
        test_target_arch(i386 __i386__ "-m32")
      else()
        if (CMAKE_SIZEOF_VOID_P EQUAL 4)
          test_target_arch(i386 "" "")
        else()
          test_target_arch(x86_64 "" "")
        endif()
      endif()
    elseif("${COMPILER_RT_DEFAULT_TARGET_ARCH}" MATCHES "powerpc")
      TEST_BIG_ENDIAN(HOST_IS_BIG_ENDIAN)
      if(HOST_IS_BIG_ENDIAN)
        test_target_arch(powerpc64 "" "-m64")
      else()
        test_target_arch(powerpc64le "" "-m64")
      endif()
    elseif("${COMPILER_RT_DEFAULT_TARGET_ARCH}" MATCHES "mipsel|mips64el")
      # Gcc doesn't accept -m32/-m64 so we do the next best thing and use
      # -mips32r2/-mips64r2. We don't use -mips1/-mips3 because we want to match
      # clang's default CPU's. In the 64-bit case, we must also specify the ABI
      # since the default ABI differs between gcc and clang.
      # FIXME: Ideally, we would build the N32 library too.
      test_target_arch(mipsel "" "-mips32r2" "--target=mipsel-linux-gnu")
      test_target_arch(mips64el "" "-mips64r2" "--target=mips64el-linux-gnu" "-mabi=n64")
    elseif("${COMPILER_RT_DEFAULT_TARGET_ARCH}" MATCHES "mips")
      test_target_arch(mips "" "-mips32r2" "--target=mips-linux-gnu")
      test_target_arch(mips64 "" "-mips64r2" "--target=mips64-linux-gnu" "-mabi=n64")
    elseif("${COMPILER_RT_DEFAULT_TARGET_ARCH}" MATCHES "arm")
      test_target_arch(arm "" "-march=armv7-a" "-mfloat-abi=soft" "-target=arm")
      test_target_arch(armhf "" "-march=armv7-a" "-mfloat-abi=hard" "-target=arm")
    elseif("${COMPILER_RT_DEFAULT_TARGET_ARCH}" MATCHES "aarch32")
      test_target_arch(aarch32 "" "-march=armv8-a" "-target" "arm")
    elseif("${COMPILER_RT_DEFAULT_TARGET_ARCH}" MATCHES "aarch64")
      test_target_arch(aarch64 "" "-march=armv8-a" "-target" "aarch64")
    elseif("${COMPILER_RT_DEFAULT_TARGET_ARCH}" MATCHES "wasm32")
      test_target_arch(wasm32 "" "--target=wasm32-unknown-unknown")
    elseif("${COMPILER_RT_DEFAULT_TARGET_ARCH}" MATCHES "wasm64")
      test_target_arch(wasm64 "" "--target=wasm64-unknown-unknown")
    endif()
    set(COMPILER_RT_OS_SUFFIX "")
  endif()
endmacro()
