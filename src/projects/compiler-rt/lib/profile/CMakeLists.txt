filter_available_targets(PROFILE_SUPPORTED_ARCH x86_64 i386)

if(APPLE)
  set(PROFILE_SOURCES
    GCDAProfiling.c
    InstrProfiling.c
    InstrProfilingBuffer.c
    InstrProfilingFile.c
    InstrProfilingPlatformDarwin.c
    InstrProfilingRuntime.cc)

  add_compiler_rt_osx_static_runtime(clang_rt.profile_osx
    ARCH ${PROFILE_SUPPORTED_ARCH}
    SOURCES ${PROFILE_SOURCES})
else()
  set(PROFILE_SOURCES
    GCDAProfiling.c
    InstrProfiling.c
    InstrProfilingBuffer.c
    InstrProfilingFile.c
    InstrProfilingPlatformOther.c
    InstrProfilingRuntime.cc)

  foreach(arch ${PROFILE_SUPPORTED_ARCH})
    add_compiler_rt_static_runtime(clang_rt.profile-${arch}
      ${arch}
      SOURCES ${PROFILE_SOURCES})
  endforeach()
endif()
