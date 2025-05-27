cmake_minimum_required(VERSION 3.10)
project(tsanpr_jni LANGUAGES CXX)

find_package(JNI REQUIRED)

set(SOURCES
    src/native/tsanpr.cpp
    src/native/TSANPR_jni.cpp
)

include_directories(
    ${JNI_INCLUDE_DIRS}
    src/native
)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

function(add_tsanpr_jni_target output_dir)
    add_library(tsanpr_jni SHARED ${SOURCES})
    set_target_properties(tsanpr_jni PROPERTIES
        OUTPUT_NAME "tsanpr_jni"
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${output_dir}"
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${output_dir}"
        ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/${output_dir}"
    )
    if(WIN32)
        set_target_properties(tsanpr_jni PROPERTIES SUFFIX ".dll" PREFIX "")
    else()
        set_target_properties(tsanpr_jni PROPERTIES SUFFIX ".so" PREFIX "lib")
    endif()
endfunction()

# Windows 32bit
if(WIN32 AND CMAKE_SIZEOF_VOID_P EQUAL 4)
    add_tsanpr_jni_target("bin/windows-x86/jni")
endif()

# Windows 64bit
if(WIN32 AND CMAKE_SIZEOF_VOID_P EQUAL 8)
    add_tsanpr_jni_target("bin/windows-x86_64/jni")
endif()

# Linux x86_64
if(UNIX AND NOT APPLE AND CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
    add_tsanpr_jni_target("bin/linux-x86_64/jni")
endif()

# Linux aarch64
if(UNIX AND NOT APPLE AND CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64")
    add_tsanpr_jni_target("bin/linux-aarch64/jni")
endif()
