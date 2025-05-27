cmake_minimum_required(VERSION 3.10)
project(tsanpr_c_example LANGUAGES C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

file(GLOB SRC_FILES *.c src/*.c)

include_directories(${PROJECT_SOURCE_DIR}/src)

# Find libpng and libjpeg
find_package(PNG REQUIRED)
find_package(JPEG REQUIRED)

set(TARGET_NAME anpr)

if(WIN32)
    if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
        # MinGW (MSYS2)
        if(CMAKE_SIZEOF_VOID_P EQUAL 8)
            set(OS_ARCH windows-x86_64-mingw)
        else()
            set(OS_ARCH windows-x86-mingw)
        endif()
    elseif(MSVC)
        if(CMAKE_SIZEOF_VOID_P EQUAL 8)
            set(OS_ARCH windows-x86_64)
        else()
            set(OS_ARCH windows-x86)
        endif()
    else()
        set(OS_ARCH windows-unknown)
    endif()
elseif(UNIX AND NOT APPLE)
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64")
        set(OS_ARCH linux-aarch64)
    else()
        set(OS_ARCH linux-x86_64)
    endif()
else()
    set(OS_ARCH unknown)
endif()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/${OS_ARCH})

add_executable(${TARGET_NAME} ${SRC_FILES})

target_link_libraries(${TARGET_NAME} PRIVATE PNG::PNG JPEG::JPEG)

if(UNIX AND NOT APPLE)
    target_link_libraries(${TARGET_NAME} PRIVATE dl)
endif()

message(STATUS "Compiler: ${CMAKE_C_COMPILER_ID}")
message(STATUS "Build target: ${OS_ARCH}")
message(STATUS "Executable output directory: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
