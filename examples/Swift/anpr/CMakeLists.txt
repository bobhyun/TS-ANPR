cmake_minimum_required(VERSION 3.10)
project(CBridge LANGUAGES C CXX)

# Platform/arch detection
if(WIN32)
    set(PLATFORM_NAME "x86_64-unknown-windows-msvc")
elseif(UNIX AND CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
    set(PLATFORM_NAME "x86_64-unknown-linux-gnu")
elseif(UNIX AND (CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "arm64"))
    set(PLATFORM_NAME "aarch64-unknown-linux-gnu")
else()
    message(FATAL_ERROR "Unsupported platform/architecture: ${CMAKE_SYSTEM_NAME} / ${CMAKE_SYSTEM_PROCESSOR}")
endif()

set(SWIFT_EXE_OUTPUT_BASE "${CMAKE_SOURCE_DIR}/.build/${PLATFORM_NAME}")

# OpenCV
find_package(OpenCV REQUIRED)

set(CBRIDGE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/CBridge")

set(SOURCES
    ${CBRIDGE_DIR}/CBridge.cpp
    ${CBRIDGE_DIR}/tsanpr.cpp
)
set(HEADERS
    ${CBRIDGE_DIR}/CBridge.h
    ${CBRIDGE_DIR}/tsanpr.h
)

add_library(CBridge SHARED ${SOURCES} ${HEADERS})

target_include_directories(CBridge
    PRIVATE
        ${OpenCV_INCLUDE_DIRS}
        ${CBRIDGE_DIR}
)

target_link_libraries(CBridge
    PRIVATE
        ${OpenCV_LIBS}
)

if (WIN32)
    target_compile_definitions(CBridge PRIVATE CBRIDGE_EXPORTS)
endif()

target_compile_features(CBridge PRIVATE cxx_std_17)

set_target_properties(CBridge PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY_DEBUG   "${SWIFT_EXE_OUTPUT_BASE}/debug"
    LIBRARY_OUTPUT_DIRECTORY_DEBUG   "${SWIFT_EXE_OUTPUT_BASE}/debug"
    RUNTIME_OUTPUT_DIRECTORY_DEBUG   "${SWIFT_EXE_OUTPUT_BASE}/debug"
    ARCHIVE_OUTPUT_DIRECTORY_RELEASE "${SWIFT_EXE_OUTPUT_BASE}/release"
    LIBRARY_OUTPUT_DIRECTORY_RELEASE "${SWIFT_EXE_OUTPUT_BASE}/release"
    RUNTIME_OUTPUT_DIRECTORY_RELEASE "${SWIFT_EXE_OUTPUT_BASE}/release"
)

# Optionally install headers and modulemap for SwiftPM
install(FILES
    ${CBRIDGE_DIR}/CBridge.h
    ${CBRIDGE_DIR}/module.modulemap
    DESTINATION include
)
