if(DEFINED ENV{USER_CMAKE_VERSION})
    message(STATUS "Required minimum cmake version: $ENV{USER_CMAKE_VERSION}")
    cmake_minimum_required(VERSION $ENV{USER_CMAKE_VERSION})
endif()
message(STATUS "Actual cmake version: ${CMAKE_VERSION}")

project(conc_usage)

include(${CMAKE_CURRENT_LIST_DIR}/../cmake/get_cpm.cmake)
cpmaddpackage(NAME async SOURCE_DIR "${CMAKE_CURRENT_LIST_DIR}/.." GIT_TAG HEAD)

add_executable(app main.cpp)
target_link_libraries(app PRIVATE async)
if(CPP_IMPLEMENTATION STREQUAL "FREESTANDING")
    target_compile_definitions(
        app PRIVATE SIMULATE_FREESTANDING STDX_FMT_FREESTANDING
                    ASYNC_FREESTANDING)
    target_compile_options(app PRIVATE -ffreestanding)
endif()
