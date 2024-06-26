cmake_minimum_required(VERSION 3.25)

project(
    baremetal-async
    VERSION 0.1.0
    LANGUAGES CXX
    DESCRIPTION
        "A header-only C++ library that implements low-level async senders and receivers framework."
)

include(cmake/get_cpm.cmake)
if(PROJECT_IS_TOP_LEVEL)
    cpmaddpackage("gh:intel/cicd-repo-infrastructure#dev")
else()
    cpmaddpackage("gh:intel/cicd-repo-infrastructure#3e2bef0")
endif()

add_versioned_package("gh:intel/cpp-std-extensions#6fe6ae9")
add_versioned_package("gh:intel/cpp-baremetal-concurrency#659771e")
add_versioned_package("gh:boostorg/mp11#boost-1.83.0")

add_library(async INTERFACE)
target_compile_features(async INTERFACE cxx_std_20)
target_include_directories(async INTERFACE include)
target_link_libraries_system(async INTERFACE boost_mp11 concurrency stdx)

if(PROJECT_IS_TOP_LEVEL)
    add_docs(docs)
    add_subdirectory(test)
    clang_tidy_interface(async)
endif()
