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

add_versioned_package("gh:intel/cpp-baremetal-concurrency#cea9d67")
add_versioned_package("gh:intel/cpp-std-extensions#48b3f4c")
add_versioned_package("gh:boostorg/mp11#boost-1.83.0")

add_library(async INTERFACE)
target_compile_features(async INTERFACE cxx_std_20)
target_link_libraries_system(async INTERFACE boost_mp11 concurrency stdx)

target_sources(
    async
    INTERFACE FILE_SET
              async
              TYPE
              HEADERS
              BASE_DIRS
              include
              FILES
              include/async/allocator.hpp
              include/async/completes_synchronously.hpp
              include/async/completion_tags.hpp
              include/async/compose.hpp
              include/async/concepts.hpp
              include/async/connect.hpp
              include/async/continue_on.hpp
              include/async/debug.hpp
              include/async/env.hpp
              include/async/forwarding_query.hpp
              include/async/get_completion_scheduler.hpp
              include/async/get_completion_signatures.hpp
              include/async/get_scheduler.hpp
              include/async/just.hpp
              include/async/just_result_of.hpp
              include/async/let_error.hpp
              include/async/let.hpp
              include/async/let_stopped.hpp
              include/async/let_value.hpp
              include/async/read_env.hpp
              include/async/repeat.hpp
              include/async/retry.hpp
              include/async/schedulers/get_expiration.hpp
              include/async/schedulers/inline_scheduler.hpp
              include/async/schedulers/priority_scheduler.hpp
              include/async/schedulers/requeue_policy.hpp
              include/async/schedulers/runloop_scheduler.hpp
              include/async/schedulers/task.hpp
              include/async/schedulers/task_manager.hpp
              include/async/schedulers/task_manager_interface.hpp
              include/async/schedulers/thread_scheduler.hpp
              include/async/schedulers/timer_manager.hpp
              include/async/schedulers/timer_manager_interface.hpp
              include/async/schedulers/time_scheduler.hpp
              include/async/schedulers/trigger_manager.hpp
              include/async/schedulers/trigger_scheduler.hpp
              include/async/sequence.hpp
              include/async/split.hpp
              include/async/stack_allocator.hpp
              include/async/start_detached.hpp
              include/async/start.hpp
              include/async/start_on.hpp
              include/async/static_allocator.hpp
              include/async/stop_token.hpp
              include/async/sync_wait.hpp
              include/async/then.hpp
              include/async/timeout_after.hpp
              include/async/type_traits.hpp
              include/async/variant_sender.hpp
              include/async/when_all.hpp
              include/async/when_any.hpp)

if(PROJECT_IS_TOP_LEVEL)
    add_docs(docs)
    add_subdirectory(test)
    clang_tidy_interface(async)
endif()
