add_tests(
    inline_scheduler
    priority_scheduler
    runloop_scheduler
    std_time_scheduler
    task_manager
    thread_scheduler
    time_scheduler
    timer_manager
    trigger_scheduler)

add_unit_test(
    "trigger_multi_tu_test"
    CATCH2
    FILES
    "trigger1.cpp"
    "trigger2.cpp"
    INCLUDE_DIRECTORIES
    ${PROJECT_SOURCE_DIR}/test
    LIBRARIES
    warnings
    async
    fmt::fmt-header-only
    pthread)

add_subdirectory(fail)
