add_tests(
    inline_scheduler
    priority_scheduler
    runloop_scheduler
    task_manager
    time_scheduler
    timer_manager
    thread_scheduler)

add_compile_fail_test("fail/exceed_priority.cpp" LIBRARIES async pthread)
add_compile_fail_test("fail/wrong_duration_type.cpp" LIBRARIES async pthread)
add_compile_fail_test("fail/no_time_point_for_spec.cpp" LIBRARIES async pthread)
