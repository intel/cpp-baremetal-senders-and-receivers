add_tests(inline_scheduler priority_scheduler runloop_scheduler task_manager
          timer_manager thread_scheduler)

add_compile_fail_test("fail/exceed_priority.cpp" LIBRARIES async pthread)
