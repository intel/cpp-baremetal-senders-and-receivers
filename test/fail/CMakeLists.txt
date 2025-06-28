add_compile_fail_test("incite_on_multiple_completions.cpp" LIBRARIES async
                      pthread)
add_compile_fail_test("incite_on_synchronous_scheduler.cpp" LIBRARIES async
                      pthread)
add_compile_fail_test("incite_on_without_function.cpp" LIBRARIES async pthread)
add_compile_fail_test("just_connect.cpp" LIBRARIES async pthread)
add_compile_fail_test("just_error_connect.cpp" LIBRARIES async pthread)
add_compile_fail_test("just_error_result_of_connect.cpp" LIBRARIES async
                      pthread)
add_compile_fail_test("just_result_of_connect.cpp" LIBRARIES async pthread)
add_compile_fail_test("just_stopped_connect.cpp" LIBRARIES async pthread)
add_compile_fail_test("read_env_connect.cpp" LIBRARIES async pthread)
add_compile_fail_test("read_env_sync_wait.cpp" LIBRARIES async pthread)
add_compile_fail_test("repeat_compose.cpp" LIBRARIES async pthread)
add_compile_fail_test("retry_compose.cpp" LIBRARIES async pthread)
add_compile_fail_test("sequence_connect.cpp" LIBRARIES async pthread)
add_compile_fail_test("split_connect.cpp" LIBRARIES async pthread)
add_compile_fail_test("sync_wait_multiple_completions.cpp" LIBRARIES async
                      pthread)
add_compile_fail_test("then_connect.cpp" LIBRARIES async pthread)
add_compile_fail_test("timeout_after_error.cpp" LIBRARIES async pthread)
add_compile_fail_test("timeout_after_stopped.cpp" LIBRARIES async pthread)
add_compile_fail_test("when_all_multiple_completions.cpp" LIBRARIES async
                      pthread)
add_compile_fail_test("when_any_uncompleteable.cpp" LIBRARIES async pthread)
add_compile_fail_test("when_any_unstoppable.cpp" LIBRARIES async pthread)
