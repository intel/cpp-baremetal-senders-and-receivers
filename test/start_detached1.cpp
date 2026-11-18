#include "detail/injected_task_manager.hpp"

#include <async/schedulers/priority_scheduler.hpp>
#include <async/start_detached.hpp>

#include <stdx/ct_string.hpp>

#include <catch2/catch_test_macros.hpp>

extern auto try_second_start() -> bool;

TEST_CASE("start_detached allocation limit works across TUs",
          "[start_detached]") {
    using S = async::fixed_priority_scheduler<0>;
    CHECK(async::start_detached<"test">(S::schedule()));
    CHECK(not try_second_start());
    async::task_mgr::service_tasks<0>();
}
