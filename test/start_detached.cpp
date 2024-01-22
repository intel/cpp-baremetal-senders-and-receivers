#include "detail/common.hpp"

#include <async/just_result_of.hpp>
#include <async/schedulers/priority_scheduler.hpp>
#include <async/schedulers/task_manager.hpp>
#include <async/start_detached.hpp>
#include <async/then.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("basic operation", "[start_detached]") {
    int var{};
    auto s = async::just_result_of([&] { var = 42; });
    CHECK(async::start_detached(s));
    CHECK(var == 42);
}

namespace {
struct hal {
    static auto schedule(async::priority_t) {}
};

using task_manager_t = async::priority_task_manager<hal, 8>;
} // namespace

template <> inline auto async::injected_task_manager<> = task_manager_t{};

TEST_CASE("start_detached starts the operation", "[start_detached]") {
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { var = 42; });
    CHECK(async::start_detached(s));
    CHECK(var == 0);
    async::task_mgr::service_tasks<0>();
    CHECK(var == 42);
}

TEST_CASE("start_detached fails when allocation limit is reached",
          "[start_detached]") {
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { ++var; });

    using Name = decltype([] {});
    CHECK(async::start_detached<Name>(s));
    CHECK(not async::start_detached<Name>(s));
    async::task_mgr::service_tasks<0>();
    CHECK(var == 1);
}

TEST_CASE("state is freed on completion", "[start_detached]") {
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { ++var; });

    using Name = decltype([] {});
    CHECK(async::start_detached<Name>(s));
    async::task_mgr::service_tasks<0>();
    CHECK(async::start_detached<Name>(s));
    async::task_mgr::service_tasks<0>();
    CHECK(var == 2);
}

TEST_CASE("start_detached is actually detached", "[start_detached]") {
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    {
        auto s = S::schedule() | async::then([&] { var = 42; });
        CHECK(async::start_detached(s));
    }
    CHECK(var == 0);
    async::task_mgr::service_tasks<0>();
    CHECK(var == 42);
}
