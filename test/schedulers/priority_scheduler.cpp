#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/just_result_of.hpp>
#include <async/on.hpp>
#include <async/schedulers/priority_scheduler.hpp>
#include <async/schedulers/task_manager.hpp>

#include <stdx/concepts.hpp>

#include <catch2/catch_test_macros.hpp>

namespace {
struct hal {
    static auto schedule(async::priority_t) {}
};

using task_manager_t = async::priority_task_manager<hal, 8>;
} // namespace

template <> inline auto async::injected_task_manager<> = task_manager_t{};

TEST_CASE("fixed_priority_scheduler fulfils concept", "[priority_scheduler]") {
    static_assert(async::scheduler<async::fixed_priority_scheduler<0>>);
}

TEST_CASE("fixed_priority_scheduler sender advertises nothing",
          "[priority_scheduler]") {
    static_assert(async::sender_of<
                  decltype(async::fixed_priority_scheduler<0>::schedule()),
                  async::set_value_t()>);
}

TEST_CASE("sender has the fixed_priority_scheduler as its completion scheduler",
          "[priority_scheduler]") {
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule();
    auto cs =
        async::get_completion_scheduler<async::set_value_t>(async::get_env(s));
    static_assert(std::same_as<decltype(cs), S>);
}

TEST_CASE("fixed_priority_scheduler schedules tasks", "[priority_scheduler]") {
    auto s = async::fixed_priority_scheduler<0>{};
    int var{};
    async::sender auto sndr =
        async::on(s, async::just_result_of([&] { var = 42; }));
    auto op = async::connect(sndr, universal_receiver{});

    async::task_mgr::service_tasks<0>();
    CHECK(var == 0);

    op.start();
    async::task_mgr::service_tasks<0>();
    CHECK(var == 42);
    CHECK(async::task_mgr::is_idle());
}

TEST_CASE("fixed_priority_scheduler is cancellable before start",
          "[priority_scheduler]") {
    auto s = async::fixed_priority_scheduler<0>{};
    int var{};
    async::sender auto sndr =
        async::on(s, async::just_result_of([&] { var = 42; }));
    auto r = stoppable_receiver{[&] { var = 17; }};
    auto op = async::connect(sndr, r);

    r.request_stop();
    op.start();
    async::task_mgr::service_tasks<0>();
    CHECK(var == 17);
    CHECK(async::task_mgr::is_idle());
}

TEST_CASE("fixed_priority_scheduler is cancellable after start",
          "[priority_scheduler]") {
    auto s = async::fixed_priority_scheduler<0>{};
    int var{};
    async::sender auto sndr =
        async::on(s, async::just_result_of([&] { var = 42; }));
    auto r = stoppable_receiver{[&] { var = 17; }};
    auto op = async::connect(sndr, r);

    op.start();
    r.request_stop();
    async::task_mgr::service_tasks<0>();
    CHECK(var == 17);
    CHECK(async::task_mgr::is_idle());
}
