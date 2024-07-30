#include "detail/common.hpp"

#include <async/allocator.hpp>
#include <async/env.hpp>
#include <async/just_result_of.hpp>
#include <async/read_env.hpp>
#include <async/schedulers/priority_scheduler.hpp>
#include <async/schedulers/task_manager.hpp>
#include <async/start_detached.hpp>
#include <async/static_allocator.hpp>
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

TEST_CASE("start_detached can be cancelled", "[start_detached]") {
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule()                    //
             | async::then([&] { var = 42; }) //
             | async::upon_stopped([&] { var = 17; });
    auto stop_src = async::start_detached(s);
    REQUIRE(stop_src.has_value());
    stop_src.value()->request_stop();
    async::task_mgr::service_tasks<0>();
    CHECK(var == 17);
}

TEST_CASE("start_detached_unstoppable has no cancellation",
          "[start_detached]") {
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule()                    //
             | async::then([&] { var = 42; }) //
             | async::upon_stopped([&] { var = 17; });
    auto stop_src = async::start_detached_unstoppable(s);
    REQUIRE(stop_src.has_value());
    static_assert(
        std::is_same_v<async::never_stop_source *,
                       std::remove_cvref_t<decltype(stop_src.value())>>);
    stop_src.value()->request_stop();
    async::task_mgr::service_tasks<0>();
    CHECK(var == 42);
}

namespace {
struct custom_allocator {
    template <typename N, typename T, typename F, typename... Args>
        requires std::is_constructible_v<T, Args...>
    static auto construct(F &&f, Args &&...args) -> bool {
        ++alloc_count;
        return async::static_allocator::template construct<N, T>(
            std::forward<F>(f), std::forward<Args>(args)...);
    }

    template <typename, typename T> static auto destruct(T const *) -> void {
        ++destroy_count;
    }

    static inline std::size_t alloc_count{};
    static inline std::size_t destroy_count{};
};
} // namespace

TEST_CASE("start_detached can use a custom environment", "[start_detached]") {
    custom_allocator::alloc_count = 0;
    custom_allocator::destroy_count = 0;
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { var = 42; });
    CHECK(async::start_detached(
        s, async::prop{async::get_allocator_t{}, custom_allocator{}}));
    CHECK(custom_allocator::alloc_count == 1);
    async::task_mgr::service_tasks<0>();
    CHECK(var == 42);
    CHECK(custom_allocator::destroy_count == 1);
}

TEST_CASE("start_detached passes the custom env to the sender chain",
          "[start_detached]") {
    auto s =
        async::read_env(async::get_allocator) | async::then([]<typename T>(T) {
            static_assert(std::is_same_v<T, custom_allocator>);
        });
    CHECK(async::start_detached(
        s, async::prop{async::get_allocator_t{}, custom_allocator{}}));
}

TEST_CASE("start_detached allows state in the custom env", "[start_detached]") {
    auto s =
        async::read_env(get_fwd) | async::then([](int x) { CHECK(x == 17); });
    CHECK(async::start_detached(s, async::prop{get_fwd, 17}));
}
