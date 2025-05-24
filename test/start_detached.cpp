#include "detail/common.hpp"
#include "detail/debug_handler.hpp"

#include <async/allocator.hpp>
#include <async/env.hpp>
#include <async/just_result_of.hpp>
#include <async/read_env.hpp>
#include <async/schedulers/priority_scheduler.hpp>
#include <async/schedulers/task_manager.hpp>
#include <async/start_detached.hpp>
#include <async/static_allocator.hpp>
#include <async/then.hpp>

#include <stdx/type_traits.hpp>

#include <catch2/catch_test_macros.hpp>

#include <cstddef>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

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
    STATIC_REQUIRE(
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

    template <typename> constexpr static auto allocation_limit = std::size_t{};

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
            STATIC_REQUIRE(std::is_same_v<T, custom_allocator>);
        });
    CHECK(async::start_detached(
        s, async::prop{async::get_allocator_t{}, custom_allocator{}}));
}

TEST_CASE("start_detached allows state in the custom env", "[start_detached]") {
    auto s =
        async::read_env(get_fwd) | async::then([](int x) { CHECK(x == 17); });
    CHECK(async::start_detached(s, async::prop{get_fwd, 17}));
}

TEST_CASE("stop_detached cancels the operation", "[start_detached]") {
    using Name = decltype([] {});
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { var = 42; });

    CHECK(async::start_detached<Name>(s));
    CHECK(async::stop_detached<Name>());
    async::task_mgr::service_tasks<0>();
    CHECK(var == 0);
}

TEST_CASE("stop_detached is idempotent, reporting a request once",
          "[start_detached]") {
    using Name = decltype([] {});
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { var = 42; });

    CHECK(async::start_detached<Name>(s));
    CHECK(async::stop_detached<Name>());
    CHECK(not async::stop_detached<Name>());
    async::task_mgr::service_tasks<0>();
    CHECK(var == 0);
}

TEST_CASE("stop_detached doesn't cancel the operation if it's already complete",
          "[start_detached]") {
    using Name = decltype([] {});
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { var = 42; });

    CHECK(async::start_detached<Name>(s));
    async::task_mgr::service_tasks<0>();
    CHECK(var == 42);
    CHECK(not async::stop_detached<Name>());
}

TEST_CASE("stop_detached doesn't affect future runs of the operation",
          "[start_detached]") {
    using Name = decltype([] {});
    CHECK(not async::stop_detached<Name>());
    {
        int var{};
        using S = async::fixed_priority_scheduler<0>;
        auto s = S::schedule() | async::then([&] { var = 42; });
        CHECK(async::start_detached<Name>(s));
        async::task_mgr::service_tasks<0>();
        CHECK(var == 42);
    }
    CHECK(not async::stop_detached<Name>());
    {
        int var{};
        using S = async::fixed_priority_scheduler<0>;
        auto s = S::schedule() | async::then([&] { var = 42; });
        CHECK(async::start_detached<Name>(s));
        async::task_mgr::service_tasks<0>();
        CHECK(var == 42);
    }
}

namespace {
struct alloc_domain;
}
template <>
constexpr inline auto async::static_allocation_limit<alloc_domain> =
    std::size_t{2};

TEST_CASE("stop_detached doesn't cancel the operation when the op state is "
          "not a singleton",
          "[start_detached]") {
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { var = 42; });

    CHECK(async::start_detached<alloc_domain>(s));
    CHECK(not async::stop_detached<alloc_domain>());
    async::task_mgr::service_tasks<0>();
    CHECK(var == 42);
}

TEST_CASE("stop_detached doesn't cancel an operation with a stack allocator",
          "[start_detached]") {
    using Name = decltype([] {});
    int var{};
    auto s = async::just_result_of([&] { var = 42; });

    CHECK(async::start_detached<Name>(s));
    CHECK(not async::stop_detached<Name>());
    CHECK(var == 42);
}

TEST_CASE("stop_detached doesn't cancel an unstoppable operation",
          "[start_detached]") {
    using Name = decltype([] {});
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { var = 42; });

    CHECK(async::start_detached_unstoppable<Name>(s));
    CHECK(not async::stop_detached<Name>());
    async::task_mgr::service_tasks<0>();
    CHECK(var == 42);
}

template <>
inline auto async::injected_debug_handler<> =
    debug_handler<async::start_detached_t, true>{};

TEST_CASE("start_detached can be named and debugged with a string",
          "[start_detached]") {
    using namespace std::string_literals;
    debug_events.clear();
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { var = 42; });
    CHECK(async::start_detached<"op">(s));
    CHECK(var == 0);
    CHECK(debug_events == std::vector{"op start_detached start"s});
    async::task_mgr::service_tasks<0>();
    CHECK(var == 42);
    CHECK(debug_events == std::vector{"op start_detached start"s,
                                      "op start_detached set_value"s});
}

TEST_CASE("start_detached_unstoppable can be named and debugged with a string",
          "[start_detached]") {
    using namespace std::string_literals;
    debug_events.clear();
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { var = 42; });
    CHECK(async::start_detached_unstoppable<"op">(s));
    CHECK(var == 0);
    CHECK(debug_events == std::vector{"op start_detached start"s});
    async::task_mgr::service_tasks<0>();
    CHECK(var == 42);
    CHECK(debug_events == std::vector{"op start_detached start"s,
                                      "op start_detached set_value"s});
}

TEST_CASE("stop_detached works with a string name", "[start_detached]") {
    using namespace std::string_literals;
    debug_events.clear();
    int var{};
    using S = async::fixed_priority_scheduler<0>;
    auto s = S::schedule() | async::then([&] { var = 42; });

    CHECK(async::start_detached<"op">(s));
    CHECK(debug_events == std::vector{"op start_detached start"s});
    CHECK(async::stop_detached<"op">());
    async::task_mgr::service_tasks<0>();
    CHECK(var == 0);
    CHECK(debug_events == std::vector{"op start_detached start"s,
                                      "op start_detached set_stopped"s});
}
