#include "detail/common.hpp"

#include <async/just_result_of.hpp>
#include <async/on.hpp>
#include <async/schedulers/time_scheduler.hpp>
#include <async/schedulers/timer_manager.hpp>

#include <stdx/concepts.hpp>

#include <catch2/catch_test_macros.hpp>

#include <chrono>
#include <concepts>
#include <functional>
#include <utility>
#include <vector>

using namespace std::chrono_literals;

namespace {
struct hal {
    using time_point_t = std::chrono::steady_clock::time_point;
    using task_t = async::timer_task<time_point_t>;

    static inline time_point_t current_time{};
    static inline bool enabled{};
    static inline std::vector<time_point_t> calls{};

    static auto enable() -> void { enabled = true; }
    static auto disable() -> void { enabled = false; }
    static auto set_event_time(time_point_t tp) -> void { calls.push_back(tp); }
    static auto now() -> time_point_t { return current_time; }
};

using timer_manager_t = async::generic_timer_manager<hal>;

std::function<void()> interrupt_fn{};

struct test_concurrency_policy {
    struct interrupt {
        ~interrupt() {
            if (interrupt_fn) {
                interrupt_fn();
            }
        }
    };

    template <typename = void, typename F, typename... Pred>
    static auto call_in_critical_section(F &&f, Pred &&...) -> decltype(auto) {
        [[maybe_unused]] interrupt raii_interrupt{};
        return std::forward<F>(f)();
    }
};
} // namespace

namespace async::timer_mgr {
template <typename Rep, typename Period>
struct time_point_for<std::chrono::duration<Rep, Period>> {
    using type = hal::time_point_t;
};
} // namespace async::timer_mgr

template <>
[[maybe_unused]] inline auto conc::injected_policy<> =
    test_concurrency_policy{};
template <>
[[maybe_unused]] inline auto async::injected_timer_manager<> =
    timer_manager_t{};

TEST_CASE("time_scheduler deduces duration type", "[time_scheduler]") {
    constexpr auto s = async::time_scheduler{10ms};
    static_assert(
        std::same_as<decltype(s),
                     async::time_scheduler<std::chrono::milliseconds> const>);
}

TEST_CASE("time_scheduler fulfils concept", "[time_scheduler]") {
    static_assert(async::scheduler<async::time_scheduler<int>>);
}

TEST_CASE("time_scheduler sender advertises nothing", "[time_scheduler]") {
    static_assert(
        async::sender_of<decltype(async::time_scheduler{0}.schedule()),
                         async::set_value_t()>);
}

TEST_CASE("sender has the time_scheduler as its completion scheduler",
          "[time_scheduler]") {
    auto s = async::time_scheduler{10ms}.schedule();
    auto cs =
        async::get_completion_scheduler<async::set_value_t>(async::get_env(s));
    static_assert(
        std::same_as<decltype(cs),
                     async::time_scheduler<std::chrono::milliseconds>>);
}

TEST_CASE("time_scheduler schedules tasks", "[time_scheduler]") {
    auto s = async::time_scheduler{10ms};
    int var{};
    async::sender auto sndr =
        async::on(s, async::just_result_of([&] { var = 42; }));
    auto op = async::connect(sndr, universal_receiver{});

    async::timer_mgr::service_task();
    CHECK(var == 0);

    async::start(op);
    CHECK(hal::enabled);
    async::timer_mgr::service_task();
    CHECK(var == 42);
    CHECK(async::timer_mgr::is_idle());
    CHECK(not hal::enabled);
}

TEST_CASE("time_scheduler is cancellable before start", "[time_scheduler]") {
    auto s = async::time_scheduler{10ms};
    int var{};
    async::sender auto sndr =
        async::on(s, async::just_result_of([&] { var = 42; }));
    auto r = stoppable_receiver{[&] { var = 17; }};
    auto op = async::connect(sndr, r);

    r.request_stop();
    async::start(op);
    CHECK(not hal::enabled);
    CHECK(var == 17);
    CHECK(async::timer_mgr::is_idle());
}

TEST_CASE("time_scheduler cancels via HAL after start", "[time_scheduler]") {
    auto s = async::time_scheduler{1s};
    int var{};
    async::sender auto sndr =
        async::on(s, async::just_result_of([&] { var = 42; }));
    auto r = stoppable_receiver{[&] { var = 17; }};
    auto op = async::connect(sndr, r);

    async::start(op);
    CHECK(not async::timer_mgr::is_idle());
    CHECK(hal::enabled);
    r.request_stop();
    CHECK(var == 17);
    CHECK(async::timer_mgr::is_idle());
    CHECK(not hal::enabled);
}
