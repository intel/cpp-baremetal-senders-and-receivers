#include "detail/common.hpp"

#include <async/just_result_of.hpp>
#include <async/schedulers/time_scheduler.hpp>
#include <async/schedulers/timer_manager.hpp>
#include <async/start_on.hpp>

#include <stdx/concepts.hpp>

#include <catch2/catch_test_macros.hpp>

#include <chrono>
#include <concepts>
#include <functional>
#include <utility>
#include <vector>

using namespace std::chrono_literals;

namespace {
struct default_domain;

template <typename Domain, typename TP> inline auto current_time = TP{};
template <typename Domain> inline auto enabled = false;
template <typename Domain, typename TP> inline auto calls = std::vector<TP>{};

template <typename Domain> struct hal {
    using time_point_t = std::chrono::steady_clock::time_point;
    using task_t = async::timer_task<time_point_t>;

    static auto enable() -> void { enabled<Domain> = true; }
    static auto disable() -> void { enabled<Domain> = false; }
    static auto set_event_time(time_point_t tp) -> void {
        calls<Domain, time_point_t>.push_back(tp);
    }
    static auto now() -> time_point_t {
        return current_time<Domain, time_point_t>;
    }
};

using timer_manager_t = async::generic_timer_manager<hal<default_domain>>;

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

template <typename Rep, typename Period>
struct async::timer_mgr::time_point_for<std::chrono::duration<Rep, Period>> {
    using type = std::chrono::steady_clock::time_point;
};

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
                     async::time_scheduler<async::timer_mgr::default_domain,
                                           std::chrono::milliseconds> const>);
}

TEST_CASE("time_scheduler fulfils concept", "[time_scheduler]") {
    static_assert(
        async::scheduler<
            async::time_scheduler<async::timer_mgr::default_domain, int>>);
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
                     async::time_scheduler<async::timer_mgr::default_domain,
                                           std::chrono::milliseconds>>);
    CHECK(cs.d == 10ms);
}

TEST_CASE("time_scheduler schedules tasks", "[time_scheduler]") {
    auto s = async::time_scheduler{10ms};
    int var{};
    async::sender auto sndr =
        async::start_on(s, async::just_result_of([&] { var = 42; }));
    auto op = async::connect(sndr, universal_receiver{});

    async::timer_mgr::service_task();
    CHECK(var == 0);

    async::start(op);
    CHECK(enabled<default_domain>);
    async::timer_mgr::service_task();
    CHECK(var == 42);
    CHECK(async::timer_mgr::is_idle());
    CHECK(not enabled<default_domain>);
}

TEST_CASE("time_scheduler is cancellable before start", "[time_scheduler]") {
    auto s = async::time_scheduler{10ms};
    int var{};
    async::sender auto sndr =
        async::start_on(s, async::just_result_of([&] { var = 42; }));
    auto r = stoppable_receiver{[&] { var = 17; }};
    auto op = async::connect(sndr, r);

    r.request_stop();
    async::start(op);
    CHECK(not enabled<default_domain>);
    CHECK(var == 17);
    CHECK(async::timer_mgr::is_idle());
}

TEST_CASE("time_scheduler cancels via HAL after start", "[time_scheduler]") {
    auto s = async::time_scheduler{1s};
    int var{};
    async::sender auto sndr =
        async::start_on(s, async::just_result_of([&] { var = 42; }));
    auto r = stoppable_receiver{[&] { var = 17; }};
    auto op = async::connect(sndr, r);

    async::start(op);
    CHECK(not async::timer_mgr::is_idle());
    CHECK(enabled<default_domain>);
    r.request_stop();
    CHECK(var == 17);
    CHECK(async::timer_mgr::is_idle());
    CHECK(not enabled<default_domain>);
}

namespace {
struct alt_domain;
using alt_timer_manager_t = async::generic_timer_manager<hal<alt_domain>>;
} // namespace
template <>
[[maybe_unused]] inline auto async::injected_timer_manager<alt_domain> =
    alt_timer_manager_t{};

TEST_CASE("time_scheduler can have different domain", "[time_scheduler]") {
    enabled<default_domain> = false;

    constexpr auto scheduler_factory =
        async::time_scheduler_factory<alt_domain>;
    auto s = scheduler_factory(10ms);
    int var{};
    async::sender auto sndr =
        async::start_on(s, async::just_result_of([&] { var = 42; }));
    auto op = async::connect(sndr, universal_receiver{});

    async::timer_mgr::service_task<alt_domain>();
    CHECK(var == 0);

    async::start(op);
    CHECK(enabled<alt_domain>);
    CHECK(not enabled<default_domain>);

    async::timer_mgr::service_task<alt_domain>();
    CHECK(var == 42);
    CHECK(async::timer_mgr::is_idle<alt_domain>());
    CHECK(not enabled<alt_domain>);
}
