#include "detail/common.hpp"

#include <async/debug.hpp>
#include <async/just_result_of.hpp>
#include <async/schedulers/time_scheduler.hpp>
#include <async/schedulers/timer_manager.hpp>
#include <async/start_on.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_format.hpp>

#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <atomic>
#include <chrono>
#include <concepts>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <thread>
#include <utility>
#include <vector>

using namespace std::chrono_literals;

namespace {
using default_domain = async::timer_mgr::default_domain;

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
                                           "time_scheduler",
                                           std::chrono::milliseconds> const>);
}

TEST_CASE("time_scheduler fulfils concept", "[time_scheduler]") {
    static_assert(
        async::scheduler<async::time_scheduler<async::timer_mgr::default_domain,
                                               "time_scheduler", int>>);
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
                                           "time_scheduler",
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

TEST_CASE("time_scheduler cancellation works on different domain",
          "[time_scheduler]") {
    constexpr auto scheduler_factory =
        async::time_scheduler_factory<alt_domain>;
    auto s = scheduler_factory(10ms);
    int var{};
    async::sender auto sndr =
        async::start_on(s, async::just_result_of([&] { var = 42; }));
    auto r = stoppable_receiver{[&] { var = 17; }};
    auto op = async::connect(sndr, r);

    async::start(op);
    CHECK(enabled<alt_domain>);
    r.request_stop();
    CHECK(var == 17);
    CHECK(async::timer_mgr::is_idle<alt_domain>());
    CHECK(not enabled<alt_domain>);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    std::mutex m{};

    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    auto signal(auto &&...) {
        if constexpr (std::is_same_v<async::debug::tag_of<Ctx>,
                                     async::time_scheduler_sender_t>) {
            std::lock_guard lock{m};
            static_assert(
                boost::mp11::mp_empty<async::debug::children_of<Ctx>>::value);
            debug_events.push_back(
                fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
        }
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("time_scheduler can be debugged", "[time_scheduler]") {
    using namespace std::string_literals;
    debug_events.clear();

    constexpr auto scheduler_factory =
        async::time_scheduler_factory<default_domain, "sched">;
    auto s = scheduler_factory(10ms).schedule();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    CHECK(debug_events == std::vector{"op sched start"s});
    async::timer_mgr::service_task();
    CHECK(debug_events ==
          std::vector{"op sched start"s, "op sched set_value"s});
}

TEST_CASE("time_scheduler produces set_stopped debug signal",
          "[time_scheduler]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto stop = async::inplace_stop_source{};
    constexpr auto scheduler_factory =
        async::time_scheduler_factory<default_domain, "sched">;
    auto s = scheduler_factory(10ms).schedule();
    auto r = with_env{
        universal_receiver{},
        async::env{async::prop{async::get_debug_interface_t{},
                               async::debug::named_interface<"op">{}},
                   async::prop{async::get_stop_token_t{}, stop.get_token()}}};
    auto op = async::connect(s, r);

    async::start(op);
    CHECK(debug_events == std::vector{"op sched start"s});
    stop.request_stop();
    async::timer_mgr::service_task();
    CHECK(debug_events ==
          std::vector{"op sched start"s, "op sched set_stopped"s});
}

namespace {
template <typename Domain> struct std_hal {
    using time_point_t = std::chrono::steady_clock::time_point;
    using task_t = async::timer_task<time_point_t>;

    static inline std::mutex m{};
    static inline std::condition_variable cv{};
    static inline std::thread t{};
    static inline time_point_t next_wakeup{};
    static inline std::atomic<bool> running{};
    static inline std::atomic<bool> stopping{};

    static auto enable() -> void {
        if (not running.exchange(true)) {
            t = std::thread{[] {
                while (not stopping) {
                    {
                        std::unique_lock lock{m};
                        cv.wait_until(lock, next_wakeup);
                    }
                    if (not stopping and now() >= next_wakeup) {
                        async::timer_mgr::service_task<Domain>();
                    }
                }
            }};
        }
    }
    static auto disable() -> void { set_event_time(time_point_t::max()); }
    static auto set_event_time(time_point_t tp) -> void {
        {
            std::lock_guard lock{m};
            next_wakeup = tp;
        }
        cv.notify_one();
    }
    static auto now() -> time_point_t {
        return std::chrono::steady_clock::now();
    }

    static auto stop() {
        stopping = true;
        cv.notify_one();
        t.join();
        stopping = false;
        running = false;
    }
};

struct std_domain;
using std_timer_manager_t = async::generic_timer_manager<std_hal<std_domain>>;
} // namespace
template <>
[[maybe_unused]] inline auto async::injected_timer_manager<std_domain> =
    std_timer_manager_t{};

TEST_CASE("std domain time_scheduler", "[time_scheduler]") {
    constexpr auto scheduler_factory =
        async::time_scheduler_factory<std_domain>;
    auto s = scheduler_factory(10ms);

    std::mutex m{};
    std::condition_variable cv{};
    int var{};
    async::sender auto sndr = async::start_on(s, async::just_result_of([&] {
                                                  {
                                                      std::lock_guard lock{m};
                                                      var = 42;
                                                  }
                                                  cv.notify_one();
                                              }));
    auto op = async::connect(sndr, universal_receiver{});
    async::start(op);
    {
        std::unique_lock lock{m};
        cv.wait(lock, [&] { return var == 42; });
    }
    std_hal<std_domain>::stop();
}
