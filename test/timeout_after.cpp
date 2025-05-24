#include "detail/common.hpp"

#include <async/just.hpp>
#include <async/just_result_of.hpp>
#include <async/schedulers/time_scheduler.hpp>
#include <async/schedulers/timer_manager.hpp>
#include <async/start_on.hpp>
#include <async/timeout_after.hpp>

#include <catch2/catch_test_macros.hpp>

#include <chrono>
#include <functional>
#include <utility>
#include <vector>

using namespace std::chrono_literals;

namespace {
using default_domain = async::timer_mgr::default_domain;

using tp_t = std::chrono::steady_clock::time_point;

template <typename Domain, typename TP> inline auto current_time = TP{};
template <typename Domain> inline auto enabled = false;

template <typename Domain> struct hal {
    using time_point_t = tp_t;
    using task_t = async::timer_task<time_point_t>;

    static auto enable() -> void { enabled<Domain> = true; }
    static auto disable() -> void { enabled<Domain> = false; }
    static auto set_event_time(time_point_t) -> void {}
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

TEST_CASE("timeout_after advertises what it sends", "[timeout_after]") {
    auto s = async::start_on(async::time_scheduler{1s}, async::just(42));
    auto to = async::timeout_after(s, 100ms, 1.0f);
    STATIC_REQUIRE(async::sender_of<decltype(to), async::set_value_t(int)>);
    STATIC_REQUIRE(async::sender_of<decltype(to), async::set_error_t(float)>);
}

TEST_CASE("timeout_after can complete successfully", "[timeout_after]") {
    current_time<default_domain, tp_t> = tp_t{};
    int var{};
    auto s = async::start_on(async::time_scheduler{1s}, async::just(42));
    auto to = async::timeout_after(s, 2s, 17);
    auto op = async::connect(to, receiver{[&](int i) { var = i; }});
    async::start(op);

    current_time<default_domain, tp_t> = tp_t{3s};
    async::timer_mgr::service_task();
    CHECK(var == 42);
    CHECK(async::timer_mgr::is_idle());
}

TEST_CASE("timeout_after can complete with error after timeout",
          "[timeout_after]") {
    current_time<default_domain, tp_t> = tp_t{};
    int var{};
    auto s = async::start_on(async::time_scheduler{1s}, async::just(42));
    auto to = async::timeout_after(s, 100ms, 17);
    auto op = async::connect(to, error_receiver{[&](int i) { var = i; }});
    async::start(op);

    current_time<default_domain, tp_t> = tp_t{3s};
    async::timer_mgr::service_task();
    CHECK(var == 17);
    CHECK(async::timer_mgr::is_idle());
}

TEST_CASE("timeout_after is pipeable", "[timeout_after]") {
    current_time<default_domain, tp_t> = tp_t{};
    int var{};
    auto s = async::start_on(async::time_scheduler{1s}, async::just(42));
    auto to = s | async::timeout_after(2s, 17);
    auto op = async::connect(to, receiver{[&](int i) { var = i; }});
    async::start(op);

    current_time<default_domain, tp_t> = tp_t{3s};
    async::timer_mgr::service_task();
    CHECK(var == 42);
    CHECK(async::timer_mgr::is_idle());
}

TEST_CASE("timeout_after can incite its own timeout", "[timeout_after]") {
    current_time<default_domain, tp_t> = tp_t{};
    int var{};
    auto s =
        async::start_on(async::time_scheduler{1s}, async::just_result_of([] {
                            current_time<default_domain, tp_t> = tp_t{3s};
                            CHECK(not async::timer_mgr::is_idle());
                            async::timer_mgr::service_task();
                            return 42;
                        }));
    auto to = s | async::timeout_after(2s, 17);
    auto op = async::connect(to, error_receiver{[&](int i) { var = i; }});
    async::start(op);

    current_time<default_domain, tp_t> = tp_t{1s};
    async::timer_mgr::service_task();
    CHECK(var == 17);
    CHECK(async::timer_mgr::is_idle());
}

namespace {
struct alt_domain;
using alt_timer_manager_t = async::generic_timer_manager<hal<alt_domain>>;
} // namespace
template <>
[[maybe_unused]] inline auto async::injected_timer_manager<alt_domain> =
    alt_timer_manager_t{};

TEST_CASE("timeout_after can work in a different domain", "[timeout_after]") {
    current_time<alt_domain, tp_t> = tp_t{};
    int var{};
    constexpr auto scheduler_factory =
        async::time_scheduler_factory<alt_domain>;
    auto s = async::start_on(scheduler_factory(1s), async::just(42));
    auto to = async::timeout_after<alt_domain>(s, 2s, 17);
    auto op = async::connect(to, receiver{[&](int i) { var = i; }});
    async::start(op);
    CHECK(enabled<alt_domain>);

    current_time<alt_domain, tp_t> = tp_t{3s};
    async::timer_mgr::service_task<alt_domain>();
    CHECK(not enabled<alt_domain>);
    CHECK(var == 42);
    CHECK(async::timer_mgr::is_idle<alt_domain>());
}

TEST_CASE("timeout_after can time out in a different domain",
          "[timeout_after]") {
    current_time<alt_domain, tp_t> = tp_t{};
    int var{};
    constexpr auto scheduler_factory =
        async::time_scheduler_factory<alt_domain>;
    auto s = async::start_on(scheduler_factory(1s), async::just(42));
    auto to = async::timeout_after<alt_domain>(s, 100ms, 17);
    auto op = async::connect(to, error_receiver{[&](int i) { var = i; }});
    async::start(op);

    current_time<alt_domain, tp_t> = tp_t{3s};
    async::timer_mgr::service_task<alt_domain>();
    CHECK(var == 17);
}

TEST_CASE("timeout_after can time out on custom channel", "[timeout_after]") {
    current_time<alt_domain, tp_t> = tp_t{};
    constexpr auto scheduler_factory =
        async::time_scheduler_factory<alt_domain>;
    auto s = async::start_on(scheduler_factory(1s), async::just(42));
    auto to =
        async::timeout_after<alt_domain, async::set_value_t>(s, 100ms, 1.0f);
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(to)>,
                     async::completion_signatures<async::set_value_t(float),
                                                  async::set_value_t(int)>>);
}

TEST_CASE("timeout_after can complete with value after timeout",
          "[timeout_after]") {
    current_time<alt_domain, tp_t> = tp_t{};
    int var{};
    constexpr auto scheduler_factory =
        async::time_scheduler_factory<alt_domain>;
    auto s = async::start_on(scheduler_factory(1s), async::just(42));
    auto to =
        async::timeout_after<alt_domain, async::set_value_t>(s, 100ms, 17);
    auto op = async::connect(to, receiver{[&](int i) { var = i; }});
    async::start(op);

    current_time<alt_domain, tp_t> = tp_t{3s};
    async::timer_mgr::service_task<alt_domain>();
    CHECK(var == 17);
}

TEST_CASE("timeout_after can complete with multiple values after timeout",
          "[timeout_after]") {
    current_time<alt_domain, tp_t> = tp_t{};
    int var{};
    constexpr auto scheduler_factory =
        async::time_scheduler_factory<alt_domain>;
    auto s = async::start_on(scheduler_factory(1s), async::just(42));
    auto to =
        async::timeout_after<alt_domain, async::set_value_t>(s, 100ms, 17, 18);
    auto op =
        async::connect(to, receiver{[&](auto... is) { var = (0 + ... + is); }});
    async::start(op);

    current_time<alt_domain, tp_t> = tp_t{3s};
    async::timer_mgr::service_task<alt_domain>();
    CHECK(var == 35);
}

TEST_CASE("timeout_after can complete with stopped after timeout",
          "[timeout_after]") {
    current_time<alt_domain, tp_t> = tp_t{};
    int var{};
    constexpr auto scheduler_factory =
        async::time_scheduler_factory<alt_domain>;
    auto s = async::start_on(scheduler_factory(1s), async::just(42));
    auto to = async::timeout_after<alt_domain, async::set_stopped_t>(s, 100ms);
    auto op = async::connect(to, stopped_receiver{[&] { var = 17; }});
    async::start(op);

    current_time<alt_domain, tp_t> = tp_t{3s};
    async::timer_mgr::service_task<alt_domain>();
    CHECK(var == 17);
}
