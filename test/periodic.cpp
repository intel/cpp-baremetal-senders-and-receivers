#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/continue_on.hpp>
#include <async/debug.hpp>
#include <async/just.hpp>
#include <async/periodic.hpp>
#include <async/schedulers/priority_scheduler.hpp>
#include <async/schedulers/task_manager.hpp>
#include <async/schedulers/time_scheduler.hpp>
#include <async/schedulers/timer_manager.hpp>
#include <async/sequence.hpp>
#include <async/then.hpp>

#include <stdx/ct_format.hpp>

#include <catch2/catch_test_macros.hpp>

#include <chrono>
#include <concepts>

using namespace std::chrono_literals;

namespace {
using default_domain = async::timer_mgr::default_domain;

template <typename Domain, typename TP> inline auto current_time = TP{};
template <typename Domain> inline auto enabled = false;
template <typename Domain, typename TP> inline auto calls = std::vector<TP>{};

template <typename Domain> struct timer_hal {
    using time_point_t = std::chrono::steady_clock::time_point;
    using task_t = async::timer_task<time_point_t>;

    static auto enable() -> void { enabled<Domain> = true; }
    static auto disable() -> void { enabled<Domain> = false; }
    static auto set_event_time(time_point_t tp) -> void {
        CHECK(enabled<Domain>);
        calls<Domain, time_point_t>.push_back(tp);
    }
    static auto now() -> time_point_t {
        CHECK(enabled<Domain>);
        return current_time<Domain, time_point_t>;
    }
};

using timer_manager_t = async::generic_timer_manager<timer_hal<default_domain>>;

struct priority_hal {
    static auto schedule(async::priority_t) {}
};

using task_manager_t = async::priority_task_manager<priority_hal, 8>;
} // namespace

template <typename Rep, typename Period>
struct async::timer_mgr::time_point_for<std::chrono::duration<Rep, Period>> {
    using type = std::chrono::steady_clock::time_point;
};

template <>
[[maybe_unused]] inline auto async::injected_timer_manager<> =
    timer_manager_t{};

template <> inline auto async::injected_task_manager<> = task_manager_t{};

TEST_CASE("periodic advertises what it sends", "[periodic]") {
    [[maybe_unused]] auto s =
        async::time_scheduler{}.schedule() | async::periodic(1s);
    static_assert(std::same_as<async::completion_signatures_of_t<decltype(s)>,
                               async::completion_signatures<>>);
}

TEST_CASE("periodic advertises sending error", "[periodic]") {
    [[maybe_unused]] auto s = async::time_scheduler{}.schedule() |
                              async::seq(async::just_error(42)) |
                              async::periodic(1s);
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_error_t(int)>>);
}

TEST_CASE("periodic advertises sending stopped", "[periodic]") {
    [[maybe_unused]] auto s = async::time_scheduler{}.schedule() |
                              async::seq(async::just_stopped()) |
                              async::periodic(1s);
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_stopped_t()>>);
}

TEST_CASE("periodic propagates forwarding queries to its child environment",
          "[periodic]") {
    auto s = custom_sender{};
    CHECK(get_fwd(async::get_env(s)) == 42);

    auto r = async::periodic(s, 1s);
    CHECK(get_fwd(async::get_env(r)) == 42);
}

TEST_CASE("periodic_until advertises what it sends", "[periodic]") {
    [[maybe_unused]] auto s =
        async::time_scheduler{}.schedule() | async::then([] { return 42; }) |
        async::periodic_until(1s, [](auto) { return true; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_value_t(int)>>);
}

TEST_CASE("periodic_n advertises what it sends", "[periodic]") {
    [[maybe_unused]] auto s = async::time_scheduler{}.schedule() |
                              async::then([] { return 42; }) |
                              async::periodic_n(1s, 2);
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_value_t(int)>>);
}

TEST_CASE("periodic repeats periodically", "[periodic]") {
    int var{};
    [[maybe_unused]] auto s =
        async::time_scheduler{}.schedule() | async::then([&] { ++var; }) |
        async::periodic_until(1s, [&] { return var == 2; });
    auto op = async::connect(s, receiver{[&] { var = 42; }});
    async::start(op);
    CHECK(enabled<default_domain>);
    CHECK(not async::timer_mgr::is_idle());
    async::timer_mgr::service_task();
    CHECK(var == 1);
    CHECK(not async::timer_mgr::is_idle());
    async::timer_mgr::service_task();
    CHECK(async::timer_mgr::is_idle());
    CHECK(var == 42);
}

TEST_CASE("periodic allows continue_on another scheduler", "[periodic]") {
    int var{};
    [[maybe_unused]] auto s =
        async::time_scheduler{}.schedule() |
        async::continue_on(async::fixed_priority_scheduler<0>{}) |
        async::then([&] { ++var; }) |
        async::periodic_until(1s, [&] { return var == 2; });
    auto op = async::connect(s, receiver{[&] { var = 42; }});
    async::start(op);
    CHECK(enabled<default_domain>);
    CHECK(not async::timer_mgr::is_idle());
    async::timer_mgr::service_task();
    async::task_mgr::service_tasks<0>();
    CHECK(var == 1);
    CHECK(not async::timer_mgr::is_idle());
    async::timer_mgr::service_task();
    async::task_mgr::service_tasks<0>();
    CHECK(async::timer_mgr::is_idle());
    CHECK(async::task_mgr::is_idle());
    CHECK(var == 42);
}

TEST_CASE("periodic_n repeats n times", "[periodic]") {
    int var{};
    [[maybe_unused]] auto s = async::time_scheduler{}.schedule() |
                              async::then([&] { ++var; }) |
                              async::periodic_n(1s, 2);
    auto op = async::connect(s, receiver{[&] { var = 42; }});
    async::start(op);
    CHECK(enabled<default_domain>);
    CHECK(not async::timer_mgr::is_idle());
    async::timer_mgr::service_task();
    CHECK(var == 1);
    CHECK(not async::timer_mgr::is_idle());
    async::timer_mgr::service_task();
    CHECK(var == 2);
    CHECK(not async::timer_mgr::is_idle());
    async::timer_mgr::service_task();
    CHECK(async::timer_mgr::is_idle());
    CHECK(var == 42);
}

TEST_CASE("periodic can be cancelled", "[periodic]") {
    int var{};
    stoppable_receiver r{[&] { var += 42; }};

    [[maybe_unused]] auto s = async::time_scheduler{}.schedule() |
                              async::then([&] { ++var; }) | async::periodic(1s);
    auto op = async::connect(s, r);
    async::start(op);
    async::timer_mgr::service_task();
    CHECK(var == 1);
    async::timer_mgr::service_task();
    CHECK(var == 2);
    r.request_stop();
    CHECK(async::timer_mgr::is_idle());
    CHECK(var == 44);
}

TEST_CASE("periodic sets the correct first expiration time", "[periodic]") {
    int var{};
    stoppable_receiver r{[&] { var = 42; }};

    using hal_t = timer_hal<default_domain>;
    using TP = typename hal_t::time_point_t;

    [[maybe_unused]] auto s =
        async::time_scheduler{}.schedule() | async::periodic(1s);
    auto op = async::connect(s, r);

    current_time<default_domain, TP> = TP{1s};
    calls<default_domain, TP>.clear();

    async::start(op);

    REQUIRE(calls<default_domain, TP>.size() == 1);
    CHECK(calls<default_domain, TP>[0] == TP{2s});
    CHECK(not async::timer_mgr::is_idle());

    r.request_stop();
    CHECK(async::timer_mgr::is_idle());
    CHECK(var == 42);
}

TEST_CASE("periodic sets the nth expiration time without drift", "[periodic]") {
    int var{};
    stoppable_receiver r{[&] { var = 42; }};

    using hal_t = timer_hal<default_domain>;
    using TP = typename hal_t::time_point_t;

    [[maybe_unused]] auto s =
        async::time_scheduler{}.schedule() | async::periodic(1s);
    auto op = async::connect(s, r);

    current_time<default_domain, TP> = TP{1s};
    calls<default_domain, TP>.clear();

    async::start(op);

    REQUIRE(calls<default_domain, TP>.size() == 1);
    CHECK(calls<default_domain, TP>[0] == TP{2s});
    CHECK(not async::timer_mgr::is_idle());

    current_time<default_domain, TP> = TP{2001ms};
    async::timer_mgr::service_task();
    REQUIRE(calls<default_domain, TP>.size() == 2);
    CHECK(calls<default_domain, TP>[1] == TP{3s});
    CHECK(not async::timer_mgr::is_idle());

    r.request_stop();
    CHECK(async::timer_mgr::is_idle());
    CHECK(var == 42);
}

TEST_CASE("periodic sets the nth expiration time safely", "[periodic]") {
    int var{};
    stoppable_receiver r{[&] { var = 42; }};

    using hal_t = timer_hal<default_domain>;
    using TP = typename hal_t::time_point_t;

    [[maybe_unused]] auto s =
        async::time_scheduler{}.schedule() | async::periodic(1s);
    auto op = async::connect(s, r);

    current_time<default_domain, TP> = TP{1s};
    calls<default_domain, TP>.clear();

    async::start(op);

    REQUIRE(calls<default_domain, TP>.size() == 1);
    CHECK(calls<default_domain, TP>[0] == TP{2s});
    CHECK(not async::timer_mgr::is_idle());

    current_time<default_domain, TP> = TP{3500ms};
    async::timer_mgr::service_task();
    REQUIRE(calls<default_domain, TP>.size() == 2);
    CHECK(calls<default_domain, TP>[1] == TP{3500ms});
    CHECK(not async::timer_mgr::is_idle());

    r.request_stop();
    CHECK(async::timer_mgr::is_idle());
    CHECK(var == 42);
}

namespace {
struct ops_time {
    using time_point_t = std::chrono::steady_clock::time_point;
    time_point_t tp{};
    std::chrono::milliseconds d{};
};
} // namespace

TEST_CASE("quantized provider advances to next tick", "[periodic]") {
    using hal_t = timer_hal<default_domain>;
    using TP = typename hal_t::time_point_t;

    ops_time t{TP{1s}, 1s};
    async::safe_quantized_expiry::fn<ops_time> e{&t};

    hal_t::enable();

    current_time<default_domain, TP> = TP{1001ms};
    CHECK(e.compute_expiration<hal_t>() == TP{2s});

    t.tp = TP{1s};
    current_time<default_domain, TP> = TP{2s};
    CHECK(e.compute_expiration<hal_t>() == TP{2s});

    t.tp = TP{1s};
    current_time<default_domain, TP> = TP{2001ms};
    CHECK(e.compute_expiration<hal_t>() == TP{3s});

    t.tp = TP{1s};
    current_time<default_domain, TP> = TP{3999ms};
    CHECK(e.compute_expiration<hal_t>() == TP{4s});

    t.tp = TP{1s};
    current_time<default_domain, TP> = TP{4s};
    CHECK(e.compute_expiration<hal_t>() == TP{4s});

    hal_t::disable();
}

TEST_CASE("periodic can be parameterized with a quantized provider",
          "[periodic]") {
    int var{};
    stoppable_receiver r{[&] { var = 42; }};

    using hal_t = timer_hal<default_domain>;
    using TP = typename hal_t::time_point_t;

    [[maybe_unused]] auto s =
        async::time_scheduler{}.schedule() |
        async::periodic<"", async::safe_quantized_expiry>(1s);
    auto op = async::connect(s, r);

    current_time<default_domain, TP> = TP{1s};
    calls<default_domain, TP>.clear();

    async::start(op);

    REQUIRE(calls<default_domain, TP>.size() == 1);
    CHECK(calls<default_domain, TP>[0] == TP{2s});
    CHECK(not async::timer_mgr::is_idle());

    current_time<default_domain, TP> = TP{5500ms};
    async::timer_mgr::service_task();
    REQUIRE(calls<default_domain, TP>.size() == 2);
    CHECK(calls<default_domain, TP>[1] == TP{6s});
    CHECK(not async::timer_mgr::is_idle());

    r.request_stop();
    CHECK(async::timer_mgr::is_idle());
    CHECK(var == 42);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    constexpr auto signal(auto &&...) {
        if constexpr (std::same_as<async::debug::tag_of<Ctx>,
                                   async::periodic_t>) {
            static_assert(not boost::mp11::mp_empty<
                          async::debug::children_of<Ctx>>::value);
            debug_events.push_back(
                fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
        }
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("periodic_until can be debugged", "[periodic]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::time_scheduler{}.schedule() |
             async::periodic_until(1s, [] { return true; });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    async::timer_mgr::service_task();
    CHECK(async::timer_mgr::is_idle());
    CHECK(debug_events == std::vector{"op periodic_until start"s,
                                      "op periodic_until eval_predicate"s,
                                      "op periodic_until set_value"s});
}

TEST_CASE("periodic_n can be debugged", "[periodic]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::time_scheduler{}.schedule() | async::periodic_n(1s, 0);
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    async::timer_mgr::service_task();
    CHECK(async::timer_mgr::is_idle());
    CHECK(debug_events == std::vector{"op periodic_n start"s,
                                      "op periodic_n eval_predicate"s,
                                      "op periodic_n set_value"s});
}

TEST_CASE("periodic can be debugged", "[periodic]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::time_scheduler{}.schedule() | async::periodic(1s);
    stoppable_receiver r{[] {}};
    auto op = async::connect(
        s, with_env{r, async::prop{async::get_debug_interface_t{},
                                   async::debug::named_interface<"op">{}}});

    async::start(op);
    r.request_stop();
    CHECK(async::timer_mgr::is_idle());
    CHECK(debug_events ==
          std::vector{"op periodic start"s, "op periodic set_stopped"s});
}

TEST_CASE("periodic can be named and debugged", "[periodic]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::time_scheduler{}.schedule() |
             async::periodic<"periodic_name">(1s);
    stoppable_receiver r{[] {}};
    auto op = async::connect(
        s, with_env{r, async::prop{async::get_debug_interface_t{},
                                   async::debug::named_interface<"op">{}}});

    async::start(op);
    r.request_stop();
    CHECK(async::timer_mgr::is_idle());
    CHECK(debug_events == std::vector{"op periodic_name start"s,
                                      "op periodic_name set_stopped"s});
}
