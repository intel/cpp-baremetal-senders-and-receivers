#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/continue_on.hpp>
#include <async/debug.hpp>
#include <async/just_result_of.hpp>
#include <async/schedulers/trigger_manager.hpp>
#include <async/schedulers/trigger_scheduler.hpp>
#include <async/start_detached.hpp>
#include <async/start_on.hpp>
#include <async/then.hpp>

#include <stdx/ct_conversions.hpp>
#include <stdx/ct_format.hpp>
#include <stdx/ct_string.hpp>

#include <catch2/catch_template_test_macros.hpp>
#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <concepts>
#include <string>
#include <vector>

namespace {
template <typename T>
constexpr auto type_string =
    stdx::ct_string<stdx::type_as_string<T>().size() + 1>{
        stdx::type_as_string<T>()};
} // namespace

TEST_CASE("trigger_scheduler fulfils concept", "[trigger_scheduler]") {
    static_assert(async::scheduler<async::trigger_scheduler<"name">>);
}

TEMPLATE_TEST_CASE("trigger_scheduler sender advertises nothing",
                   "[trigger_scheduler]", decltype([] {})) {
    static_assert(async::sender_of<decltype(async::trigger_scheduler<
                                            type_string<TestType>>::schedule()),
                                   async::set_value_t()>);
}

TEMPLATE_TEST_CASE(
    "trigger_scheduler sender advertises args that will be used to trigger it",
    "[trigger_scheduler]", decltype([] {})) {
    static_assert(
        async::sender_of<decltype(async::trigger_scheduler<
                                  type_string<TestType>, int>::schedule()),
                         async::set_value_t(int const &)>);
}

TEMPLATE_TEST_CASE(
    "sender has the trigger_scheduler as its completion scheduler",
    "[trigger_scheduler]", decltype([] {})) {
    using S = async::trigger_scheduler<type_string<TestType>>;
    auto s = S::schedule();
    auto cs =
        async::get_completion_scheduler<async::set_value_t>(async::get_env(s));
    static_assert(std::same_as<decltype(cs), S>);
}

TEMPLATE_TEST_CASE("trigger_scheduler schedules tasks", "[trigger_scheduler]",
                   decltype([] {})) {
    constexpr auto name = type_string<TestType>;
    auto s = async::trigger_scheduler<name>{};
    int var{};
    async::sender auto sndr =
        async::start_on(s, async::just_result_of([&] { var = 42; }));
    auto op = async::connect(sndr, universal_receiver{});

    async::triggers<name>.run();
    CHECK(var == 0);

    async::start(op);
    async::triggers<name>.run();
    CHECK(var == 42);
    CHECK(async::triggers<name>.empty());
}

TEMPLATE_TEST_CASE("trigger_scheduler can be triggered with arguments",
                   "[trigger_scheduler]", decltype([] {})) {
    constexpr auto name = type_string<TestType>;
    auto s = async::trigger_scheduler<name, int>{};
    int var{};
    async::sender auto sndr =
        s.schedule() | async::then([&](auto x) { var = x; });
    auto op = async::connect(sndr, universal_receiver{});

    async::run_triggers<name>(42);
    CHECK(var == 0);

    async::start(op);
    CHECK(not async::triggers<name, int>.empty());
    async::run_triggers<name>(42);
    CHECK(var == 42);
    CHECK(async::triggers<name, int>.empty());
}

TEMPLATE_TEST_CASE("trigger_scheduler is cancellable before start",
                   "[trigger_scheduler]", decltype([] {})) {
    constexpr auto name = type_string<TestType>;
    auto s = async::trigger_scheduler<name>{};
    int var{};
    async::sender auto sndr =
        async::start_on(s, async::just_result_of([&] { var = 42; }));
    auto r = stoppable_receiver{[&] { var = 17; }};
    auto op = async::connect(sndr, r);

    r.request_stop();
    async::start(op);
    CHECK(var == 17);
    CHECK(async::triggers<name>.empty());
}

TEMPLATE_TEST_CASE("trigger_scheduler is cancellable after start",
                   "[trigger_scheduler]", decltype([] {})) {
    constexpr auto name = type_string<TestType>;
    auto s = async::trigger_scheduler<name>{};
    int var{};
    async::sender auto sndr =
        async::start_on(s, async::just_result_of([&] { var = 42; }));
    auto r = stoppable_receiver{[&] { var = 17; }};
    auto op = async::connect(sndr, r);

    async::start(op);
    r.request_stop();
    CHECK(var == 17);
    CHECK(async::triggers<name>.empty());
}

TEST_CASE("request and response", "[trigger_scheduler]") {
    int var{};

    using client_context = async::trigger_scheduler<"client">;
    using server_context = async::trigger_scheduler<"server">;

    auto s = client_context::schedule() //
             | async::then([&] {
                   ++var;
                   return 42;
               })                                   //
             | async::continue_on(server_context{}) //
             | async::then([&](auto i) {
                   ++var;
                   return i * 2;
               })                                   //
             | async::continue_on(client_context{}) //
             | async::then([&](auto i) { var += i; });
    CHECK(async::start_detached(s));

    CHECK(var == 0);
    async::triggers<"client">.run();
    CHECK(var == 1);
    async::triggers<"server">.run();
    CHECK(var == 2);
    async::triggers<"client">.run();
    CHECK(var == 86);
    CHECK(async::triggers<"client">.empty());
    CHECK(async::triggers<"server">.empty());
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string L, stdx::ct_string S,
              typename Ctx>
    constexpr auto signal(auto &&...) {
        debug_events.push_back(fmt::format("{} {} {}", C, L, S));
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("trigger_scheduler can be debugged", "[trigger_scheduler]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::trigger_scheduler<"sched">::schedule();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    CHECK(debug_events == std::vector{"op sched start"s});
    async::triggers<"sched">.run();
    CHECK(debug_events ==
          std::vector{"op sched start"s, "op sched set_value"s});
}

TEST_CASE("trigger_scheduler produces set_stopped debug signal",
          "[trigger_scheduler]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto stop = async::inplace_stop_source{};
    auto s = async::trigger_scheduler<"sched">::schedule();
    auto r = with_env{
        universal_receiver{},
        async::env{async::prop{async::get_debug_interface_t{},
                               async::debug::named_interface<"op">{}},
                   async::prop{async::get_stop_token_t{}, stop.get_token()}}};
    auto op = async::connect(s, r);

    async::start(op);
    CHECK(debug_events == std::vector{"op sched start"s});
    stop.request_stop();
    CHECK(debug_events ==
          std::vector{"op sched start"s, "op sched set_stopped"s});
}
