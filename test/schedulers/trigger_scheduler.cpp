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

#include <boost/mp11/list.hpp>
#include <catch2/catch_template_test_macros.hpp>
#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <atomic>
#include <concepts>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

namespace {
template <typename T>
constexpr auto type_string =
    stdx::ct_string<stdx::type_as_string<T>().size() + 1>{
        stdx::type_as_string<T>()};
} // namespace

TEST_CASE("trigger_scheduler fulfils concept", "[trigger_scheduler]") {
    STATIC_CHECK(async::scheduler<async::trigger_scheduler<"name">>);
}

TEMPLATE_TEST_CASE("trigger_scheduler sender advertises nothing",
                   "[trigger_scheduler]", decltype([] {})) {
    using expected_t = async::completion_signatures<async::set_value_t(),
                                                    async::set_stopped_t()>;
    using actual_t = async::completion_signatures_of_t<
        decltype(async::trigger_scheduler<type_string<TestType>>::schedule())>;
    STATIC_CHECK(std::same_as<actual_t, expected_t>);
}

TEMPLATE_TEST_CASE(
    "trigger_scheduler sender advertises args that will be used to trigger it",
    "[trigger_scheduler]", decltype([] {})) {
    STATIC_CHECK(
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
    STATIC_CHECK(std::same_as<decltype(cs), S>);
}

TEMPLATE_TEST_CASE("trigger_scheduler schedules tasks", "[trigger_scheduler]",
                   decltype([] {})) {
    constexpr auto name = type_string<TestType>;
    auto s = async::trigger_scheduler<name>{};
    int var{};
    async::sender auto sndr =
        async::start_on(s, async::just_result_of([&] { var = 42; }));
    auto op = async::connect(sndr, universal_receiver{});

    async::triggers<stdx::cts_t<name>>.run();
    CHECK(var == 0);

    async::start(op);
    async::triggers<stdx::cts_t<name>>.run();
    CHECK(var == 42);
    CHECK(async::triggers<stdx::cts_t<name>>.empty());
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
    CHECK(not async::triggers<stdx::cts_t<name>, int>.empty());
    async::run_triggers<name>(42);
    CHECK(var == 42);
    CHECK(async::triggers<stdx::cts_t<name>, int>.empty());
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
    CHECK(async::triggers<stdx::cts_t<name>>.empty());
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
    CHECK(async::triggers<stdx::cts_t<name>>.empty());
}

TEMPLATE_TEST_CASE("trigger_scheduler is cancellable by cancel_triggers",
                   "[trigger_scheduler]", decltype([] {})) {
    constexpr auto name = type_string<TestType>;
    auto s = async::trigger_scheduler<name>{};
    int var{};
    async::sender auto sndr =
        async::start_on(s, async::just_result_of([&] { var = 42; }));
    auto r = stopped_receiver{[&] { var = 17; }};
    auto op = async::connect(sndr, r);

    async::start(op);
    async::cancel_triggers<name>();
    CHECK(var == 17);
    CHECK(async::triggers<stdx::cts_t<name>>.empty());
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
    async::run_triggers<"client">();
    CHECK(var == 1);
    async::run_triggers<"server">();
    CHECK(var == 2);
    async::run_triggers<"client">();
    CHECK(var == 86);
    CHECK(async::triggers<stdx::cts_t<"client">>.empty());
    CHECK(async::triggers<stdx::cts_t<"server">>.empty());
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    std::mutex m;

    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    auto signal(auto &&...) {
        if constexpr (std::is_same_v<async::debug::tag_of<Ctx>,
                                     async::trigger_scheduler_sender_t>) {
            std::lock_guard lock{m};
            STATIC_CHECK(
                boost::mp11::mp_empty<async::debug::children_of<Ctx>>::value);
            debug_events.push_back(
                fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
        }
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
    async::run_triggers<"sched">();
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

TEMPLATE_TEST_CASE("trigger_scheduler runs all triggers by default",
                   "[trigger_scheduler]", decltype([] {})) {
    constexpr auto name = type_string<TestType>;
    auto s = async::trigger_scheduler<name>{};

    int var1{};
    async::sender auto sndr1 =
        async::start_on(s, async::just_result_of([&] { var1 = 42; }));
    CHECK(async::start_detached(sndr1));

    int var2{};
    async::sender auto sndr2 =
        async::start_on(s, async::just_result_of([&] { var2 = 42; }));
    CHECK(async::start_detached(sndr2));

    async::run_triggers<name>();
    CHECK(var1 == 42);
    CHECK(var2 == 42);
    CHECK(async::triggers<stdx::cts_t<name>>.empty());
}

TEMPLATE_TEST_CASE("trigger_scheduler can be policized for multi-stimulus",
                   "[trigger_scheduler]", decltype([] {})) {
    constexpr auto name = type_string<TestType>;
    auto s = async::trigger_scheduler<name>{};

    int var1{};
    async::sender auto sndr1 =
        async::start_on(s, async::just_result_of([&] { var1 = 42; }));
    CHECK(async::start_detached(sndr1));

    int var2{};
    async::sender auto sndr2 =
        async::start_on(s, async::just_result_of([&] { var2 = 17; }));
    CHECK(async::start_detached(sndr2));

    async::run_one_trigger<name>();
    CHECK(var1 == 42);
    CHECK(not async::triggers<stdx::cts_t<name>>.empty());

    async::run_one_trigger<name>();
    CHECK(var2 == 17);
    CHECK(async::triggers<stdx::cts_t<name>>.empty());
}

TEMPLATE_TEST_CASE("triggered task can start a new task on the same trigger",
                   "[trigger_scheduler]", decltype([] {})) {
    constexpr auto name = type_string<TestType>;
    auto s = async::trigger_scheduler<name>{};

    auto start = [&](auto f) {
        async::sender auto sndr = async::start_on(s, async::just_result_of(f));
        CHECK(async::start_detached(sndr));
    };

    int var{};
    start([&] {
        if (var++ == 0) {
            start([&] { var = 42; });
        }
    });

    async::run_one_trigger<name>();
    CHECK(var == 1);
    CHECK(not async::triggers<stdx::cts_t<name>>.empty());

    async::run_one_trigger<name>();
    CHECK(var == 42);
    CHECK(async::triggers<stdx::cts_t<name>>.empty());
}

TEST_CASE("thread safety for immediate execution", "[trigger_scheduler]") {
    auto s = async::trigger_scheduler<"rqp_imm">{};

    std::atomic<bool> ready1{};
    std::atomic<bool> ready2{};
    int var1{};
    int var2{};

    auto start = [&](auto f) {
        async::sender auto sndr = async::start_on(s, async::just_result_of(f));
        CHECK(async::start_detached(sndr));
    };

    auto t1 = std::thread{[&] {
        start([&] {
            var1 = 17;
            start([&] { ++var1; });
        });
        ready1 = true;
        ready1.notify_one();
    }};
    auto t2 = std::thread{[&] {
        start([&] { var2 = 42; });
        ready2 = true;
        ready2.notify_one();
    }};

    ready1.wait(false);
    async::run_triggers<"rqp_imm", async::requeue_policy::immediate>();

    ready2.wait(false);
    async::run_triggers<"rqp_imm", async::requeue_policy::immediate>();

    t1.join();
    t2.join();
    CHECK(var1 == 18);
    CHECK(var2 == 42);
    CHECK(async::triggers<stdx::cts_t<"rqp_imm">>.empty());
}
