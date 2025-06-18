#include "detail/common.hpp"
#include "detail/debug_handler.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/just_result_of.hpp>
#include <async/repeat.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/schedulers/trigger_scheduler.hpp>
#include <async/sequence.hpp>
#include <async/start_on.hpp>
#include <async/then.hpp>
#include <async/variant_sender.hpp>
#include <async/when_all.hpp>

#include <catch2/catch_test_macros.hpp>

#include <concepts>
#include <string>
#include <vector>

TEST_CASE("repeat advertises what it sends", "[repeat]") {
    [[maybe_unused]] auto s = async::just(42) | async::repeat();
    STATIC_REQUIRE(std::same_as<async::completion_signatures_of_t<decltype(s)>,
                                async::completion_signatures<>>);
}

TEST_CASE("repeat advertises sending error/stopped", "[repeat]") {
    int var{};
    [[maybe_unused]] auto s =
        async::make_variant_sender(
            var == 0, [] { return async::just_error(42); },
            [] { return async::just_stopped(); }) |
        async::repeat();
    [[maybe_unused]] auto r = stoppable_receiver([] {});
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<
                         decltype(s), async::env_of_t<decltype(r)>>,
                     async::completion_signatures<async::set_error_t(int),
                                                  async::set_stopped_t()>>);
}

TEST_CASE("repeat propagates forwarding queries to its child environment",
          "[repeat]") {
    auto s = custom_sender{};
    CHECK(get_fwd(async::get_env(s)) == 42);

    auto r = async::repeat(s);
    CHECK(get_fwd(async::get_env(r)) == 42);
}

TEST_CASE("repeat repeats", "[repeat]") {
    int var{};

    auto sub = async::just() | async::sequence([&] {
                   return async::make_variant_sender(
                       ++var == 2, [] { return async::just_error(42); },
                       [] { return async::just(17); });
               });
    auto s = sub | async::repeat();
    auto op = async::connect(s, error_receiver{[&](auto i) { var += i; }});
    async::start(op);
    CHECK(var == 44);
}

TEST_CASE("repeat_until advertises what it sends", "[repeat]") {
    [[maybe_unused]] auto s =
        async::just(42) | async::repeat_until([](auto) { return true; });
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_value_t(int)>>);
}

TEST_CASE("repeat_n advertises what it sends", "[repeat]") {
    [[maybe_unused]] auto s = async::just(42) | async::repeat_n(2);
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_value_t(int)>>);
}

TEST_CASE("repeat_n repeats n times", "[repeat]") {
    int var{};

    auto sub = async::just() | async::sequence([&] {
                   ++var;
                   return async::just(42);
               });
    auto s = sub | async::repeat_n(1);
    auto op = async::connect(s, receiver{[&](auto i) { var += i; }});
    async::start(op);
    CHECK(var == 44);
}

TEST_CASE("repeat_until completes when true", "[repeat]") {
    int var{};

    auto sub = async::just() | async::sequence([&] {
                   ++var;
                   return async::just(42);
               });
    auto s = async::repeat_until(sub, [&](auto i) { return i == 42; });
    auto op = async::connect(s, receiver{[&](auto i) { var += i; }});
    async::start(op);
    CHECK(var == 43);
}

TEST_CASE("repeat_until is pipeable", "[repeat]") {
    int var{};

    auto sub = async::just() | async::sequence([&] {
                   ++var;
                   return async::just(42);
               });
    auto s = sub | async::repeat_until([&](auto i) { return i == 42; });
    auto op = async::connect(s, receiver{[&](auto i) { var += i; }});
    async::start(op);
    CHECK(var == 43);
}

TEST_CASE("repeat can be cancelled", "[repeat]") {
    int var{};
    stoppable_receiver r{[&] { var += 42; }};

    auto sub = async::just() | async::sequence([&] {
                   if (++var == 2) {
                       r.request_stop();
                   }
                   return async::just(17);
               });
    auto s = async::when_all(sub, stoppable_just()) | async::repeat();
    auto op = async::connect(s, r);
    async::start(op);
    CHECK(var == 44);
}

TEST_CASE("repeat can be cancelled even for a sender that cannot", "[repeat]") {
    int var{};
    stoppable_receiver r{[&] { var += 42; }};

    auto sub = async::just_result_of([&] {
        if (++var == 2) {
            r.request_stop();
        }
    });
    auto s = sub | async::repeat();
    auto op = async::connect(s, r);
    async::start(op);
    CHECK(var == 44);
}

TEST_CASE("repeat may complete synchronously", "[repeat]") {
    int var{};
    auto sub = async::just() | async::then([&] { ++var; });
    auto s = async::repeat_until(sub, [&](auto i) { return i == 42; });
    STATIC_REQUIRE(async::synchronous<decltype(s)>);
}

TEST_CASE("repeat may not complete synchronously", "[repeat]") {
    int var{};
    auto sub =
        async::thread_scheduler<>::schedule() | async::then([&] { ++var; });
    auto s = async::repeat_until(sub, [&](auto i) { return i == 42; });
    STATIC_REQUIRE(not async::synchronous<decltype(s)>);
}

TEST_CASE("repeat op state may be synchronous", "[repeat]") {
    int var{};
    auto sub = async::just() | async::then([&] {
                   ++var;
                   return var;
               });
    auto s = async::repeat_until(sub, [&](auto i) { return i == 42; });
    auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(async::synchronous<decltype(op)>);
}

TEST_CASE("repeat op state may not be synchronous", "[repeat]") {
    int var{};
    auto sub = async::thread_scheduler<>::schedule() | async::then([&] {
                   ++var;
                   return var;
               });
    auto s = async::repeat_until(sub, [&](auto i) { return i == 42; });
    auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(not async::synchronous<decltype(op)>);
}

template <>
inline auto async::injected_debug_handler<> = debug_handler<async::repeat_t>{};

TEST_CASE("repeat_until can be debugged", "[repeat]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s =
        async::just(42) | async::repeat_until([](auto i) { return i == 42; });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    CHECK(debug_events == std::vector{"op repeat_until start"s,
                                      "op repeat_until eval_predicate"s,
                                      "op repeat_until set_value"s});
}

TEST_CASE("repeat_n can be debugged", "[repeat]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just(42) | async::repeat_n(0);
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    CHECK(debug_events == std::vector{"op repeat_n start"s,
                                      "op repeat_n eval_predicate"s,
                                      "op repeat_n set_value"s});
}

TEST_CASE("repeat can be debugged", "[repeat]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just_error(42) | async::repeat();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    CHECK(debug_events ==
          std::vector{"op repeat start"s, "op repeat set_error"s});
}

TEST_CASE("repeat can be named and debugged", "[repeat]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just_error(42) | async::repeat<"repeat_name">();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    CHECK(debug_events ==
          std::vector{"op repeat_name start"s, "op repeat_name set_error"s});
}

TEST_CASE(
    "repeat_until with a synchronous sender does not cause stack overflow",
    "[repeat]") {
    int var{};

    auto sub = async::just_result_of([&] {
        ++var;
        return var;
    });
    auto s = sub | async::repeat_until([&](auto i) { return i == 1'000'000; });
    auto op = async::connect(s, receiver{[&](auto i) {
                                 CHECK(i == 1'000'000);
                                 var = -1;
                             }});
    async::start(op);
    CHECK(var == -1);
}

TEST_CASE("repeat with a loop function", "[repeat]") {
    int sum{};
    int var{};
    stoppable_receiver r{[&] { var += 42; }};

    auto sub =
        async::trigger_scheduler<"sched">{}.schedule() | async::then([&] {
            if (++var == 2) {
                r.request_stop();
            }
            return 17;
        });
    auto s = sub | async::repeat([&](auto n) {
                 sum += n;
                 async::run_triggers<"sched">();
             });
    auto op = async::connect(s, r);
    async::start(op);
    async::run_triggers<"sched">();

    CHECK(var == 44);
    CHECK(sum == 34);
    CHECK(async::triggers<"sched">.empty());
}
