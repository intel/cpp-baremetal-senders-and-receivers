#include "detail/common.hpp"
#include "detail/debug_handler.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/just_result_of.hpp>
#include <async/retry.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/sequence.hpp>
#include <async/start_on.hpp>
#include <async/then.hpp>
#include <async/variant_sender.hpp>
#include <async/when_all.hpp>

#include <catch2/catch_test_macros.hpp>

#include <concepts>
#include <string>
#include <vector>

TEST_CASE("retry advertises what it sends", "[retry]") {
    [[maybe_unused]] auto s = async::just(42) | async::retry();
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_value_t(int)>>);
}

TEST_CASE("retry advertises what it sends (nothing!)", "[retry]") {
    [[maybe_unused]] auto s = async::just_error(42) | async::retry();
    STATIC_REQUIRE(std::same_as<async::completion_signatures_of_t<decltype(s)>,
                                async::completion_signatures<>>);
}

TEST_CASE("retry advertises sending stopped", "[retry]") {
    int var{};
    [[maybe_unused]] auto s =
        async::make_variant_sender(
            var == 0, [] { return async::just_error(42); },
            [] { return async::just_stopped(); }) |
        async::retry();
    [[maybe_unused]] auto r = stoppable_receiver([] {});
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<
                         decltype(s), async::env_of_t<decltype(r)>>,
                     async::completion_signatures<async::set_stopped_t()>>);
}

TEST_CASE("retry advertises sending stopped even when its wrapped sender is "
          "not stoppable",
          "[retry]") {
    [[maybe_unused]] auto s = async::just() | async::retry();
    [[maybe_unused]] auto r = stoppable_receiver([] {});
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<
                         decltype(s), async::env_of_t<decltype(r)>>,
                     async::completion_signatures<async::set_stopped_t(),
                                                  async::set_value_t()>>);
}

TEST_CASE("retry propagates forwarding queries to its child environment",
          "[retry]") {
    auto s = custom_sender{};
    CHECK(get_fwd(async::get_env(s)) == 42);

    auto r = async::retry(s);
    CHECK(get_fwd(async::get_env(r)) == 42);
}

TEST_CASE("retry retries on error", "[retry]") {
    int var{};

    auto sub = async::just() | async::sequence([&] {
                   return async::make_variant_sender(
                       ++var == 2, [] { return async::just(42); },
                       [] { return async::just_error(17); });
               });
    auto s = sub | async::retry();
    auto op = async::connect(s, receiver{[&](auto i) { var += i; }});
    async::start(op);
    CHECK(var == 44);
}

TEST_CASE("retry_until advertises what it sends", "[retry]") {
    [[maybe_unused]] auto s =
        async::just_error(42) | async::retry_until([](auto) { return true; });
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_error_t(int)>>);
}

TEST_CASE("retry_until retries on error", "[retry]") {
    int var{};

    auto sub = async::just() | async::sequence([&] {
                   ++var;
                   return async::just_error(17);
               });
    auto s = sub | async::retry_until([&](auto i) { return i + var == 20; });
    auto op = async::connect(s, receiver{[] {}});
    async::start(op);
    CHECK(var == 3);
}

TEST_CASE("retry can be cancelled", "[retry]") {
    int var{};
    stoppable_receiver r{[&] { var += 42; }};

    auto sub = async::just() | async::sequence([&] {
                   if (++var == 2) {
                       r.request_stop();
                   }
                   return async::just_error(17);
               });
    auto s = async::when_all(sub, stoppable_just()) | async::retry();
    auto op = async::connect(s, r);
    async::start(op);
    CHECK(var == 44);
}

TEST_CASE("retry can be cancelled even for a sender that cannot", "[retry]") {
    int var{};
    stoppable_receiver r{[&] { var += 42; }};

    auto sub = async::just_error_result_of([&] {
        if (++var == 2) {
            r.request_stop();
        }
        return 42;
    });
    auto s = sub | async::retry();
    auto op = async::connect(s, r);
    async::start(op);
    CHECK(var == 44);
}

TEST_CASE("retry may complete synchronously", "[retry]") {
    int var{};
    auto sub = async::just() | async::then([&] { ++var; });
    auto s = async::retry_until(sub, [&](auto i) { return i == 42; });
    STATIC_REQUIRE(async::synchronous<decltype(s)>);
}

TEST_CASE("retry may not complete synchronously", "[retry]") {
    int var{};
    auto sub =
        async::thread_scheduler<>::schedule() | async::then([&] { ++var; });
    auto s = async::retry_until(sub, [&](auto i) { return i == 42; });
    STATIC_REQUIRE(not async::synchronous<decltype(s)>);
}

TEST_CASE("retry op state may be synchronous", "[retry]") {
    int var{};
    auto sub = async::just() | async::then([&] {
                   ++var;
                   return var;
               });
    auto s = async::retry_until(sub, [&](auto i) { return i == 42; });
    auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(async::synchronous<decltype(op)>);
}

TEST_CASE("retry op state may not be synchronous", "[retry]") {
    int var{};
    auto sub = async::thread_scheduler<>::schedule() | async::then([&] {
                   ++var;
                   return var;
               });
    auto s = async::retry_until(sub, [&](auto i) { return i == 42; });
    auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(not async::synchronous<decltype(op)>);
}

template <>
inline auto async::injected_debug_handler<> = debug_handler<async::retry_t>{};

TEST_CASE("retry_until can be debugged", "[retry]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s =
        async::just_error(42) | async::retry_until([](auto) { return true; });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    CHECK(debug_events == std::vector{"op retry_until start"s,
                                      "op retry_until eval_predicate"s,
                                      "op retry_until set_error"s});
}

TEST_CASE("retry can be debugged", "[retry]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just(42) | async::retry();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    CHECK(debug_events ==
          std::vector{"op retry start"s, "op retry set_value"s});
}

TEST_CASE("retry can be named and debugged", "[retry]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just(42) | async::retry<"retry_name">();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    CHECK(debug_events ==
          std::vector{"op retry_name start"s, "op retry_name set_value"s});
}

TEST_CASE("retry_until with a synchronous sender does not cause stack overflow",
          "[retry]") {
    int var{};

    auto sub = async::just_error_result_of([&] {
        ++var;
        return var;
    });
    auto s = sub | async::retry_until([&](auto i) { return i == 1'000'000; });
    auto op = async::connect(s, error_receiver{[&](auto i) {
                                 CHECK(i == 1'000'000);
                                 var = -1;
                             }});
    async::start(op);
    CHECK(var == -1);
}
