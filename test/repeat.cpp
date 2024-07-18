#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/repeat.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/sequence.hpp>
#include <async/start_on.hpp>
#include <async/variant_sender.hpp>
#include <async/when_all.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("repeat advertises what it sends", "[repeat]") {
    [[maybe_unused]] auto s = async::just(42) | async::repeat();
    static_assert(std::same_as<async::completion_signatures_of_t<decltype(s)>,
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
    static_assert(
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
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_value_t(int)>>);
}

TEST_CASE("repeat_n advertises what it sends", "[repeat]") {
    [[maybe_unused]] auto s = async::just(42) | async::repeat_n(2);
    static_assert(
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
    only_stoppable_receiver r{[&] { var += 42; }};

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
