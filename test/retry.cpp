#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/inline_scheduler.hpp>
#include <async/just.hpp>
#include <async/let_value.hpp>
#include <async/on.hpp>
#include <async/retry.hpp>
#include <async/variant_sender.hpp>
#include <async/when_all.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("retry advertises what it sends", "[retry]") {
    [[maybe_unused]] auto s = async::just(42) | async::retry();
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_value_t(int)>>);
}

TEST_CASE("retry advertises what it sends (nothing!)", "[retry]") {
    [[maybe_unused]] auto s = async::just_error(42) | async::retry();
    static_assert(std::same_as<async::completion_signatures_of_t<decltype(s)>,
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
    static_assert(
        std::same_as<async::completion_signatures_of_t<
                         decltype(s), async::env_of_t<decltype(r)>>,
                     async::completion_signatures<async::set_stopped_t()>>);
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

    auto sub = async::just() | async::let_value([&] {
                   return async::make_variant_sender(
                       ++var == 2, [] { return async::just(42); },
                       [] { return async::just_error(17); });
               });
    auto s = sub | async::retry();
    auto op = async::connect(s, receiver{[&](auto i) { var += i; }});
    op.start();
    CHECK(var == 44);
}

TEST_CASE("retry_until advertises what it sends", "[retry]") {
    [[maybe_unused]] auto s =
        async::just_error(42) | async::retry_until([] { return true; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_error_t(int)>>);
}

TEST_CASE("retry_until retries on error", "[retry]") {
    int var{};

    auto sub = async::just() | async::let_value([&] {
                   ++var;
                   return async::just_error(17);
               });
    auto s = sub | async::retry_until([&] { return var == 3; });
    auto op = async::connect(s, receiver{[] {}});
    op.start();
    CHECK(var == 3);
}

TEST_CASE("retry can be cancelled", "[retry]") {
    int var{};
    only_stoppable_receiver r{[&] { var += 42; }};

    auto sub = async::just() | async::let_value([&] {
                   if (++var == 2) {
                       r.request_stop();
                   }
                   return async::just_error(17);
               });
    auto s = async::when_all(sub) | async::retry();
    auto op = async::connect(s, r);
    op.start();
    CHECK(var == 44);
}
