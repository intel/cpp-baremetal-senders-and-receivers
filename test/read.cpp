#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/just.hpp>
#include <async/on.hpp>
#include <async/read.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/stop_token.hpp>
#include <async/sync_wait.hpp>
#include <async/tags.hpp>
#include <async/then.hpp>
#include <async/type_traits.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("read advertises what it sends", "[read]") {
    [[maybe_unused]] auto r = stoppable_receiver{[] {}};
    using E = async::env_of_t<decltype(r)>;
    using ST = async::stop_token_of_t<E>;
    static_assert(
        async::sender_of<decltype(async::read(async::get_stop_token_t{})),
                         async::set_value_t(ST), E>);
    static_assert(
        std::is_same_v<async::completion_signatures_of_t<decltype(async::read(
                           async::get_stop_token_t{}))>,
                       async::completion_signatures<async::set_value_t(
                           async::never_stop_token)>>);
}

TEST_CASE("read sends a value", "[read]") {
    int value{};
    auto r = stoppable_receiver{[&] { ++value; }};

    auto s = async::get_stop_token() |
             async::then([&](async::in_place_stop_token) { value = 42; });
    auto op = async::connect(s, r);
    async::start(op);
    CHECK(value == 43);
}

TEST_CASE("read with sync_wait", "[read]") {
    auto s = async::get_scheduler() | async::let_value([&](auto sched) {
                 return async::on(sched, async::just(42));
             });

    auto value = s | async::sync_wait();
    REQUIRE(value.has_value());
    CHECK(get<0>(*value) == 42);
}
