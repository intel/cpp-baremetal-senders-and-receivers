#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/just.hpp>
#include <async/let_value.hpp>
#include <async/read_env.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/start_on.hpp>
#include <async/stop_token.hpp>
#include <async/sync_wait.hpp>
#include <async/then.hpp>
#include <async/type_traits.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("read_env advertises what it sends", "[read_env]") {
    [[maybe_unused]] auto r = stoppable_receiver{[] {}};
    using E = async::env_of_t<decltype(r)>;
    using ST = async::stop_token_of_t<E>;
    static_assert(
        async::sender_of<decltype(async::read_env(async::get_stop_token_t{})),
                         async::set_value_t(ST const &), E>);
    static_assert(std::is_same_v<
                  async::completion_signatures_of_t<decltype(async::read_env(
                      async::get_stop_token_t{}))>,
                  async::completion_signatures<async::set_value_t(
                      async::never_stop_token)>>);
}

TEST_CASE("read_env sends a value", "[read_env]") {
    int value{};
    auto r = stoppable_receiver{[&] { ++value; }};

    auto s = async::get_stop_token() |
             async::then([&](async::inplace_stop_token) { value = 42; });
    auto op = async::connect(s, r);
    async::start(op);
    CHECK(value == 43);
}

TEST_CASE("read_env with sync_wait", "[read_env]") {
    auto s = async::get_scheduler() | async::let_value([&](auto sched) {
                 return async::start_on(sched, async::just(42));
             });

    auto value = s | async::sync_wait();
    REQUIRE(value.has_value());
    CHECK(get<0>(*value) == 42);
}
