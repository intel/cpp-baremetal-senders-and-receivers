#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/just.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/start_on.hpp>
#include <async/when_all.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("start_on", "[start_on]") {
    int value{};

    auto s = async::start_on(async::inline_scheduler{}, async::just(42));
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("start_on error", "[start_on]") {
    int value{};

    auto s = async::start_on(async::inline_scheduler{}, async::just_error(42));
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("start_on advertises what it sends", "[start_on]") {
    [[maybe_unused]] auto s =
        async::start_on(async::inline_scheduler{}, async::just(42));
    static_assert(async::sender_of<decltype(s), async::set_value_t(int)>);
}

TEST_CASE("start_on advertises errors", "[start_on]") {
    [[maybe_unused]] auto s =
        async::start_on(async::inline_scheduler{}, async::just_error(42));
    static_assert(async::sender_of<decltype(s), async::set_error_t(int)>);
}

TEST_CASE("move-only value", "[start_on]") {
    int value{};

    auto s =
        async::start_on(async::inline_scheduler{}, async::just(move_only{42}));
    static_assert(async::singleshot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(std::move(s),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("singleshot sender first", "[start_on]") {
    int value{};

    auto s = async::start_on(singleshot_scheduler{}, async::just(42));
    static_assert(async::singleshot_sender<decltype(s), universal_receiver>);
    auto op =
        async::connect(std::move(s), receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("start_on advertises what it sends according to the receiver",
          "[start_on]") {
    [[maybe_unused]] auto s =
        async::start_on(async::inline_scheduler{}, async::when_all());
    static_assert(not async::sender_of<decltype(s), async::set_stopped_t()>);

    [[maybe_unused]] auto r = stoppable_receiver{[] {}};
    static_assert(async::sender_of<decltype(s), async::set_stopped_t(),
                                   async::env_of_t<decltype(r)>>);
}
