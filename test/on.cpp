#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/just.hpp>
#include <async/just_result_of.hpp>
#include <async/on.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/stop_token.hpp>
#include <async/tags.hpp>
#include <async/when_all.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("on", "[on]") {
    int value{};

    auto s = async::on(async::inline_scheduler{}, async::just(42));
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("on error", "[on]") {
    int value{};

    auto s = async::on(async::inline_scheduler{}, async::just_error(42));
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("on advertises what it sends", "[on]") {
    [[maybe_unused]] auto s =
        async::on(async::inline_scheduler{}, async::just(42));
    static_assert(async::sender_of<decltype(s), async::set_value_t(int)>);
}

TEST_CASE("on advertises errors", "[on]") {
    [[maybe_unused]] auto s =
        async::on(async::inline_scheduler{}, async::just_error(42));
    static_assert(async::sender_of<decltype(s), async::set_error_t(int)>);
}

TEST_CASE("move-only value", "[on]") {
    int value{};

    auto s = async::on(async::inline_scheduler{}, async::just(move_only{42}));
    static_assert(async::singleshot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(std::move(s),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(std::move(op));
    CHECK(value == 42);
}

TEST_CASE("singleshot sender first", "[on]") {
    int value{};

    auto s = async::on(singleshot_scheduler{}, async::just(42));
    static_assert(async::singleshot_sender<decltype(s), universal_receiver>);
    auto op =
        async::connect(std::move(s), receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("on advertises what it sends according to the receiver", "[on]") {
    [[maybe_unused]] auto s =
        async::on(async::inline_scheduler{}, async::when_all());
    static_assert(not async::sender_of<decltype(s), async::set_stopped_t()>);

    [[maybe_unused]] auto r = only_stoppable_receiver{[] {}};
    static_assert(async::sender_of<decltype(s), async::set_stopped_t(),
                                   async::env_of_t<decltype(r)>>);
}
