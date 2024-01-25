#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/just.hpp>
#include <async/let_value.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/tags.hpp>
#include <async/then.hpp>
#include <async/variant_sender.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("let_value", "[let_value]") {
    int value{};

    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    auto l = async::let_value(s, [] { return async::just(42); });
    auto op = async::connect(l, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_value error", "[let_value]") {
    int value{};

    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    auto l = async::let_value(s, [] { return async::just_error(42); });
    auto op = async::connect(l, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_value stopped", "[let_value]") {
    int value{};

    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    auto l = async::let_value(s, [] { return async::just_stopped(); });
    auto op = async::connect(l, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_value advertises what it sends", "[let_value]") {
    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    [[maybe_unused]] auto l =
        async::let_value(s, [] { return async::just(42); });
    static_assert(async::sender_of<decltype(l), async::set_value_t(int)>);
}

TEST_CASE("let_value advertises errors", "[let_value]") {
    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    [[maybe_unused]] auto l =
        async::let_value(s, [] { return async::just_error(42); });
    static_assert(async::sender_of<decltype(l), async::set_error_t(int)>);
}

TEST_CASE("let_value advertises stopped", "[let_value]") {
    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    [[maybe_unused]] auto l =
        async::let_value(s, [] { return async::just_stopped(); });
    static_assert(async::sender_of<decltype(l), async::set_stopped_t()>);
}

TEST_CASE("let_value is pipeable", "[let_value]") {
    int value{};

    auto sched = async::inline_scheduler{};
    auto l =
        sched.schedule() | async::let_value([] { return async::just(42); });
    auto op = async::connect(l, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only value", "[let_value]") {
    int value{};

    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    auto l = async::let_value(s, [] { return async::just(move_only{42}); });
    static_assert(async::singleshot_sender<decltype(l), universal_receiver>);
    auto op = async::connect(std::move(l),
                             receiver{[&](auto &&mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_value with variant", "[let_value]") {
    int value{};

    auto s = async::inline_scheduler{}.schedule() |
             async::then([&] { return value; }) | async::let_value([](auto i) {
                 return async::make_variant_sender(
                     i % 2 == 0, [=] { return async::just(i / 2); },
                     [=] { return async::just(i * 3 + 1); });
             });

    static_assert(
        std::is_same_v<async::completion_signatures_of_t<decltype(s)>,
                       async::completion_signatures<async::set_value_t(int)>>);

    auto r = receiver{[&](auto i) { value = i; }};
    value = 6;
    async::start(async::connect(s, r));
    CHECK(value == 3);
    async::start(async::connect(s, r));
    CHECK(value == 10);
    async::start(async::connect(s, r));
    CHECK(value == 5);
    async::start(async::connect(s, r));
    CHECK(value == 16);
    async::start(async::connect(s, r));
    CHECK(value == 8);
    async::start(async::connect(s, r));
    CHECK(value == 4);
    async::start(async::connect(s, r));
    CHECK(value == 2);
    async::start(async::connect(s, r));
    CHECK(value == 1);
}

TEST_CASE("let_value propagates error (order 1)", "[let_value]") {
    int value{};

    auto s = async::just_error(41) |
             async::upon_error([](auto i) { return ++i; }) |
             async::let_value([] { return async::just(17); });
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_value propagates error (order 2)", "[let_value]") {
    int value{};

    auto s = async::just_error(41) |
             async::let_value([] { return async::just(17); }) |
             async::upon_error([](auto i) { return ++i; });
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_value propagates stopped (order 1)", "[let_value]") {
    int value{};

    auto s = async::just_stopped() | async::upon_stopped([&] { value = 41; }) |
             async::let_value([] { return async::just(17); });
    auto op = async::connect(s, stopped_receiver{[&] { ++value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_value propagates stopped (order 2)", "[let_value]") {
    int value{};

    auto s = async::just_stopped() |
             async::let_value([] { return async::just(17); }) |
             async::upon_stopped([&] { value = 41; });
    auto op = async::connect(s, stopped_receiver{[&] { ++value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_value propagates forwarding queries to its child environment",
          "[let_value]") {
    auto s = custom_sender{};
    CHECK(get_fwd(async::get_env(s)) == 42);

    auto l = async::let_value(s, [] { return async::just(17); });
    CHECK(get_fwd(async::get_env(l)) == 42);
}
