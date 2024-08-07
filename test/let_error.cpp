#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/just_result_of.hpp>
#include <async/let_error.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/start_on.hpp>
#include <async/then.hpp>
#include <async/variant_sender.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("let_error", "[let_error]") {
    int value{};

    auto s = async::just_error(0);
    auto l = async::let_error(s, [](auto) { return async::just(42); });
    auto op = async::connect(l, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_error error", "[let_error]") {
    int value{};

    auto s = async::just_error(0);
    auto l = async::let_error(s, [](auto) { return async::just_error(42); });
    auto op = async::connect(l, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_error stopped", "[let_error]") {
    int value{};

    auto s = async::just_error(0);
    auto l = async::let_error(s, [](auto) { return async::just_stopped(); });
    auto op = async::connect(l, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_error advertises what it sends", "[let_error]") {
    auto s = async::just_error(false);
    [[maybe_unused]] auto l =
        async::let_error(s, [](auto) { return async::just(42); });
    static_assert(async::sender_of<decltype(l), async::set_value_t(int)>);
}

TEST_CASE("let_error advertises errors", "[let_error]") {
    auto s = async::just_error(false);
    [[maybe_unused]] auto l =
        async::let_error(s, [](auto) { return async::just_error(42); });
    static_assert(async::sender_of<decltype(l), async::set_error_t(int)>);
}

TEST_CASE("let_error advertises stopped", "[let_error]") {
    auto s = async::just_error(false);
    [[maybe_unused]] auto l =
        async::let_error(s, [](auto) { return async::just_stopped(); });
    static_assert(async::sender_of<decltype(l), async::set_stopped_t()>);
}

TEST_CASE("let_error is pipeable", "[let_error]") {
    int value{};

    auto l = async::just_error(0) |
             async::let_error([](auto) { return async::just(42); });
    auto op = async::connect(l, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_error is adaptor-pipeable", "[let_error]") {
    int value{};

    auto l = async::let_error([](int) { return async::just_error(42); }) |
             async::let_error([](int i) { return async::just(i * 2); });
    auto op = async::connect(async::just_error(0) | l,
                             receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 84);
}

TEST_CASE("move-only value", "[let_error]") {
    int value{};

    auto s = async::just_error(0);
    auto l =
        async::let_error(s, [](auto) { return async::just(move_only{42}); });
    static_assert(async::singleshot_sender<decltype(l), universal_receiver>);
    auto op = async::connect(std::move(l),
                             receiver{[&](auto &&mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_error with variant", "[let_error]") {
    int value{};

    auto s = async::just_error(0) |
             async::upon_error([&](auto) { return value; }) |
             async::let_error([](auto i) {
                 return async::make_variant_sender(
                     i % 2 == 0, [=] { return async::just(i / 2); },
                     [=] { return async::just(i * 3 + 1); });
             });

    static_assert(
        std::is_same_v<async::completion_signatures_of_t<decltype(s)>,
                       async::completion_signatures<async::set_value_t(int)>>);

    auto r = receiver{[&](auto i) { value = i; }};
    value = 6;
    {
        auto o = async::connect(s, r);
        async::start(o);
    }
    CHECK(value == 3);
    {
        auto o = async::connect(s, r);
        async::start(o);
    }
    CHECK(value == 10);
    {
        auto o = async::connect(s, r);
        async::start(o);
    }
    CHECK(value == 5);
    {
        auto o = async::connect(s, r);
        async::start(o);
    }
    CHECK(value == 16);
    {
        auto o = async::connect(s, r);
        async::start(o);
    }
    CHECK(value == 8);
    {
        auto o = async::connect(s, r);
        async::start(o);
    }
    CHECK(value == 4);
    {
        auto o = async::connect(s, r);
        async::start(o);
    }
    CHECK(value == 2);
    {
        auto o = async::connect(s, r);
        async::start(o);
    }
    CHECK(value == 1);
}

TEST_CASE("let_error propagates value (order 1)", "[let_error]") {
    int value{};

    auto s = async::just(41) | async::then([](auto i) { return ++i; }) |
             async::let_error([] { return async::just(17); });
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_error propagates value (order 2)", "[let_error]") {
    int value{};

    auto s = async::just(41) |
             async::let_error([] { return async::just(17); }) |
             async::then([](auto i) { return ++i; });
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_error propagates stopped (order 1)", "[let_error]") {
    int value{};

    auto s = async::just_stopped() | async::upon_stopped([&] { value = 41; }) |
             async::let_error([] { return async::just(17); });
    auto op = async::connect(s, stopped_receiver{[&] { ++value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_error propagates stopped (order 2)", "[let_error]") {
    int value{};

    auto s = async::just_stopped() |
             async::let_error([] { return async::just(17); }) |
             async::upon_stopped([&] { value = 41; });
    auto op = async::connect(s, stopped_receiver{[&] { ++value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_error advertises pass-through completions", "[let_error]") {
    [[maybe_unused]] auto l = async::just(42) | async::let_error([](auto) {});
    static_assert(async::sender_of<decltype(l), async::set_value_t(int)>);
}

TEST_CASE("let_error can be single shot", "[let_error]") {
    [[maybe_unused]] auto l =
        async::just_error(42) |
        async::let_error([](auto i) { return async::just(move_only{i}); });
    static_assert(async::singleshot_sender<decltype(l)>);
}

TEST_CASE("let_error can be single shot with passthrough", "[let_error]") {
    [[maybe_unused]] auto l =
        async::just(move_only{42}) | async::let_error([](auto) { return 42; });
    static_assert(async::singleshot_sender<decltype(l)>);
}

TEST_CASE("let_error stores result of input sender", "[let_error]") {
    int value{};
    auto s = async::just_error_result_of([] { return 42; }) |
             async::let_error([](int &v) { return async::just(&v); });

    auto op = async::connect(s, receiver{[&](int const *i) { value = *i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_error op state may complete synchronously", "[let_error]") {
    auto s = async::just_error(42) |
             async::let_error([](auto) { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(async::synchronous<decltype(op)>);
}

TEST_CASE(
    "let_error op state may not complete synchronously if antecedent does not",
    "[let_error]") {
    auto const s =
        async::start_on(async::thread_scheduler{}, async::just_error(42)) |
        async::let_error([](auto) { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(not async::synchronous<decltype(op)>);
}

TEST_CASE(
    "let_error op state may not complete synchronously if subsequent does not",
    "[let_error]") {
    auto const s = async::just_error(42) | async::let_error([](auto) {
                       return async::thread_scheduler{}.schedule();
                   });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(not async::synchronous<decltype(op)>);
}
