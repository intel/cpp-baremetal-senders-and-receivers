#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/let_stopped.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/start_on.hpp>
#include <async/then.hpp>
#include <async/variant_sender.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("let_stopped", "[let_stopped]") {
    int value{};

    auto s = async::just_stopped();
    auto l = async::let_stopped(s, [] { return async::just(42); });
    auto op = async::connect(l, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped error", "[let_stopped]") {
    int value{};

    auto s = async::just_stopped();
    auto l = async::let_stopped(s, [] { return async::just_error(42); });
    auto op = async::connect(l, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped advertises what it sends", "[let_stopped]") {
    auto s = async::just_stopped();
    [[maybe_unused]] auto l =
        async::let_stopped(s, [] { return async::just(42); });
    static_assert(async::sender_of<decltype(l), async::set_value_t(int)>);
}

TEST_CASE("let_stopped advertises errors", "[let_stopped]") {
    auto s = async::just_stopped();
    [[maybe_unused]] auto l =
        async::let_stopped(s, [] { return async::just_error(42); });
    static_assert(async::sender_of<decltype(l), async::set_error_t(int)>);
}

TEST_CASE("let_stopped is pipeable", "[let_stopped]") {
    int value{};

    auto l = async::just_stopped() |
             async::let_stopped([] { return async::just(42); });
    auto op = async::connect(l, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped is adaptor-pipeable", "[let_stopped]") {
    int value{};

    auto l = async::let_stopped([] { return async::just_stopped(); }) |
             async::let_stopped([] { return async::just(42); });
    auto op = async::connect(async::just_stopped() | l,
                             receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only value", "[let_stopped]") {
    int value{};

    auto s = async::just_stopped();
    auto l = async::let_stopped(s, [] { return async::just(move_only{42}); });
    static_assert(async::singleshot_sender<decltype(l), universal_receiver>);
    auto op = async::connect(std::move(l),
                             receiver{[&](auto &&mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped propagates value (order 1)", "[let_stopped]") {
    int value{};

    auto s = async::just(41) | async::then([](auto i) { return ++i; }) |
             async::let_stopped([] { return async::just(17); });
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped propagates value (order 2)", "[let_stopped]") {
    int value{};

    auto s = async::just(41) |
             async::let_stopped([] { return async::just(17); }) |
             async::then([](auto i) { return ++i; });
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped propagates error (order 1)", "[let_stopped]") {
    int value{};

    auto s = async::just_error(41) |
             async::upon_error([](auto i) { return ++i; }) |
             async::let_stopped([] { return async::just(17); });
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped propagates error (order 2)", "[let_stopped]") {
    int value{};

    auto s = async::just_error(41) |
             async::let_stopped([] { return async::just(17); }) |
             async::upon_error([](auto i) { return ++i; });
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped advertises pass-through completions", "[let_stopped]") {
    [[maybe_unused]] auto l = async::just(42) | async::let_stopped([] {});
    static_assert(async::sender_of<decltype(l), async::set_value_t(int)>);
}

TEST_CASE("let_stopped can be single shot", "[let_stopped]") {
    [[maybe_unused]] auto l = async::just_stopped() | async::let_stopped([] {
                                  return async::just(move_only{42});
                              });
    static_assert(async::singleshot_sender<decltype(l)>);
}

TEST_CASE("let_stopped can be single shot with passthrough", "[let_stopped]") {
    [[maybe_unused]] auto l = async::just(move_only{42}) |
                              async::let_stopped([](auto) { return 42; });
    static_assert(async::singleshot_sender<decltype(l)>);
}

TEST_CASE("let_stopped op state may complete synchronously", "[let_stopped]") {
    auto s = async::just_stopped() |
             async::let_stopped([] { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(async::synchronous<decltype(op)>);
}

TEST_CASE("let_stopped op state may not complete synchronously if antecedent "
          "does not",
          "[let_stopped]") {
    auto const s =
        async::start_on(async::thread_scheduler{}, async::just_stopped()) |
        async::let_stopped([] { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(not async::synchronous<decltype(op)>);
}

TEST_CASE("let_stopped op state may not complete synchronously if subsequent "
          "does not",
          "[let_stopped]") {
    auto const s = async::just_stopped() | async::let_stopped([] {
                       return async::thread_scheduler{}.schedule();
                   });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(not async::synchronous<decltype(op)>);
}
