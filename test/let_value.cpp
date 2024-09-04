#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/just_result_of.hpp>
#include <async/let_value.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/schedulers/thread_scheduler.hpp>
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

TEST_CASE("let_value is adaptor-pipeable", "[let_value]") {
    int value{};

    auto l = async::let_value([] { return async::just(42); }) |
             async::let_value([](int i) { return async::just(i * 2); });
    auto sched = async::inline_scheduler{};
    auto op = async::connect(sched.schedule() | l,
                             receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 84);
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

TEST_CASE("let_value propagates error", "[let_value]") {
    bool let_called{};
    int value{};

    auto s = async::just_error(42) | async::let_value([&] {
                 let_called = true;
                 return async::just();
             });
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not let_called);
}

TEST_CASE("let_value propagates stopped", "[let_value]") {
    bool let_called{};
    int value{};

    auto s = async::just_stopped() | async::let_value([&] {
                 let_called = true;
                 return async::just();
             });
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not let_called);
}

TEST_CASE("let_value advertises pass-through completions", "[let_value]") {
    [[maybe_unused]] auto l =
        async::just_error(42) | async::let_value([](auto) {});
    static_assert(async::sender_of<decltype(l), async::set_error_t(int)>);
}

TEST_CASE("let_value can be single shot", "[let_value]") {
    [[maybe_unused]] auto l = async::just(42) | async::let_value([](auto i) {
                                  return async::just(move_only{i});
                              });
    static_assert(async::singleshot_sender<decltype(l)>);
}

TEST_CASE("let_value can be single shot with passthrough", "[let_value]") {
    [[maybe_unused]] auto l = async::just_error(move_only{42}) |
                              async::let_value([](auto) { return 42; });
    static_assert(async::singleshot_sender<decltype(l)>);
}

TEST_CASE("let_value stores result of input sender", "[let_value]") {
    int value{};
    auto s = async::just_result_of([] { return 42; }) |
             async::let_value([](int &v) { return async::just(&v); });

    auto op = async::connect(s, receiver{[&](int const *i) { value = *i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_value op state may complete synchronously", "[let_value]") {
    auto const s =
        async::just() | async::let_value([] { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(async::synchronous<decltype(op)>);
}

TEST_CASE(
    "let_value op state may not complete synchronously if antecedent does not",
    "[let_value]") {
    auto const s = async::thread_scheduler{}.schedule() |
                   async::let_value([] { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(not async::synchronous<decltype(op)>);
}

TEST_CASE(
    "let_value op state may not complete synchronously if subsequent does not",
    "[let_value]") {
    auto const s = async::just() | async::let_value([] {
                       return async::thread_scheduler{}.schedule();
                   });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(not async::synchronous<decltype(op)>);
}
