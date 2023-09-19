#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/inline_scheduler.hpp>
#include <async/just.hpp>
#include <async/then.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("then", "[then]") {
    int value{};

    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    auto n = async::then(s, [] { return 42; });
    auto op = async::connect(n, receiver{[&](auto i) { value = i; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("then advertises what it sends", "[then]") {
    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    [[maybe_unused]] auto n = async::then(s, [] { return 42; });
    static_assert(async::sender_of<decltype(n), async::set_value_t(int)>);
}

TEST_CASE("then can send a reference", "[then]") {
    int value{};
    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    [[maybe_unused]] auto n = async::then(s, [&]() -> int & { return value; });
    static_assert(async::sender_of<decltype(n), async::set_value_t(int &)>);
}

TEST_CASE("then is pipeable", "[then]") {
    int value{};

    auto sched = async::inline_scheduler{};
    auto n = sched.schedule() | async::then([] { return 42; });
    auto op = async::connect(n, receiver{[&](auto i) { value = i; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("then can send nothing", "[then]") {
    int value{};

    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    auto n1 = async::then(s, [] {});
    static_assert(async::sender_of<decltype(n1), async::set_value_t()>);
    auto n2 = async::then(n1, [] {});
    static_assert(async::sender_of<decltype(n2), async::set_value_t()>);
    auto op = async::connect(n2, receiver{[&] { value = 42; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("move-only value", "[then]") {
    int value{};

    auto sched = async::inline_scheduler{};
    auto n = sched.schedule() | async::then([] { return move_only{42}; });
    auto op = async::connect(std::move(n),
                             receiver{[&](auto mo) { value = mo.value; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("single-shot sender", "[then]") {
    [[maybe_unused]] auto n = async::inline_scheduler::schedule<
                                  async::inline_scheduler::singleshot>() |
                              async::then([] {});
    static_assert(async::singleshot_sender<decltype(n), universal_receiver>);
}

TEST_CASE("then propagates error (order 1)", "[then]") {
    int value{};

    auto s = async::just_error(0) | async::then([] { return 17; }) |
             async::upon_error([](auto) { return 42; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_error_t(int)>>);
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("then propagates error (order 2)", "[then]") {
    int value{};

    auto s = async::just_error(0) | async::upon_error([](auto) { return 42; }) |
             async::then([] { return 17; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_error_t(int)>>);
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("then propagates stopped (order 1)", "[then]") {
    int value{};

    auto s = async::just_stopped() | async::upon_stopped([&] { value = 41; }) |
             async::then([] { return 17; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_stopped_t()>>);
    auto op = async::connect(s, stopped_receiver{[&] { ++value; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("then propagates stopped (order 2)", "[then]") {
    int value{};

    auto s = async::just_stopped() | async::then([] { return 17; }) |
             async::upon_stopped([&] { value = 41; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_stopped_t()>>);
    auto op = async::connect(s, stopped_receiver{[&] { ++value; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("then propagates forwarding queries to its child environment",
          "[then]") {
    auto s = custom_sender{};
    CHECK(get_fwd(async::get_env(s)) == 42);

    auto t = async::then(s, [] {});
    CHECK(get_fwd(async::get_env(t)) == 42);
}

TEST_CASE("then advertises what it sends (variadic)", "[then]") {
    auto s = async::just(true, false) |
             async::then([](auto) { return 42; }, [](auto) { return 17; });
    static_assert(async::sender_of<decltype(s), async::set_value_t(int, int)>);
}

TEST_CASE("then (variadic)", "[then]") {
    int x{};
    int y{};
    auto s = async::just(2, 3) | async::then([](auto i) { return i * 2; },
                                             [](auto i) { return i * 3; });
    auto op = async::connect(s, receiver{[&](auto i, auto j) {
                                 x = i;
                                 y = j;
                             }});
    op.start();
    CHECK(x == 4);
    CHECK(y == 9);
}

TEST_CASE("variadic then can have void-returning functions", "[then]") {
    int x{};
    int y{42};
    auto s = async::just(2, 3) |
             async::then([](auto i) { return i * 2; }, [](auto) {});
    static_assert(async::sender_of<decltype(s), async::set_value_t(int)>);
    auto op = async::connect(s, receiver{[&](auto i) { x = i; }});
    op.start();
    CHECK(x == 4);
    CHECK(y == 42);
}

TEST_CASE("move-only value (variadic)", "[then]") {
    int x{};
    auto s = async::just(2, 3) |
             async::then([](auto i) { return move_only{i}; }, [](auto) {});
    static_assert(
        async::sender_of<decltype(s), async::set_value_t(move_only<int>)>);
    auto op = async::connect(s, receiver{[&](auto i) { x = i.value; }});
    op.start();
    CHECK(x == 2);
}
