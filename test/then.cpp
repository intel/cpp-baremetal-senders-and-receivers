#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/just.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/then.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("then", "[then]") {
    int value{};

    auto sched = async::inline_scheduler{};
    auto s = sched.schedule();
    auto n = async::then(s, [] { return 42; });
    auto op = async::connect(n, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("then propagates a value", "[then]") {
    int value{};

    auto s = async::just(42);
    auto n = async::then(s, [](auto i) { return i * 2; });
    auto op = async::connect(n, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 84);
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
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("then is adaptor-pipeable", "[then]") {
    int value{};

    auto n = async::then([] { return 42; }) |
             async::then([](int i) { return i * 2; });
    auto sched = async::inline_scheduler{};
    auto op = async::connect(sched.schedule() | n,
                             receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 84);
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
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only value", "[then]") {
    int value{};

    auto sched = async::inline_scheduler{};
    auto n = sched.schedule() | async::then([] { return move_only{42}; });
    auto op = async::connect(std::move(n),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only lambda", "[then]") {
    int value{};
    auto sched = async::inline_scheduler{};
    auto n = sched.schedule() |
             async::then([mo = move_only{42}]() -> move_only<int> const && {
                 return std::move(mo);
             });
    static_assert(async::singleshot_sender<decltype(n), universal_receiver>);
    auto op = async::connect(std::move(n),
                             receiver{[&](auto &&mo) { value = mo.value; }});
    async::start(op);
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
    async::start(op);
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
    async::start(op);
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
    async::start(op);
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
    async::start(op);
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
    async::start(op);
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
    async::start(op);
    CHECK(x == 4);
    CHECK(y == 42);
}

TEST_CASE("move-only value (from then) (variadic)", "[then]") {
    int x{};
    auto s = async::just(2, 3) |
             async::then([](auto i) { return move_only{i}; }, [](auto) {});
    static_assert(
        async::sender_of<decltype(s), async::set_value_t(move_only<int>)>);
    auto op = async::connect(s, receiver{[&](auto i) { x = i.value; }});
    async::start(op);
    CHECK(x == 2);
}

TEST_CASE("move-only value (to then) (variadic)", "[then]") {
    int x{};
    auto s = async::just(move_only{2}, move_only{3}) |
             async::then([](auto i) { return i.value; }, [](auto) {});
    static_assert(async::sender_of<decltype(s), async::set_value_t(int)>);
    auto op = async::connect(std::move(s), receiver{[&](auto i) { x = i; }});
    async::start(op);
    CHECK(x == 2);
}

TEST_CASE("variadic then can take heteroadic functions", "[then]") {
    int x{};
    int y{42};
    bool z{};
    auto s = async::just(2, 3, 4) |
             async::then([](auto i, auto j) { return i + j; },
                         [](auto k) { return k; }, [] { return true; });
    static_assert(
        async::sender_of<decltype(s), async::set_value_t(int, int, bool)>);
    auto op = async::connect(s, receiver{[&](auto i, auto j, auto k) {
                                 x = i;
                                 y = j;
                                 z = k;
                             }});
    async::start(op);
    CHECK(x == 5);
    CHECK(y == 4);
    CHECK(z);
}

TEST_CASE("then can handle a reference", "[then]") {
    int value{};
    auto s = async::just() | async::then([&]() -> int & { return value; });
    auto op =
        async::connect(s, receiver{[&](auto &i) {
                           CHECK(std::addressof(value) == std::addressof(i));
                       }});
    async::start(op);
}
