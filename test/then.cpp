#include "detail/common.hpp"
#include "detail/debug_handler.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/just.hpp>
#include <async/just_result_of.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/then.hpp>

#include <catch2/catch_test_macros.hpp>

#include <concepts>
#include <memory>
#include <string>
#include <utility>
#include <vector>

TEST_CASE("then", "[then]") {
    int value{};

    auto s = async::just();
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
    auto s = async::just();
    [[maybe_unused]] auto n = async::then(s, [] { return 42; });
    STATIC_REQUIRE(async::sender_of<decltype(n), async::set_value_t(int)>);
}

TEST_CASE("then can send a reference", "[then]") {
    int value{};
    auto s = async::just();
    [[maybe_unused]] auto n = async::then(s, [&]() -> int & { return value; });
    STATIC_REQUIRE(async::sender_of<decltype(n), async::set_value_t(int &)>);
}

TEST_CASE("then is pipeable", "[then]") {
    int value{};

    auto n = async::just() | async::then([] { return 42; });
    auto op = async::connect(n, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("then is adaptor-pipeable", "[then]") {
    int value{};

    auto n = async::then([] { return 42; }) |
             async::then([](int i) { return i * 2; });
    auto op =
        async::connect(async::just() | n, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 84);
}

TEST_CASE("then can send nothing", "[then]") {
    int value{};

    auto s = async::just();
    auto n1 = async::then(s, [] {});
    STATIC_REQUIRE(async::sender_of<decltype(n1), async::set_value_t()>);
    auto n2 = async::then(n1, [] {});
    STATIC_REQUIRE(async::sender_of<decltype(n2), async::set_value_t()>);
    auto op = async::connect(n2, receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only value", "[then]") {
    int value{};

    auto n = async::just() | async::then([] { return move_only{42}; });
    auto op = async::connect(std::move(n),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only lambda", "[then]") {
    int value{};
    auto n = async::just() |
             async::then([mo = move_only{42}]() -> move_only<int> const && {
                 return std::move(mo);
             });
    STATIC_REQUIRE(async::singleshot_sender<decltype(n), universal_receiver>);
    auto op = async::connect(std::move(n),
                             receiver{[&](auto &&mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("single-shot sender", "[then]") {
    [[maybe_unused]] auto n = async::inline_scheduler<>::schedule<
                                  async::inline_scheduler<>::singleshot>() |
                              async::then([] {});
    STATIC_REQUIRE(async::singleshot_sender<decltype(n), universal_receiver>);
}

TEST_CASE("then propagates error", "[then]") {
    bool then_called{};
    int value{};

    auto s = async::just_error(42) | async::then([&] { then_called = true; });
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_error_t(int)>>);
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not then_called);
}

TEST_CASE("then propagates stopped", "[then]") {
    bool then_called{};
    int value{};

    auto s = async::just_stopped() | async::then([&] { then_called = true; });
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_stopped_t()>>);
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not then_called);
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
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_value_t(int, int)>);
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
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_value_t(int)>);
    auto op = async::connect(s, receiver{[&](auto i) { x = i; }});
    async::start(op);
    CHECK(x == 4);
    CHECK(y == 42);
}

TEST_CASE("move-only value (from then) (variadic)", "[then]") {
    int x{};
    auto s = async::just(2, 3) |
             async::then([](auto i) { return move_only{i}; }, [](auto) {});
    STATIC_REQUIRE(
        async::sender_of<decltype(s), async::set_value_t(move_only<int>)>);
    auto op = async::connect(s, receiver{[&](auto i) { x = i.value; }});
    async::start(op);
    CHECK(x == 2);
}

TEST_CASE("move-only value (to then) (variadic)", "[then]") {
    int x{};
    auto s = async::just(move_only{2}, move_only{3}) |
             async::then([](auto i) { return i.value; }, [](auto) {});
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_value_t(int)>);
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
    STATIC_REQUIRE(
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

template <>
inline auto async::injected_debug_handler<> = debug_handler<async::then_t>{};

TEST_CASE("then can be debugged with a string", "[then]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just() | async::then([] {});
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op then set_value"s});
}

TEST_CASE("then can be named and debugged with a string", "[then]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just() | async::then<"then_name">([] {});
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op then_name set_value"s});
}
