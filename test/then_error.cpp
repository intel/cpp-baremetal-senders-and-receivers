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

TEST_CASE("then_error", "[then_error]") {
    int value{};

    auto s = async::just();
    auto n = async::then_error(s, [] { return 42; });
    auto op = async::connect(n, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("then_error propagates a value", "[then_error]") {
    int value{};

    auto s = async::just(42);
    auto n = async::then_error(s, [](auto i) { return i * 2; });
    auto op = async::connect(n, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 84);
}

TEST_CASE("then_error advertises what it sends", "[then_error]") {
    auto s = async::just();
    [[maybe_unused]] auto n = async::then_error(s, [] { return 42; });
    STATIC_REQUIRE(async::sender_of<decltype(n), async::set_error_t(int)>);
}

TEST_CASE("then_error can send a reference", "[then_error]") {
    int value{};
    auto s = async::just();
    [[maybe_unused]] auto n =
        async::then_error(s, [&]() -> int & { return value; });
    STATIC_REQUIRE(async::sender_of<decltype(n), async::set_error_t(int &)>);
}

TEST_CASE("then_error is pipeable", "[then_error]") {
    int value{};

    auto n = async::just() | async::then_error([] { return 42; });
    auto op = async::connect(n, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("then_error is adaptor-pipeable", "[then_error]") {
    int value{};

    auto n = async::then_error([] { return 42; }) |
             async::transform_error([](int i) { return i * 2; });
    auto op = async::connect(async::just() | n,
                             error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 84);
}

TEST_CASE("then_error can send nothing", "[then_error]") {
    int value{};

    auto s = async::just();
    auto n1 = async::then_error(s, [] {});
    STATIC_REQUIRE(async::sender_of<decltype(n1), async::set_error_t()>);
    auto n2 = async::then_error(n1, [] {});
    STATIC_REQUIRE(async::sender_of<decltype(n2), async::set_error_t()>);
    auto op = async::connect(n2, error_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only value", "[then_error]") {
    int value{};

    auto n = async::just() | async::then_error([] { return move_only{42}; });
    auto op = async::connect(
        std::move(n), error_receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only lambda", "[then_error]") {
    int value{};
    auto n =
        async::just() |
        async::then_error([mo = move_only{42}]() mutable -> move_only<int> {
            return std::move(mo);
        });
    STATIC_REQUIRE(async::singleshot_sender<decltype(n), universal_receiver>);
    auto op = async::connect(
        std::move(n), error_receiver{[&](auto &&mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("single-shot sender", "[then_error]") {
    [[maybe_unused]] auto n = async::inline_scheduler<>::schedule<
                                  async::inline_scheduler<>::singleshot>() |
                              async::then_error([] {});
    STATIC_REQUIRE(async::singleshot_sender<decltype(n), universal_receiver>);
}

TEST_CASE("then_error propagates error", "[then]") {
    bool then_error_called{};
    int value{};

    auto s = async::just_error(42) |
             async::then_error([&] { then_error_called = true; });
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_error_t(int)>>);
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not then_error_called);
}

TEST_CASE("then_error propagates stopped", "[then_error]") {
    bool then_error_called{};
    int value{};

    auto s = async::just_stopped() |
             async::then_error([&] { then_error_called = true; });
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_stopped_t()>>);
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not then_error_called);
}

TEST_CASE("then_error propagates forwarding queries to its child environment ",
          "[then_error]") {
    auto s = custom_sender{};
    CHECK(get_fwd(async::get_env(s)) == 42);

    auto t = async::then_error(s, [] {});
    CHECK(get_fwd(async::get_env(t)) == 42);
}

namespace {
template <auto> struct arg_t {
    int value{};
};
} // namespace

TEST_CASE("then_error advertises what it sends (variadic)", "[then_error]") {
    auto s = async::just(arg_t<0>{}, arg_t<1>{}) |
             async::then_error([](arg_t<0>) { return 42; },
                               [](arg_t<1>) { return 17; });
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_error_t(int, int)>);
}

TEST_CASE("then_error (variadic)", "[then_error]") {
    int x{};
    int y{};
    auto s = async::just(arg_t<0>{2}, arg_t<1>{3}) |
             async::then_error([](arg_t<0> i) { return i.value * 2; },
                               [](arg_t<1> i) { return i.value * 3; });
    auto op = async::connect(s, error_receiver{[&](auto i, auto j) {
                                 x = i;
                                 y = j;
                             }});
    async::start(op);
    CHECK(x == 4);
    CHECK(y == 9);
}

TEST_CASE("variadic then_error can have void-returning functions",
          "[then_error]") {
    int x{};
    int y{42};
    auto s = async::just(arg_t<0>{2}, arg_t<1>{3}) |
             async::then_error([](arg_t<0> i) { return i.value * 2; },
                               [](arg_t<1>) {});
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_error_t(int)>);
    auto op = async::connect(s, error_receiver{[&](auto i) { x = i; }});
    async::start(op);
    CHECK(x == 4);
    CHECK(y == 42);
}

TEST_CASE("move-only value (from then_error) (variadic)", "[then_error]") {
    int x{};
    auto s = async::just(arg_t<0>{2}, arg_t<1>{3}) |
             async::then_error([](arg_t<0> i) { return move_only{i.value}; },
                               [](arg_t<1>) {});
    STATIC_REQUIRE(
        async::sender_of<decltype(s), async::set_error_t(move_only<int>)>);
    auto op = async::connect(s, error_receiver{[&](auto i) { x = i.value; }});
    async::start(op);
    CHECK(x == 2);
}

TEST_CASE("move-only value (to then_error) (variadic)", "[then_error]") {
    int x{};
    auto s =
        async::just(move_only{2}, move_only{3}) |
        async::then_error([](auto i) { return i.value; }, [](auto, auto) {});
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_error_t(int)>);
    auto op =
        async::connect(std::move(s), error_receiver{[&](auto i) { x = i; }});
    async::start(op);
    CHECK(x == 2);
}

TEST_CASE("variadic then_error can take heteroadic functions", "[then_error]") {
    int x{};
    int y{42};
    bool z{};
    auto s = async::just(arg_t<0>{2}, arg_t<1>{3}, arg_t<2>{4}) |
             async::then_error(
                 [](arg_t<0> i, arg_t<1> j) { return i.value + j.value; },
                 [](arg_t<2> k) { return k.value; }, [] { return true; });
    STATIC_REQUIRE(
        async::sender_of<decltype(s), async::set_error_t(int, int, bool)>);
    auto op = async::connect(s, error_receiver{[&](auto i, auto j, auto k) {
                                 x = i;
                                 y = j;
                                 z = k;
                             }});
    async::start(op);
    CHECK(x == 5);
    CHECK(y == 4);
    CHECK(z);
}

TEST_CASE("then_error can handle a reference", "[then_error]") {
    int value{};
    auto s =
        async::just() | async::then_error([&]() -> int & { return value; });
    auto op =
        async::connect(s, error_receiver{[&](auto &i) {
                           CHECK(std::addressof(value) == std::addressof(i));
                       }});
    async::start(op);
}

template <>
inline auto async::injected_debug_handler<> =
    debug_handler<async::then_error_t>{};

TEST_CASE("then_error can be debugged with a string", "[then_error]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just() | async::then_error([] {});
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op then_error set_error"s});
}

TEST_CASE("then_error can be named and debugged with a string",
          "[then_error]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just() | async::then_error<"then_error_name">([] {});
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op then_error_name set_error"s});
}
