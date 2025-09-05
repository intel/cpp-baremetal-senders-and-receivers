#include "detail/common.hpp"
#include "detail/debug_handler.hpp"

#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/then_each.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("then_each", "[then_each]") {
    int value{};

    auto s = async::just();
    auto n = async::then_each(s, [] { return 42; }, [] { return 17; });
    auto op =
        async::connect(n, receiver{[&](auto i, auto j) { value = i + j; }});
    async::start(op);
    CHECK(value == 59);
}

TEST_CASE("then_each propagates a value", "[then_each]") {
    int value{};

    auto s = async::just(42);
    auto n = async::then_each(
        s, [](auto i) { return i * 2; }, [](auto i) { return i + 1; });
    auto op =
        async::connect(n, receiver{[&](auto i, auto j) { value = i + j; }});
    async::start(op);
    CHECK(value == 127);
}

TEST_CASE("then_each advertises what it sends", "[then_each]") {
    auto s = async::just();
    [[maybe_unused]] auto n =
        async::then_each(s, [] { return 42; }, [] { return 3.14f; });
    STATIC_REQUIRE(
        async::sender_of<decltype(n), async::set_value_t(int, float)>);
}

TEST_CASE("then_each can send a reference", "[then_each]") {
    int value{};
    auto s = async::just();
    [[maybe_unused]] auto n = async::then_each(
        s, [&]() -> int & { return value; }, [&]() -> int & { return value; });
    STATIC_REQUIRE(
        async::sender_of<decltype(n), async::set_value_t(int &, int &)>);
}

TEST_CASE("then_each is pipeable", "[then_each]") {
    int value{};

    auto n =
        async::just() | async::then_each([] { return 42; }, [] { return 17; });
    auto op =
        async::connect(n, receiver{[&](auto i, auto j) { value = i + j; }});
    async::start(op);
    CHECK(value == 59);
}

TEST_CASE("then_each is adaptor-pipeable", "[then_each]") {
    int value{};

    auto n = async::then_each([] { return 42; }, [] { return 17; }) |
             async::then_each([](int i, int j) { return i + j; },
                              [](int i, int j) { return i - j; });
    auto op = async::connect(async::just() | n,
                             receiver{[&](auto i, auto j) { value = i + j; }});
    async::start(op);
    CHECK(value == 84);
}

TEST_CASE("then_each can send nothing", "[then_each]") {
    int value{};

    auto s = async::just();
    auto n1 = async::then_each(s, [] {});
    STATIC_REQUIRE(async::sender_of<decltype(n1), async::set_value_t()>);
    auto n2 = async::then_each(n1, [] {}, [] {});
    STATIC_REQUIRE(async::sender_of<decltype(n2), async::set_value_t()>);
    auto op = async::connect(n2, receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("then_each deals with void-returning functions", "[then_each]") {
    int value{};

    auto s = async::just(42) |
             async::then_each([](auto i) { return i * 2; }, [](auto) {},
                              [](auto i) { return i + 1; });
    auto op =
        async::connect(s, receiver{[&](auto i, auto j) { value = i + j; }});
    async::start(op);
    CHECK(value == 127);
}

TEST_CASE("move-only value", "[then_each]") {
    int value{};

    auto n = async::just() | async::then_each([] { return move_only{42}; });
    auto op = async::connect(std::move(n),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only lambda", "[then_each]") {
    int value{};
    auto n =
        async::just() |
        async::then_each([mo = move_only{42}]() -> move_only<int> const && {
            return std::move(mo);
        });
    STATIC_REQUIRE(async::singleshot_sender<decltype(n), universal_receiver>);
    auto op = async::connect(std::move(n),
                             receiver{[&](auto &&mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("single-shot sender", "[then_each]") {
    [[maybe_unused]] auto n = async::inline_scheduler<>::schedule<
                                  async::inline_scheduler<>::singleshot>() |
                              async::then_each([] {});
    STATIC_REQUIRE(async::singleshot_sender<decltype(n), universal_receiver>);
}

TEST_CASE("then_each propagates error", "[then_each]") {
    bool then_each_called{};
    int value{};

    auto s = async::just_error(42) |
             async::then_each([&] { then_each_called = true; });
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_error_t(int)>>);
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not then_each_called);
}

TEST_CASE("then_each propagates stopped", "[then_each]") {
    bool then_each_called{};
    int value{};

    auto s = async::just_stopped() |
             async::then_each([&] { then_each_called = true; });
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_stopped_t()>>);
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not then_each_called);
}

TEST_CASE("then_each propagates forwarding queries to its child environment",
          "[then_each]") {
    auto s = custom_sender{};
    CHECK(get_fwd(async::get_env(s)) == 42);

    auto t = async::then_each(s, [] {});
    CHECK(get_fwd(async::get_env(t)) == 42);
}

TEST_CASE("then_each can handle a reference", "[then_each]") {
    int value{};
    auto s = async::just() | async::then_each([&]() -> int & { return value; });
    auto op =
        async::connect(s, receiver{[&](auto &i) {
                           CHECK(std::addressof(value) == std::addressof(i));
                       }});
    async::start(op);
}

template <>
inline auto async::injected_debug_handler<> =
    debug_handler<async::then_each_t>{};

TEST_CASE("then_each can be debugged with a string", "[then_each]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just() | async::then_each([] {});
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op then_each set_value"s});
}

TEST_CASE("then_each can be named and debugged with a string", "[then_each]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just() | async::then_each<"then_each_name">([] {});
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op then_each_name set_value"s});
}
