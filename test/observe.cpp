#include "detail/common.hpp"

#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/observe.hpp>
#include <async/start.hpp>

#include <catch2/catch_test_macros.hpp>

#include <concepts>

TEST_CASE("observe", "[observe]") {
    int observed_value{};
    int value{};

    auto j = async::just(42);
    auto s = async::observe(j, [&](auto const &i) { observed_value = i; });
    auto r = receiver{[&](auto i) { value = i; }};
    auto op = async::connect(s, r);
    async::start(op);

    CHECK(observed_value == 42);
    CHECK(value == 42);
}

TEST_CASE("observer advertises what upstream sends", "[observe]") {
    auto s = async::just(42);
    [[maybe_unused]] auto n = async::observe(s, [] {});
    STATIC_REQUIRE(async::sender_of<decltype(n), async::set_value_t(int)>);
}

TEST_CASE("observe is pipeable", "[observe]") {
    int value{};

    auto n = async::just(42) | async::observe([] {});
    auto op = async::connect(n, receiver{[&](auto i) { value = i; }});
    async::start(op);

    CHECK(value == 42);
}

TEST_CASE("observe is adaptor-pipeable", "[observe]") {
    int observed_value{};
    int value{};

    auto n = async::observe([&] { observed_value = 42; }) |
             async::observe([&](int i) { observed_value += i; });
    auto op = async::connect(async::just(42) | n,
                             receiver{[&](auto i) { value = i; }});
    async::start(op);

    CHECK(observed_value == 84);
    CHECK(value == 42);
}

TEST_CASE("observe receives const ref values", "[observe]") {
    int value{};

    auto s = async::just(42) | async::observe([&]<typename T>(T &&) {
                 CHECK(std::same_as<T, int const &>);
             });
    auto r = receiver{[&](auto i) { value = i; }};
    auto op = async::connect(s, r);
    async::start(op);

    CHECK(value == 42);
}

TEST_CASE("observe can observe only what it wants", "[observe]") {
    int observed_value{};
    float value{};

    auto n =
        async::just(42, 3.14f) |
        async::observe([&](std::same_as<int> auto i) { observed_value = i; });
    auto op = async::connect(n, receiver{[&](auto, auto f) { value = f; }});
    async::start(op);

    CHECK(observed_value == 42);
    CHECK(value == 3.14f);
}

TEST_CASE("observe can observe multiple things independently", "[observe]") {
    int observed_int_value{};
    float observed_float_value{};
    float value{};

    auto n = async::just(42, 3.14f) |
             async::observe(
                 [&](std::same_as<int> auto i) { observed_int_value = i; },
                 [&](std::same_as<float> auto f) { observed_float_value = f; });
    auto op = async::connect(n, receiver{[&](auto, auto f) { value = f; }});
    async::start(op);

    CHECK(observed_int_value == 42);
    CHECK(observed_float_value == 3.14f);
    CHECK(value == 3.14f);
}

TEST_CASE("observe a move-only value", "[then]") {
    int observed_value{};
    int value{};

    auto n = async::just(move_only{42}) |
             async::observe([&](auto const &mo) { observed_value = mo.value; });
    auto op = async::connect(std::move(n),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);

    CHECK(observed_value == 42);
    CHECK(value == 42);
}

TEST_CASE("observe all channels by default (error)", "[observe]") {
    int observed_value{};
    int value{};

    auto n = async::just_error(42) |
             async::observe([&](auto i) { observed_value = i; });
    auto op = async::connect(n, error_receiver{[&](auto i) { value = i; }});
    async::start(op);

    CHECK(observed_value == 42);
    CHECK(value == 42);
}

TEST_CASE("observe all channels by default (stopped)", "[observe]") {
    int observed_value{};
    int value{};

    auto n =
        async::just_stopped() | async::observe([&] { observed_value = 42; });
    auto op = async::connect(n, stopped_receiver{[&] { value = 42; }});
    async::start(op);

    CHECK(observed_value == 42);
    CHECK(value == 42);
}

TEST_CASE("observe selected channels", "[observe]") {
    int observed_value{17};
    constexpr auto channels = async::set_value | async::set_error;

    auto n = async::just_error(42) |
             async::observe<channels>([&](auto i) { observed_value = i; });
    auto op = async::connect(n, receiver{[] {}});
    async::start(op);

    CHECK(observed_value == 42);
}

TEST_CASE("don't observe non-selected channels", "[observe]") {
    constexpr auto channels = async::set_value | async::set_error;
    int observed_value{17};

    auto n = async::just_stopped() |
             async::observe<channels>([&] { observed_value = 42; });
    auto op = async::connect(n, receiver{[] {}});
    async::start(op);

    CHECK(observed_value == 17);
}

TEST_CASE("observe can observe the completion channel", "[observe]") {
    int observed_value{};
    int value{};

    auto s = async::just_error(42) |
             async::observe([&]<async::channel_tag T>(auto const &i) {
                 STATIC_CHECK(std::same_as<T, async::set_error_t>);
                 observed_value = i;
             });
    auto r = error_receiver{[&](auto i) { value = i; }};
    auto op = async::connect(s, r);
    async::start(op);

    CHECK(observed_value == 42);
    CHECK(value == 42);
}
