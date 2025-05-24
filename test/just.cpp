#include "detail/common.hpp"
#include "detail/debug_handler.hpp"

#include <async/allocator.hpp>
#include <async/completes_synchronously.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/just.hpp>

#include <catch2/catch_test_macros.hpp>

#include <string>
#include <type_traits>
#include <utility>
#include <vector>

TEST_CASE("one value", "[just]") {
    int value{};
    auto s = async::just(42);
    auto r = receiver{[&](auto i) { value = i; }};
    auto op = async::connect(s, r);
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("multiple values", "[just]") {
    int value{};
    auto s = async::just(1, 2, 3);
    auto op = async::connect(
        s, receiver{[&](auto... is) { value = (0 + ... + is); }});
    async::start(op);
    CHECK(value == 6);
}

TEST_CASE("just advertises what it sends", "[just]") {
    STATIC_REQUIRE(
        async::sender_of<decltype(async::just(42)), async::set_value_t(int)>);
}

TEST_CASE("move-only value", "[just]") {
    int value{};
    auto s = async::just(move_only{42});
    STATIC_REQUIRE(async::singleshot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(
        std::move(s), receiver{[&](move_only<int> mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("copy sender", "[just]") {
    int value{};
    auto const s = async::just(42);
    STATIC_REQUIRE(async::multishot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move sender", "[just]") {
    int value{};
    auto s = async::just(42);
    STATIC_REQUIRE(async::multishot_sender<decltype(s), universal_receiver>);
    auto op =
        async::connect(std::move(s), receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("void sender", "[just]") {
    bool rcvd{};
    auto s = async::just();
    auto op = async::connect(s, receiver{[&] { rcvd = true; }});
    async::start(op);
    CHECK(rcvd);
}

TEST_CASE("just has a stack allocator", "[just]") {
    STATIC_REQUIRE(
        std::is_same_v<
            async::allocator_of_t<async::env_of_t<decltype(async::just(42))>>,
            async::stack_allocator>);
}

TEST_CASE("just op state is synchronous", "[just]") {
    [[maybe_unused]] auto op = async::connect(async::just(42), receiver{[] {}});
    STATIC_REQUIRE(async::synchronous<decltype(op)>);
}

template <>
inline auto async::injected_debug_handler<> =
    debug_handler<async::just_t, true>{};

TEST_CASE("just can be debugged with a string", "[just]") {
    using namespace std::string_literals;
    debug_events.clear();
    auto s = async::just();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op just start"s, "op just set_value"s});
}

TEST_CASE("just can be named and debugged with a string", "[just]") {
    using namespace std::string_literals;
    debug_events.clear();
    auto s = async::just<"just_name">();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op just_name start"s, "op just_name set_value"s});
}
