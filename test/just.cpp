#include "detail/common.hpp"

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/just.hpp>
#include <async/tags.hpp>

#include <catch2/catch_test_macros.hpp>

#include <utility>

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
    static_assert(
        async::sender_of<decltype(async::just(42)), async::set_value_t(int)>);
}

TEST_CASE("move-only value", "[just]") {
    int value{};
    auto s = async::just(move_only{42});
    static_assert(async::singleshot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(
        std::move(s), receiver{[&](move_only<int> mo) { value = mo.value; }});
    async::start(std::move(op));
    CHECK(value == 42);
}

TEST_CASE("copy sender", "[just]") {
    int value{};
    auto const s = async::just(42);
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move sender", "[just]") {
    int value{};
    auto s = async::just(42);
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);
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
    static_assert(
        std::is_same_v<
            async::allocator_of_t<async::env_of_t<decltype(async::just(42))>>,
            async::stack_allocator>);
}
