#include "detail/common.hpp"

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/just.hpp>

#include <catch2/catch_test_macros.hpp>

#include <utility>

TEST_CASE("one value", "[just_error]") {
    int value{};
    auto s = async::just_error(42);
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("just_error advertises what it sends", "[just_error]") {
    static_assert(async::sender_of<decltype(async::just_error(42)),
                                   async::set_error_t(int)>);
}

TEST_CASE("move-only value", "[just_error]") {
    int value{};
    auto s = async::just_error(move_only{42});
    static_assert(async::singleshot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(
        std::move(s),
        error_receiver{[&](move_only<int> mo) { value = mo.value; }});
    async::start(std::move(op));
    CHECK(value == 42);
}

TEST_CASE("copy sender", "[just_error]") {
    int value{};
    auto const s = async::just_error(42);
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move sender", "[just_error]") {
    int value{};
    auto s = async::just_error(42);
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(std::move(s),
                             error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("just_error has a stack allocator", "[just_error]") {
    static_assert(
        std::is_same_v<async::allocator_of_t<
                           async::env_of_t<decltype(async::just_error(42))>>,
                       async::stack_allocator>);
}
