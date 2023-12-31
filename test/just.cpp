#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/just.hpp>

#include <catch2/catch_test_macros.hpp>

#include <utility>

TEST_CASE("one value", "[just]") {
    int value{};
    auto s = async::just(42);
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("multiple values", "[just]") {
    int value{};
    auto s = async::just(1, 2, 3);
    auto op = async::connect(
        s, receiver{[&](auto... is) { value = (0 + ... + is); }});
    op.start();
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
    op.start();
    CHECK(value == 42);
}

TEST_CASE("copy sender", "[just]") {
    int value{};
    auto const s = async::just(42);
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("move sender", "[just]") {
    int value{};
    auto s = async::just(42);
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);
    auto op =
        async::connect(std::move(s), receiver{[&](auto i) { value = i; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("void sender", "[just]") {
    bool rcvd{};
    auto s = async::just();
    auto op = async::connect(s, receiver{[&] { rcvd = true; }});
    op.start();
    CHECK(rcvd);
}
