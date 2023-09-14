#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/just_result_of.hpp>

#include <catch2/catch_test_macros.hpp>

#include <utility>

TEST_CASE("one function", "[just_result_of]") {
    int value{};
    auto s = async::just_result_of([] { return 42; });
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("one function can return void", "[just_result_of]") {
    int value{};
    auto s = async::just_result_of([] {});
    auto op = async::connect(s, receiver{[&] { value = 42; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("multiple functions", "[just_result_of]") {
    int value{};
    auto s = async::just_result_of([] { return 1; }, [] { return 2; },
                                   [] { return 3; });
    auto op = async::connect(
        s, receiver{[&](auto... is) { value = (0 + ... + is); }});
    op.start();
    CHECK(value == 6);
}

TEST_CASE("multiple functions, some returning void", "[just_result_of]") {
    int value{};
    auto s = async::just_result_of([] { return 1; }, [] {}, [] { return 3; });
    auto op = async::connect(
        s, receiver{[&](auto... is) { value = (0 + ... + is); }});
    op.start();
    CHECK(value == 4);
}

TEST_CASE("just_result_of advertises what it sends", "[just_result_of]") {
    static_assert(
        async::sender_of<decltype(async::just_result_of([] { return 42; })),
                         async::set_value_t(int)>);
    static_assert(async::sender_of<decltype(async::just_result_of([] {})),
                                   async::set_value_t()>);
}

TEST_CASE("just_result_of advertises what it sends (excluding voids)",
          "[just_result_of]") {
    static_assert(
        std::same_as<
            async::completion_signatures_of_t<decltype(async::just_result_of(
                [] { return 42; }, [] {}, [] { return 17; }))>,
            async::completion_signatures<async::set_value_t(int, int)>>);
}

TEST_CASE("move-only value", "[just_result_of]") {
    int value{};
    auto s = async::just_result_of([] { return move_only{42}; });
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(std::move(s),
                             receiver{[&](auto &&mo) { value = mo.value; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("move-only lambda", "[just_result_of]") {
    int value{};
    auto s = async::just_result_of(
        [mo = move_only{42}]() -> move_only<int> const && {
            return std::move(mo);
        });
    static_assert(async::singleshot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(std::move(s),
                             receiver{[&](auto &&mo) { value = mo.value; }});
    op.start();
    CHECK(value == 42);
}

TEST_CASE("copyable lambda", "[just_result_of]") {
    static_assert(async::multishot_sender<decltype(async::just_result_of(
                                              [] { return 42; })),
                                          universal_receiver>);
}
