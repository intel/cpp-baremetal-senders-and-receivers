#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/just_result_of.hpp>
#include <async/tags.hpp>

#include <catch2/catch_test_macros.hpp>

#include <utility>

TEST_CASE("one function", "[just_error_result_of]") {
    int value{};
    auto s = async::just_error_result_of([] { return 42; });
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("one function can return void", "[just_error_result_of]") {
    int value{};
    auto s = async::just_error_result_of([] {});
    auto op = async::connect(s, error_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("multiple functions", "[just_error_result_of]") {
    int value{};
    auto s = async::just_error_result_of([] { return 1; }, [] { return 2; },
                                         [] { return 3; });
    auto op = async::connect(
        s, error_receiver{[&](auto... is) { value = (0 + ... + is); }});
    async::start(op);
    CHECK(value == 6);
}

TEST_CASE("multiple functions, some returning void", "[just_error_result_of]") {
    int value{};
    auto s =
        async::just_error_result_of([] { return 1; }, [] {}, [] { return 3; });
    auto op = async::connect(
        s, error_receiver{[&](auto... is) { value = (0 + ... + is); }});
    async::start(op);
    CHECK(value == 4);
}

TEST_CASE("just_error_result_of advertises what it sends",
          "[just_error_result_of]") {
    static_assert(async::sender_of<decltype(async::just_error_result_of(
                                       [] { return 42; })),
                                   async::set_error_t(int)>);
}

TEST_CASE("just_error_result_of advertises what it sends (excluding voids)",
          "[just_error_result_of]") {
    static_assert(
        std::same_as<async::completion_signatures_of_t<
                         decltype(async::just_error_result_of([] { return 42; },
                                                              [] {}))>,
                     async::completion_signatures<async::set_error_t(int)>>);
}

TEST_CASE("move-only value", "[just_error_result_of]") {
    int value{};
    auto s = async::just_error_result_of([] { return move_only{42}; });
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(
        std::move(s), error_receiver{[&](auto &&mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only lambda", "[just_error_result_of]") {
    int value{};
    auto s = async::just_error_result_of(
        [mo = move_only{42}]() -> move_only<int> const && {
            return std::move(mo);
        });
    static_assert(async::singleshot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(
        std::move(s), error_receiver{[&](auto &&mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("copyable lambda", "[just_error_result_of]") {
    static_assert(async::multishot_sender<decltype(async::just_error_result_of(
                                              [] { return 42; })),
                                          universal_receiver>);
}
