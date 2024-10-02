#include "detail/common.hpp"

#include <async/allocator.hpp>
#include <async/completes_synchronously.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/just_result_of.hpp>
#include <async/stack_allocator.hpp>

#include <stdx/ct_format.hpp>
#include <stdx/type_traits.hpp>

#include <boost/mp11/list.hpp>
#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <concepts>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

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

TEST_CASE("just_error_result_of has a stack allocator",
          "[just_error_result_of]") {
    static_assert(
        std::is_same_v<
            async::allocator_of_t<async::env_of_t<
                decltype(async::just_error_result_of([] { return 42; }))>>,
            async::stack_allocator>);
}

TEST_CASE("just_error_result_of op state is synchronous",
          "[just_error_result_of]") {
    [[maybe_unused]] auto op = async::connect(
        async::just_error_result_of([] { return 42; }), receiver{[] {}});
    static_assert(async::synchronous<decltype(op)>);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    constexpr auto signal(auto &&...) {
        static_assert(std::is_same_v<async::debug::tag_of<Ctx>,
                                     async::just_error_result_of_t>);
        static_assert(
            boost::mp11::mp_empty<async::debug::children_of<Ctx>>::value);
        debug_events.push_back(
            fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("just_error_result_of can be debugged with a string",
          "[just_error_result_of]") {
    using namespace std::string_literals;
    debug_events.clear();
    auto s = async::just_error_result_of([] { return 42; });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op just_error_result_of start"s,
                                      "op just_error_result_of set_error"s});
}

TEST_CASE("just_error_result_of can be named and debugged with a string",
          "[just_error_result_of]") {
    using namespace std::string_literals;
    debug_events.clear();
    auto s = async::just_error_result_of<"just_error_result_of_name">(
        [] { return 42; });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op just_error_result_of_name start"s,
                      "op just_error_result_of_name set_error"s});
}
