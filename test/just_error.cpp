#include "detail/common.hpp"

#include <async/allocator.hpp>
#include <async/completes_synchronously.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/just.hpp>

#include <stdx/ct_format.hpp>
#include <stdx/type_traits.hpp>

#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <string>
#include <type_traits>
#include <utility>
#include <vector>

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
    async::start(op);
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

TEST_CASE("just_error op state is synchronous", "[just_error]") {
    [[maybe_unused]] auto op =
        async::connect(async::just_error(42), receiver{[] {}});
    static_assert(async::synchronous<decltype(op)>);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string L, stdx::ct_string S,
              typename Ctx>
    constexpr auto signal(auto &&...) {
        debug_events.push_back(fmt::format("{} {} {}", C, L, S));
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("just_error can be debugged with a string", "[just_error]") {
    using namespace std::string_literals;
    debug_events.clear();
    auto s = async::just_error(42);
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op just_error start"s, "op just_error set_error"s});
}

TEST_CASE("just_error can be named and debugged with a string",
          "[just_error]") {
    using namespace std::string_literals;
    debug_events.clear();
    auto s = async::just_error<"just_error_name">(42);
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op just_error_name start"s,
                                      "op just_error_name set_error"s});
}
