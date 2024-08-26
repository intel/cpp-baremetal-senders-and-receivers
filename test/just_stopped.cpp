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

TEST_CASE("one value", "[just_stopped]") {
    int value{};
    auto s = async::just_stopped();
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("just_stopped advertises what it sends", "[just_stopped]") {
    static_assert(async::sender_of<decltype(async::just_stopped()),
                                   async::set_stopped_t()>);
}

TEST_CASE("copy sender", "[just_stopped]") {
    int value{};
    auto const s = async::just_stopped();
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move sender", "[just_stopped]") {
    int value{};
    auto s = async::just_stopped();
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);
    auto op =
        async::connect(std::move(s), stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("just_stopped has a stack allocator", "[just_stopped]") {
    static_assert(
        std::is_same_v<async::allocator_of_t<
                           async::env_of_t<decltype(async::just_stopped())>>,
                       async::stack_allocator>);
}

TEST_CASE("just_stopped op state is synchronous", "[just_stopped]") {
    [[maybe_unused]] auto op =
        async::connect(async::just_stopped(), receiver{[] {}});
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

TEST_CASE("just_stopped can be debugged with a string", "[just_stopped]") {
    using namespace std::string_literals;
    debug_events.clear();
    auto s = async::just_stopped();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op just_stopped start"s,
                                      "op just_stopped set_stopped"s});
}

TEST_CASE("just_stopped can be named and debugged with a string",
          "[just_stopped]") {
    using namespace std::string_literals;
    debug_events.clear();
    auto s = async::just_stopped<"just_stopped_name">();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op just_stopped_name start"s,
                                      "op just_stopped_name set_stopped"s});
}
