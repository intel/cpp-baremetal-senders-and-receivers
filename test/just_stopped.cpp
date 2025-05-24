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

TEST_CASE("one value", "[just_stopped]") {
    int value{};
    auto s = async::just_stopped();
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("just_stopped advertises what it sends", "[just_stopped]") {
    STATIC_REQUIRE(async::sender_of<decltype(async::just_stopped()),
                                    async::set_stopped_t()>);
}

TEST_CASE("copy sender", "[just_stopped]") {
    int value{};
    auto const s = async::just_stopped();
    STATIC_REQUIRE(async::multishot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move sender", "[just_stopped]") {
    int value{};
    auto s = async::just_stopped();
    STATIC_REQUIRE(async::multishot_sender<decltype(s), universal_receiver>);
    auto op =
        async::connect(std::move(s), stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("just_stopped has a stack allocator", "[just_stopped]") {
    STATIC_REQUIRE(
        std::is_same_v<async::allocator_of_t<
                           async::env_of_t<decltype(async::just_stopped())>>,
                       async::stack_allocator>);
}

TEST_CASE("just_stopped op state is synchronous", "[just_stopped]") {
    [[maybe_unused]] auto op =
        async::connect(async::just_stopped(), receiver{[] {}});
    STATIC_REQUIRE(async::synchronous<decltype(op)>);
}

template <>
inline auto async::injected_debug_handler<> =
    debug_handler<async::just_stopped_t, true>{};

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
