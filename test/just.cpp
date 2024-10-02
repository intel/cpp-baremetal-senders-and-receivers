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

#include <boost/mp11/list.hpp>
#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

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
    static_assert(
        async::sender_of<decltype(async::just(42)), async::set_value_t(int)>);
}

TEST_CASE("move-only value", "[just]") {
    int value{};
    auto s = async::just(move_only{42});
    static_assert(async::singleshot_sender<decltype(s), universal_receiver>);
    auto op = async::connect(
        std::move(s), receiver{[&](move_only<int> mo) { value = mo.value; }});
    async::start(op);
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

TEST_CASE("just op state is synchronous", "[just]") {
    [[maybe_unused]] auto op = async::connect(async::just(42), receiver{[] {}});
    static_assert(async::synchronous<decltype(op)>);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    constexpr auto signal(auto &&...) {
        static_assert(std::is_same_v<async::debug::tag_of<Ctx>, async::just_t>);
        static_assert(
            boost::mp11::mp_empty<async::debug::children_of<Ctx>>::value);
        debug_events.push_back(
            fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

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
