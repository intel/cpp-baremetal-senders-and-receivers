#include "detail/common.hpp"
#include "detail/debug_handler.hpp"

#include <async/connect.hpp>
#include <async/into_variant.hpp>
#include <async/just.hpp>
#include <async/schedulers/trigger_scheduler.hpp>
#include <async/start.hpp>
#include <async/then.hpp>
#include <async/variant_sender.hpp>

#include <stdx/tuple.hpp>
#include <stdx/utility.hpp>

#include <catch2/catch_test_macros.hpp>

#include <concepts>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

TEST_CASE("into_variant advertises what it sends", "[into_variant]") {
    auto s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just(3.14f); });
    [[maybe_unused]] auto iv = async::into_variant(s);
    STATIC_REQUIRE(
        async::sender_of<decltype(iv),
                         async::set_value_t(std::variant<stdx::tuple<int>,
                                                         stdx::tuple<float>>)>);
    STATIC_REQUIRE(not async::sender_of<decltype(iv), async::set_error_t()>);
}

TEST_CASE("into_variant advertises errors", "[into_variant]") {
    auto s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just_error(3.14f); });
    [[maybe_unused]] auto iv = async::into_variant(s);
    STATIC_REQUIRE(
        async::sender_of<decltype(iv),
                         async::set_value_t(std::variant<stdx::tuple<int>>)>);
    STATIC_REQUIRE(async::sender_of<decltype(iv), async::set_error_t(float)>);
}

TEST_CASE("basic operation", "[into_variant]") {
    int value{};
    auto s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just(3.14f); });
    auto iv = async::into_variant(s);
    auto op = async::connect(
        iv, receiver{[&](auto i) {
            std::visit(stdx::overload{[&](stdx::tuple<int>) { value = 1; },
                                      [&](stdx::tuple<float>) { value = 2; }},
                       i);
        }});
    async::start(op);
    CHECK(value == 2);
}

TEST_CASE("sending 2 possible completions", "[into_variant]") {
    int value{};
    auto s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just(3.14f, 17); });
    auto iv = async::into_variant(s);
    auto op = async::connect(
        iv, receiver{[&](auto i) {
            std::visit(
                stdx::overload{[&](stdx::tuple<int>) { value = 1; },
                               [&](stdx::tuple<float, int>) { value = 2; }},
                i);
        }});
    async::start(op);
    CHECK(value == 2);
}

TEST_CASE("sending N possible completions", "[into_variant]") {
    int value{};

    auto const s = async::make_variant_sender(
        async::match([](auto i, auto j) { return i + j == 3; }) >>
            [](auto, auto) { return async::just(42); },
        async::match([](auto i, auto j) { return i + j == 5; }) >>
            [](auto, auto) { return async::just(3.14f); },
        async::otherwise >> [](auto, auto) { return async::just(); }, 3, 2);

    auto iv = async::into_variant(s);
    auto op = async::connect(
        iv, receiver{[&](auto i) {
            std::visit(stdx::overload{[&](stdx::tuple<int>) { value = 1; },
                                      [&](stdx::tuple<float>) { value = 2; },
                                      [&](stdx::tuple<>) { value = 3; }},
                       i);
        }});
    async::start(op);
    CHECK(value == 2);
}

TEST_CASE("sending 2 possible completions (same type)", "[into_variant]") {
    int value{};
    auto s = async::make_variant_sender(
        true, [] { return async::just(42); },
        [] { return async::trigger_scheduler<"", int>{}.schedule(); });
    auto iv = async::into_variant(s);
    auto op = async::connect(
        iv, receiver{[&](auto i) {
            static_assert(
                std::same_as<decltype(i), std::variant<stdx::tuple<int>>>);
            std::visit([&](stdx::tuple<int> const &t) { value = get<0>(t); },
                       i);
        }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("sending void values", "[into_variant]") {
    int value{};
    auto s = async::make_variant_sender(
        false, [] { return async::just(42); }, [] { return async::just(); });
    auto iv = async::into_variant(s);
    auto op = async::connect(
        iv, receiver{[&](auto i) {
            std::visit(stdx::overload{[&](stdx::tuple<int>) { value = 1; },
                                      [&](stdx::tuple<>) { value = 2; }},
                       i);
        }});
    async::start(op);
    CHECK(value == 2);
}

TEST_CASE("sending move-only values", "[into_variant]") {
    int value{};
    auto s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just(move_only{42}); });
    auto iv = async::into_variant(std::move(s));
    auto op = async::connect(
        std::move(iv), receiver{[&](auto i) {
            std::visit(
                stdx::overload{[&](stdx::tuple<int>) { value = 1; },
                               [&](stdx::tuple<move_only<int>>) { value = 2; }},
                std::move(i));
        }});
    async::start(op);
    CHECK(value == 2);
}

TEST_CASE("into_variant is pipeable", "[into_variant]") {
    int value{};
    auto s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just(3.14f); });
    auto iv = s | async::into_variant();
    auto op = async::connect(
        iv, receiver{[&](auto i) {
            std::visit(stdx::overload{[&](stdx::tuple<int>) { value = 1; },
                                      [&](stdx::tuple<float>) { value = 2; }},
                       i);
        }});
    async::start(op);
    CHECK(value == 2);
}

TEST_CASE("into_variant is adaptor-pipeable", "[into_variant]") {
    int value{};
    auto s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just(3.14f); });

    auto iv = async::into_variant() | async::then([](auto i) { return i; });
    auto op = async::connect(
        s | iv, receiver{[&](auto i) {
            std::visit(stdx::overload{[&](stdx::tuple<int>) { value = 1; },
                                      [&](stdx::tuple<float>) { value = 2; }},
                       i);
        }});
    async::start(op);
    CHECK(value == 2);
}

namespace {
template <typename... Ts>
using alt_variant = std::variant<std::monostate, Ts...>;
}

TEST_CASE("alternative variant", "[into_variant]") {
    int value{};
    auto s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just(3.14f); });
    auto iv = async::into_variant<"alt", alt_variant>(s);
    auto op = async::connect(
        iv, receiver{[&](auto i) {
            CHECK(std::same_as<decltype(i),
                               std::variant<std::monostate, stdx::tuple<int>,
                                            stdx::tuple<float>>>);
            value = 42;
        }});
    async::start(op);
    CHECK(value == 42);
}

template <>
inline auto async::injected_debug_handler<> =
    debug_handler<async::into_variant_t>{};

TEST_CASE("into_variant can be debugged with a string", "[into_variant]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::make_variant_sender(
                 false, [] { return async::just(42); },
                 [] { return async::just(3.14f); }) |
             async::into_variant();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    CHECK(debug_events == std::vector{"op into_variant set_value"s});
}

TEST_CASE("into_variant can be named and debugged with a string",
          "[into_variant]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::make_variant_sender(
                 false, [] { return async::just(42); },
                 [] { return async::just(3.14f); }) |
             async::into_variant<"iv_name">();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    CHECK(debug_events == std::vector{"op iv_name set_value"s});
}
