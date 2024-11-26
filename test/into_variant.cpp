#include "detail/common.hpp"

#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/into_variant.hpp>
#include <async/just.hpp>
#include <async/start.hpp>
#include <async/then.hpp>
#include <async/variant_sender.hpp>

#include <stdx/ct_format.hpp>
#include <stdx/tuple.hpp>
#include <stdx/utility.hpp>

#include <boost/mp11/list.hpp>
#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

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
    static_assert(
        async::sender_of<decltype(iv),
                         async::set_value_t(std::variant<stdx::tuple<int>,
                                                         stdx::tuple<float>>)>);
    static_assert(not async::sender_of<decltype(iv), async::set_error_t()>);
}

TEST_CASE("into_variant advertises errors", "[into_variant]") {
    auto s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just_error(3.14f); });
    [[maybe_unused]] auto iv = async::into_variant(s);
    static_assert(
        async::sender_of<decltype(iv),
                         async::set_value_t(std::variant<stdx::tuple<int>>)>);
    static_assert(async::sender_of<decltype(iv), async::set_error_t(float)>);
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

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    constexpr auto signal(auto &&...) {
        using namespace stdx::literals;
        if constexpr (std::is_same_v<async::debug::tag_of<Ctx>,
                                     async::into_variant_t>) {
            static_assert(not boost::mp11::mp_empty<
                          async::debug::children_of<Ctx>>::value);
            debug_events.push_back(
                fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
        }
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

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
