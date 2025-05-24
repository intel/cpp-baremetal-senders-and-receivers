#include <async/debug.hpp>
#include <async/env.hpp>

#include <stdx/ct_conversions.hpp>
#include <stdx/ct_string.hpp>

#include <catch2/catch_test_macros.hpp>

#include <concepts>
#include <functional>
#include <memory>
#include <type_traits>

namespace {
template <stdx::ct_string C, stdx::ct_string L, stdx::ct_string S>
bool handled{};

template <stdx::ct_string X, stdx::ct_string Y, typename... Ts>
struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S,
              async::debug::contextlike Ctx, typename... Args>
    constexpr auto signal(Args &&...) {
        static_assert((... and std::same_as<std::remove_cvref_t<Args>, Ts>));
        handled<X, Y, S> = true;
    }
};

template <stdx::ct_string Name> struct context {
    template <typename...> struct list;
    struct tag;
    constexpr static auto name = Name;
    using children = list<>;
    using type = int;
};
} // namespace

template <>
inline auto async::injected_debug_handler<stdx::cts_t<"A">, stdx::cts_t<"B">> =
    debug_handler<"AX", "BY", int>{};

template <>
inline auto async::injected_debug_handler<stdx::cts_t<"A">> =
    debug_handler<"AX", "", float>{};

template <>
inline auto async::injected_debug_handler<> = debug_handler<"", "", bool>{};

TEST_CASE("send a debug signal (fallback handler)", "[debug]") {
    handled<"", "", "signal"> = false;
    async::debug::signal<"X", "signal", context<"Y">>(true);
    CHECK(handled<"", "", "signal">);
}

TEST_CASE("send a debug signal (handler for chain name)", "[debug]") {
    handled<"AX", "", "signal"> = false;
    async::debug::signal<"A", "signal", context<"Y">>(1.0f);
    CHECK(handled<"AX", "", "signal">);
}

TEST_CASE("send a debug signal (handler for chain and link name)", "[debug]") {
    handled<"AX", "BY", "signal"> = false;
    async::debug::signal<"A", "signal", context<"B">>(42);
    CHECK(handled<"AX", "BY", "signal">);
}

TEST_CASE("default debug interface", "[debug]") {
    auto i = async::get_debug_interface(async::empty_env{});
    STATIC_REQUIRE(std::same_as<decltype(i), async::debug::default_interface>);
}

TEST_CASE("supplied debug interface", "[debug]") {
    auto iface = async::debug::make_named_interface<"testA">();
    auto e = async::prop{async::get_debug_interface_t{}, std::cref(iface)};
    auto &i = async::get_debug_interface(e);
    CHECK(std::addressof(i) == std::addressof(iface));
}

template <>
inline auto async::injected_debug_handler<stdx::cts_t<"named_chain_0">,
                                          stdx::cts_t<"named_link_0">> =
    debug_handler<"chain_0", "link_0", int, double>{};

TEST_CASE("named interface signal", "[debug]") {
    handled<"chain_0", "link_0", "signal"> = false;
    auto iface = async::debug::make_named_interface<"named_chain_0">(42);
    iface.template signal<"signal", context<"named_link_0">>(1.0);
    CHECK(handled<"chain_0", "link_0", "signal">);
}

namespace {
template <typename... Ts> struct debug_handler_B {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx,
              typename... Args>
    constexpr auto signal(Args &&...) {
        static_assert((... and std::same_as<std::remove_cvref_t<Args>, Ts>));
        handled<C, async::debug::name_of<Ctx>, S> = true;
    }
};
} // namespace

template <>
inline auto async::injected_debug_handler<stdx::cts_t<"chainB">> =
    debug_handler_B<int, double>{};

TEST_CASE("send debug signal", "[debug]") {
    handled<"chainB", "linkB", "signal"> = false;
    auto iface = async::debug::make_named_interface<"chainB">(42);
    auto e = async::prop{async::get_debug_interface_t{}, std::cref(iface)};
    async::debug_signal<"signal", context<"linkB">>(e, 1.0);
    CHECK(handled<"chainB", "linkB", "signal">);
}
