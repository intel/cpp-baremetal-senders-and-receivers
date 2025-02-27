#pragma once

#include <async/concepts.hpp>
#include <async/debug_context.hpp>
#include <async/forwarding_query.hpp>

#include <stdx/ct_string.hpp>
#include <stdx/tuple.hpp>

#include <utility>

namespace async {
namespace debug {
struct null_handler {
    template <stdx::ct_string C, stdx::ct_string S, contextlike Ctx>
    constexpr auto signal(auto &&...) -> void {}
};
} // namespace debug

template <typename...>
inline auto injected_debug_handler = debug::null_handler{};

namespace debug {
template <stdx::ct_string ChainName, stdx::ct_string LinkName,
          typename... DummyArgs>
constexpr static auto get_handler() -> auto & {
    if constexpr (not std::same_as<
                      decltype(injected_debug_handler<stdx::cts_t<ChainName>,
                                                      stdx::cts_t<LinkName>,
                                                      DummyArgs...>),
                      null_handler>) {
        return injected_debug_handler<stdx::cts_t<ChainName>,
                                      stdx::cts_t<LinkName>, DummyArgs...>;
    } else if constexpr (not std::same_as<
                             decltype(injected_debug_handler<
                                      stdx::cts_t<ChainName>, DummyArgs...>),
                             null_handler>) {
        return injected_debug_handler<stdx::cts_t<ChainName>, DummyArgs...>;
    } else {
        return injected_debug_handler<DummyArgs...>;
    }
}

template <stdx::ct_string ChainName, stdx::ct_string Signal, contextlike Ctx,
          typename... DummyArgs, typename... Args>
auto signal(Args &&...args) -> void {
    auto &handler = get_handler<ChainName, name_of<Ctx>, DummyArgs...>();
    handler.template signal<ChainName, Signal, Ctx>(
        std::forward<Args>(args)...);
}

template <stdx::ct_string ChainName, typename... Ts>
struct named_interface : stdx::tuple<Ts...> {
    template <stdx::ct_string Signal, contextlike Ctx, typename... Args>
    constexpr auto signal(Args &&...args) const -> void {
        this->apply([&](Ts const &...ts) {
            debug::signal<ChainName, Signal, Ctx>(ts...,
                                                  std::forward<Args>(args)...);
        });
    }
};

template <stdx::ct_string Name>
constexpr auto make_named_interface = []<typename... Ts>(Ts &&...ts) {
    return named_interface<Name, std::remove_cvref_t<Ts>...>{
        std::forward<Ts>(ts)...};
};

using default_interface = named_interface<"unknown">;
} // namespace debug

constexpr inline struct get_debug_interface_t : forwarding_query_t {
    constexpr static auto name = stdx::ct_string{"get_debug_interface"};

    template <typename T>
        requires true // more constrained
    [[nodiscard]] constexpr auto operator()(T &&t) const noexcept(noexcept(
        std::forward<T>(t).query(std::declval<get_debug_interface_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }

    [[nodiscard]] constexpr auto operator()(auto &&) const
        -> debug::default_interface {
        return {};
    }
} get_debug_interface{};

template <stdx::ct_string Signal, debug::contextlike Ctx, queryable Q,
          typename... Args>
constexpr auto debug_signal(Q &&q, Args &&...args) -> void {
    get_debug_interface(q).template signal<Signal, Ctx>(
        std::forward<Args>(args)...);
}
} // namespace async
