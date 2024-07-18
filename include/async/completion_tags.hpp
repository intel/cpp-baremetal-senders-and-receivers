#pragma once

#include <utility>

namespace async {
constexpr inline struct set_value_t {
    template <typename R, typename... Ts>
    constexpr auto operator()(R &&r, Ts &&...ts) const noexcept(
        noexcept(std::forward<R>(r).set_value(std::forward<Ts>(ts)...)))
        -> decltype(std::forward<R>(r).set_value(std::forward<Ts>(ts)...)) {
        return std::forward<R>(r).set_value(std::forward<Ts>(ts)...);
    }
} set_value{};

constexpr inline struct set_error_t {
    template <typename R, typename... Ts>
    constexpr auto operator()(R &&r, Ts &&...ts) const noexcept(
        noexcept(std::forward<R>(r).set_error(std::forward<Ts>(ts)...)))
        -> decltype(std::forward<R>(r).set_error(std::forward<Ts>(ts)...)) {
        return std::forward<R>(r).set_error(std::forward<Ts>(ts)...);
    }
} set_error{};

constexpr inline struct set_stopped_t {
    template <typename R>
    constexpr auto operator()(R &&r) const
        noexcept(noexcept(std::forward<R>(r).set_stopped()))
            -> decltype(std::forward<R>(r).set_stopped()) {
        return std::forward<R>(r).set_stopped();
    }
} set_stopped{};
} // namespace async
