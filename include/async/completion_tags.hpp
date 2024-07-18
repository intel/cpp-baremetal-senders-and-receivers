#pragma once

#include <type_traits>
#include <utility>

namespace async {
constexpr inline struct set_value_t {
    template <typename R, typename... Ts>
    constexpr auto operator()(R &&r, Ts &&...ts) const noexcept(
        noexcept(std::forward<R>(r).set_value(std::forward<Ts>(ts)...)))
        -> decltype(std::forward<R>(r).set_value(std::forward<Ts>(ts)...)) {
        static_assert(std::is_rvalue_reference_v<R &&>,
                      "set_value must be called on an rvalue reference");
        return std::forward<R>(r).set_value(std::forward<Ts>(ts)...);
    }
} set_value{};

constexpr inline struct set_error_t {
    template <typename R, typename... Ts>
    constexpr auto operator()(R &&r, Ts &&...ts) const noexcept(
        noexcept(std::forward<R>(r).set_error(std::forward<Ts>(ts)...)))
        -> decltype(std::forward<R>(r).set_error(std::forward<Ts>(ts)...)) {
        static_assert(std::is_rvalue_reference_v<R &&>,
                      "set_error must be called on an rvalue reference");
        return std::forward<R>(r).set_error(std::forward<Ts>(ts)...);
    }
} set_error{};

constexpr inline struct set_stopped_t {
    template <typename R>
    constexpr auto operator()(R &&r) const
        noexcept(noexcept(std::forward<R>(r).set_stopped()))
            -> decltype(std::forward<R>(r).set_stopped()) {
        static_assert(std::is_rvalue_reference_v<R &&>,
                      "set_stopped must be called on an rvalue reference");
        return std::forward<R>(r).set_stopped();
    }
} set_stopped{};
} // namespace async
