#pragma once

#include <stdx/ct_string.hpp>
#include <stdx/type_traits.hpp>

#include <boost/mp11/algorithm.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
constexpr inline struct set_value_t {
    constexpr static auto name = stdx::ct_string{"set_value"};

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
    constexpr static auto name = stdx::ct_string{"set_error"};

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
    constexpr static auto name = stdx::ct_string{"set_stopped"};

    template <typename R>
    constexpr auto operator()(R &&r) const
        noexcept(noexcept(std::forward<R>(r).set_stopped()))
            -> decltype(std::forward<R>(r).set_stopped()) {
        static_assert(std::is_rvalue_reference_v<R &&>,
                      "set_stopped must be called on an rvalue reference");
        return std::forward<R>(r).set_stopped();
    }
} set_stopped{};

template <typename T>
concept channel_tag =
    std::same_as<set_value_t, T> or std::same_as<set_error_t, T> or
    std::same_as<set_stopped_t, T>;

template <channel_tag T, channel_tag U>
[[nodiscard]] consteval auto operator|(T, U)
    -> boost::mp11::mp_unique<stdx::type_list<T, U>> {
    return {};
}

template <channel_tag T, channel_tag... Us>
[[nodiscard]] consteval auto operator|(T, stdx::type_list<Us...>)
    -> boost::mp11::mp_unique<stdx::type_list<T, Us...>> {
    return {};
}

template <channel_tag T, channel_tag... Us>
[[nodiscard]] consteval auto operator|(stdx::type_list<Us...>, T)
    -> boost::mp11::mp_unique<stdx::type_list<T, Us...>> {
    return {};
}

template <channel_tag... Ts, channel_tag... Us>
[[nodiscard]] consteval auto operator|(stdx::type_list<Ts...>,
                                       stdx::type_list<Us...>)
    -> boost::mp11::mp_unique<stdx::type_list<Ts..., Us...>> {
    return {};
}
} // namespace async
