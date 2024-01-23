#pragma once

#include <async/forwarding_query.hpp>

#include <stdx/type_traits.hpp>

#include <utility>

namespace async {
constexpr inline struct set_value_t {
    template <typename... Ts>
    constexpr auto operator()(Ts &&...ts) const
        noexcept(noexcept(tag_invoke(std::declval<set_value_t>(),
                                     std::forward<Ts>(ts)...)))
            -> decltype(tag_invoke(*this, std::forward<Ts>(ts)...)) {
        return tag_invoke(*this, std::forward<Ts>(ts)...);
    }
} set_value{};

constexpr inline struct set_error_t {
    template <typename... Ts>
    constexpr auto operator()(Ts &&...ts) const
        noexcept(noexcept(tag_invoke(std::declval<set_error_t>(),
                                     std::forward<Ts>(ts)...)))
            -> decltype(tag_invoke(*this, std::forward<Ts>(ts)...)) {
        return tag_invoke(*this, std::forward<Ts>(ts)...);
    }
} set_error{};

constexpr inline struct set_stopped_t {
    template <typename... Ts>
    constexpr auto operator()(Ts &&...ts) const
        noexcept(noexcept(tag_invoke(std::declval<set_stopped_t>(),
                                     std::forward<Ts>(ts)...)))
            -> decltype(tag_invoke(*this, std::forward<Ts>(ts)...)) {
        return tag_invoke(*this, std::forward<Ts>(ts)...);
    }
} set_stopped{};

template <typename T>
concept channel_tag =
    std::same_as<set_value_t, T> or std::same_as<set_error_t, T> or
    std::same_as<set_stopped_t, T>;

constexpr inline struct connect_t {
    template <typename... Ts>
        requires true
    constexpr auto operator()(Ts &&...ts) const
        noexcept(noexcept(tag_invoke(std::declval<connect_t>(),
                                     std::forward<Ts>(ts)...)))
            -> decltype(tag_invoke(*this, std::forward<Ts>(ts)...)) {
        return tag_invoke(*this, std::forward<Ts>(ts)...);
    }

    template <typename... Ts>
    constexpr auto operator()(Ts &&...) const -> void {
        static_assert(stdx::always_false_v<Ts...>,
                      "No function call for connect: are the arguments a "
                      "sender and receiver?");
    }
} connect{};

template <typename S, typename R>
using connect_result_t =
    decltype(connect(std::declval<S>(), std::declval<R>()));

struct get_scheduler_t : forwarding_query_t {
    template <typename T>
        requires true
    constexpr auto operator()(T &&t) const
        noexcept(noexcept(tag_invoke(std::declval<get_scheduler_t>(),
                                     std::forward<T>(t))))
            -> decltype(tag_invoke(*this, std::forward<T>(t))) {
        return tag_invoke(*this, std::forward<T>(t));
    }

    template <typename T> constexpr auto operator()(T &&) const -> void {
        static_assert(stdx::always_false_v<T>,
                      "No function call for get_scheduler: does the argument "
                      "provide a tag_invoke(get_scheduler_t)?");
    }
};

template <typename E> auto get_scheduler(E &&e) {
    return get_scheduler_t{}(std::forward<E>(e));
}
} // namespace async
