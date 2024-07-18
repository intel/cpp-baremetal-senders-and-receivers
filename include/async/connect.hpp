#pragma once

#include <stdx/type_traits.hpp>

#include <utility>

namespace async {
constexpr inline struct connect_t {
    template <typename...> struct failure_t {};

    template <typename... Ts>
        requires true
    constexpr auto operator()(Ts &&...ts) const
        noexcept(noexcept(tag_invoke(std::declval<connect_t>(),
                                     std::forward<Ts>(ts)...)))
            -> decltype(tag_invoke(*this, std::forward<Ts>(ts)...)) {
        return tag_invoke(*this, std::forward<Ts>(ts)...);
    }

    template <typename... Ts>
    constexpr auto operator()(Ts &&...) const -> failure_t<Ts...> {
        static_assert(stdx::always_false_v<Ts...>,
                      "No function call for connect: are the arguments a "
                      "sender and receiver?");
        return {};
    }
} connect{};

template <typename S, typename R>
using connect_result_t =
    decltype(connect(std::declval<S>(), std::declval<R>()));
} // namespace async
