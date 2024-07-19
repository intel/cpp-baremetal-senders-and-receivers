#pragma once

#include <stdx/type_traits.hpp>

#include <utility>

namespace async {
constexpr inline struct connect_t {
    template <typename S, typename R>
    constexpr auto operator()(S &&s, R &&r) const
        noexcept(noexcept(std::forward<S>(s).connect(std::forward<R>(r))))
            -> decltype(std::forward<S>(s).connect(std::forward<R>(r))) {
        return std::forward<S>(s).connect(std::forward<R>(r));
    }
} connect{};

template <typename S, typename R>
using connect_result_t =
    decltype(connect(std::declval<S>(), std::declval<R>()));
} // namespace async
