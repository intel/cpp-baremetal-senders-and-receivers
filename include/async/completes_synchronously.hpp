#pragma once

#include <async/env.hpp>
#include <async/forwarding_query.hpp>

#include <stdx/ct_string.hpp>

#include <type_traits>
#include <utility>

namespace async {
constexpr inline struct completes_synchronously_t : forwarding_query_t {
    constexpr static auto name = stdx::ct_string{"completes_synchronously"};

    template <typename T>
        requires true // more constrained
    constexpr auto operator()(T &&t) const noexcept(noexcept(
        std::forward<T>(t).query(std::declval<completes_synchronously_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }

    constexpr auto operator()(auto &&) const -> std::false_type { return {}; }
} completes_synchronously{};

template <typename S>
concept synchronous = static_cast<bool>(completes_synchronously(env_of_t<S>{}));
template <typename S> using synchronous_t = std::bool_constant<synchronous<S>>;
} // namespace async
