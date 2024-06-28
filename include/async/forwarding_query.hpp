#pragma once

#include <stdx/concepts.hpp>

#include <utility>

namespace async {
constexpr inline struct forwarding_query_t {
    template <typename T>
    constexpr auto operator()(T &&t) const noexcept(
        noexcept(std::forward<T>(t).query(std::declval<forwarding_query_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }

    template <typename T> constexpr auto operator()(T const &) const -> bool {
        return stdx::derived_from<T, forwarding_query_t>;
    }
} forwarding_query{};
} // namespace async
