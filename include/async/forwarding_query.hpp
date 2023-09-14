#pragma once

#include <stdx/concepts.hpp>

#include <utility>

namespace async {
constexpr inline struct forwarding_query_t {
    template <typename T>
    constexpr auto operator()(T &&t) const
        -> decltype(tag_invoke(std::declval<forwarding_query_t>(),
                               std::forward<T>(t))) {
        return tag_invoke(*this, std::forward<T>(t));
    }

    template <typename T> constexpr auto operator()(T const &) const -> bool {
        return stdx::derived_from<T, forwarding_query_t>;
    }
} forwarding_query{};
} // namespace async
