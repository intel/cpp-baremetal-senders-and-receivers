#pragma once

#include <async/forwarding_query.hpp>

#include <utility>

namespace async {
constexpr inline struct completes_synchronously_t : forwarding_query_t {
    template <typename T>
        requires true // more constrained
    constexpr auto operator()(T &&t) const noexcept(noexcept(
        std::forward<T>(t).query(std::declval<completes_synchronously_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }

    constexpr auto operator()(auto &&) const -> bool { return false; }
} completes_synchronously{};
} // namespace async
