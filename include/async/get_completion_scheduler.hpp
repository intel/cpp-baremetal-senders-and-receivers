#pragma once

#include <async/forwarding_query.hpp>

#include <utility>

namespace async {
template <typename Tag> struct get_completion_scheduler_t : forwarding_query_t {
    template <typename T>
    constexpr auto operator()(T &&t) const noexcept(noexcept(
        std::forward<T>(t).query(std::declval<get_completion_scheduler_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }
};

template <typename Tag>
constexpr inline auto get_completion_scheduler =
    get_completion_scheduler_t<Tag>{};
} // namespace async
