#pragma once

#include <async/forwarding_query.hpp>

#include <utility>

namespace async {
template <typename Tag> struct get_completion_scheduler_t : forwarding_query_t {
    template <typename T>
    constexpr auto operator()(T &&t) const
        -> decltype(tag_invoke(std::declval<get_completion_scheduler_t>(),
                               std::forward<T>(t))) {
        return tag_invoke(*this, std::forward<T>(t));
    }
};

template <typename Tag>
constexpr inline auto get_completion_scheduler =
    get_completion_scheduler_t<Tag>{};
} // namespace async
