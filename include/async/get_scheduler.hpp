#pragma once

#include <async/forwarding_query.hpp>

#include <stdx/type_traits.hpp>

#include <utility>

namespace async {
struct get_scheduler_t : forwarding_query_t {
    template <typename T>
    constexpr auto operator()(T &&t) const noexcept(
        noexcept(std::forward<T>(t).query(std::declval<get_scheduler_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }
};

template <typename E> auto get_scheduler(E &&e) {
    return get_scheduler_t{}(std::forward<E>(e));
}
} // namespace async
