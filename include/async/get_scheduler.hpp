#pragma once

#include <async/forwarding_query.hpp>

#include <stdx/type_traits.hpp>

#include <utility>

namespace async {
struct get_scheduler_t : forwarding_query_t {
    template <typename...> struct failure_t {};

    template <typename T>
        requires true
    constexpr auto operator()(T &&t) const noexcept(
        noexcept(std::forward<T>(t).query(std::declval<get_scheduler_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }

    template <typename T>
    constexpr auto operator()(T &&) const -> failure_t<T> {
        static_assert(stdx::always_false_v<T>,
                      "No function call for get_scheduler: does the argument "
                      "provide a get_scheduler_t query?");
        return {};
    }
};

template <typename E> auto get_scheduler(E &&e) {
    return get_scheduler_t{}(std::forward<E>(e));
}
} // namespace async
