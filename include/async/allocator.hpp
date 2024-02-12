#pragma once

#include <async/forwarding_query.hpp>
#include <async/static_allocator.hpp>

#include <type_traits>
#include <utility>

namespace async {
namespace detail {
struct prototype_constructible {
    constexpr explicit prototype_constructible(int) {}
};
struct prototype_domain;
} // namespace detail

template <typename T>
concept allocator =
    std::is_empty_v<T> and requires(T &, detail::prototype_constructible *p) {
        {
            T::template construct<detail::prototype_domain,
                                  detail::prototype_constructible>(
                [](detail::prototype_constructible) {}, 0)
        } -> std::same_as<bool>;
        {
            T::template destruct<detail::prototype_domain>(p)
        } -> std::same_as<void>;
    };

constexpr inline struct get_allocator_t : forwarding_query_t {
    template <typename T>
        requires true // more constrained
    constexpr auto operator()(T &&t) const
        -> decltype(tag_invoke(std::declval<get_allocator_t>(),
                               std::forward<T>(t))) {
        return tag_invoke(*this, std::forward<T>(t));
    }

    constexpr auto operator()(auto &&) const -> static_allocator { return {}; }
} get_allocator;

template <typename T>
using allocator_of_t = decltype(get_allocator(std::declval<T>()));
} // namespace async
