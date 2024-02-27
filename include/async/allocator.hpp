#pragma once

#include <async/forwarding_query.hpp>
#include <async/static_allocator.hpp>

#include <type_traits>
#include <utility>

namespace async {
namespace archetypes {
struct constructible {
    constexpr explicit constructible(int) {}
};
struct domain;
} // namespace archetypes

template <typename T>
concept allocator = std::is_empty_v<T> and requires(
                                               T &,
                                               archetypes::constructible *p) {
    {
        T::template construct<archetypes::domain, archetypes::constructible>(
            [](archetypes::constructible) {}, 0)
    } -> std::same_as<bool>;
    { T::template destruct<archetypes::domain>(p) } -> std::same_as<void>;
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
