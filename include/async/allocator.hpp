#pragma once

#include <async/completes_synchronously.hpp>
#include <async/forwarding_query.hpp>
#include <async/stack_allocator.hpp>
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
    {
        T::template allocation_limit<archetypes::domain>
    } -> std::same_as<std::size_t const &>;
};

constexpr inline struct get_allocator_t : forwarding_query_t {
    template <typename T>
        requires true // more constrained
    constexpr auto operator()(T &&t) const noexcept(
        noexcept(std::forward<T>(t).query(std::declval<get_allocator_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }

    template <typename T> constexpr auto operator()(T &&) const {
        if constexpr (completes_synchronously(T{})) {
            return stack_allocator{};
        } else {
            return static_allocator{};
        }
    }
} get_allocator;

template <typename T>
using allocator_of_t =
    std::remove_cvref_t<decltype(get_allocator(std::declval<T>()))>;
} // namespace async
