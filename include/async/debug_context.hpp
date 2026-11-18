#pragma once

#include <stdx/ct_string.hpp>

namespace async {
namespace debug {
template <typename T> struct context_for;

namespace detail {
template <typename T>
constexpr auto erased_context = []<typename C>(C) {
    struct Ctx : C {
        using context [[maybe_unused]] = C;
    };
    return Ctx{};
}(context_for<T>{});
} // namespace detail

template <typename T>
using erased_context_for = decltype(detail::erased_context<T>);

template <typename T> constexpr auto name_of = stdx::name_of_v<T>;
template <typename T> using tag_of = typename T::tag;
template <typename T> using children_of = typename T::children;
template <typename T> using type_of = typename T::type;

template <typename T> constexpr auto contextlike_v = false;

template <typename T>
concept contextlike = stdx::named<T> and requires {
    typename T::tag;
    typename T::type;
} and[]<template <typename...> typename L, typename... Ts>(L<Ts...> *) {
    return (... and contextlike_v<Ts>);
}
(std::add_pointer_t<typename T::children>{});

template <contextlike T> constexpr auto contextlike_v<T> = contextlike<T>;
} // namespace debug
} // namespace async
