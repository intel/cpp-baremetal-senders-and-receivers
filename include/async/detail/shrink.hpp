#pragma once

#include <stdx/ct_string.hpp>

#include <type_traits>

namespace async::detail {
template <auto Value> struct shrinkwrap {
    constexpr static auto value = Value;
};

template <stdx::ct_string S,
          auto F = []()->auto (*)() -> shrinkwrap<S> { return nullptr; }>
consteval auto shrink() {
    return F;
}

template <stdx::ct_string S> using shrink_t = decltype(shrink<S>());

template <typename T>
constexpr auto expand_v = std::invoke_result_t<std::invoke_result_t<T>>::value;
} // namespace async::detail
