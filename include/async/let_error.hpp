#pragma once

#include <async/concepts.hpp>
#include <async/let.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <type_traits>
#include <utility>

namespace async {
namespace _let_error {
template <typename S, typename F>
using sender = _let::sender<S, F, set_error_t>;
} // namespace _let_error

template <stdx::callable F>
[[nodiscard]] constexpr auto let_error(F &&f)
    -> _let::pipeable<std::remove_cvref_t<F>, _let_error::sender> {
    return {std::forward<F>(f)};
}

template <sender S, stdx::callable F>
[[nodiscard]] constexpr auto let_error(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | let_error(std::forward<F>(f));
}
} // namespace async
