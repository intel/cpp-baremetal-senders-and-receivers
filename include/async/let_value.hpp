#pragma once

#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/let.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>

#include <type_traits>
#include <utility>

namespace async {
namespace _let_value {
template <stdx::ct_string Name, typename S, typename F>
using sender = _let::sender<Name, S, F, set_value_t>;
} // namespace _let_value

template <stdx::ct_string Name = "let_value", stdx::callable F>
[[nodiscard]] constexpr auto let_value(F &&f) {
    return compose(
        _let::pipeable<Name, std::remove_cvref_t<F>, _let_value::sender>{
            std::forward<F>(f)});
}

template <stdx::ct_string Name = "let_value", sender S, stdx::callable F>
[[nodiscard]] constexpr auto let_value(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | let_value<Name>(std::forward<F>(f));
}
} // namespace async
