#pragma once

#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/let.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <type_traits>
#include <utility>

namespace async {
namespace _let_value {
template <typename S, typename F>
using sender = _let::sender<S, F, set_value_t>;
} // namespace _let_value

template <stdx::callable F> [[nodiscard]] constexpr auto let_value(F &&f) {
    return _compose::adaptor{
        stdx::tuple{_let::pipeable<std::remove_cvref_t<F>, _let_value::sender>{
            std::forward<F>(f)}}};
}

template <sender S, stdx::callable F>
[[nodiscard]] constexpr auto let_value(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | let_value(std::forward<F>(f));
}
} // namespace async
