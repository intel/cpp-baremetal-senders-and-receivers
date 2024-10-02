#pragma once

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
namespace _let_stopped {
template <stdx::ct_string Name, typename S, typename F>
using sender = _let::sender<Name, S, F, set_stopped_t>;
} // namespace _let_stopped

template <stdx::ct_string Name = "let_stopped", stdx::callable F>
[[nodiscard]] constexpr auto let_stopped(F &&f) {
    return _compose::adaptor{
        _let::pipeable<Name, std::remove_cvref_t<F>, _let_stopped::sender>{
            std::forward<F>(f)}};
}

template <stdx::ct_string Name = "let_stopped", sender S, stdx::callable F>
[[nodiscard]] constexpr auto let_stopped(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | let_stopped<Name>(std::forward<F>(f));
}
} // namespace async
