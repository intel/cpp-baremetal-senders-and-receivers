#pragma once

#include <async/concepts.hpp>
#include <async/let.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <type_traits>
#include <utility>

namespace async {
namespace _let_stopped {
template <typename S, typename F>
class sender : public _let::sender<sender<S, F>, S, F, set_stopped_t> {
    using base = _let::sender<sender, S, F, set_stopped_t>;

    template <typename Env>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> make_completion_signatures<
            S, Env, typename base::template dependent_signatures<Env>,
            detail::default_set_value, detail::default_set_error,
            completion_signatures<>> {
        return {};
    }

  public:
    [[no_unique_address]] S s;
    [[no_unique_address]] F f;
};
} // namespace _let_stopped

template <stdx::callable F>
[[nodiscard]] constexpr auto let_stopped(F &&f)
    -> _let::pipeable<std::remove_cvref_t<F>, _let_stopped::sender> {
    return {std::forward<F>(f)};
}

template <sender S, stdx::callable F>
[[nodiscard]] constexpr auto let_stopped(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | let_stopped(std::forward<F>(f));
}
} // namespace async
