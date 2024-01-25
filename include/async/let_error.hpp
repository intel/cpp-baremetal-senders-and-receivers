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
template <typename F, typename Ops, typename Rcvr>
struct first_receiver : _let::second_receiver<Ops, Rcvr> {
    [[no_unique_address]] F f;

  private:
    template <stdx::same_as_unqualified<first_receiver> Self, typename... Args>
    friend auto tag_invoke(set_error_t, Self &&self, Args &&...args) -> void {
        self.ops->complete_first(
            std::forward<Self>(self).f(std::forward<Args>(args)...));
    }
};

template <typename S, typename F>
class sender
    : public _let::sender<sender<S, F>, S, F, set_error_t, first_receiver> {
    using base = _let::sender<sender, S, F, set_error_t, first_receiver>;

    template <typename Env>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> make_completion_signatures<
            S, Env, typename base::template dependent_signatures<Env>,
            detail::default_set_value, base::template signatures> {
        return {};
    }

  public:
    [[no_unique_address]] S s;
    [[no_unique_address]] F f;
};
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
