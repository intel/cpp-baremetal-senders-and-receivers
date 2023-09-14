#pragma once

#include <async/concepts.hpp>
#include <async/let.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <type_traits>
#include <utility>

namespace async {
namespace _let_value {
template <typename F, typename Ops, typename Rcvr>
struct first_receiver : _let::second_receiver<Ops, Rcvr> {
    template <typename... Args> auto set_value(Args &&...args) const & -> void {
        this->ops->complete_first(f(std::forward<Args>(args)...));
    }
    template <typename... Args> auto set_value(Args &&...args) && -> void {
        this->ops->complete_first(std::move(f)(std::forward<Args>(args)...));
    }

    [[no_unique_address]] F f;
};

template <typename S, typename F>
class sender
    : public _let::sender<sender<S, F>, S, F, set_value_t, first_receiver> {
    using base = _let::sender<sender, S, F, set_value_t, first_receiver>;

    template <typename Env>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> make_completion_signatures<
            S, Env, typename base::template dependent_signatures<Env>,
            base::template signatures> {
        return {};
    }

  public:
    [[no_unique_address]] S s;
    [[no_unique_address]] F f;
};
} // namespace _let_value

template <stdx::callable F>
[[nodiscard]] constexpr auto let_value(F &&f)
    -> _let::pipeable<std::remove_cvref_t<F>, _let_value::sender> {
    return {std::forward<F>(f)};
}

template <sender S, stdx::callable F>
[[nodiscard]] constexpr auto let_value(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | let_value(std::forward<F>(f));
}
} // namespace async
