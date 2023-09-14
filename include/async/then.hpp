#pragma once

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/forwarding_query.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/type_traits.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/list.hpp>

#include <concepts>
#include <functional>
#include <type_traits>
#include <utility>

namespace async {
namespace _then {

template <typename R, typename F> struct base_receiver : R {
    [[no_unique_address]] F f;

    template <typename Tag, typename... Args> auto set(Args &&...args) -> void {
        if constexpr (std::is_void_v<std::invoke_result_t<F, Args...>>) {
            std::invoke(f, std::forward<Args>(args)...);
            Tag{}(static_cast<R &>(*this));
        } else {
            Tag{}(static_cast<R &>(*this),
                  std::invoke(f, std::forward<Args>(args)...));
        }
    }
};

template <typename Tag, typename R> struct tag_receiver;

template <typename R> struct tag_receiver<set_value_t, R> : R {
    template <typename... Args> auto set_value(Args &&...args) -> void {
        this->template set<set_value_t>(std::forward<Args>(args)...);
    }
};

template <typename R> struct tag_receiver<set_error_t, R> : R {
    template <typename... Args> auto set_error(Args &&...args) -> void {
        this->template set<set_error_t>(std::forward<Args>(args)...);
    }
};
template <typename R> struct tag_receiver<set_stopped_t, R> : R {
    auto set_stopped() -> void { this->template set<set_stopped_t>(); }
};

template <typename Tag, typename R, typename F>
using receiver = tag_receiver<Tag, base_receiver<R, F>>;

template <typename Tag, typename S, typename F> class sender {
    template <receiver_from<sender> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender &&self,
                                                   R &&r) {
        return connect(std::move(self).s,
                       receiver<Tag, std::remove_cvref_t<R>, F>{
                           {{std::forward<R>(r)}, std::move(self).f}});
    }

    template <typename Self, receiver_from<sender> R>
        requires std::same_as<sender, std::remove_cvref_t<Self>> and
                 multishot_sender<S, R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r) {
        return connect(std::forward<Self>(self).s,
                       receiver<Tag, std::remove_cvref_t<R>, F>{
                           {{std::forward<R>(r)}, std::forward<Self>(self).f}});
    }

    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   sender const &sndr) {
        return forward_env_of(sndr.s);
    }

    template <typename... As> using to_signature = Tag(As...);
    template <typename... Ts>
    using as_signature =
        detail::eat_void_t<std::invoke_result_t<F, Ts...>, to_signature>;
    template <typename... As>
    using signatures = completion_signatures<as_signature<As...>>;

    template <typename Env>
        requires std::same_as<Tag, set_value_t>
    [[nodiscard]] friend constexpr auto
    tag_invoke(get_completion_signatures_t, sender const &, Env const &) {
        return make_completion_signatures<S, Env, completion_signatures<>,
                                          signatures>{};
    }

    template <typename Env>
        requires std::same_as<Tag, set_error_t>
    [[nodiscard]] friend constexpr auto
    tag_invoke(get_completion_signatures_t, sender const &, Env const &) {
        return make_completion_signatures<S, Env, completion_signatures<>,
                                          detail::default_set_value,
                                          signatures>{};
    }

    template <typename Env>
        requires std::same_as<Tag, set_stopped_t>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> make_completion_signatures<S, Env> {
        return {};
    }

  public:
    using is_sender = void;

    [[no_unique_address]] S s;
    [[no_unique_address]] F f;
};

template <typename Tag, typename F> struct pipeable {
    F f;

  private:
    template <async::sender S, typename Self>
        requires std::same_as<pipeable, std::remove_cvref_t<Self>>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Tag, std::remove_cvref_t<S>, F>{
            std::forward<S>(s), std::forward<Self>(self).f};
    }
};
} // namespace _then

template <stdx::callable F>
[[nodiscard]] constexpr auto then(F &&f)
    -> _then::pipeable<set_value_t, std::remove_cvref_t<F>> {
    return {std::forward<F>(f)};
}

template <sender S, stdx::callable F>
[[nodiscard]] constexpr auto then(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | then(std::forward<F>(f));
}

template <stdx::callable F>
[[nodiscard]] constexpr auto upon_error(F &&f)
    -> _then::pipeable<set_error_t, std::remove_cvref_t<F>> {
    return {std::forward<F>(f)};
}

template <sender S, stdx::callable F>
[[nodiscard]] constexpr auto upon_error(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | upon_error(std::forward<F>(f));
}

template <stdx::callable F>
[[nodiscard]] constexpr auto upon_stopped(F &&f)
    -> _then::pipeable<set_stopped_t, std::remove_cvref_t<F>> {
    return {std::forward<F>(f)};
}

template <sender S, stdx::callable F>
[[nodiscard]] constexpr auto upon_stopped(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | upon_stopped(std::forward<F>(f));
}
} // namespace async
