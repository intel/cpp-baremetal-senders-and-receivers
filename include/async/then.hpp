#pragma once

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/forwarding_query.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/tuple.hpp>
#include <stdx/tuple_algorithms.hpp>
#include <stdx/type_traits.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/list.hpp>

#include <concepts>
#include <functional>
#include <type_traits>
#include <utility>

namespace async {
namespace _then {

template <typename R, typename... Fs> struct base_receiver : R {
    [[no_unique_address]] stdx::tuple<Fs...> fs;

    struct void_t {};
    template <typename T>
    using nonvoid_result_t = std::bool_constant<not std::is_same_v<void_t, T>>;

    template <typename Tag, typename... Args> auto set(Args &&...args) -> void {
        auto const exec = []<typename F, typename Arg>(F &&f, Arg &&arg) {
            if constexpr (std::is_void_v<std::invoke_result_t<F, Arg>>) {
                std::invoke(std::forward<F>(f), std::forward<Arg>(arg));
                return void_t{};
            } else {
                return std::invoke(std::forward<F>(f), std::forward<Arg>(arg));
            }
        };
        auto results = stdx::transform(
            exec, fs, stdx::tuple<Args...>{std::forward<Args>(args)...});
        auto filtered_results =
            stdx::filter<nonvoid_result_t>(std::move(results));

        std::move(filtered_results).apply([&]<typename... Ts>(Ts &&...ts) {
            Tag{}(static_cast<R &>(*this), std::forward<Ts>(ts)...);
        });
    }
};

template <typename R, typename F> struct base_receiver<R, F> : R {
    [[no_unique_address]] stdx::tuple<F> fs;

    template <typename Tag, typename... Args> auto set(Args &&...args) -> void {
        if constexpr (std::is_void_v<std::invoke_result_t<F, Args...>>) {
            std::invoke(get<0>(fs), std::forward<Args>(args)...);
            Tag{}(static_cast<R &>(*this));
        } else {
            Tag{}(static_cast<R &>(*this),
                  std::invoke(get<0>(fs), std::forward<Args>(args)...));
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

template <typename Tag, typename R, typename... Fs>
using receiver = tag_receiver<Tag, base_receiver<R, Fs...>>;

namespace detail {
template <typename Tag> struct as_signature {
    template <typename... As> using fn = Tag(As...);
};

template <typename Tag, typename... Fs> struct to_signature {
    // clang has a bug here, we cannot just use an alias
    template <typename... Ts> struct no_voids_t {
        using type = boost::mp11::mp_apply_q<
            as_signature<Tag>,
            boost::mp11::mp_remove_if<
                boost::mp11::mp_list<std::invoke_result_t<Fs, Ts>...>,
                std::is_void>>;
    };

    template <typename... Ts>
    using type = completion_signatures<typename no_voids_t<Ts...>::type>;
};

template <typename Tag, typename F> struct to_signature<Tag, F> {
    template <typename... Ts>
    using no_voids = boost::mp11::mp_apply_q<
        as_signature<Tag>,
        boost::mp11::mp_remove_if<
            boost::mp11::mp_list<std::invoke_result_t<F, Ts...>>,
            std::is_void>>;

    template <typename... Ts>
    using type = completion_signatures<no_voids<Ts...>>;
};
} // namespace detail

template <typename Tag, typename S, typename... Fs> class sender {
    template <receiver_from<sender> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender &&self,
                                                   R &&r) {
        return connect(std::move(self).s,
                       receiver<Tag, std::remove_cvref_t<R>, Fs...>{
                           {{std::forward<R>(r)}, std::move(self).fs}});
    }

    template <typename Self, receiver_from<sender> R>
        requires std::same_as<sender, std::remove_cvref_t<Self>> and
                 multishot_sender<S, R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r) {
        return connect(
            std::forward<Self>(self).s,
            receiver<Tag, std::remove_cvref_t<R>, Fs...>{
                {{std::forward<R>(r)}, std::forward<Self>(self).fs}});
    }

    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   sender const &sndr) {
        return forward_env_of(sndr.s);
    }

    template <typename... Ts>
    using signatures =
        typename detail::to_signature<Tag, Fs...>::template type<Ts...>;

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
                                          ::async::detail::default_set_value,
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
    [[no_unique_address]] stdx::tuple<Fs...> fs;
};

template <typename Tag, typename... Fs> struct pipeable {
    stdx::tuple<Fs...> fs;

  private:
    template <async::sender S, typename Self>
        requires std::same_as<pipeable, std::remove_cvref_t<Self>>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Tag, std::remove_cvref_t<S>, Fs...>{
            std::forward<S>(s), std::forward<Self>(self).fs};
    }
};
} // namespace _then

template <stdx::callable... Fs>
[[nodiscard]] constexpr auto then(Fs &&...fs)
    -> _then::pipeable<set_value_t, std::remove_cvref_t<Fs>...> {
    return {std::forward<Fs>(fs)...};
}

template <sender S, stdx::callable... Fs>
[[nodiscard]] constexpr auto then(S &&s, Fs &&...fs) -> sender auto {
    return std::forward<S>(s) | then(std::forward<Fs>(fs)...);
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
