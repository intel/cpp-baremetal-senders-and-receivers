#pragma once

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/forwarding_query.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/functional.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/list.hpp>

#include <type_traits>
#include <utility>
#include <variant>

namespace async::_let {
namespace detail {
template <channel_tag Tag, typename F, typename... Ts>
auto invoke(F &&f, Ts &&...ts)
    -> decltype(std::forward<F>(f).template operator()<Tag>(
        std::forward<Ts>(ts)...)) {
    return std::forward<F>(f).template operator()<Tag>(std::forward<Ts>(ts)...);
}

template <channel_tag Tag, typename F, typename... Ts>
auto invoke(F &&f, Ts &&...ts)
    -> decltype(std::forward<F>(f)(std::forward<Ts>(ts)...)) {
    return std::forward<F>(f)(std::forward<Ts>(ts)...);
}

template <typename T>
using call_type =
    stdx::conditional_t<std::is_copy_constructible_v<T>, T &, T &&>;

template <typename Sig, typename F> struct invoked_type;
template <typename Tag, typename... Args, typename F>
struct invoked_type<Tag(Args...), F> {
    using type = decltype(invoke<Tag>(std::declval<F>(),
                                      std::declval<call_type<Args>>()...));
};

template <typename E> struct completions_of {
    template <typename T> using fn = completion_signatures_of_t<T, E>;
};

template <typename Sig, typename F>
using dependent_sender_t = typename invoked_type<Sig, F>::type;

template <typename F> struct dependent_sender_q {
    template <typename Sig> using fn = dependent_sender_t<Sig, F>;
};

template <typename... Ts>
using decayed_tuple = stdx::tuple<std::decay_t<Ts>...>;

template <typename T, typename U>
using arg_compatible = std::bool_constant<stdx::same_as_unqualified<T, U> and
                                          stdx::convertible_to<T, U>>;

template <typename Sig1, typename Sig2>
struct sig_compatible : std::false_type {};

template <typename Tag, typename... Args1, typename... Args2>
struct sig_compatible<Tag(Args1...), Tag(Args2...)>
    : std::bool_constant<(... and arg_compatible<Args1, Args2>::value)> {};

template <typename Sig1> struct sig_compatible_q {
    template <typename Sig2> using fn = sig_compatible<Sig1, Sig2>;
};
} // namespace detail

template <typename Ops, typename Rcvr, channel_tag... Tags> struct receiver {
    using is_receiver = void;
    Ops *ops;

    [[nodiscard]] constexpr auto
    query(get_env_t) const -> forwarding_env<env_of_t<Rcvr>> {
        return forward_env_of(ops->rcvr);
    }

    template <typename... Args>
    constexpr auto set_value(Args &&...args) && -> void {
        handle<set_value_t>(std::forward<Args>(args)...);
    }
    template <typename... Args>
    constexpr auto set_error(Args &&...args) && -> void {
        handle<set_error_t>(std::forward<Args>(args)...);
    }
    constexpr auto set_stopped() && -> void { handle<set_stopped_t>(); }

  private:
    template <channel_tag T, typename... Args>
    auto handle(Args &&...args) -> void {
        if constexpr ((... or std::same_as<T, Tags>)) {
            ops->template complete_first<T, T(Args...)>(
                std::forward<Args>(args)...);
        } else {
            T{}(std::move(ops->rcvr), std::forward<Args>(args)...);
        }
    }
};

template <typename Sndr, typename Rcvr, typename Func, typename Sigs,
          channel_tag... Tags>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using first_rcvr = receiver<op_state, Rcvr, Tags...>;

    template <typename S, typename R, typename F>
    constexpr op_state(S &&s, R &&r, F &&f)
        : rcvr{std::forward<R>(r)}, fn{std::forward<F>(f)},
          state{std::in_place_index<0>, stdx::with_result_of{[&] {
                    return connect(std::forward<S>(s), first_rcvr{this});
                }}} {}
    constexpr op_state(op_state &&) = delete;

    template <channel_tag Tag, typename Sig, typename... Args>
    auto complete_first(Args &&...args) -> void {
        using index =
            boost::mp11::mp_find_if_q<Sigs, detail::sig_compatible_q<Sig>>;
        static_assert(index::value < boost::mp11::mp_size<Sigs>::value);

        auto &sent_args =
            results.template emplace<index::value + 1>(stdx::with_result_of{
                [&] { return stdx::make_tuple(std::forward<Args>(args)...); }});

        auto make_op_state = [&]<typename Tuple>(Tuple &&t) -> decltype(auto) {
            return state.template emplace<index::value + 1>(
                stdx::with_result_of{[&] {
                    return connect(std::forward<Tuple>(t).apply(
                                       [&]<typename... As>(As &&...as) {
                                           return detail::invoke<Tag>(
                                               fn, std::forward<As>(as)...);
                                       }),
                                   rcvr);
                }});
        };

        if constexpr (std::is_copy_constructible_v<
                          std::remove_cvref_t<decltype(sent_args)>>) {
            async::start(make_op_state(sent_args));
        } else {
            async::start(make_op_state(std::move(sent_args)));
        }
    }

    constexpr auto start() & -> void { async::start(std::get<0>(state)); }

    template <typename Sig>
    using dependent_connect_result_t =
        connect_result_t<detail::dependent_sender_t<Sig, Func>, Rcvr>;

    using dependent_ops = boost::mp11::mp_apply<
        std::variant,
        boost::mp11::mp_transform<dependent_connect_result_t, Sigs>>;

    using first_ops = connect_result_t<Sndr, first_rcvr>;
    using state_t = boost::mp11::mp_push_front<dependent_ops, first_ops>;

    using first_results_t =
        ::async::detail::variantify<std::variant,
                                    detail::decayed_tuple>::template fn<Sigs>;
    using results_t =
        boost::mp11::mp_push_front<first_results_t, std::monostate>;

    [[no_unique_address]] Rcvr rcvr;
    [[no_unique_address]] Func fn;
    results_t results;
    state_t state;
};

template <typename S, typename F, channel_tag... Tags> struct sender {
    using is_sender = void;

    [[no_unique_address]] S s;
    [[no_unique_address]] F f;

    [[nodiscard]] constexpr auto query(get_env_t) const & {
        return forward_env_of(s);
    }
    [[nodiscard]] constexpr auto query(get_env_t) && {
        return forward_env_of(std::move(s));
    }

  private:
    template <typename E>
    using raw_completions =
        boost::mp11::mp_partition_q<completion_signatures_of_t<S, E>,
                                    ::async::detail::with_any_tag<Tags...>>;

    template <typename E>
    using unchanged_completions = boost::mp11::mp_second<raw_completions<E>>;

    template <typename E>
    using dependent_senders = boost::mp11::mp_unique<
        boost::mp11::mp_transform_q<detail::dependent_sender_q<F>,
                                    boost::mp11::mp_first<raw_completions<E>>>>;

    template <typename R>
    using is_multishot_leftover_sender = stdx::conditional_t<
        boost::mp11::mp_empty<
            boost::mp11::mp_second<raw_completions<env_of_t<R>>>>::value,
        std::true_type, std::bool_constant<multishot_sender<S, R>>>;

    template <typename R> struct is_multishot_sender {
        template <typename T>
        using fn = std::bool_constant<multishot_sender<T, R>>;
    };

    template <typename E>
    using dependent_completions =
        boost::mp11::mp_flatten<boost::mp11::mp_transform_q<
            detail::completions_of<E>, dependent_senders<E>>>;

  public:
    template <receiver_from<sender> R>
    [[nodiscard]] constexpr auto connect(R &&r)
        && -> _let::op_state<
               S, std::remove_cvref_t<R>, F,
               boost::mp11::mp_first<raw_completions<env_of_t<R>>>, Tags...> {
        return {std::move(s), std::forward<R>(r), std::move(f)};
    }

    template <receiver_from<sender> R>
        requires multishot_sender<S> and
                     is_multishot_leftover_sender<R>::value and
                     boost::mp11::mp_all_of_q<dependent_senders<env_of_t<R>>,
                                              is_multishot_sender<R>>::value
    [[nodiscard]] constexpr auto connect(R &&r) const
        & -> _let::op_state<S, std::remove_cvref_t<R>, F,
                            boost::mp11::mp_first<raw_completions<env_of_t<R>>>,
                            Tags...> {
        return {s, std::forward<R>(r), f};
    }

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> boost::mp11::mp_unique<boost::mp11::mp_append<
            unchanged_completions<Env>, dependent_completions<Env>>> {
        return {};
    }
};

template <typename F, template <typename...> typename Sndr> struct pipeable {
    [[no_unique_address]] F f;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return Sndr<std::remove_cvref_t<S>, F>{std::forward<S>(s),
                                               std::forward<Self>(self).f};
    }
};
} // namespace async::_let
