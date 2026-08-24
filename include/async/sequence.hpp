#pragma once

#include <async/completes_synchronously.hpp>
#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>

#include <stdx/ct_string.hpp>
#include <stdx/functional.hpp>
#include <stdx/tuple.hpp>
#include <stdx/type_traits.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/function.hpp>

#include <concepts>
#include <type_traits>
#include <utility>
#include <variant>

namespace async {
namespace _seq {
template <typename F>
constexpr auto sender_from(F &&f) -> std::invoke_result_t<F> {
    static_assert(sender<std::invoke_result_t<F>>,
                  "sequence only accepts senders, or nullary functions "
                  "that return senders");
    return std::forward<F>(f)();
}

template <sender S> constexpr auto sender_from(S &&s) -> S { return s; }

template <typename S>
using sender_from_t = decltype(sender_from(std::declval<S>()));

template <typename S, typename R>
concept is_multishot =
    multishot_sender<S, async::detail::universal_receiver<env_of_t<R>>> or
    (std::copy_constructible<S> and std::invocable<S>);

template <typename Ops, typename Rcvr> struct base_receiver {
    using is_receiver = void;

    Ops *ops;

    [[nodiscard]] constexpr auto query(async::get_env_t) const
        -> forwarding_env<env_of_t<Rcvr>> {
        return forward_env_of(ops->rcvr);
    }

    template <typename... Args>
    auto set_error(Args &&...args) const && -> void {
        ops->template passthrough<set_error_t>(std::forward<Args>(args)...);
    }
    auto set_stopped() const && -> void {
        ops->template passthrough<set_stopped_t>();
    }
};

template <std::size_t N, typename Ops, typename Rcvr>
struct receiver : base_receiver<Ops, Rcvr> {
    auto set_value(auto &&...) const && -> void {
        this->ops->template complete<N>();
    }
};

template <stdx::ct_string Name, typename Rcvr, typename Sndr, typename... Rest>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    template <std::size_t N> using receiver_t = receiver<N, op_state, Rcvr>;

    template <typename R, stdx::same_as_unqualified<Sndr> S,
              stdx::same_as_unqualified<Rest>... Rs>
    constexpr op_state(R &&r, S &&s, Rs &&...rs)
        : rcvr{std::forward<R>(r)}, rest{std::forward<Rs>(rs)...},
          state{std::in_place_index<0>, stdx::with_result_of{[&] {
                    return connect(sender_from(std::forward<S>(s)),
                                   receiver_t<0>{this});
                }}} {}
    constexpr op_state(op_state &&) = delete;

    template <std::size_t N, typename... Args> auto complete() -> void {
        if constexpr (N + 1 == sizeof...(Rest)) {
            debug_signal<set_value_t::name,
                         debug::erased_context_for<op_state>>(get_env(rcvr));
            auto &op = state.template emplace<N + 1>(stdx::with_result_of{[&] {
                return connect(sender_from(get<N>(std::move(rest))),
                               std::move(rcvr));
            }});
            async::start(op);
        } else {
            auto &op = state.template emplace<N + 1>(stdx::with_result_of{[&] {
                return connect(sender_from(get<N>(std::move(rest))),
                               receiver_t<N + 1>{this});
            }});
            async::start(op);
        }
    }

    template <channel_tag Tag, typename... Args>
    auto passthrough(Args &&...args) -> void {
        debug_signal<Tag::name, debug::erased_context_for<op_state>>(
            get_env(rcvr));
        Tag{}(std::move(rcvr), std::forward<Args>(args)...);
    }

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        async::start(std::get<0>(state));
    }

    [[nodiscard]] constexpr static auto query(get_env_t) {
        return prop{completes_synchronously_t{},
                    boost::mp11::mp_apply<
                        boost::mp11::mp_all,
                        boost::mp11::mp_transform<synchronous_t, ops_t>>{}};
    }

    [[no_unique_address]] Rcvr rcvr;
    [[no_unique_address]] stdx::tuple<Rest...> rest;

    using first_ops_t = connect_result_t<sender_from_t<Sndr>, receiver_t<0>>;
    template <typename N>
    using nth_ops_t =
        connect_result_t<sender_from_t<stdx::nth_t<N::value, Rest...>>,
                         receiver_t<N::value + 1>>;
    using medial_ops_t =
        boost::mp11::mp_transform<nth_ops_t,
                                  boost::mp11::mp_iota_c<sizeof...(Rest) - 1>>;
    using last_ops_t = connect_result_t<
        sender_from_t<stdx::nth_t<sizeof...(Rest) - 1, Rest...>>, Rcvr>;

    using ops_t = boost::mp11::mp_push_back<
        boost::mp11::mp_push_front<medial_ops_t, first_ops_t>, last_ops_t>;

    boost::mp11::mp_apply<std::variant, ops_t> state;
};

template <stdx::ct_string Name, typename... Sndrs> struct sender {
    using is_sender = void;
    using seq_sender_tag = void;
    constexpr static auto name = Name;

    [[no_unique_address]] stdx::tuple<Sndrs...> sndrs;

  private:
    static_assert((... and async::sender<sender_from_t<Sndrs>>),
                  "The function(s) passed to sequence must return a sender");

    template <typename Env>
    using dependent_completions = completion_signatures_of_t<
        sender_from_t<stdx::nth_t<sizeof...(Sndrs) - 1, Sndrs...>>, Env>;

    template <typename Env>
    using unchanged_completions = boost::mp11::mp_append<
        error_signatures_of_t<sender_from_t<Sndrs>, Env>...,
        stopped_signatures_of_t<sender_from_t<Sndrs>, Env>...>;

    template <typename R>
    using op_state_for = op_state<Name, std::remove_cvref_t<R>, Sndrs...>;

  public:
    template <async::receiver R>
    [[nodiscard]] constexpr auto connect(R &&r) && -> op_state_for<R> {
        check_connect<sender &&, R>();
        return std::move(sndrs).apply(
            [&]<typename... Ss>(Ss &&...ss) -> op_state_for<R> {
                return {std::forward<R>(r), std::forward<Ss>(ss)...};
            });
    }

    template <async::receiver R>
        requires(... and is_multishot<Sndrs, R>)
    [[nodiscard]] constexpr auto connect(R &&r) const & -> op_state_for<R> {
        check_connect<sender, R>();
        return sndrs.apply([&]<typename... Ss>(Ss &&...ss) -> op_state_for<R> {
            return {std::forward<R>(r), std::forward<Ss>(ss)...};
        });
    }

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> boost::mp11::mp_unique<boost::mp11::mp_append<
            unchanged_completions<Env>, dependent_completions<Env>>> {
        return {};
    }

    [[nodiscard]] constexpr static auto query(get_env_t) {
        return prop{
            completes_synchronously_t{},
            std::bool_constant<(... and synchronous<sender_from_t<Sndrs>>)>{}};
    }
};

template <typename S, stdx::ct_string Name>
concept matching_sequence_sender = async::sender<S> and requires {
    typename std::remove_cvref_t<S>::seq_sender_tag;
} and Name == std::remove_cvref_t<S>::name;

template <stdx::ct_string Name, typename S2> struct pipeable {
    [[no_unique_address]] S2 s2;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Name, std::remove_cvref_t<S>, S2>{
            std::forward<S>(s), std::forward<Self>(self).s2};
    }

    template <matching_sequence_sender<Name> S,
              stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return std::forward<S>(s).sndrs.apply([&]<typename... Ts>(Ts &&...ts) {
            return sender<Name, std::remove_cvref_t<Ts>..., S2>{
                std::forward<Ts>(ts)..., std::forward<Self>(self).s2};
        });
    }
};
} // namespace _seq

template <stdx::ct_string Name = "sequence", typename... Fs>
    requires(sizeof...(Fs) > 0)
[[nodiscard]] constexpr auto sequence(Fs &&...fs) {
    if constexpr (sizeof...(Fs) == 1) {
        return compose(_seq::pipeable<Name, std::remove_cvref_t<Fs>...>{
            std::forward<Fs>(fs)...});
    } else {
        return _seq::sender<Name, std::remove_cvref_t<Fs>...>{
            std::forward<Fs>(fs)...};
    }
}

template <stdx::ct_string Name = "seq", sender... Ss>
    requires(sizeof...(Ss) > 0)
[[nodiscard]] constexpr auto seq(Ss &&...ss) {
    return sequence<Name>(std::forward<Ss>(ss)...);
}

struct sequence_t;

template <stdx::ct_string Name, typename Rcvr, typename Sndr, typename... Sndrs>
struct debug::context_for<_seq::op_state<Name, Rcvr, Sndr, Sndrs...>> {
    using tag = sequence_t;
    constexpr static auto name = Name;
    using type = _seq::op_state<Name, Rcvr, Sndr, Sndrs...>;
    using children = boost::mp11::mp_apply<
        stdx::type_list, boost::mp11::mp_transform<debug::erased_context_for,
                                                   typename type::ops_t>>;
};
} // namespace async
