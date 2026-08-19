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
namespace _sequence {
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

template <typename Ops, typename Rcvr>
struct receiver : base_receiver<Ops, Rcvr> {
    auto set_value(auto &&...) const && -> void { this->ops->complete(); }
};

template <stdx::ct_string Name, typename Sndr, std::invocable Func,
          typename Rcvr>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using first_rcvr = receiver<op_state, Rcvr>;

    template <stdx::same_as_unqualified<Sndr> S, typename F, typename R>
    constexpr op_state(S &&s, F &&f, R &&r)
        : func{std::forward<F>(f)}, rcvr{std::forward<R>(r)},
          state{std::in_place_index<0>, stdx::with_result_of{[&] {
                    return connect(std::forward<S>(s), first_rcvr{this});
                }}} {}
    constexpr op_state(op_state &&) = delete;

    template <typename... Args> auto complete() -> void {
        debug_signal<set_value_t::name, debug::erased_context_for<op_state>>(
            get_env(rcvr));
        auto &op = state.template emplace<1>(stdx::with_result_of{
            [&] { return connect(std::move(func)(), std::move(rcvr)); }});
        async::start(op);
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
                    std::bool_constant<synchronous<first_ops> and
                                       synchronous<second_ops>>{}};
    }

    [[no_unique_address]] Func func;
    [[no_unique_address]] Rcvr rcvr;

    using dependent_sender = std::invoke_result_t<Func>;
    using first_ops = connect_result_t<Sndr, first_rcvr>;
    using second_ops = connect_result_t<dependent_sender, Rcvr>;
    std::variant<first_ops, second_ops> state;
};

namespace detail {
template <async::sender S> struct wrapper {
    [[no_unique_address]] S s;
    [[nodiscard]] constexpr auto operator()() && -> S { return std::move(s); }
};
template <typename S> wrapper(S) -> wrapper<S>;
} // namespace detail

template <stdx::ct_string Name, typename S, std::invocable F> struct sender {
    using is_sender = void;

    [[no_unique_address]] S s;
    [[no_unique_address]] F f;

  private:
    using dependent_sender = std::invoke_result_t<F>;
    static_assert(async::sender<dependent_sender>,
                  "The function passed to sequence must return a sender");

    template <typename Env>
    using dependent_completions =
        completion_signatures_of_t<dependent_sender, Env>;

    template <typename Env>
    using unchanged_completions =
        boost::mp11::mp_append<error_signatures_of_t<S, Env>,
                               stopped_signatures_of_t<S, Env>>;

  public:
    template <async::receiver R>
    [[nodiscard]] constexpr auto
    connect(R &&r) && -> op_state<Name, S, F, std::remove_cvref_t<R>> {
        check_connect<sender &&, R>();
        return {std::move(s), std::move(f), std::forward<R>(r)};
    }

    template <async::receiver R>
        requires multishot_sender<
                     S, async::detail::universal_receiver<env_of_t<R>>> and
                 std::copy_constructible<S> and std::copy_constructible<F>
    [[nodiscard]] constexpr auto
    connect(R &&r) const & -> op_state<Name, S, F, std::remove_cvref_t<R>> {
        check_connect<sender, R>();
        return {s, f, std::forward<R>(r)};
    }

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> boost::mp11::mp_unique<boost::mp11::mp_append<
            unchanged_completions<Env>, dependent_completions<Env>>> {
        return {};
    }

    [[nodiscard]] constexpr static auto query(get_env_t) {
        return prop{completes_synchronously_t{},
                    std::bool_constant<synchronous<S> and
                                       synchronous<dependent_sender>>{}};
    }
};

template <stdx::ct_string Name, std::invocable F> struct pipeable {
    [[no_unique_address]] F f;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Name, std::remove_cvref_t<S>, F>{
            std::forward<S>(s), std::forward<Self>(self).f};
    }
};
} // namespace _sequence

template <stdx::ct_string Name = "sequence", std::invocable F>
[[nodiscard]] constexpr auto sequence(F &&f) {
    return compose(
        _sequence::pipeable<Name, std::remove_cvref_t<F>>{std::forward<F>(f)});
}

template <stdx::ct_string Name = "sequence", sender S, std::invocable F>
[[nodiscard]] constexpr auto sequence(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | sequence<Name>(std::forward<F>(f));
}

namespace _seq {
template <std::size_t N, typename Ops, typename Rcvr>
struct receiver : _sequence::base_receiver<Ops, Rcvr> {
    auto set_value(auto &&...) const && -> void {
        this->ops->template complete<N>();
    }
};

template <stdx::ct_string Name, typename Rcvr, typename Sndr, typename... Sndrs>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    template <std::size_t N> using receiver_t = receiver<N, op_state, Rcvr>;

    template <typename R, stdx::same_as_unqualified<Sndr> S,
              stdx::same_as_unqualified<Sndrs>... Ss>
    constexpr op_state(R &&r, S &&s, Ss &&...ss)
        : rcvr{std::forward<R>(r)}, sndrs{std::forward<Ss>(ss)...},
          state{std::in_place_index<0>, stdx::with_result_of{[&] {
                    return connect(std::forward<S>(s), receiver_t<0>{this});
                }}} {}
    constexpr op_state(op_state &&) = delete;

    template <std::size_t N, typename... Args> auto complete() -> void {
        if constexpr (N + 1 == sizeof...(Sndrs)) {
            debug_signal<set_value_t::name,
                         debug::erased_context_for<op_state>>(get_env(rcvr));
            auto &op = state.template emplace<N + 1>(stdx::with_result_of{[&] {
                return connect(get<N>(std::move(sndrs)), std::move(rcvr));
            }});
            async::start(op);
        } else {
            auto &op = state.template emplace<N + 1>(stdx::with_result_of{[&] {
                return connect(get<N>(std::move(sndrs)),
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
    [[no_unique_address]] stdx::tuple<Sndrs...> sndrs;

    using first_ops_t = connect_result_t<Sndr, receiver_t<0>>;
    template <typename N>
    using nth_ops_t = connect_result_t<stdx::nth_t<N::value, Sndrs...>,
                                       receiver_t<N::value + 1>>;
    using medial_ops_t =
        boost::mp11::mp_transform<nth_ops_t,
                                  boost::mp11::mp_iota_c<sizeof...(Sndrs) - 1>>;
    using last_ops_t =
        connect_result_t<stdx::nth_t<sizeof...(Sndrs) - 1, Sndrs...>, Rcvr>;

    using ops_t = boost::mp11::mp_push_back<
        boost::mp11::mp_push_front<medial_ops_t, first_ops_t>, last_ops_t>;

    boost::mp11::mp_apply<std::variant, ops_t> state;
};

template <stdx::ct_string Name, typename... Sndrs> struct sender {
    using is_sender = void;

    [[no_unique_address]] stdx::tuple<Sndrs...> sndrs;

  private:
    template <typename Env>
    using dependent_completions =
        completion_signatures_of_t<stdx::nth_t<sizeof...(Sndrs) - 1, Sndrs...>,
                                   Env>;

    template <typename Env>
    using unchanged_completions =
        boost::mp11::mp_append<error_signatures_of_t<Sndrs, Env>...,
                               stopped_signatures_of_t<Sndrs, Env>...>;

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
        requires(... and
                 multishot_sender<
                     Sndrs, async::detail::universal_receiver<env_of_t<R>>>) and
                (... and std::copy_constructible<Sndrs>)
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
        return prop{completes_synchronously_t{},
                    std::bool_constant<(... and synchronous<Sndrs>)>{}};
    }
};

template <stdx::ct_string Name, typename S2> struct pipeable {
    [[no_unique_address]] S2 s2;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Name, std::remove_cvref_t<S>, S2>{
            std::forward<S>(s), std::forward<Self>(self).s2};
    }
};
} // namespace _seq

template <stdx::ct_string Name = "seq", sender... S>
    requires(sizeof...(S) > 0)
[[nodiscard]] constexpr auto seq(S &&...s) {
    if constexpr (sizeof...(S) == 1) {
        return compose(_seq::pipeable<Name, std::remove_cvref_t<S>...>{
            std::forward<S>(s)...});
    } else {
        return _seq::sender<Name, std::remove_cvref_t<S>...>{
            std::forward<S>(s)...};
    }
}

struct sequence_t;
struct seq_t;

template <stdx::ct_string Name, typename Sndr, typename Func, typename Rcvr>
struct debug::context_for<_sequence::op_state<Name, Sndr, Func, Rcvr>> {
    using tag = sequence_t;
    constexpr static auto name = Name;
    using type = _sequence::op_state<Name, Sndr, Func, Rcvr>;
    using children =
        stdx::type_list<debug::erased_context_for<typename type::first_ops>,
                        debug::erased_context_for<typename type::second_ops>>;
};

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
