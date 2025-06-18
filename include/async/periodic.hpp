#pragma once

#include <async/completes_synchronously.hpp>
#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/schedulers/get_expiration.hpp>
#include <async/schedulers/timer_manager_interface.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/functional.hpp>

#include <boost/mp11/algorithm.hpp>

#include <algorithm>
#include <concepts>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>

namespace async {
namespace _periodic {
template <typename Ops> struct first_expiration_provider {
    using time_point_t = typename Ops::time_point_t;
    Ops *ops;

    template <typename Hal>
    [[nodiscard]] auto compute_expiration() const -> time_point_t {
        ops->tp = Hal::now() + ops->d;
        return ops->tp;
    }
};

template <typename Ops> struct unsafe_nth_expiration_provider {
    using time_point_t = typename Ops::time_point_t;
    Ops *ops;

    template <typename Hal>
    [[nodiscard]] auto compute_expiration() const -> time_point_t {
        ops->tp += ops->d;
        return ops->tp;
    }
};

template <typename Ops> struct safe_immediate_nth_expiration_provider {
    using time_point_t = typename Ops::time_point_t;
    Ops *ops;

    template <typename Hal>
    [[nodiscard]] auto compute_expiration() const -> time_point_t {
        ops->tp += ops->d;
        ops->tp = std::max(ops->tp, Hal::now());
        return ops->tp;
    }
};

template <typename Ops> struct safe_quantized_nth_expiration_provider {
    using time_point_t = typename Ops::time_point_t;
    Ops *ops;

    template <typename Hal>
    [[nodiscard]] auto compute_expiration() const -> time_point_t {
        ops->tp += ops->d;
        if (auto now = Hal::now(); ops->tp < now) {
            auto diff = --(now - ops->tp);
            auto num_ticks = (diff / ops->d) + 1;
            ops->tp += ops->d * num_ticks;
        }
        return ops->tp;
    }
};

template <typename Ops, template <typename> typename Expiry> struct receiver {
    using is_receiver = void;

    Ops *ops;

    template <typename... Args>
    auto set_value(Args &&...args) const && -> void {
        ops->repeat(std::forward<Args>(args)...);
    }
    template <typename... Args>
    auto set_error(Args &&...args) const && -> void {
        ops->template passthrough<set_error_t>(std::forward<Args>(args)...);
    }
    auto set_stopped() const && -> void {
        ops->template passthrough<set_stopped_t>();
    }

    [[nodiscard]] constexpr auto query(get_env_t) const
        -> overriding_env<timer_mgr::get_expiration_t, Expiry<Ops>,
                          typename Ops::downstream_receiver_t> {
        return override_env_with<timer_mgr::get_expiration_t>(
            Expiry<Ops>{this->ops}, this->ops->get_receiver());
    }
};

constexpr auto never_stop = [](auto &&...) { return false; };

template <typename Pred, typename Sig, typename = void>
struct is_callable : std::false_type {};
template <typename Pred, typename... Args>
struct is_callable<Pred, set_value_t(Args...),
                   std::void_t<std::invoke_result_t<Pred, Args...>>>
    : std::true_type {};

template <typename Pred> struct callable_with {
    template <typename Sig> using fn = is_callable<Pred, Sig>;
};

template <template <typename> typename Expiry> struct expiry_wrapper {
    template <typename T> using fn = Expiry<T>;
};

template <stdx::ct_string Name, typename Expiry, typename Sndr, typename Rcvr,
          typename Duration, stdx::callable Pred>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using downstream_receiver_t = Rcvr;
    using time_point_t = timer_mgr::time_point_for_t<Duration>;

    using first_receiver_t = receiver<op_state, first_expiration_provider>;
    using nth_receiver_t = receiver<op_state, Expiry::template fn>;

    using value_completions =
        value_signatures_of_t<Sndr, env_of_t<nth_receiver_t>>;
    static_assert(
        boost::mp11::mp_all_of_q<value_completions, callable_with<Pred>>::value,
        "Predicate is not callable with value completions of sender");

    using first_state_t = async::connect_result_t<Sndr &, first_receiver_t>;
    using nth_state_t = async::connect_result_t<Sndr &, nth_receiver_t>;

    template <stdx::same_as_unqualified<Sndr> S,
              stdx::same_as_unqualified<Rcvr> R,
              stdx::same_as_unqualified<Pred> P>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr op_state(S &&s, R &&r, Duration dur, P &&p)
        : sndr{std::forward<S>(s)}, rcvr{std::forward<R>(r)}, d{dur},
          pred{std::forward<P>(p)} {}
    constexpr op_state(op_state &&) = delete;

    constexpr auto start() & -> void {
        static_assert(not synchronous<first_state_t>,
                      "periodic doesn't make sense with synchronous senders");

        state.template emplace<1>(stdx::with_result_of{
            [&] { return connect(sndr, first_receiver_t{this}); }});
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        async::start(std::get<1>(state));
    }

    constexpr auto restart() & -> void {
        if constexpr (not stoppable_sender<Sndr, env_of_t<Rcvr>>) {
            if (get_stop_token(get_env(rcvr)).stop_requested()) {
                passthrough<set_stopped_t>();
                return;
            }
        }

        state.template emplace<2>(stdx::with_result_of{
            [&] { return connect(sndr, nth_receiver_t{this}); }});
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        async::start(std::get<2>(state));
    }

    template <typename... Args> auto repeat(Args &&...args) -> void {
        if constexpr (not std::same_as<
                          Pred, std::remove_cvref_t<decltype(never_stop)>>) {
            debug_signal<"eval_predicate", debug::erased_context_for<op_state>>(
                get_env(rcvr));
            if (pred(args...)) {
                passthrough<set_value_t>(std::forward<Args>(args)...);
                return;
            }
        }
        restart();
    }

    template <channel_tag Tag, typename... Args>
    auto passthrough(Args &&...args) -> void {
        debug_signal<Tag::name, debug::erased_context_for<op_state>>(
            get_env(rcvr));
        Tag{}(std::move(rcvr), std::forward<Args>(args)...);
        state.template emplace<0>();
    }

    [[nodiscard]] auto get_receiver() const -> Rcvr const & { return rcvr; }

    [[no_unique_address]] Sndr sndr;
    [[no_unique_address]] Rcvr rcvr;
    Duration d;
    [[no_unique_address]] Pred pred;
    time_point_t tp{};
    std::variant<std::monostate, first_state_t, nth_state_t> state{};
};

template <stdx::ct_string Name, typename Expiry, typename Sndr,
          typename Duration, typename Pred>
struct sender {
    using is_sender = void;
    [[no_unique_address]] Sndr sndr;
    Duration d;
    [[no_unique_address]] Pred p;

    [[nodiscard]] constexpr auto query(async::get_env_t) const {
        return forward_env_of(sndr);
    }

    template <typename...> using signatures = completion_signatures<>;

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &) {
        if constexpr (std::same_as<Pred,
                                   std::remove_cvref_t<decltype(never_stop)>>) {
            return transform_completion_signatures_of<
                Sndr, Env, completion_signatures<>, signatures>{};
        } else {
            return completion_signatures_of_t<Sndr, Env>{};
        }
    }

    template <async::receiver R>
        requires multishot_sender<
            Sndr, typename op_state<Name, Expiry, Sndr, std::remove_cvref_t<R>,
                                    Duration, Pred>::nth_receiver_t>
    [[nodiscard]] constexpr auto
    connect(R &&r) const & -> op_state<Name, Expiry, Sndr,
                                       std::remove_cvref_t<R>, Duration, Pred> {
        return {sndr, std::forward<R>(r), d, p};
    }
};

template <stdx::ct_string Name, typename Expiry, typename Duration,
          stdx::callable Pred>
struct pipeable {
    Duration d;
    Pred p;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Name, Expiry, std::remove_cvref_t<S>, Duration, Pred>{
            std::forward<S>(s), std::forward<Self>(self).d,
            std::forward<Self>(self).p};
    }
};
} // namespace _periodic

using safe_immediate_expiry = _periodic::expiry_wrapper<
    _periodic::safe_immediate_nth_expiration_provider>;
using safe_quantized_expiry = _periodic::expiry_wrapper<
    _periodic::safe_quantized_nth_expiration_provider>;
using unsafe_expiry =
    _periodic::expiry_wrapper<_periodic::unsafe_nth_expiration_provider>;

template <stdx::ct_string Name = "periodic_until",
          typename Expiry = safe_immediate_expiry, typename Duration,
          typename P>
[[nodiscard]] constexpr auto periodic_until(Duration d, P &&p)
    -> _periodic::pipeable<Name, Expiry, Duration, std::remove_cvref_t<P>> {
    return {d, std::forward<P>(p)};
}

template <stdx::ct_string Name = "periodic_until",
          typename Expiry = safe_immediate_expiry, sender S, typename Duration,
          typename P>
[[nodiscard]] auto periodic_until(S &&s, Duration d, P &&p) {
    return std::forward<S>(s) |
           periodic_until<Name, Expiry>(d, std::forward<P>(p));
}

template <stdx::ct_string Name = "periodic",
          typename Expiry = safe_immediate_expiry, typename Duration>
[[nodiscard]] constexpr auto periodic(Duration d) {
    return periodic_until<Name, Expiry>(d, _periodic::never_stop);
}

template <stdx::ct_string Name = "periodic",
          typename Expiry = safe_immediate_expiry, sender S, typename Duration>
[[nodiscard]] auto periodic(S &&s, Duration d) {
    return std::forward<S>(s) | periodic<Name, Expiry>(d);
}

template <stdx::ct_string Name = "periodic_n",
          typename Expiry = safe_immediate_expiry, typename Duration>
[[nodiscard]] constexpr auto periodic_n(Duration d, unsigned int n) {
    return periodic_until<Name, Expiry>(
        d, [n](auto &&...) mutable { return n-- == 0; });
}

template <stdx::ct_string Name = "periodic_n",
          typename Expiry = safe_immediate_expiry, sender S, typename Duration>
[[nodiscard]] auto periodic_n(S &&s, Duration d, unsigned int n) {
    return std::forward<S>(s) | periodic_n<Name, Expiry>(d, n);
}

struct periodic_t;

template <stdx::ct_string Name, typename Expiry, typename... Ts>
struct debug::context_for<_periodic::op_state<Name, Expiry, Ts...>> {
    using tag = periodic_t;
    constexpr static auto name = Name;
    using type = _periodic::op_state<Name, Expiry, Ts...>;
    using children = stdx::type_list<
        debug::erased_context_for<typename type::first_state_t>>;
};
} // namespace async
