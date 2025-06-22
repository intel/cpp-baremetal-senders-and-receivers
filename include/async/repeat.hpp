#pragma once

#include <async/completes_synchronously.hpp>
#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/functional.hpp>

#include <boost/mp11/algorithm.hpp>

#include <concepts>
#include <optional>
#include <type_traits>
#include <utility>

namespace async {
namespace _repeat {
template <typename Ops, typename Rcvr> struct receiver {
    using is_receiver = void;

    Ops *ops;

    [[nodiscard]] constexpr auto query(get_env_t) const
        -> forwarding_env<env_of_t<Rcvr>> {
        return forward_env_of(ops->rcvr);
    }

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
};

constexpr auto never_stop = [](auto &&...) { return false; };
constexpr auto no_loop_fn = [](auto &&...) {};

template <typename Pred, typename Sig, typename = void>
struct is_callable : std::false_type {};
template <typename Pred, typename... Args>
struct is_callable<Pred, set_value_t(Args...),
                   std::void_t<std::invoke_result_t<Pred, Args...>>>
    : std::true_type {};

template <typename Pred> struct callable_with {
    template <typename Sig> using fn = is_callable<Pred, Sig>;
};

template <stdx::ct_string Name, typename Sndr, typename Rcvr,
          stdx::callable Pred, typename LoopFn>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using receiver_t = receiver<op_state, Rcvr>;
    using value_completions = value_signatures_of_t<Sndr, env_of_t<receiver_t>>;
    static_assert(
        boost::mp11::mp_all_of_q<value_completions, callable_with<Pred>>::value,
        "Predicate is not callable with value completions of sender");
    using state_t = async::connect_result_t<Sndr &, receiver_t>;

    template <
        stdx::same_as_unqualified<Sndr> S, stdx::same_as_unqualified<Rcvr> R,
        stdx::same_as_unqualified<Pred> P, stdx::same_as_unqualified<LoopFn> F>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr op_state(S &&s, R &&r, P &&p, F &&f)
        : sndr{std::forward<S>(s)}, rcvr{std::forward<R>(r)},
          pred{std::forward<P>(p)}, loop_fn{std::forward<F>(f)} {}
    constexpr op_state(op_state &&) = delete;

    constexpr auto start() & -> void {
        setup();
        if constexpr (synchronous<state_t>) {
            static_assert(
                std::same_as<LoopFn, std::remove_cvref_t<decltype(no_loop_fn)>>,
                "repeat cannot have a loop function when the sender "
                "it wraps is synchronous!");
            while (state.has_value()) {
                begin_loop();
            }
        } else {
            begin_loop();
        }
    }

    constexpr auto setup() -> void {
        state.emplace(stdx::with_result_of{
            [&] { return connect(sndr, receiver_t{this}); }});
    }

    constexpr auto begin_loop() -> bool {
        if constexpr (not stoppable_sender<Sndr, env_of_t<Rcvr>>) {
            if (get_stop_token(get_env(rcvr)).stop_requested()) {
                passthrough<set_stopped_t>();
                return false;
            }
        }
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        async::start(*state);
        return true;
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
        setup();
        if constexpr (not synchronous<state_t>) {
            if (begin_loop()) {
                loop_fn(std::forward<Args>(args)...);
            }
        }
    }

    template <channel_tag Tag, typename... Args>
    auto passthrough(Args &&...args) -> void {
        state.reset();
        debug_signal<Tag::name, debug::erased_context_for<op_state>>(
            get_env(rcvr));
        Tag{}(std::move(rcvr), std::forward<Args>(args)...);
    }

    [[nodiscard]] constexpr auto query(async::get_env_t) const {
        return prop{completes_synchronously_t{}, synchronous_t<state_t>{}};
    }

    [[no_unique_address]] Sndr sndr;
    [[no_unique_address]] Rcvr rcvr;
    [[no_unique_address]] Pred pred;
    [[no_unique_address]] LoopFn loop_fn;

    std::optional<state_t> state{};
};

template <stdx::ct_string Name, typename Sndr, typename Pred, typename LoopFn>
struct sender {
    using is_sender = void;
    [[no_unique_address]] Sndr sndr;
    [[no_unique_address]] Pred p;
    [[no_unique_address]] LoopFn f;

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

    template <receiver_from<Sndr> R>
        requires multishot_sender<Sndr, R>
    [[nodiscard]] constexpr auto connect(R &&r)
        const & -> op_state<Name, Sndr, std::remove_cvref_t<R>, Pred, LoopFn> {
        return {sndr, std::forward<R>(r), p, f};
    }
};

template <stdx::ct_string Name, stdx::callable Pred, stdx::callable LoopFn>
struct pipeable {
    Pred p;
    LoopFn f;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Name, std::remove_cvref_t<S>, Pred, LoopFn>{
            std::forward<S>(s), std::forward<Self>(self).p,
            std::forward<Self>(self).f};
    }
};
} // namespace _repeat

template <stdx::ct_string Name = "repeat_until", typename P,
          typename F = std::remove_cvref_t<decltype(_repeat::no_loop_fn)>>
[[nodiscard]] constexpr auto repeat_until(P &&p, F &&f = {})
    -> _repeat::pipeable<Name, std::remove_cvref_t<P>, std::remove_cvref_t<F>> {
    return {std::forward<P>(p), std::forward<F>(f)};
}

template <stdx::ct_string Name = "repeat_until", sender S, typename... Args>
[[nodiscard]] auto repeat_until(S &&s, Args &&...args) {
    return std::forward<S>(s) | repeat_until(std::forward<Args>(args)...);
}

template <stdx::ct_string Name = "repeat", typename... Args>
[[nodiscard]] constexpr auto repeat(Args &&...args) {
    return repeat_until<Name>(_repeat::never_stop, std::forward<Args>(args)...);
}

template <stdx::ct_string Name = "repeat", sender S, typename... Args>
[[nodiscard]] auto repeat(S &&s, Args &&...args) {
    return std::forward<S>(s) | repeat<Name>(std::forward<Args>(args)...);
}

template <stdx::ct_string Name = "repeat_n", typename... Args>
[[nodiscard]] constexpr auto repeat_n(unsigned int n, Args &&...args) {
    return repeat_until<Name>([n](auto &&...) mutable { return n-- == 0; },
                              std::forward<Args>(args)...);
}

template <stdx::ct_string Name = "repeat_n", sender S, typename... Args>
[[nodiscard]] auto repeat_n(S &&s, unsigned int n, Args &&...args) {
    return std::forward<S>(s) | repeat_n<Name>(n, std::forward<Args>(args)...);
}

struct repeat_t;

template <stdx::ct_string Name, typename... Ts>
struct debug::context_for<_repeat::op_state<Name, Ts...>> {
    using tag = repeat_t;
    constexpr static auto name = Name;
    using type = _repeat::op_state<Name, Ts...>;
    using children =
        stdx::type_list<debug::erased_context_for<typename type::state_t>>;
};
} // namespace async
