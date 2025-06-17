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
          stdx::callable Pred>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using receiver_t = receiver<op_state, Rcvr>;
    using value_completions = value_signatures_of_t<Sndr, env_of_t<receiver_t>>;
    static_assert(
        boost::mp11::mp_all_of_q<value_completions, callable_with<Pred>>::value,
        "Predicate is not callable with value completions of sender");
    using state_t = async::connect_result_t<Sndr &, receiver_t>;

    template <stdx::same_as_unqualified<Sndr> S,
              stdx::same_as_unqualified<Rcvr> R,
              stdx::same_as_unqualified<Pred> P>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr op_state(S &&s, R &&r, P &&p)
        : sndr{std::forward<S>(s)}, rcvr{std::forward<R>(r)},
          pred{std::forward<P>(p)} {}
    constexpr op_state(op_state &&) = delete;

    constexpr auto start() & -> void {
        setup();
        if constexpr (synchronous_t<state_t>::value) {
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

    constexpr auto begin_loop() -> void {
        if constexpr (not stoppable_sender<Sndr, env_of_t<Rcvr>>) {
            if (get_stop_token(get_env(rcvr)).stop_requested()) {
                passthrough<set_stopped_t>();
                return;
            }
        }
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        async::start(*state);
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
        if constexpr (not synchronous_t<state_t>::value) {
            begin_loop();
        }
    }

    template <channel_tag Tag, typename... Args>
    auto passthrough(Args &&...args) -> void {
        debug_signal<Tag::name, debug::erased_context_for<op_state>>(
            get_env(rcvr));
        Tag{}(std::move(rcvr), std::forward<Args>(args)...);
        state.reset();
    }

    [[nodiscard]] constexpr auto query(async::get_env_t) const {
        return prop{completes_synchronously_t{}, synchronous_t<state_t>{}};
    }

    [[no_unique_address]] Sndr sndr;
    [[no_unique_address]] Rcvr rcvr;
    [[no_unique_address]] Pred pred;

    std::optional<state_t> state{};
};

template <stdx::ct_string Name, typename Sndr, typename Pred> struct sender {
    using is_sender = void;
    [[no_unique_address]] Sndr sndr;
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

    template <receiver_from<Sndr> R>
        requires multishot_sender<Sndr, R>
    [[nodiscard]] constexpr auto connect(
        R &&r) const & -> op_state<Name, Sndr, std::remove_cvref_t<R>, Pred> {
        return {sndr, std::forward<R>(r), p};
    }
};

template <stdx::ct_string Name, stdx::callable Pred> struct pipeable {
    Pred p;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Name, std::remove_cvref_t<S>, Pred>{
            std::forward<S>(s), std::forward<Self>(self).p};
    }
};
} // namespace _repeat

template <stdx::ct_string Name = "repeat_until", typename P>
[[nodiscard]] constexpr auto repeat_until(P &&p)
    -> _repeat::pipeable<Name, std::remove_cvref_t<P>> {
    return {std::forward<P>(p)};
}

template <stdx::ct_string Name = "repeat_until", sender S, typename P>
[[nodiscard]] auto repeat_until(S &&s, P &&p) {
    return std::forward<S>(s) | repeat_until(std::forward<P>(p));
}

template <stdx::ct_string Name = "repeat">
[[nodiscard]] constexpr auto repeat() {
    return repeat_until<Name>(_repeat::never_stop);
}

template <stdx::ct_string Name = "repeat", sender S>
[[nodiscard]] auto repeat(S &&s) {
    return std::forward<S>(s) | repeat<Name>();
}

template <stdx::ct_string Name = "repeat_n">
[[nodiscard]] constexpr auto repeat_n(unsigned int n) {
    return repeat_until<Name>([n](auto &&...) mutable { return n-- == 0; });
}

template <stdx::ct_string Name = "repeat_n", sender S>
[[nodiscard]] auto repeat_n(S &&s, unsigned int n) {
    return std::forward<S>(s) | repeat_n<Name>(n);
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
