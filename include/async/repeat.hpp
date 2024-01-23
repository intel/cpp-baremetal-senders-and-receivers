#pragma once

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/functional.hpp>

#include <concepts>
#include <optional>
#include <type_traits>
#include <utility>

namespace async {
namespace _repeat {
template <typename Ops, typename Rcvr> struct receiver {
    using is_receiver = void;

    Ops *ops;

  private:
    template <typename... Args>
    friend constexpr auto tag_invoke(set_value_t, receiver const &r,
                                     Args &&...args) -> void {
        r.ops->repeat(std::forward<Args>(args)...);
    }
    template <typename... Args>
    friend constexpr auto tag_invoke(set_error_t, receiver const &r,
                                     Args &&...args) -> void {
        set_error(r.ops->rcvr, std::forward<Args>(args)...);
    }
    friend constexpr auto tag_invoke(set_stopped_t, receiver const &r) -> void {
        set_stopped(r.ops->rcvr);
    }

    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   receiver const &self)
        -> detail::forwarding_env<env_of_t<Rcvr>> {
        return forward_env_of(self.ops->rcvr);
    }
};

constexpr auto never_stop = [] { return false; };

// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
template <typename Sndr, typename Rcvr, typename Pred> struct op_state {
    template <typename S, typename R, typename P>
        requires(std::same_as<Sndr, std::remove_cvref_t<S>> and
                 std::same_as<Rcvr, std::remove_cvref_t<R>> and
                 std::same_as<Pred, std::remove_cvref_t<P>>)
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr op_state(S &&s, R &&r, P &&p)
        : sndr{std::forward<S>(s)}, rcvr{std::forward<R>(r)},
          pred{std::forward<P>(p)} {}
    constexpr op_state(op_state &&) = delete;

    auto restart() -> void {
        state.emplace(stdx::with_result_of{
            [&] { return connect(sndr, receiver<op_state, Rcvr>{this}); }});
        state->start();
    }

    template <typename... Args> auto repeat(Args &&...args) -> void {
        if constexpr (not std::same_as<
                          Pred, std::remove_cvref_t<decltype(never_stop)>>) {
            if (pred()) {
                set_value(rcvr, std::forward<Args>(args)...);
                return;
            }
        }
        restart();
    }

    auto start() -> void { restart(); }

    [[no_unique_address]] Sndr sndr;
    [[no_unique_address]] Rcvr rcvr;
    [[no_unique_address]] Pred pred;

    using state_t = async::connect_result_t<Sndr &, receiver<op_state, Rcvr>>;
    std::optional<state_t> state{};
};

template <typename Sndr, typename Pred> struct sender {
    using is_sender = void;
    [[no_unique_address]] Sndr sndr;
    [[no_unique_address]] Pred p;

  private:
    template <typename...> using signatures = completion_signatures<>;

    template <typename Env>
    [[nodiscard]] friend constexpr auto
    tag_invoke(get_completion_signatures_t, sender const &, Env const &) {
        if constexpr (std::same_as<Pred,
                                   std::remove_cvref_t<decltype(never_stop)>>) {
            return make_completion_signatures<
                Sndr, Env, completion_signatures<>, signatures>{};
        } else {
            return completion_signatures_of_t<Sndr, Env>{};
        }
    }

    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   sender const &self) {
        return forward_env_of(self.sndr);
    }

    template <typename Self, receiver_from<Sndr> R>
        requires std::same_as<sender, std::remove_cvref_t<Self>> and
                 multishot_sender<Sndr, R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r)
        -> op_state<Sndr, std::remove_cvref_t<R>, Pred> {
        return {std::forward<Self>(self).sndr, std::forward<R>(r),
                std::forward<Self>(self).p};
    }
};

template <stdx::predicate Pred> struct pipeable {
    Pred p;

  private:
    template <async::sender S, typename Self>
        requires std::same_as<pipeable, std::remove_cvref_t<Self>>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<std::remove_cvref_t<S>, Pred>{std::forward<S>(s),
                                                    std::forward<Self>(self).p};
    }
};
} // namespace _repeat

template <stdx::predicate P>
[[nodiscard]] constexpr auto repeat_until(P &&p)
    -> _repeat::pipeable<std::remove_cvref_t<P>> {
    return {std::forward<P>(p)};
}

template <sender S, stdx::predicate P>
[[nodiscard]] auto repeat_until(S &&s, P &&p) {
    return std::forward<S>(s) | repeat(std::forward<P>(p));
}

[[nodiscard]] constexpr auto repeat() {
    return repeat_until(_repeat::never_stop);
}

template <sender S> [[nodiscard]] auto repeat(S &&s) {
    return std::forward<S>(s) | repeat();
}

[[nodiscard]] constexpr auto repeat_n(unsigned int n) {
    return repeat_until([n]() mutable { return n-- == 0; });
}

template <sender S> [[nodiscard]] auto repeat_n(S &&s, unsigned int n) {
    return std::forward<S>(s) | repeat_n(n);
}
} // namespace async
