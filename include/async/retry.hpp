#pragma once

#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/functional.hpp>

#include <boost/mp11/algorithm.hpp>

#include <concepts>
#include <optional>
#include <type_traits>
#include <utility>

namespace async {
namespace _retry {

template <typename Ops, typename Rcvr> struct receiver {
    using is_receiver = void;

    Ops *ops;

    [[nodiscard]] constexpr auto
    query(async::get_env_t) const -> detail::forwarding_env<env_of_t<Rcvr>> {
        return forward_env_of(ops->rcvr);
    }

    template <typename... Args>
    auto set_value(Args &&...args) const && -> void {
        async::set_value(std::move(ops->rcvr), std::forward<Args>(args)...);
    }
    template <typename... Args>
    auto set_error(Args &&...args) const && -> void {
        ops->retry(std::forward<Args>(args)...);
    }
    auto set_stopped() const && -> void {
        async::set_stopped(std::move(ops->rcvr));
    }
};

constexpr auto never_stop = [](auto &&...) { return false; };

template <typename Pred, typename Sig, typename = void>
struct is_callable : std::false_type {};
template <typename Pred, typename... Args>
struct is_callable<Pred, set_error_t(Args...),
                   std::void_t<std::invoke_result_t<Pred, Args...>>>
    : std::true_type {};

template <typename Pred> struct callable_with {
    template <typename Sig> using fn = is_callable<Pred, Sig>;
};

// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
template <typename Sndr, typename Rcvr, typename Pred> struct op_state {
    using receiver_t = receiver<op_state, Rcvr>;
    using value_completions = error_signatures_of_t<Sndr, env_of_t<receiver_t>>;
    static_assert(
        boost::mp11::mp_all_of_q<value_completions, callable_with<Pred>>::value,
        "Predicate is not callable with error completions of sender");

    template <stdx::same_as_unqualified<Sndr> S,
              stdx::same_as_unqualified<Rcvr> R,
              stdx::same_as_unqualified<Pred> P>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr op_state(S &&s, R &&r, P &&p)
        : sndr{std::forward<S>(s)}, rcvr{std::forward<R>(r)},
          pred{std::forward<P>(p)} {}
    constexpr op_state(op_state &&) = delete;

    constexpr auto start() & -> void {
        auto &op = state.emplace(stdx::with_result_of{
            [&] { return connect(sndr, receiver_t{this}); }});
        async::start(op);
    }

    template <typename... Args> auto retry(Args &&...args) -> void {
        if constexpr (not std::same_as<
                          Pred, std::remove_cvref_t<decltype(never_stop)>>) {
            if (pred(args...)) {
                set_error(std::move(rcvr), std::forward<Args>(args)...);
                return;
            }
        }
        start();
    }

    [[no_unique_address]] Sndr sndr;
    [[no_unique_address]] Rcvr rcvr;
    [[no_unique_address]] Pred pred;

    using state_t = async::connect_result_t<Sndr &, receiver_t>;
    std::optional<state_t> state{};
};

template <typename Sndr, typename Pred> struct sender {
    using is_sender = void;
    [[no_unique_address]] Sndr sndr;
    [[no_unique_address]] Pred p;

    [[nodiscard]] constexpr auto query(get_env_t) const {
        return forward_env_of(sndr);
    }

    template <typename...> using signatures = completion_signatures<>;

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &) {
        if constexpr (std::same_as<Pred,
                                   std::remove_cvref_t<decltype(never_stop)>>) {
            return transform_completion_signatures_of<
                Sndr, Env, completion_signatures<>, detail::default_set_value,
                signatures>{};
        } else {
            return completion_signatures_of_t<Sndr, Env>{};
        }
    }

  private:
    template <stdx::same_as_unqualified<sender> Self, receiver_from<Sndr> R>
        requires multishot_sender<Sndr, R>
    [[nodiscard]] friend constexpr auto
    tag_invoke(connect_t, Self &&self,
               R &&r) -> op_state<Sndr, std::remove_cvref_t<R>, Pred> {
        return {std::forward<Self>(self).sndr, std::forward<R>(r),
                std::forward<Self>(self).p};
    }
};

template <typename Pred> struct pipeable {
    Pred p;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<std::remove_cvref_t<S>, Pred>{std::forward<S>(s),
                                                    std::forward<Self>(self).p};
    }
};
} // namespace _retry

template <typename P>
[[nodiscard]] constexpr auto
retry_until(P &&p) -> _retry::pipeable<std::remove_cvref_t<P>> {
    return {std::forward<P>(p)};
}

template <sender S, typename P> [[nodiscard]] auto retry_until(S &&s, P &&p) {
    return std::forward<S>(s) | retry_until(std::forward<P>(p));
}

[[nodiscard]] constexpr auto retry() { return retry_until(_retry::never_stop); }

template <sender S> [[nodiscard]] auto retry(S &&s) {
    return std::forward<S>(s) | retry();
}

} // namespace async
