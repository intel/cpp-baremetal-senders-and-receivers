#pragma once

#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/tags.hpp>

#include <stdx/functional.hpp>

#include <concepts>
#include <type_traits>
#include <utility>
#include <variant>

namespace async {
namespace _sequence {
template <typename Ops, typename Rcvr> struct receiver {
    using is_receiver = void;

    Ops *ops;

  private:
    friend auto tag_invoke(set_value_t, receiver const &self, auto &&...)
        -> void {
        self.ops->complete_first();
    }

    template <channel_tag Tag, typename... Args>
    friend auto tag_invoke(Tag, receiver const &self, Args &&...args) -> void {
        Tag{}(self.ops->rcvr, std::forward<Args>(args)...);
    }

    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   receiver const &r)
        -> ::async::detail::forwarding_env<env_of_t<Rcvr>> {
        return forward_env_of(r.ops->rcvr);
    }
};

template <typename Sndr, std::invocable Func, typename Rcvr>
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

    template <typename... Args> auto complete_first() -> void {
        auto &op = state.template emplace<1>(stdx::with_result_of{
            [&] { return connect(std::move(func)(), std::move(rcvr)); }});
        start(std::move(op));
    }

    [[no_unique_address]] Func func;
    [[no_unique_address]] Rcvr rcvr;

    using dependent_sender = std::invoke_result_t<Func>;
    using first_ops = connect_result_t<Sndr, first_rcvr>;
    using second_ops = connect_result_t<dependent_sender, Rcvr>;
    std::variant<first_ops, second_ops> state;

  private:
    template <stdx::same_as_unqualified<op_state> O>
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        start(std::get<0>(std::forward<O>(o).state));
    }
};

namespace detail {
template <async::sender S> struct wrapper {
    using eager_sender_t = S;
    [[no_unique_address]] S s;
    [[nodiscard]] constexpr auto operator()() && -> S { return std::move(s); }
};
template <typename S> wrapper(S) -> wrapper<S>;

template <typename F>
concept has_eager_sender = requires(F f) {
    { std::move(f)() } -> std::same_as<typename F::eager_sender_t>;
    { f.s } -> std::same_as<typename F::eager_sender_t &>;
};
} // namespace detail

template <typename S, std::invocable F> struct sender {
    using is_sender = void;

    [[no_unique_address]] S s;
    [[no_unique_address]] F f;

  private:
    using dependent_sender = std::invoke_result_t<F>;
    static_assert(async::sender<dependent_sender>,
                  "The function passed to sequence must return a sender");

    template <async::receiver R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender &&self,
                                                   R &&r)
        -> op_state<S, F, std::remove_cvref_t<R>> {
        check_connect<sender &&, R>();
        return {std::move(self).s, std::move(self).f, std::forward<R>(r)};
    }

    template <stdx::same_as_unqualified<sender> Self, async::receiver R>
        requires multishot_sender<S> and std::copy_constructible<S> and
                 std::copy_constructible<F>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r)
        -> op_state<S, F, std::remove_cvref_t<R>> {
        check_connect<Self, R>();
        return {std::forward<Self>(self).s, std::forward<Self>(self).f,
                std::forward<R>(r)};
    }

    template <typename Env>
    using dependent_completions =
        completion_signatures_of_t<dependent_sender, Env>;

    template <typename Env>
    using unchanged_completions =
        boost::mp11::mp_append<error_signatures_of_t<S, Env>,
                               stopped_signatures_of_t<S, Env>>;

    template <typename Env>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> boost::mp11::mp_unique<boost::mp11::mp_append<
            unchanged_completions<Env>, dependent_completions<Env>>> {
        return {};
    }

    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   sender const &sndr)
        requires detail::has_eager_sender<F> or
                 std::is_empty_v<env_of_t<dependent_sender>>
    {
        if constexpr (detail::has_eager_sender<F>) {
            return forward_env_of(sndr.f.s);
        } else {
            return decltype(forward_env_of(std::declval<dependent_sender>())){};
        }
    }
};

template <std::invocable F> struct pipeable {
    [[no_unique_address]] F f;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<std::remove_cvref_t<S>, F>{std::forward<S>(s),
                                                 std::forward<Self>(self).f};
    }
};
} // namespace _sequence

template <std::invocable F> [[nodiscard]] constexpr auto sequence(F &&f) {
    return _compose::adaptor{stdx::tuple{
        _sequence::pipeable<std::remove_cvref_t<F>>{std::forward<F>(f)}}};
}

template <sender S, std::invocable F>
[[nodiscard]] constexpr auto sequence(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | sequence(std::forward<F>(f));
}

template <sender S> [[nodiscard]] constexpr auto seq(S &&s) {
    return sequence(_sequence::detail::wrapper{std::forward<S>(s)});
}
} // namespace async
