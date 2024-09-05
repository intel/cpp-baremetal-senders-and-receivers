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

#include <concepts>
#include <type_traits>
#include <utility>
#include <variant>

namespace async {
namespace _sequence {
template <typename Ops, typename Rcvr> struct receiver {
    using is_receiver = void;

    Ops *ops;

    [[nodiscard]] constexpr auto
    query(async::get_env_t) const -> forwarding_env<env_of_t<Rcvr>> {
        return forward_env_of(ops->rcvr);
    }

    auto set_value(auto &&...) const && -> void { ops->complete_first(); }
    template <typename... Args>
    auto set_error(Args &&...args) const && -> void {
        ops->template passthrough<set_error_t>(std::forward<Args>(args)...);
    }
    auto set_stopped() const && -> void {
        ops->template passthrough<set_stopped_t>();
    }
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

    template <typename... Args> auto complete_first() -> void {
        debug_signal<set_value_t::name, Name, op_state>(get_env(rcvr));
        auto &op = state.template emplace<1>(stdx::with_result_of{
            [&] { return connect(std::move(func)(), std::move(rcvr)); }});
        async::start(op);
    }

    template <channel_tag Tag, typename... Args>
    auto passthrough(Args &&...args) -> void {
        debug_signal<Tag::name, Name, op_state>(get_env(rcvr));
        Tag{}(std::move(rcvr), std::forward<Args>(args)...);
    }

    constexpr auto start() & -> void {
        debug_signal<"start", Name, op_state>(get_env(rcvr));
        async::start(std::get<0>(state));
    }

    [[nodiscard]] constexpr static auto query(get_env_t) {
        return prop{completes_synchronously_t{},
                    std::bool_constant < synchronous<first_ops> and
                        synchronous < second_ops >> {}};
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
        requires multishot_sender<S> and std::copy_constructible<S> and
                     std::copy_constructible<F>
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
                    std::bool_constant < synchronous<S> and
                        synchronous < dependent_sender >> {}};
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
    return _compose::adaptor{stdx::tuple{
        _sequence::pipeable<Name, std::remove_cvref_t<F>>{std::forward<F>(f)}}};
}

template <stdx::ct_string Name = "sequence", sender S, std::invocable F>
[[nodiscard]] constexpr auto sequence(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | sequence<Name>(std::forward<F>(f));
}

template <stdx::ct_string Name = "seq", sender S>
[[nodiscard]] constexpr auto seq(S &&s) {
    return sequence<Name>(_sequence::detail::wrapper{std::forward<S>(s)});
}
} // namespace async
