#pragma once

#include <async/completes_synchronously.hpp>
#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/functional.hpp>
#include <stdx/tuple.hpp>

#include <concepts>
#include <type_traits>
#include <utility>
#include <variant>

namespace async {
namespace _incite_on {
template <typename Ops, typename Rcvr> struct receiver {
    using is_receiver = void;

    Ops *ops;

    [[nodiscard]] constexpr auto query(async::get_env_t) const
        -> forwarding_env<env_of_t<Rcvr>> {
        return forward_env_of(ops->rcvr);
    }

    template <typename F> auto set_value(F &&f) const && -> void {
        static_assert(stdx::invocable<std::remove_cvref_t<F>>,
                      "Sender passed to incite_on must send a function "
                      "that incites the scheduler!");
        ops->complete_first(std::forward<F>(f));
    }
    template <typename... Args>
    auto set_error(Args &&...args) const && -> void {
        ops->template passthrough<set_error_t>(std::forward<Args>(args)...);
    }
    auto set_stopped() const && -> void {
        ops->template passthrough<set_stopped_t>();
    }
};

template <typename S, typename Tag, typename E>
concept single_sender =
    stdx::tuple_size_v<typename async::detail::gather_signatures<
        Tag, completion_signatures_of_t<S, E>, stdx::tuple, stdx::tuple>> <= 1;

template <typename Sched, typename Rcvr, typename Sndr>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using first_rcvr = receiver<op_state, Rcvr>;
    static_assert(single_sender<Sndr, set_value_t, env_of_t<Rcvr>>,
                  "Sender passed to incite_on must send a single function "
                  "that incites the scheduler!");

    template <stdx::same_as_unqualified<Sched> Sch,
              stdx::same_as_unqualified<Sndr> S, typename R>
    constexpr op_state(Sch &&sch, S &&s, R &&r)
        : sched{std::forward<Sch>(sch)}, rcvr{std::forward<R>(r)},
          state{std::in_place_index<0>, stdx::with_result_of{[&] {
                    return connect(std::forward<S>(s), first_rcvr{this});
                }}} {}
    constexpr op_state(op_state &&) = delete;

    template <typename F> auto complete_first(F &&f) -> void {
        debug_signal<set_value_t::name, debug::erased_context_for<op_state>>(
            get_env(rcvr));
        auto &op = state.template emplace<1>(stdx::with_result_of{
            [&] { return connect(sched.schedule(), std::move(rcvr)); }});
        async::start(op);
        std::forward<F>(f)();
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

    [[no_unique_address]] Sched sched;
    [[no_unique_address]] Rcvr rcvr;

    using dependent_sender = decltype(sched.schedule());
    using first_ops = connect_result_t<Sndr, first_rcvr>;
    using second_ops = connect_result_t<dependent_sender, Rcvr>;
    std::variant<first_ops, second_ops> state;
};

template <typename Sched, typename S> struct sender {
    using is_sender = void;

    [[no_unique_address]] Sched sched;
    [[no_unique_address]] S s;

  private:
    using dependent_sender = decltype(sched.schedule());
    static_assert(
        not synchronous<dependent_sender>,
        "The scheduler passed to incite_on cannot have a synchronous sender!");

    template <typename Env>
    using dependent_completions = stdx::conditional_t<
        boost::mp11::mp_empty<value_signatures_of_t<S, Env>>::value,
        completion_signatures<>,
        completion_signatures_of_t<dependent_sender, Env>>;
    template <typename Env>
    using unchanged_completions =
        boost::mp11::mp_append<error_signatures_of_t<S, Env>,
                               stopped_signatures_of_t<S, Env>>;

  public:
    template <async::receiver R>
    [[nodiscard]] constexpr auto
    connect(R &&r) && -> op_state<Sched, std::remove_cvref_t<R>, S> {
        check_connect<sender &&, R>();
        return {std::move(sched), std::move(s), std::forward<R>(r)};
    }

    template <async::receiver R>
        requires multishot_sender<S> and std::copy_constructible<S> and
                 std::copy_constructible<Sched>
    [[nodiscard]] constexpr auto
    connect(R &&r) const & -> op_state<Sched, std::remove_cvref_t<R>, S> {
        check_connect<sender, R>();
        return {sched, s, std::forward<R>(r)};
    }

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> boost::mp11::mp_unique<boost::mp11::mp_append<
            unchanged_completions<Env>, dependent_completions<Env>>> {
        return {};
    }
};

template <typename Sched> struct pipeable {
    Sched sched;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Sched, std::remove_cvref_t<S>>{
            std::forward<Self>(self).sched, std::forward<S>(s)};
    }
};
} // namespace _incite_on

template <typename Sched>
[[nodiscard]] constexpr auto incite_on(Sched &&sched) {
    return _compose::adaptor{_incite_on::pipeable<std::remove_cvref_t<Sched>>{
        std::forward<Sched>(sched)}};
}

template <sender S, typename Sched>
[[nodiscard]] constexpr auto incite_on(S &&s, Sched &&sched) -> sender auto {
    return std::forward<S>(s) | incite_on(std::forward<Sched>(sched));
}

struct incite_on_t;

template <typename... Ts>
struct debug::context_for<_incite_on::op_state<Ts...>> {
    using tag = incite_on_t;
    constexpr static auto name = stdx::ct_string{"incite_on"};
    using type = _incite_on::op_state<Ts...>;
    using children =
        stdx::type_list<debug::erased_context_for<typename type::first_ops>,
                        debug::erased_context_for<typename type::second_ops>>;
};
} // namespace async
