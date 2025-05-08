#pragma once

#include <async/completes_synchronously.hpp>
#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>

#include <stdx/atomic.hpp>
#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/functional.hpp>
#include <stdx/tuple.hpp>
#include <stdx/type_traits.hpp>
#include <stdx/utility.hpp>

#include <conc/concurrency.hpp>

#include <concepts>
#include <cstddef>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>

namespace async {
namespace _when_any {
template <typename S, std::size_t> struct sub_sender : S {
    using sender_t = S;
};

template <typename SubOps> struct sub_receiver {
    using is_receiver = void;

    SubOps *ops;

    [[nodiscard]] constexpr auto query(get_env_t) const
        -> overriding_env<get_stop_token_t, typename SubOps::stop_token_t,
                          typename SubOps::receiver_t> {
        return override_env_with<get_stop_token_t>(ops->get_stop_token(),
                                                   ops->get_receiver());
    }

    template <typename... Args>
    auto set_value(Args &&...args) const && -> void {
        ops->template emplace<set_value_t>(std::forward<Args>(args)...);
    }
    template <typename... Args>
    auto set_error(Args &&...args) const && -> void {
        ops->template emplace<set_error_t>(std::forward<Args>(args)...);
    }
    auto set_stopped() const && -> void {
        ops->template emplace<set_stopped_t>();
    }
};

template <typename Ops, typename R, typename S, typename StopToken>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct sub_op_state {
    using sender_t = typename S::sender_t;
    using receiver_t = R;
    using stop_token_t = StopToken;

    constexpr explicit(true) sub_op_state(S &&s)
        : ops{connect(static_cast<sender_t &&>(std::move(s)),
                      sub_receiver<sub_op_state>{this})} {}
    constexpr explicit(true) sub_op_state(S const &s)
        : ops{connect(static_cast<sender_t const &>(s),
                      sub_receiver<sub_op_state>{this})} {}
    constexpr sub_op_state(sub_op_state &&) = delete;

    template <typename Tag, typename... Args>
    auto emplace(Args &&...args) -> void {
        static_cast<Ops &>(*this).template emplace<Tag>(
            std::forward<Args>(args)...);
    }

    [[nodiscard]] auto get_receiver() const -> receiver_t const & {
        return static_cast<Ops const &>(*this).rcvr;
    }

    [[nodiscard]] auto get_stop_token() const -> stop_token_t {
        return static_cast<Ops const &>(*this).get_stop_token();
    }

    using ops_t = connect_result_t<sender_t, sub_receiver<sub_op_state>>;
    ops_t ops;
};

template <typename... Ts>
using decayed_tuple = stdx::tuple<std::remove_cvref_t<Ts>...>;

// Policy: when the first sender completes successfully, all other senders are
// stopped. If no senders complete successfully, the first error is reported.
// Stopped is reported only if all senders are stopped.
struct first_successful {
    template <typename Mutex, typename Tag, typename Completions,
              typename... Args>
    static auto emplace(Completions &completions, auto &stop_source,
                        Args &&...args) -> void {
        using T = decayed_tuple<Tag, Args...>;
        using index = boost::mp11::mp_find<Completions, T>;
        static_assert(index::value < boost::mp11::mp_size<Completions>::value);

        conc::call_in_critical_section<Mutex>([&] {
            auto const should_store = [&] {
                if constexpr (std::same_as<Tag, set_value_t>) {
                    return std::visit(
                        stdx::overload{
                            []<typename Tuple>(Tuple const &) {
                                return not std::same_as<
                                    boost::mp11::mp_first<Tuple>, set_value_t>;
                            },
                            [](std::monostate) { return true; }},
                        completions);
                }
                return completions.index() == 0;
            }();

            if (should_store) {
                completions.template emplace<index::value>(
                    stdx::make_tuple(Tag{}, std::forward<Args>(args)...));
            }
        });
        if constexpr (std::same_as<Tag, set_value_t>) {
            stop_source.request_stop();
        }
    }
};

// Policy: when the first sender completes either with success or error, all
// other senders are stopped. The first success or error is reported. Stopped is
// reported only if all senders are stopped.
struct first_noncancelled {
    template <typename Mutex, typename Tag, typename Completions,
              typename... Args>
    static auto emplace(Completions &completions, auto &stop_source,
                        Args &&...args) -> void {
        using T = decayed_tuple<Tag, Args...>;
        using index = boost::mp11::mp_find<Completions, T>;
        static_assert(index::value < boost::mp11::mp_size<Completions>::value);

        conc::call_in_critical_section<Mutex>([&] {
            auto const should_store = [&] {
                if constexpr (std::same_as<Tag, set_stopped_t>) {
                    return completions.index() == 0;
                }
                return std::visit(
                    stdx::overload{
                        []<typename Tuple>(Tuple const &) {
                            return std::same_as<boost::mp11::mp_first<Tuple>,
                                                set_stopped_t>;
                        },
                        [](std::monostate) { return true; }},
                    completions);
            }();

            if (should_store) {
                completions.template emplace<index::value>(
                    stdx::make_tuple(Tag{}, std::forward<Args>(args)...));
            }
        });
        if constexpr (not std::same_as<Tag, set_stopped_t>) {
            stop_source.request_stop();
        }
    }
};

// Policy: when the first sender completes on any channel, all other senders are
// stopped.
struct first_complete {
    template <typename Mutex, typename Tag, typename Completions,
              typename... Args>
    static auto emplace(Completions &completions, auto &stop_source,
                        Args &&...args) -> void {
        using T = decayed_tuple<Tag, Args...>;
        using index = boost::mp11::mp_find<Completions, T>;
        static_assert(index::value < boost::mp11::mp_size<Completions>::value);

        conc::call_in_critical_section<Mutex>([&] {
            if (completions.index() == 0) {
                completions.template emplace<index::value>(
                    stdx::make_tuple(Tag{}, std::forward<Args>(args)...));
            }
        });
        stop_source.request_stop();
    }
};

template <typename Tag> struct prepend {
    template <typename L> using fn = boost::mp11::mp_push_front<L, Tag>;
};

template <typename Tag, typename L>
using apply_tag = boost::mp11::mp_transform_q<prepend<Tag>, L>;

template <stdx::ct_string Name, typename StopPolicy, typename Rcvr,
          typename... Sndrs>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state : sub_op_state<op_state<Name, StopPolicy, Rcvr, Sndrs...>, Rcvr,
                               Sndrs, inplace_stop_token>... {
    template <typename S>
    using sub_op_state_t = sub_op_state<op_state, Rcvr, S, inplace_stop_token>;

    using child_ops_t =
        stdx::type_list<typename sub_op_state_t<Sndrs>::ops_t...>;

    struct stop_callback_fn {
        auto operator()() -> void { stop_source->request_stop(); }
        inplace_stop_source *stop_source;
    };

    template <typename S, typename R>
    constexpr op_state(S &&s, R &&r)
        : sub_op_state_t<Sndrs>{std::forward<S>(s)}...,
          rcvr{std::forward<R>(r)} {}
    constexpr op_state(op_state &&) = delete;

    using env_t = overriding_env<get_stop_token_t, inplace_stop_token, Rcvr>;

    using completions_t = boost::mp11::mp_unique<boost::mp11::mp_append<
        std::variant<std::monostate>,
        apply_tag<set_value_t, value_types_of_t<Sndrs, env_t, decayed_tuple,
                                                std::variant>>...,
        apply_tag<set_error_t, error_types_of_t<Sndrs, env_t, decayed_tuple,
                                                std::variant>>...,
        apply_tag<set_stopped_t, stopped_types_of_t<Sndrs, env_t, decayed_tuple,
                                                    std::variant>>...>>;

    struct mutex;

    template <typename Tag, typename... Args>
    auto emplace(Args &&...args) -> void {
        StopPolicy::template emplace<mutex, Tag>(completions, stop_source,
                                                 std::forward<Args>(args)...);
        if (--count == 0) {
            complete();
        }
    }

    auto complete() -> void {
        stop_cb.reset();
        if constexpr (not async::unstoppable_token<
                          async::stop_token_of_t<async::env_of_t<Rcvr>>>) {
            if (async::get_stop_token(async::get_env(rcvr)).stop_requested()) {
                debug_signal<set_stopped_t::name,
                             debug::erased_context_for<op_state>>(
                    get_env(rcvr));
                set_stopped(std::move(rcvr));
                return;
            }
        }

        std::visit(
            stdx::overload{
                [&]<typename T>(T &&t) {
                    std::forward<T>(t).apply([&]<typename Tag,
                                                 typename... Args>(
                                                 Tag tag, Args &&...args) {
                        using ctx = debug::erased_context_for<op_state>;
                        // clang-20 has some issues here if we do the obvious
                        // thing of using Tag::name - this workaround is
                        // hopefully temporary
                        if constexpr (std::same_as<Tag, set_value_t>) {
                            debug_signal<set_value_t::name, ctx>(get_env(rcvr));
                        } else if constexpr (std::same_as<Tag, set_error_t>) {
                            debug_signal<set_error_t::name, ctx>(get_env(rcvr));
                        }
                        tag(std::move(rcvr), std::forward<Args>(args)...);
                    });
                },
                [](std::monostate) {}},
            std::move(completions));
    }

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        stop_cb.emplace(async::get_stop_token(get_env(rcvr)),
                        stop_callback_fn{std::addressof(stop_source)});
        if (stop_source.stop_requested()) {
            debug_signal<set_stopped_t::name,
                         debug::erased_context_for<op_state>>(get_env(rcvr));
            set_stopped(std::move(rcvr));
        } else {
            count = sizeof...(Sndrs);
            (async::start(static_cast<sub_op_state_t<Sndrs> &>(*this).ops),
             ...);
        }
    }

    [[nodiscard]] auto get_stop_token() const -> inplace_stop_token {
        return stop_source.get_token();
    }

    using stop_callback_t =
        stop_callback_for_t<stop_token_of_t<env_of_t<Rcvr>>, stop_callback_fn>;

    [[no_unique_address]] Rcvr rcvr;
    completions_t completions{};
    stdx::atomic<std::size_t> count;
    inplace_stop_source stop_source;
    std::optional<stop_callback_t> stop_cb{};
};

template <stdx::ct_string Name, typename StopPolicy, typename Rcvr,
          typename... Sndrs>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct nostop_op_state
    : sub_op_state<nostop_op_state<Name, StopPolicy, Rcvr, Sndrs...>, Rcvr,
                   Sndrs, never_stop_token>... {
    template <typename S>
    using sub_op_state_t =
        sub_op_state<nostop_op_state, Rcvr, S, never_stop_token>;

    using child_ops_t =
        stdx::type_list<typename sub_op_state_t<Sndrs>::ops_t...>;

    template <typename S, typename R>
    constexpr nostop_op_state(S &&s, R &&r)
        : sub_op_state_t<Sndrs>{std::forward<S>(s)}...,
          rcvr{std::forward<R>(r)} {}
    constexpr nostop_op_state(nostop_op_state &&) = delete;

    using env_t = overriding_env<get_stop_token_t, never_stop_token, Rcvr>;

    using completions_t = boost::mp11::mp_unique<boost::mp11::mp_append<
        std::variant<std::monostate>,
        apply_tag<set_value_t, value_types_of_t<Sndrs, env_t, decayed_tuple,
                                                std::variant>>...,
        apply_tag<set_error_t, error_types_of_t<Sndrs, env_t, decayed_tuple,
                                                std::variant>>...,
        apply_tag<set_stopped_t, stopped_types_of_t<Sndrs, env_t, decayed_tuple,
                                                    std::variant>>...>>;

    struct mutex;

    template <typename Tag, typename... Args>
    auto emplace(Args &&...args) -> void {
        StopPolicy::template emplace<mutex, Tag>(completions, stop_source,
                                                 std::forward<Args>(args)...);
        if (--count == 0) {
            complete();
        }
    }

    auto complete() -> void {
        std::visit(
            stdx::overload{
                [&]<typename T>(T &&t) {
                    std::forward<T>(t).apply(
                        [&]<typename Tag, typename... Args>(Tag tag,
                                                            Args &&...args) {
                            debug_signal<Tag::name, debug::erased_context_for<
                                                        nostop_op_state>>(
                                get_env(rcvr));
                            tag(std::move(rcvr), std::forward<Args>(args)...);
                        });
                },
                [](std::monostate) {}},
            std::move(completions));
    }

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<nostop_op_state>>(
            get_env(rcvr));
        count = sizeof...(Sndrs);
        (async::start(static_cast<sub_op_state_t<Sndrs> &>(*this).ops), ...);
    }

    [[nodiscard]] auto get_stop_token() const -> never_stop_token { return {}; }

    [[no_unique_address]] Rcvr rcvr;
    completions_t completions{};
    stdx::atomic<std::size_t> count;
    [[no_unique_address]] never_stop_source stop_source{};
};

using stopping_env = prop<get_stop_token_t, inplace_stop_token>;
template <typename S>
concept not_stoppable = not stoppable_sender<S, stopping_env>;

template <stdx::ct_string Name, typename StopPolicy, typename Rcvr,
          typename... Sndrs>
constexpr auto select_op_state() {
    if constexpr ((... and not_stoppable<Sndrs>)) {
        return std::type_identity<
            nostop_op_state<Name, StopPolicy, Rcvr, Sndrs...>>{};
    } else {
        return std::type_identity<op_state<Name, StopPolicy, Rcvr, Sndrs...>>{};
    }
}

template <stdx::ct_string Name, typename StopPolicy, typename Rcvr,
          typename... Sndrs>
using op_state_t = typename decltype(select_op_state<Name, StopPolicy, Rcvr,
                                                     Sndrs...>())::type;

template <stdx::ct_string Name, typename StopPolicy, typename... Sndrs>
struct sender : Sndrs... {
    using is_sender = void;

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> boost::mp11::mp_unique<
            boost::mp11::mp_append<completion_signatures_of_t<Sndrs, Env>...>> {
        return {};
    }

    template <typename R>
    [[nodiscard]] constexpr auto connect(R &&r)
        && -> op_state_t<Name, StopPolicy, std::remove_cvref_t<R>, Sndrs...> {
        check_connect<sender &&, R>();
        return {std::move(*this), std::forward<R>(r)};
    }

    template <typename R>
        requires(
            ... and
            multishot_sender<
                typename Sndrs::sender_t,
                detail::universal_receiver<env_of_t<std::remove_cvref_t<R>>>>)
    [[nodiscard]] constexpr auto connect(R &&r) const
        & -> op_state_t<Name, StopPolicy, std::remove_cvref_t<R>, Sndrs...> {
        check_connect<sender const &, R>();
        return {*this, std::forward<R>(r)};
    }
};

template <stdx::ct_string Name, typename StopPolicy, typename Rcvr>
struct op_state<Name, StopPolicy, Rcvr> {
    using child_ops_t = stdx::type_list<>;

    struct stop_callback_fn {
        auto operator()() -> void {
            debug_signal<set_stopped_t::name,
                         debug::erased_context_for<op_state>>(
                get_env(ops->rcvr));
            set_stopped(std::move(ops->rcvr));
            ops->stop_cb.reset();
        }
        op_state *ops;
    };

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        if constexpr (unstoppable_token<stop_token_of_t<env_of_t<Rcvr>>>) {
            static_assert(stdx::always_false_v<Rcvr>,
                          "Starting when_any<> but the connected receiver "
                          "cannot cancel!");
        }
        stop_cb.emplace(async::get_stop_token(get_env(rcvr)),
                        stop_callback_fn{this});
    }

    [[nodiscard]] constexpr static auto query(get_env_t) noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }

    using stop_callback_t =
        stop_callback_for_t<stop_token_of_t<env_of_t<Rcvr>>, stop_callback_fn>;

    [[no_unique_address]] Rcvr rcvr;
    std::optional<stop_callback_t> stop_cb{};
};

template <stdx::ct_string Name, typename StopPolicy>
struct sender<Name, StopPolicy> {
    using is_sender = void;

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> completion_signatures<set_stopped_t()> {
        return {};
    }

    template <typename Env>
        requires unstoppable_token<stop_token_of_t<Env>>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> completion_signatures<> {
        return {};
    }

    template <typename R>
    [[nodiscard]] constexpr auto connect(R &&r) const
        -> op_state<Name, StopPolicy, std::remove_cvref_t<R>> {
        check_connect<sender const &, R>();
        return {std::forward<R>(r)};
    }

    [[nodiscard]] constexpr static auto query(get_env_t) noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }
};

template <stdx::ct_string Name, typename Sndr> struct pipeable {
    Sndr sndr;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Name, first_complete, sub_sender<Sndr, 0>,
                      sub_sender<std::remove_cvref_t<S>, 1>>{
            std::forward<Self>(self).sndr, std::forward<S>(s)};
    }
};
} // namespace _when_any

template <stdx::ct_string Name = "when_any",
          typename StopPolicy = _when_any::first_noncancelled, sender... Sndrs>
[[nodiscard]] constexpr auto when_any(Sndrs &&...sndrs) -> sender auto {
    if constexpr (sizeof...(Sndrs) == 1) {
        return (sndrs, ...);
    } else {
        return [&]<auto... Is>(std::index_sequence<Is...>) {
            return _when_any::sender<
                Name, StopPolicy,
                _when_any::sub_sender<std::remove_cvref_t<Sndrs>, Is>...>{
                {std::forward<Sndrs>(sndrs)}...};
        }(std::make_index_sequence<sizeof...(Sndrs)>{});
    }
}

template <stdx::ct_string Name = "first_successful", sender... Sndrs>
[[nodiscard]] constexpr auto first_successful(Sndrs &&...sndrs) -> sender auto {
    return when_any<Name, _when_any::first_successful>(
        std::forward<Sndrs>(sndrs)...);
}

template <stdx::ct_string Name = "stop_when", typename Trigger>
[[nodiscard]] constexpr auto stop_when(Trigger &&t) {
    return _compose::adaptor{
        _when_any::pipeable<Name, std::remove_cvref_t<Trigger>>{
            std::forward<Trigger>(t)}};
}

template <stdx::ct_string Name = "stop_when", sender Sndr, sender Trigger>
[[nodiscard]] constexpr auto stop_when(Sndr &&s, Trigger &&t) -> sender auto {
    return std::forward<Sndr>(s) | stop_when<Name>(std::forward<Trigger>(t));
}

struct when_any_t;

template <stdx::ct_string Name, typename... Ts>
struct debug::context_for<_when_any::op_state<Name, Ts...>> {
    using tag = when_any_t;
    constexpr static auto name = Name;
    using type = _when_any::op_state<Name, Ts...>;
    using children = boost::mp11::mp_transform<debug::erased_context_for,
                                               typename type::child_ops_t>;
};

template <stdx::ct_string Name, typename... Ts>
struct debug::context_for<_when_any::nostop_op_state<Name, Ts...>> {
    using tag = when_any_t;
    constexpr static auto name = Name;
    using type = _when_any::nostop_op_state<Name, Ts...>;
    using children = boost::mp11::mp_transform<debug::erased_context_for,
                                               typename type::child_ops_t>;
};
} // namespace async
