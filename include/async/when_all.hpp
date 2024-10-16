#pragma once

#include <async/completes_synchronously.hpp>
#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/stop_token.hpp>

#include <stdx/atomic.hpp>
#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/tuple.hpp>
#include <stdx/tuple_algorithms.hpp>
#include <stdx/type_traits.hpp>
#include <stdx/utility.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/function.hpp>
#include <boost/mp11/list.hpp>

#include <concepts>
#include <cstddef>
#include <optional>
#include <type_traits>
#include <utility>

namespace async {
namespace _when_all {
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
        ops->emplace_value(std::forward<Args>(args)...);
    }
    template <typename... Args>
    auto set_error(Args &&...args) const && -> void {
        ops->emplace_error(std::forward<Args>(args)...);
    }
    auto set_stopped() const && -> void { ops->emplace_stopped(); }
};

template <typename S, typename Tag, typename E>
concept single_sender = requires {
    typename async::detail::gather_signatures<
        Tag, completion_signatures_of_t<S, E>, stdx::tuple,
        std::type_identity_t>;
};

template <typename E, typename S> struct sub_op_storage {
    using values_t = stdx::tuple<>;

    auto store(auto &&...) -> void {}
    auto load() -> values_t { return {}; }
};

template <typename E, single_sender<set_value_t, E> S>
struct sub_op_storage<E, S> {
    using values_t = value_types_of_t<typename S::sender_t, E, stdx::tuple,
                                      std::type_identity_t>;
    using ref_values_t =
        boost::mp11::mp_transform<std::add_lvalue_reference_t, values_t>;

    template <typename... Args> auto store(Args &&...args) -> void {
        v = stdx::make_tuple(std::forward<Args>(args)...);
    }
    auto load() -> ref_values_t {
        return v->apply([](auto &...args) { return ref_values_t{args...}; });
    }

    std::optional<values_t> v{};
};

template <typename Ops, typename R, typename S, typename StopToken>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct sub_op_state : sub_op_storage<env_of_t<R>, S> {
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

    template <typename... Args> auto emplace_value(Args &&...args) -> void {
        this->store(std::forward<Args>(args)...);
        static_cast<Ops &>(*this).notify();
    }
    template <typename... Args> auto emplace_error(Args &&...args) -> void {
        static_cast<Ops &>(*this).notify_error(std::forward<Args>(args)...);
    }
    auto emplace_stopped() -> void {
        static_cast<Ops &>(*this).notify_stopped();
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

template <typename...> struct error_op_state;

template <typename E> struct error_op_state<E, boost::mp11::mp_list<>> {
    [[nodiscard]] auto have_error() const -> bool { return false; }
    template <stdx::ct_string Name, typename Op>
    [[nodiscard]] auto release_error(auto &&) const -> bool {
        return false;
    }
    using signatures = completion_signatures<>;
};

template <typename E, single_sender<set_error_t, E>... Sndrs>
struct error_op_state<E, boost::mp11::mp_list<Sndrs...>> {
    template <typename... Args> auto store_error(Args &&...args) -> void {
        if (not caught_error.exchange(true)) {
            e.emplace(std::forward<Args>(args)...);
        }
    }
    template <stdx::ct_string Name, typename Op, typename R>
    [[nodiscard]] auto release_error(R &&r) -> bool {
        if (caught_error) {
            debug_signal<set_error_t::name, debug::erased_context_for<Op>>(
                get_env(r));
            set_error(std::forward<R>(r), std::move(*e));
            return true;
        }
        return false;
    }

    // All senders should send the same error type
    using error_t = std::common_type_t<error_types_of_t<
        typename Sndrs::sender_t, E, std::optional, std::type_identity_t>...>;
    using signatures =
        completion_signatures<set_error_t(typename error_t::value_type)>;

    error_t e{};
    stdx::atomic<bool> caught_error{};
};

template <typename E> struct is_error_sender {
    template <typename S>
    using fn = std::bool_constant<single_sender<S, set_error_t, E>>;
};
template <typename E, typename... Sndrs>
using error_senders = boost::mp11::mp_copy_if_q<boost::mp11::mp_list<Sndrs...>,
                                                is_error_sender<E>>;

template <stdx::ct_string Name, typename Rcvr, typename... Sndrs>
struct op_state
    : error_op_state<env_of_t<Rcvr>, error_senders<env_of_t<Rcvr>, Sndrs...>>,
      sub_op_state<op_state<Name, Rcvr, Sndrs...>, Rcvr, Sndrs,
                   inplace_stop_token>... {
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

    auto notify() -> void {
        if (--count == 0) {
            complete();
        }
    }

    template <typename... Args> auto notify_error(Args &&...args) -> void {
        this->store_error(std::forward<Args>(args)...);
        stop_source.request_stop();
        if (--count == 0) {
            complete();
        }
    }

    auto notify_stopped() -> void {
        stop_source.request_stop();
        if (--count == 0) {
            complete();
        }
    }

    template <typename S>
    using single_value_sender_t =
        std::bool_constant<single_sender<S, set_value_t, env_of_t<Rcvr>>>;

    auto complete() -> void {
        stop_cb.reset();
        if (this->template release_error<Name, op_state>(std::move(rcvr))) {
        } else if (stop_source.stop_requested()) {
            debug_signal<set_stopped_t::name,
                         debug::erased_context_for<op_state>>(get_env(rcvr));
            set_stopped(std::move(rcvr));
        } else {
            using value_senders =
                boost::mp11::mp_copy_if<boost::mp11::mp_list<Sndrs...>,
                                        single_value_sender_t>;
            [&]<typename... Ss>(boost::mp11::mp_list<Ss...>) {
                debug_signal<set_value_t::name,
                             debug::erased_context_for<op_state>>(
                    get_env(rcvr));
                stdx::tuple_cat(
                    static_cast<sub_op_state_t<Ss> &&>(*this).load()...)
                    .apply([&](auto &...args) {
                        set_value(std::move(rcvr), std::move(args)...);
                    });
            }(value_senders{});
        }
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
    stdx::atomic<std::size_t> count{};
    inplace_stop_source stop_source{};
    std::optional<stop_callback_t> stop_cb{};
};

template <stdx::ct_string Name, typename Rcvr, typename... Sndrs>
struct nostop_op_state
    : error_op_state<env_of_t<Rcvr>, error_senders<env_of_t<Rcvr>, Sndrs...>>,
      sub_op_state<nostop_op_state<Name, Rcvr, Sndrs...>, Rcvr, Sndrs,
                   never_stop_token>... {
    template <typename S>
    using sub_op_state_t =
        sub_op_state<nostop_op_state, Rcvr, S, never_stop_token>;

    using child_ops_t =
        stdx::type_list<typename sub_op_state_t<Sndrs>::ops_t...>;

    template <typename S, typename R>
    constexpr nostop_op_state(S &&s, R &&r)
        : sub_op_state_t<Sndrs>{std::forward<S>(s)}...,
          rcvr{std::forward<R>(r)} {}

    auto notify() -> void {
        if (--count == 0) {
            complete();
        }
    }

    template <typename... Args> auto notify_error(Args &&...args) -> void {
        this->store_error(std::forward<Args>(args)...);
        if (--count == 0) {
            complete();
        }
    }

    template <typename S>
    using single_value_sender_t =
        std::bool_constant<single_sender<S, set_value_t, env_of_t<Rcvr>>>;

    auto complete() -> void {
        if (this->template release_error<Name, nostop_op_state>(
                std::move(rcvr))) {
        } else {
            using value_senders =
                boost::mp11::mp_copy_if<boost::mp11::mp_list<Sndrs...>,
                                        single_value_sender_t>;
            [&]<typename... Ss>(boost::mp11::mp_list<Ss...>) {
                debug_signal<set_value_t::name,
                             debug::erased_context_for<nostop_op_state>>(
                    get_env(rcvr));
                stdx::tuple_cat(
                    static_cast<sub_op_state_t<Ss> &&>(*this).load()...)
                    .apply([&](auto &...args) {
                        set_value(std::move(rcvr), std::move(args)...);
                    });
            }(value_senders{});
        }
    }

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<nostop_op_state>>(
            get_env(rcvr));
        count = sizeof...(Sndrs);
        (async::start(static_cast<sub_op_state_t<Sndrs> &>(*this).ops), ...);
    }

    [[nodiscard]] auto get_stop_token() const -> never_stop_token { return {}; }

    [[no_unique_address]] Rcvr rcvr;
    stdx::atomic<std::size_t> count{};
};

template <stdx::ct_string Name, typename Rcvr, typename... Sndrs>
struct sync_op_state
    : error_op_state<env_of_t<Rcvr>, error_senders<env_of_t<Rcvr>, Sndrs...>>,
      sub_op_state<sync_op_state<Name, Rcvr, Sndrs...>, Rcvr, Sndrs,
                   never_stop_token>... {
    template <typename S>
    using sub_op_state_t =
        sub_op_state<sync_op_state, Rcvr, S, never_stop_token>;

    using child_ops_t =
        stdx::type_list<typename sub_op_state_t<Sndrs>::ops_t...>;

    template <typename S, typename R>
    constexpr sync_op_state(S &&s, R &&r)
        : sub_op_state_t<Sndrs>{std::forward<S>(s)}...,
          rcvr{std::forward<R>(r)} {}

    auto notify() -> void {}

    template <typename... Args> auto notify_error(Args &&...args) -> void {
        this->store_error(std::forward<Args>(args)...);
    }

    template <typename S>
    using single_value_sender_t =
        std::bool_constant<single_sender<S, set_value_t, env_of_t<Rcvr>>>;

    auto complete() -> void {
        if (this->template release_error<Name, sync_op_state>(
                std::move(rcvr))) {
        } else {
            using value_senders =
                boost::mp11::mp_copy_if<boost::mp11::mp_list<Sndrs...>,
                                        single_value_sender_t>;
            [&]<typename... Ss>(boost::mp11::mp_list<Ss...>) {
                debug_signal<set_value_t::name,
                             debug::erased_context_for<sync_op_state>>(
                    get_env(rcvr));
                stdx::tuple_cat(
                    static_cast<sub_op_state_t<Ss> &&>(*this).load()...)
                    .apply([&](auto &...args) {
                        set_value(std::move(rcvr), std::move(args)...);
                    });
            }(value_senders{});
        }
    }

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<sync_op_state>>(
            get_env(rcvr));
        (async::start(static_cast<sub_op_state_t<Sndrs> &>(*this).ops), ...);
        complete();
    }

    [[nodiscard]] constexpr static auto query(get_env_t) noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }

    [[nodiscard]] auto get_stop_token() const -> never_stop_token { return {}; }

    [[no_unique_address]] Rcvr rcvr;
};

template <typename S, typename R>
concept not_stoppable = not stoppable_sender<S, env_of_t<R>>;

template <stdx::ct_string Name, typename Rcvr, typename... Sndrs>
constexpr auto select_op_state() {
    if constexpr ((... and synchronous<Sndrs>)) {
        return std::type_identity<sync_op_state<Name, Rcvr, Sndrs...>>{};
    } else if constexpr ((... and not_stoppable<Sndrs, Rcvr>)) {
        return std::type_identity<nostop_op_state<Name, Rcvr, Sndrs...>>{};
    } else {
        return std::type_identity<op_state<Name, Rcvr, Sndrs...>>{};
    }
}

template <stdx::ct_string Name, typename Rcvr, typename... Sndrs>
using op_state_t =
    typename decltype(select_op_state<Name, Rcvr, Sndrs...>())::type;

template <stdx::ct_string Name, typename... Sndrs> struct sender : Sndrs... {
    using is_sender = void;

    template <typename... Ts>
    using as_value_signature = completion_signatures<set_value_t(Ts...)>;

    template <typename E>
    using signatures = boost::mp11::mp_unique<boost::mp11::mp_append<
        boost::mp11::mp_apply<as_value_signature,
                              boost::mp11::mp_append<typename sub_op_storage<
                                  E, Sndrs>::values_t...>>,
        typename error_op_state<E, error_senders<E, Sndrs...>>::signatures,
        detail::default_set_stopped<Sndrs, E>...>>;

    template <typename Env>
    [[nodiscard]] constexpr static auto
    get_completion_signatures(Env const &) -> signatures<Env> {
        return {};
    }

    template <typename R>
    [[nodiscard]] constexpr auto
    connect(R &&r) && -> op_state_t<Name, std::remove_cvref_t<R>, Sndrs...> {
        check_connect<sender &&, R>();
        return {std::move(*this), std::forward<R>(r)};
    }

    template <typename R>
        requires(... and multishot_sender<typename Sndrs::sender_t,
                                          std::remove_cvref_t<R>>)
    [[nodiscard]] constexpr auto connect(
        R &&r) const & -> op_state_t<Name, std::remove_cvref_t<R>, Sndrs...> {
        check_connect<sender const &, R>();
        return {*this, std::forward<R>(r)};
    }

    [[nodiscard]] constexpr static auto query(get_env_t) {
        if constexpr ((... and synchronous<Sndrs>)) {
            return prop{completes_synchronously_t{}, std::true_type{}};
        } else {
            return empty_env{};
        }
    }
};

template <stdx::ct_string Name, typename Rcvr> struct op_state<Name, Rcvr> {
    using child_ops_t = stdx::type_list<>;

    [[no_unique_address]] Rcvr rcvr;

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
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
        debug_signal<set_value_t::name, debug::erased_context_for<op_state>>(
            get_env(rcvr));
        set_value(std::move(rcvr));
    }

    [[nodiscard]] constexpr static auto query(get_env_t) noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }
};

template <stdx::ct_string Name> struct sender<Name> {
    using is_sender = void;

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> completion_signatures<set_value_t(), set_stopped_t()> {
        return {};
    }

    template <typename Env>
        requires unstoppable_token<stop_token_of_t<Env>>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> completion_signatures<set_value_t()> {
        return {};
    }

    template <receiver_from<sender> R>
    [[nodiscard]] constexpr static auto
    connect(R &&r) -> op_state<Name, std::remove_cvref_t<R>> {
        return {std::forward<R>(r)};
    }

    [[nodiscard]] constexpr static auto query(get_env_t) noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }
};
} // namespace _when_all

template <stdx::ct_string Name = "when_all", sender... Sndrs>
[[nodiscard]] constexpr auto when_all(Sndrs &&...sndrs) -> sender auto {
    if constexpr (sizeof...(Sndrs) == 1) {
        return (sndrs, ...);
    } else {
        return [&]<auto... Is>(std::index_sequence<Is...>) {
            return _when_all::sender<
                Name, _when_all::sub_sender<std::remove_cvref_t<Sndrs>, Is>...>{
                {std::forward<Sndrs>(sndrs)}...};
        }(std::make_index_sequence<sizeof...(Sndrs)>{});
    }
}

struct when_all_t;

template <stdx::ct_string Name, typename... Ts>
struct debug::context_for<_when_all::op_state<Name, Ts...>> {
    using tag = when_all_t;
    constexpr static auto name = Name;
    using type = _when_all::op_state<Name, Ts...>;
    using children = boost::mp11::mp_transform<debug::erased_context_for,
                                               typename type::child_ops_t>;
};

template <stdx::ct_string Name, typename... Ts>
struct debug::context_for<_when_all::nostop_op_state<Name, Ts...>> {
    using tag = when_all_t;
    constexpr static auto name = Name;
    using type = _when_all::nostop_op_state<Name, Ts...>;
    using children = boost::mp11::mp_transform<debug::erased_context_for,
                                               typename type::child_ops_t>;
};

template <stdx::ct_string Name, typename... Ts>
struct debug::context_for<_when_all::sync_op_state<Name, Ts...>> {
    using tag = when_all_t;
    constexpr static auto name = Name;
    using type = _when_all::sync_op_state<Name, Ts...>;
    using children = boost::mp11::mp_transform<debug::erased_context_for,
                                               typename type::child_ops_t>;
};
} // namespace async
