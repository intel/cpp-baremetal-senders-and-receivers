#pragma once

#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/get_completion_signatures.hpp>
#include <async/start.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>

#include <stdx/compiler.hpp>
#include <stdx/concepts.hpp>
#include <stdx/function_traits.hpp>
#include <stdx/type_traits.hpp>

#include <concepts>
#include <type_traits>

namespace async {
struct set_value_t;
struct set_error_t;
struct set_stopped_t;

template <typename T>
concept channel_tag =
    std::same_as<set_value_t, T> or std::same_as<set_error_t, T> or
    std::same_as<set_stopped_t, T>;

template <typename T>
concept queryable = std::destructible<T>;

template <typename O>
concept operation_state =
    queryable<O> and std::is_object_v<O> and requires(O &o) {
        { start(o) } -> std::same_as<void>;
    };

namespace detail {
template <typename T>
concept movable_value = std::move_constructible<std::remove_cvref_t<T>> and
                        std::constructible_from<std::remove_cvref_t<T>, T>;
}

struct receiver_base {};
template <typename R>
constexpr inline bool enable_receiver = requires { typename R::is_receiver; } or
                                        stdx::derived_from<R, receiver_base>;

template <typename R>
concept receiver = enable_receiver<std::remove_cvref_t<R>> and
                   requires(std::remove_cvref_t<R> const &r) {
                       { get_env(r) } -> queryable;
                   } and detail::movable_value<R>;

namespace detail {
template <typename Signature, typename R>
concept valid_completion_for = requires(Signature *sig) {
    []<typename Tag, typename... Args>(Tag (*)(Args...))
        requires std::invocable<Tag, std::remove_cvref_t<R>, Args...>
    {}(sig);
};
} // namespace detail

template <typename R, typename Completions>
concept receiver_of = receiver<R> and requires(Completions *completions) {
    []<detail::valid_completion_for<R>... Sigs>(
        completion_signatures<Sigs...> *) {}(completions);
};

namespace detail {
template <typename...>
constexpr inline auto valid_completion_signatures_v = false;
template <typename... Sigs>
constexpr inline auto
    valid_completion_signatures_v<completion_signatures<Sigs...>> = true;

template <typename Completions>
concept valid_completion_signatures =
    valid_completion_signatures_v<Completions>;
} // namespace detail

struct sender_base {};
template <typename S>
constexpr inline bool enable_sender =
    requires { typename S::is_sender; } or stdx::derived_from<S, sender_base>;

template <typename S>
concept sender = enable_sender<std::remove_cvref_t<S>> and
                 requires(std::remove_cvref_t<S> const &s) {
                     { get_env(s) } -> queryable;
                 } and detail::movable_value<S>;

template <typename S, typename E = empty_env>
concept sender_in = sender<S> and requires(S &&s, E &&e) {
    {
        get_completion_signatures(std::forward<S>(s), std::forward<E>(e))
    } -> detail::valid_completion_signatures;
};

template <typename R, typename S>
concept receiver_from =
    receiver<R> and sender_in<S, env_of_t<R>> and
    receiver_of<R, completion_signatures_of_t<S, env_of_t<R>>>;

template <typename S, typename R>
concept sender_to = receiver_from<R, S> and requires(S &&s, R &&r) {
    { connect(std::forward<S>(s), std::forward<R>(r)) } -> operation_state;
};

namespace detail {
template <typename> struct sender_of_helper;
template <typename R, typename... Args> struct sender_of_helper<R(Args...)> {
    using tag = R;
    template <typename... As> using as_signature = R(As...);
};

template <typename...> constexpr inline bool matching_sig_v = false;
template <typename R1, typename... Args1, typename R2, typename... Args2>
constexpr inline bool matching_sig_v<R1(Args1...), R2(Args2...)> =
    std::same_as<R1(Args1 &&...), R2(Args2 &&...)>;
} // namespace detail

template <typename S, typename Sig, typename E = empty_env>
concept sender_of =
    sender_in<S, E> and
    detail::matching_sig_v<
        Sig, typename detail::gather_signatures<
                 typename detail::sender_of_helper<Sig>::tag,
                 completion_signatures_of_t<S, E>,
                 detail::sender_of_helper<Sig>::template as_signature,
                 std::type_identity_t>>;

namespace detail {
template <typename E = empty_env> struct universal_receiver : receiver_base {
    [[nodiscard]] constexpr auto query(get_env_t) const noexcept -> E {
        return {};
    }

    constexpr auto set_value(auto &&...) const && noexcept -> void {}
    constexpr auto set_error(auto &&...) const && noexcept -> void {}
    constexpr auto set_stopped() const && noexcept -> void {}
};

template <typename S, typename R>
concept value_category_sender_to =
    operation_state<decltype(connect(std::declval<S>(), std::declval<R>()))>;
} // namespace detail

template <typename S, typename R = detail::universal_receiver<>>
concept multishot_sender =
    sender<S> and detail::value_category_sender_to<S &, R>;

template <typename S, typename R = detail::universal_receiver<>>
concept singleshot_sender = sender<S> and not multishot_sender<S, R> and
                            detail::value_category_sender_to<S &&, R>;

template <typename S>
concept scheduler = queryable<S> and
                    requires(S &&s) {
                        { std::forward<S>(s).schedule() } -> sender;
                        {
                            get_completion_scheduler<set_value_t>(
                                get_env(std::forward<S>(s).schedule()))
                        } -> stdx::same_as_unqualified<S>;
                    } and std::equality_comparable<std::remove_cvref_t<S>> and
                    std::copy_constructible<std::remove_cvref_t<S>>;

template <typename S, typename R> CONSTEVAL auto check_connect() -> void {
    if constexpr (not receiver_from<R, S>) {
        static_assert(
            stdx::always_false_v<S, R,
                                 completion_signatures_of_t<S, env_of_t<R>>>,
            "Can't connect sender and receiver!");
    }
}

template <typename S, typename E = empty_env>
concept stoppable_sender = sends_stopped<S, E>;
} // namespace async
