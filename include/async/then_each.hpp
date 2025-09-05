#pragma once

#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/forwarding_query.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/tuple.hpp>
#include <stdx/tuple_algorithms.hpp>
#include <stdx/type_traits.hpp>
#include <stdx/utility.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/list.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

template <typename...> struct undef;

namespace async {
namespace _then_each {

namespace detail {
struct void_t {};
template <typename T>
using nonvoid_result_t = std::bool_constant<not std::is_same_v<void_t, T>>;
} // namespace detail

template <stdx::ct_string Name, typename S, typename R, typename... Fs>
struct receiver {
    using is_receiver = void;
    [[no_unique_address]] R r;
    [[no_unique_address]] stdx::tuple<Fs...> fs;

    [[nodiscard]] constexpr auto query(get_env_t) const
        -> forwarding_env<env_of_t<R>> {
        return forward_env_of(r);
    }

    template <typename... Args>
    constexpr auto set_value(Args &&...args) && -> void {
        std::move(fs).apply([&](auto &&...funcs) {
            auto const invoke = [&]<typename F>(F &&f) -> decltype(auto) {
                if constexpr (std::is_void_v<
                                  std::invoke_result_t<F, Args...>>) {
                    std::forward<F>(f)(args...);
                    return detail::void_t{};
                } else {
                    return std::forward<F>(f)(args...);
                }
            };
            auto results = stdx::tuple<decltype(invoke(FWD(funcs)))...>{
                invoke(FWD(funcs))...};
            auto filtered_results =
                stdx::filter<detail::nonvoid_result_t>(std::move(results));

            debug_signal<set_value_t::name,
                         debug::erased_context_for<receiver>>(get_env(r));
            std::move(filtered_results).apply([&]<typename... Ts>(Ts &&...ts) {
                set_value_t{}(std::move(r), std::forward<Ts>(ts)...);
            });
        });
    }
    template <typename... Args>
    constexpr auto set_error(Args &&...args) && -> void {
        debug_signal<set_error_t::name, debug::erased_context_for<receiver>>(
            get_env(r));
        set_error_t{}(std::move(r), std::forward<Args>(args)...);
    }
    constexpr auto set_stopped() && -> void {
        debug_signal<set_stopped_t::name, debug::erased_context_for<receiver>>(
            get_env(r));
        set_stopped_t{}(std::move(r));
    }
    using sender_t = S;
};

namespace detail {
template <typename... As> using as_signature = set_value_t(As...);
}

template <stdx::ct_string Name, typename S, typename... Fs> struct sender {
    template <async::receiver R>
    [[nodiscard]] constexpr auto connect(R &&r) && {
        check_connect<sender &&, R>();
        return async::connect(std::move(s),
                              receiver<Name, S, std::remove_cvref_t<R>, Fs...>{
                                  std::forward<R>(r), std::move(fs)});
    }

    template <async::receiver R>
        requires multishot_sender<
                     S, async::detail::universal_receiver<env_of_t<R>>> and
                 (... and std::copy_constructible<Fs>)
    [[nodiscard]] constexpr auto connect(R &&r) const & {
        check_connect<sender const &, R>();
        return async::connect(s,
                              receiver<Name, S, std::remove_cvref_t<R>, Fs...>{
                                  std::forward<R>(r), fs});
    }

    template <typename... Ts>
    using signature_over_fs = boost::mp11::mp_apply<
        detail::as_signature,
        boost::mp11::mp_remove_if<
            completion_signatures<std::invoke_result_t<Fs, Ts...>...>,
            std::is_void>>;

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &) {
        return transform_completion_signatures_of<
            S, Env, completion_signatures<>, signature_over_fs>{};
    }

    using is_sender = void;

    [[no_unique_address]] S s;
    [[no_unique_address]] stdx::tuple<Fs...> fs;

    [[nodiscard]] constexpr auto query(get_env_t) const {
        return forward_env_of(s);
    }
};

template <stdx::ct_string Name, typename... Fs> struct pipeable {
    stdx::tuple<Fs...> fs;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Name, std::remove_cvref_t<S>, Fs...>{
            std::forward<S>(s), std::forward<Self>(self).fs};
    }
};
} // namespace _then_each

template <stdx::ct_string Name = "then_each", stdx::callable... Fs>
[[nodiscard]] constexpr auto then_each(Fs &&...fs) {
    return compose(_then_each::pipeable<Name, std::remove_cvref_t<Fs>...>{
        std::forward<Fs>(fs)...});
}

template <stdx::ct_string Name = "then_each", sender S, stdx::callable... Fs>
[[nodiscard]] constexpr auto then_each(S &&s, Fs &&...fs) -> sender auto {
    return std::forward<S>(s) | then_each<Name>(std::forward<Fs>(fs)...);
}

struct then_each_t;

template <stdx::ct_string Name, typename... Ts>
struct debug::context_for<_then_each::receiver<Name, Ts...>> {
    using tag = then_each_t;
    constexpr static auto name = Name;
    using type = _then_each::receiver<Name, Ts...>;
    using children = stdx::type_list<debug::erased_context_for<
        connect_result_t<typename type::sender_t &&, type &&>>>;
};
} // namespace async
