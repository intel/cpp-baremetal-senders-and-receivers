#pragma once

#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/debug.hpp>
#include <async/debug_context.hpp>
#include <async/env.hpp>

#include <stdx/call_by_need.hpp>
#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/tuple.hpp>
#include <stdx/type_traits.hpp>

#include <boost/mp11/algorithm.hpp>

#include <type_traits>
#include <utility>

namespace async {
namespace _observe {
template <channel_tag T> struct caller_t {
    template <typename F, typename... Args>
    constexpr static auto invoke(F &&f, Args &&...args)
        -> decltype(std::forward<F>(f)(std::forward<Args>(args)...)) {
        return std::forward<F>(f)(std::forward<Args>(args)...);
    }

    template <typename F, typename... Args>
        requires true
    constexpr static auto invoke(F &&f, Args &&...args)
        -> decltype(std::forward<F>(f).template operator()<T>(
            std::forward<Args>(args)...)) {
        return std::forward<F>(f).template operator()<T>(
            std::forward<Args>(args)...);
    }
};

template <stdx::ct_string Name, typename HandleTags, typename S, typename R,
          typename... Fs>
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
        handle<set_value_t>(std::forward<Args>(args)...);
    }
    template <typename... Args>
    constexpr auto set_error(Args &&...args) && -> void {
        handle<set_error_t>(std::forward<Args>(args)...);
    }
    constexpr auto set_stopped() && -> void { handle<set_stopped_t>(); }

    using sender_t = S;

  private:
    template <channel_tag T, typename... Args>
    auto handle(Args &&...args) -> void {
        if constexpr (boost::mp11::mp_contains<HandleTags, T>::value) {
            stdx::call_by_need<caller_t<T>>(
                std::move(fs), stdx::tuple<Args const &...>{args...});
        }
        debug_signal<T::name, debug::erased_context_for<receiver>>(get_env(r));
        T{}(std::move(r), std::forward<Args>(args)...);
    }
};

template <stdx::ct_string Name, typename HandleTags, typename S, typename... Fs>
struct sender {
    template <async::receiver R>
    [[nodiscard]] constexpr auto connect(R &&r) && {
        check_connect<sender &&, R>();
        return async::connect(
            std::move(s),
            receiver<Name, HandleTags, S, std::remove_cvref_t<R>, Fs...>{
                std::forward<R>(r), std::move(fs)});
    }

    template <async::receiver R>
        requires multishot_sender<
                     S, async::detail::universal_receiver<env_of_t<R>>> and
                 (... and std::copy_constructible<Fs>)
    [[nodiscard]] constexpr auto connect(R &&r) const & {
        check_connect<sender const &, R>();
        return async::connect(
            s, receiver<Name, HandleTags, S, std::remove_cvref_t<R>, Fs...>{
                   std::forward<R>(r), fs});
    }

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> completion_signatures_of_t<S, Env> {
        return {};
    }

    using is_sender = void;

    [[no_unique_address]] S s;
    [[no_unique_address]] stdx::tuple<Fs...> fs;

    [[nodiscard]] constexpr auto query(get_env_t) const {
        return forward_env_of(s);
    }
};

template <stdx::ct_string Name, typename HandleTags, typename... Fs>
struct pipeable {
    [[no_unique_address]] stdx::tuple<Fs...> fs;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Name, HandleTags, std::remove_cvref_t<S>, Fs...>{
            std::forward<S>(s), std::forward<Self>(self).fs};
    }
};
} // namespace _observe

template <
    auto Channels = stdx::type_list<set_value_t, set_error_t, set_stopped_t>{},
    stdx::ct_string Name = "observe", stdx::callable... Fs>
[[nodiscard]] constexpr auto observe(Fs &&...fs) {
    return compose(
        _observe::pipeable<Name, std::remove_cvref_t<decltype(Channels)>,
                           std::remove_cvref_t<Fs>...>{
            std::forward<Fs>(fs)...});
}

template <
    auto Channels = stdx::type_list<set_value_t, set_error_t, set_stopped_t>{},
    stdx::ct_string Name = "observe", sender S, stdx::callable... Fs>
[[nodiscard]] constexpr auto observe(S &&s, Fs &&...fs) -> sender auto {
    return std::forward<S>(s) |
           observe<Channels, Name>(std::forward<Fs>(fs)...);
}

struct observe_t;

template <stdx::ct_string Name, typename HandleTags, typename... Ts>
struct debug::context_for<_observe::receiver<Name, HandleTags, Ts...>> {
    using tag = observe_t;
    constexpr static auto name = Name;
    using type = _observe::receiver<Name, HandleTags, Ts...>;
    using children = stdx::type_list<debug::erased_context_for<
        connect_result_t<typename type::sender_t &&, type &&>>>;
};
} // namespace async
