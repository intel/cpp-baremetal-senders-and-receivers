#pragma once

#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/forwarding_query.hpp>
#include <async/type_traits.hpp>

#include <stdx/call_by_need.hpp>
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

namespace async {
namespace _then {
template <stdx::ct_string Name, typename HandleTag, typename CompleteTag,
          typename S, typename R, typename... Fs>
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
    template <typename T, typename... Args>
    auto handle(Args &&...args) -> void {
        if constexpr (std::same_as<HandleTag, T>) {
            auto results = stdx::call_by_need(
                std::move(fs),
                stdx::tuple<Args &&...>{std::forward<Args>(args)...});
            debug_signal<CompleteTag::name,
                         debug::erased_context_for<receiver>>(get_env(r));
            std::move(results).apply([&]<typename... Ts>(Ts &&...ts) {
                CompleteTag{}(std::move(r), std::forward<Ts>(ts)...);
            });
        } else {
            debug_signal<T::name, debug::erased_context_for<receiver>>(
                get_env(r));
            T{}(std::move(r), std::forward<Args>(args)...);
        }
    }
};

namespace detail {
template <typename Tag> struct as_signature {
    template <typename... As> using fn = Tag(As...);
};
} // namespace detail

template <stdx::ct_string Name, typename HandleTag, typename CompleteTag,
          typename S, typename... Fs>
struct sender {
    template <async::receiver R>
    [[nodiscard]] constexpr auto connect(R &&r) && {
        check_connect<sender &&, R>();
        return async::connect(
            std::move(s),
            receiver<Name, HandleTag, CompleteTag, S, std::remove_cvref_t<R>,
                     Fs...>{std::forward<R>(r), std::move(fs)});
    }

    template <async::receiver R>
        requires multishot_sender<
                     S, async::detail::universal_receiver<env_of_t<R>>> and
                 (... and std::copy_constructible<Fs>)
    [[nodiscard]] constexpr auto connect(R &&r) const & {
        check_connect<sender const &, R>();
        return async::connect(
            s, receiver<Name, HandleTag, CompleteTag, S, std::remove_cvref_t<R>,
                        Fs...>{std::forward<R>(r), fs});
    }

    template <typename... Ts>
    using signatures =
        boost::mp11::mp_apply_q<detail::as_signature<CompleteTag>,
                                decltype(stdx::call_by_need(
                                    std::declval<stdx::tuple<Fs...>>(),
                                    std::declval<stdx::tuple<Ts...>>()))>;

    template <typename Env>
        requires std::same_as<HandleTag, set_value_t>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &) {
        return transform_completion_signatures_of<
            S, Env, completion_signatures<>, signatures>{};
    }

    template <typename Env>
        requires std::same_as<HandleTag, set_error_t>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &) {
        return transform_completion_signatures_of<
            S, Env, completion_signatures<>, ::async::detail::default_set_value,
            signatures>{};
    }

    template <typename Env>
        requires std::same_as<HandleTag, set_stopped_t>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &) {
        if constexpr (not sends_stopped<S, Env>) {
            return completion_signatures_of_t<S, Env>{};
        } else {
            return transform_completion_signatures_of<
                S, Env, completion_signatures<signatures<>>,
                ::async::detail::default_set_value,
                ::async::detail::default_set_error, completion_signatures<>>{};
        }
    }

    using is_sender = void;

    [[no_unique_address]] S s;
    [[no_unique_address]] stdx::tuple<Fs...> fs;

    [[nodiscard]] constexpr auto query(get_env_t) const {
        return forward_env_of(s);
    }
};

template <stdx::ct_string Name, typename HandleTag, typename CompleteTag,
          typename... Fs>
struct pipeable {
    stdx::tuple<Fs...> fs;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Name, HandleTag, CompleteTag, std::remove_cvref_t<S>,
                      Fs...>{std::forward<S>(s), std::forward<Self>(self).fs};
    }
};
} // namespace _then

template <stdx::ct_string Name = "then", stdx::callable... Fs>
[[nodiscard]] constexpr auto then(Fs &&...fs) {
    return compose(
        _then::pipeable<Name, set_value_t, set_value_t,
                        std::remove_cvref_t<Fs>...>{std::forward<Fs>(fs)...});
}

template <stdx::ct_string Name = "then", sender S, stdx::callable... Fs>
[[nodiscard]] constexpr auto then(S &&s, Fs &&...fs) -> sender auto {
    return std::forward<S>(s) | then<Name>(std::forward<Fs>(fs)...);
}

template <stdx::ct_string Name = "upon_error", stdx::callable F>
[[nodiscard]] constexpr auto upon_error(F &&f) {
    return compose(
        _then::pipeable<Name, set_error_t, set_value_t, std::remove_cvref_t<F>>{
            std::forward<F>(f)});
}

template <stdx::ct_string Name = "upon_error", sender S, stdx::callable F>
[[nodiscard]] constexpr auto upon_error(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | upon_error<Name>(std::forward<F>(f));
}

template <stdx::ct_string Name = "upon_stopped", stdx::callable F>
[[nodiscard]] constexpr auto upon_stopped(F &&f) {
    return compose(_then::pipeable<Name, set_stopped_t, set_value_t,
                                   std::remove_cvref_t<F>>{std::forward<F>(f)});
}

template <stdx::ct_string Name = "upon_stopped", sender S, stdx::callable F>
[[nodiscard]] constexpr auto upon_stopped(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | upon_stopped<Name>(std::forward<F>(f));
}

template <stdx::ct_string Name = "then_error", stdx::callable... Fs>
[[nodiscard]] constexpr auto then_error(Fs &&...fs) {
    return compose(
        _then::pipeable<Name, set_value_t, set_error_t,
                        std::remove_cvref_t<Fs>...>{std::forward<Fs>(fs)...});
}

template <stdx::ct_string Name = "then_error", sender S, stdx::callable... Fs>
[[nodiscard]] constexpr auto then_error(S &&s, Fs &&...fs) -> sender auto {
    return std::forward<S>(s) | then_error<Name>(std::forward<Fs>(fs)...);
}

template <stdx::ct_string Name = "transform_error", stdx::callable... Fs>
[[nodiscard]] constexpr auto transform_error(Fs &&...fs) {
    return compose(
        _then::pipeable<Name, set_error_t, set_error_t,
                        std::remove_cvref_t<Fs>...>{std::forward<Fs>(fs)...});
}

template <stdx::ct_string Name = "transform_error", sender S,
          stdx::callable... Fs>
[[nodiscard]] constexpr auto transform_error(S &&s, Fs &&...fs) -> sender auto {
    return std::forward<S>(s) | transform_error<Name>(std::forward<Fs>(fs)...);
}

struct then_t;
struct upon_error_t;
struct upon_stopped_t;

struct then_error_t;
struct transform_error_t;

namespace _then {
namespace detail {
template <typename HandleTag, typename CompleteTag> struct debug_tag;

template <> struct debug_tag<set_value_t, set_value_t> {
    using type = then_t;
};
template <> struct debug_tag<set_error_t, set_value_t> {
    using type = upon_error_t;
};
template <> struct debug_tag<set_stopped_t, set_value_t> {
    using type = upon_stopped_t;
};

template <> struct debug_tag<set_value_t, set_error_t> {
    using type = then_error_t;
};
template <> struct debug_tag<set_error_t, set_error_t> {
    using type = transform_error_t;
};

template <typename HandleTag, typename CompleteTag>
using debug_tag_t = typename debug_tag<HandleTag, CompleteTag>::type;
} // namespace detail
} // namespace _then

template <stdx::ct_string Name, typename HandleTag, typename CompleteTag,
          typename... Ts>
struct debug::context_for<
    _then::receiver<Name, HandleTag, CompleteTag, Ts...>> {
    using tag = _then::detail::debug_tag_t<HandleTag, CompleteTag>;
    constexpr static auto name = Name;
    using type = _then::receiver<Name, HandleTag, CompleteTag, Ts...>;
    using children = stdx::type_list<debug::erased_context_for<
        connect_result_t<typename type::sender_t &&, type &&>>>;
};
} // namespace async
