#pragma once

#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/type_traits.hpp>

#include <stdx/ct_string.hpp>
#include <stdx/tuple.hpp>
#include <stdx/type_traits.hpp>

#include <type_traits>
#include <utility>
#include <variant>

namespace async {
namespace _into_variant {
template <stdx::ct_string Name, typename S, typename V, typename Rcvr>
struct receiver {
    using is_receiver = void;
    using sender_t = S;

    [[nodiscard]] constexpr auto query(async::get_env_t) const
        -> forwarding_env<env_of_t<Rcvr>> {
        return forward_env_of(r);
    }

    template <typename... Args> auto set_value(Args &&...args) && -> void {
        debug_signal<set_value_t::name, debug::erased_context_for<receiver>>(
            get_env(r));
        std::move(r).set_value(
            V{stdx::make_tuple(std::forward<Args>(args)...)});
    }

    template <typename... Args> auto set_error(Args &&...args) && -> void {
        debug_signal<set_error_t::name, debug::erased_context_for<receiver>>(
            get_env(r));
        std::move(r).set_error(std::forward<Args>(args)...);
    }
    auto set_stopped() && -> void {
        debug_signal<set_stopped_t::name, debug::erased_context_for<receiver>>(
            get_env(r));
        std::move(r).set_stopped();
    }

    [[no_unique_address]] Rcvr r;
};

namespace detail {
template <typename... As> using discard_signatures = completion_signatures<>;

template <typename... Ts>
using decayed_tuple = stdx::tuple<std::decay_t<Ts>...>;

template <typename S, typename Env, template <typename...> typename V>
using variant_t =
    boost::mp11::mp_unique<value_types_of_t<S, Env, decayed_tuple, V>>;
} // namespace detail

template <stdx::ct_string Name, typename S, template <typename...> typename V>
struct sender {
    using is_sender = void;

    template <async::receiver R>
    [[nodiscard]] constexpr auto connect(R &&r) && {
        check_connect<sender &&, R>();
        using env_t = env_of_t<std::remove_cvref_t<R>>;
        using variant_t = detail::variant_t<S, env_t, V>;
        return async::connect(
            std::move(s), receiver<Name, S, variant_t, std::remove_cvref_t<R>>{
                              std::forward<R>(r)});
    }

    template <async::receiver R>
        requires multishot_sender<
            S, async::detail::universal_receiver<env_of_t<R>>>
    [[nodiscard]] constexpr auto connect(R &&r) const & {
        check_connect<sender const &, R>();
        using env_t = env_of_t<std::remove_cvref_t<R>>;
        using variant_t = detail::variant_t<S, env_t, V>;
        return async::connect(
            s, receiver<Name, S, variant_t, std::remove_cvref_t<R>>{
                   std::forward<R>(r)});
    }

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &) {
        using variant_t = detail::variant_t<S, Env, V>;
        return transform_completion_signatures_of<
            S, Env, completion_signatures<set_value_t(variant_t)>,
            detail::discard_signatures>{};
    }

    [[no_unique_address]] S s;
};

template <stdx::ct_string Name, template <typename...> typename V>
struct pipeable {
  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&) -> async::sender auto {
        return sender<Name, std::remove_cvref_t<S>, V>{std::forward<S>(s)};
    }
};
} // namespace _into_variant

template <stdx::ct_string Name = "into_variant",
          template <typename...> typename V = std::variant>
[[nodiscard]] constexpr auto into_variant() {
    return compose(_into_variant::pipeable<Name, V>{});
}

template <stdx::ct_string Name = "into_variant",
          template <typename...> typename V = std::variant, sender S>
[[nodiscard]] constexpr auto into_variant(S &&s) -> sender auto {
    return std::forward<S>(s) | into_variant<Name, V>();
}

struct into_variant_t;

template <stdx::ct_string Name, typename... Ts>
struct debug::context_for<_into_variant::receiver<Name, Ts...>> {
    using tag = into_variant_t;
    constexpr static auto name = Name;
    using type = _into_variant::receiver<Name, Ts...>;
    using children = stdx::type_list<debug::erased_context_for<
        connect_result_t<typename type::sender_t &&, type &&>>>;
};
} // namespace async
