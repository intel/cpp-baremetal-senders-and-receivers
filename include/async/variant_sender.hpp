#pragma once

#include <async/concepts.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/function_traits.hpp>
#include <stdx/functional.hpp>
#include <stdx/tuple.hpp>
#include <stdx/utility.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/list.hpp>

#include <array>
#include <concepts>
#include <cstddef>
#include <functional>
#include <limits>
#include <type_traits>
#include <utility>
#include <variant>

namespace async {
namespace detail {
template <typename P, typename F> struct match_option : P, F {
    using is_match_option = void;

    template <typename... Args>
    using return_t = std::invoke_result_t<F, Args...>;

    [[nodiscard]] constexpr auto test(auto const &...args) const -> bool {
        return std::invoke(static_cast<P const &>(*this), args...);
    }

    template <typename... Args>
    [[nodiscard]] constexpr auto invoke(Args &&...args) const
        -> decltype(auto) {
        return std::invoke(static_cast<F const &>(*this),
                           std::forward<Args>(args)...);
    }
};
template <typename P, typename F> match_option(P, F) -> match_option<P, F>;

template <typename P> struct match : P {
  private:
    template <stdx::same_as_unqualified<match> M, typename F>
    [[nodiscard]] friend constexpr auto operator>>(M &&m, F &&f)
        -> match_option<P, std::remove_cvref_t<F>> {
        return {std::forward<M>(m), std::forward<F>(f)};
    }
};
template <typename P> match(P) -> match<P>;
} // namespace detail

template <stdx::callable P> [[nodiscard]] constexpr auto match(P &&p) {
    return detail::match{std::forward<P>(p)};
}
[[nodiscard]] constexpr inline auto match(bool b) {
    return detail::match{[=](auto &&...) -> bool { return b; }};
}
template <stdx::callable P, stdx::callable F>
[[nodiscard]] constexpr auto match(P &&p, F &&f) {
    return detail::match_option{std::forward<P>(p), std::forward<F>(f)};
}
template <stdx::callable F> [[nodiscard]] constexpr auto match(bool b, F &&f) {
    return detail::match_option{[=](auto &&...) { return b; },
                                std::forward<F>(f)};
}

constexpr inline auto otherwise = match(true);

template <typename... Options> struct matcher : Options... {
    template <typename... Args>
    [[nodiscard]] constexpr auto run(Args const &...args) const {
        using Ret =
            std::variant<typename Options::template return_t<Args...>...>;
        constexpr auto selectors =
            [&]<std::size_t... Is>(std::index_sequence<Is...>) {
                using F = auto (*)(matcher const &, Args const &...)->Ret;
                return std::array<F, sizeof...(Is)>{
                    selector<Options, Is, Ret, Args...>::select...};
            }(std::make_index_sequence<sizeof...(Options)>{});
        return selectors[first_matching_index(args...)](*this, args...);
    }

  private:
    template <typename Option, std::size_t I, typename Ret, typename... Args>
    struct selector {
        constexpr static auto select(matcher const &m, Args const &...args)
            -> Ret {
            return Ret{std::in_place_index<I>, stdx::with_result_of{[&] {
                           return static_cast<Option const &>(m).invoke(
                               args...);
                       }}};
        }
    };

    [[nodiscard]] constexpr auto first_matching_index(auto const &...args) const
        -> std::size_t {
        std::size_t index{std::numeric_limits<std::size_t>::max()};
        auto const f = [&]<typename F>() {
            ++index;
            return static_cast<F const &>(*this).test(args...);
        };
        (... or f.template operator()<Options>());
        return index;
    }
};
template <typename... Options> matcher(Options...) -> matcher<Options...>;

template <stdx::callable F1, stdx::callable F2>
[[nodiscard]] constexpr auto select(bool b, F1 &&f1, F2 &&f2) {
    using R = std::variant<stdx::return_t<std::remove_cvref_t<F1>>,
                           stdx::return_t<std::remove_cvref_t<F2>>>;
    if (b) {
        return R{std::in_place_index<0>,
                 stdx::with_result_of{[&] { return std::forward<F1>(f1)(); }}};
    }
    return R{std::in_place_index<1>,
             stdx::with_result_of{[&] { return std::forward<F2>(f2)(); }}};
}

namespace detail {
template <typename T>
concept matchable_option = requires { typename T::is_match_option; };

template <typename T>
using is_match_option = std::bool_constant<matchable_option<T>>;

template <typename T>
constexpr auto count_match_option = matchable_option<T> ? 1 : 0;
} // namespace detail

template <typename... Args> constexpr auto make_variant(Args &&...args) {
    constexpr auto NumOpts =
        (0 + ... + detail::count_match_option<std::remove_cvref_t<Args>>);
    constexpr auto NumArgs = sizeof...(Args) - NumOpts;
    return [&]<std::size_t... Is, std::size_t... Js>(
               std::index_sequence<Is...>, std::index_sequence<Js...>) {
        auto t = stdx::tuple<Args...>{std::forward<Args>(args)...};
        return matcher{std::move(t)[stdx::index<Is>]...}.run(
            std::move(t)[stdx::index<NumOpts + Js>]...);
    }(std::make_index_sequence<NumOpts>{}, std::make_index_sequence<NumArgs>{});
}

namespace _variant {
template <typename... Ops> struct op_state {
    using variant_t = boost::mp11::mp_unique<std::variant<Ops...>>;
    variant_t v;

  private:
    template <stdx::same_as_unqualified<op_state> O>
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        std::visit([](auto &&ops) { start(FWD(ops)); }, std::forward<O>(o).v);
    }
};

template <typename... Sndrs> struct sender : std::variant<Sndrs...> {
    using is_sender = void;

  private:
    template <typename Env>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> boost::mp11::mp_unique<
            boost::mp11::mp_append<completion_signatures_of_t<Sndrs, Env>...>> {
        return {};
    }

    template <typename R>
    using ops_t = op_state<connect_result_t<Sndrs, std::remove_cvref_t<R>>...>;

    template <typename Sndr, typename R>
    [[nodiscard]] constexpr static auto connect_impl(Sndr &&sndr, R &&r)
        -> ops_t<R> {
        return std::visit(
            [&]<typename S>(S &&s) -> ops_t<R> {
                using V = typename ops_t<R>::variant_t;
                using O = connect_result_t<S, R>;
                constexpr auto I = boost::mp11::mp_find<V, O>::value;
                return {V{std::in_place_index<I>, stdx::with_result_of{[&] {
                              return connect(std::forward<S>(s),
                                             std::forward<R>(r));
                          }}}};
            },
            std::forward<Sndr>(sndr));
    }

    template <receiver_from<sender> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender &&self,
                                                   R &&r) {
        return connect_impl(std::move(self), std::forward<R>(r));
    }

    template <stdx::same_as_unqualified<sender> Self, receiver_from<sender> R>
        requires(... and multishot_sender<Sndrs, R>)
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r) {
        return connect_impl(std::forward<Self>(self), std::forward<R>(r));
    }
};
} // namespace _variant

namespace detail {
template <typename ArgList> struct return_t {
    template <matchable_option T>
    using fn = boost::mp11::mp_apply<T::template return_t, ArgList>;
};

template <typename T> using is_sender = std::bool_constant<sender<T>>;
} // namespace detail

template <typename... Args> constexpr auto make_variant_sender(Args &&...args) {
    using options_args = boost::mp11::mp_partition<
        boost::mp11::mp_list<std::remove_cvref_t<Args>...>,
        detail::is_match_option>;

    using senders = boost::mp11::mp_transform_q<
        detail::return_t<boost::mp11::mp_second<options_args>>,
        boost::mp11::mp_first<options_args>>;
    static_assert(
        boost::mp11::mp_all_of<senders, detail::is_sender>::value,
        "All match options passed to make_variant_sender must return senders.");

    return boost::mp11::mp_apply<_variant::sender, senders>{
        make_variant(std::forward<Args>(args)...)};
}

template <stdx::callable F1, stdx::callable F2>
constexpr auto make_variant_sender(bool b, F1 &&f1, F2 &&f2) {
    using senders = boost::mp11::mp_transform<
        stdx::return_t,
        boost::mp11::mp_list<std::remove_cvref_t<F1>, std::remove_cvref_t<F2>>>;
    static_assert(
        boost::mp11::mp_all_of<senders, detail::is_sender>::value,
        "All functions passed to make_variant_sender must return senders.");

    return boost::mp11::mp_apply<_variant::sender, senders>{
        select(b, std::forward<F1>(f1), std::forward<F2>(f2))};
}
} // namespace async
