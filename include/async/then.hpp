#pragma once

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/forwarding_query.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/tuple.hpp>
#include <stdx/tuple_algorithms.hpp>
#include <stdx/type_traits.hpp>
#include <stdx/utility.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/list.hpp>

#include <concepts>
#include <functional>
#include <type_traits>
#include <utility>

namespace async {
namespace _then {

namespace detail {
template <typename... Ts> struct args {
    using tuple_t = stdx::tuple<Ts...>;

    template <typename F, std::size_t Offset, std::size_t... Is>
    constexpr static auto try_invoke_impl(std::index_sequence<Is...>)
        -> std::invoke_result_t<
            F, decltype(std::declval<tuple_t>()[stdx::index<Offset + Is>])...>;

    template <typename F, typename Offset, typename N>
    using try_invoke = decltype(try_invoke_impl<F, Offset::value>(
        std::make_index_sequence<N::value>{}));

    template <typename F, typename Offset, typename N>
    using has_arg_count = boost::mp11::mp_valid<try_invoke, F, Offset, N>;

    template <typename F, typename Offset, typename N> struct arity;

    template <typename F, typename Offset, typename N>
    using arity_t = typename arity<F, Offset, N>::type;

    template <typename F, typename Offset, typename N> struct arity {
        using type = boost::mp11::mp_eval_if<
            has_arg_count<F, Offset, N>, N, arity_t, F, Offset,
            std::integral_constant<std::size_t, N::value + 1u>>;
    };

    template <typename T> constexpr static auto compute_arities(T const &t) {
        auto const init =
            std::pair{stdx::tuple{}, std::integral_constant<std::size_t, 0>{}};
        auto const binop = []<typename Acc, typename F>(Acc, F &&) {
            using Offset = typename Acc::second_type;
            using A = arity_t<std::remove_cvref_t<F>, Offset,
                              std::integral_constant<std::size_t, 0>>;
            return std::pair{
                boost::mp11::mp_push_back<typename Acc::first_type, A>{},
                boost::mp11::mp_plus<Offset, A>{}};
        };
        return t.fold_left(init, binop).first;
    }

    template <typename... Fs>
    using arities_t =
        decltype(compute_arities(std::declval<stdx::tuple<Fs...>>()));
};

template <typename Arities, typename... Fs>
using offsets_t = boost::mp11::mp_pop_back<boost::mp11::mp_partial_sum<
    boost::mp11::mp_push_front<Arities, boost::mp11::mp_int<0>>,
    boost::mp11::mp_int<0>, boost::mp11::mp_plus>>;

struct void_t {};
template <typename T>
using nonvoid_result_t = std::bool_constant<not std::is_same_v<void_t, T>>;

template <std::size_t Offset>
constexpr auto invoke =
    []<typename F, typename T, std::size_t... Is>(
        F &&f, T &&t, std::index_sequence<Is...>) -> decltype(auto) {
    if constexpr (std::is_void_v<std::invoke_result_t<
                      F, decltype(std::forward<T>(
                             t)[stdx::index<Offset + Is>])...>>) {
        std::invoke(std::forward<F>(f),
                    std::forward<T>(t)[stdx::index<Offset + Is>]...);
        return void_t{};
    } else {
        return std::invoke(std::forward<F>(f),
                           std::forward<T>(t)[stdx::index<Offset + Is>]...);
    }
};
} // namespace detail

template <typename Tag, typename R, typename... Fs> struct receiver {
    using is_receiver = void;
    [[no_unique_address]] R r;
    [[no_unique_address]] stdx::tuple<Fs...> fs;

  private:
    template <stdx::same_as_unqualified<receiver> Self, typename... Args>
    friend auto tag_invoke(Tag, Self &&self, Args &&...args) -> void {
        using arities =
            typename detail::args<Args &&...>::template arities_t<Fs...>;
        using offsets = typename detail::offsets_t<arities, Fs...>;

        auto arg_tuple = stdx::tuple<Args &&...>{std::forward<Args>(args)...};
        auto const invoke = [&]<typename F, typename Offset, typename Arity>(
                                F &&func, Offset, Arity) -> decltype(auto) {
            return detail::invoke<Offset::value>(
                std::forward<F>(func), std::move(arg_tuple),
                std::make_index_sequence<Arity::value>{});
        };
        auto results = stdx::transform(invoke, std::forward<Self>(self).fs,
                                       offsets{}, arities{});
        auto filtered_results =
            stdx::filter<detail::nonvoid_result_t>(std::move(results));

        std::move(filtered_results).apply([&]<typename... Ts>(Ts &&...ts) {
            Tag{}(std::forward<Self>(self).r, std::forward<Ts>(ts)...);
        });
    }

    template <typename T, stdx::same_as_unqualified<receiver> Self,
              typename... Args>
    friend auto tag_invoke(T, Self &&self, Args &&...args)
        -> decltype(T{}(std::forward<Self>(self).r,
                        std::forward<Args>(args)...)) {
        return T{}(std::forward<Self>(self).r, std::forward<Args>(args)...);
    }
};

namespace detail {
template <typename Tag> struct as_signature {
    template <typename... As> using fn = Tag(As...);
};

template <typename Tag, typename... Fs> struct to_signature {
    template <typename... Ts> struct invoke {
        template <typename F, std::size_t Offset, std::size_t... Is>
        constexpr static auto invoke_result(std::index_sequence<Is...>)
            -> std::invoke_result_t<
                F, decltype(std::declval<stdx::tuple<Ts...>>()
                                [stdx::index<Offset + Is>])...>;

        template <typename F, typename Offset, typename Arity>
        using fn = decltype(invoke_result<F, Offset::value>(
            std::make_index_sequence<Arity::value>{}));
    };

    // clang has a bug here, we cannot just use an alias
    template <typename... Ts> struct no_voids_t {
        using arities = typename detail::args<Ts...>::template arities_t<Fs...>;
        using offsets = typename detail::offsets_t<arities, Fs...>;

        using type = boost::mp11::mp_apply_q<
            as_signature<Tag>,
            boost::mp11::mp_remove_if<
                boost::mp11::mp_transform_q<invoke<Ts...>,
                                            boost::mp11::mp_list<Fs...>,
                                            offsets, arities>,
                std::is_void>>;
    };

    template <typename... Ts>
    using type = completion_signatures<typename no_voids_t<Ts...>::type>;
};
} // namespace detail

template <typename Tag, typename S, typename... Fs> class sender {
    template <async::receiver R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender &&self,
                                                   R &&r) {
        check_connect<sender &&, R>();
        return connect(std::move(self).s,
                       receiver<Tag, std::remove_cvref_t<R>, Fs...>{
                           std::forward<R>(r), std::move(self).fs});
    }

    template <stdx::same_as_unqualified<sender> Self, async::receiver R>
        requires multishot_sender<S> and (... and std::copy_constructible<Fs>)
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r) {
        check_connect<Self, R>();
        return connect(std::forward<Self>(self).s,
                       receiver<Tag, std::remove_cvref_t<R>, Fs...>{
                           std::forward<R>(r), std::forward<Self>(self).fs});
    }

    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   sender const &sndr) {
        return forward_env_of(sndr.s);
    }

    template <typename... Ts>
    using signatures =
        typename detail::to_signature<Tag, Fs...>::template type<Ts...>;

    template <typename Env>
        requires std::same_as<Tag, set_value_t>
    [[nodiscard]] friend constexpr auto
    tag_invoke(get_completion_signatures_t, sender const &, Env const &) {
        return transform_completion_signatures_of<
            S, Env, completion_signatures<>, signatures>{};
    }

    template <typename Env>
        requires std::same_as<Tag, set_error_t>
    [[nodiscard]] friend constexpr auto
    tag_invoke(get_completion_signatures_t, sender const &, Env const &) {
        return transform_completion_signatures_of<
            S, Env, completion_signatures<>, ::async::detail::default_set_value,
            signatures>{};
    }

    template <typename Env>
        requires std::same_as<Tag, set_stopped_t>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> transform_completion_signatures_of<S, Env> {
        return {};
    }

  public:
    using is_sender = void;

    [[no_unique_address]] S s;
    [[no_unique_address]] stdx::tuple<Fs...> fs;
};

template <typename Tag, typename... Fs> struct pipeable {
    stdx::tuple<Fs...> fs;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Tag, std::remove_cvref_t<S>, Fs...>{
            std::forward<S>(s), std::forward<Self>(self).fs};
    }
};
} // namespace _then

template <stdx::callable... Fs>
[[nodiscard]] constexpr auto then(Fs &&...fs)
    -> _then::pipeable<set_value_t, std::remove_cvref_t<Fs>...> {
    return {std::forward<Fs>(fs)...};
}

template <sender S, stdx::callable... Fs>
[[nodiscard]] constexpr auto then(S &&s, Fs &&...fs) -> sender auto {
    return std::forward<S>(s) | then(std::forward<Fs>(fs)...);
}

template <stdx::callable F>
[[nodiscard]] constexpr auto upon_error(F &&f)
    -> _then::pipeable<set_error_t, std::remove_cvref_t<F>> {
    return {std::forward<F>(f)};
}

template <sender S, stdx::callable F>
[[nodiscard]] constexpr auto upon_error(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | upon_error(std::forward<F>(f));
}

template <stdx::callable F>
[[nodiscard]] constexpr auto upon_stopped(F &&f)
    -> _then::pipeable<set_stopped_t, std::remove_cvref_t<F>> {
    return {std::forward<F>(f)};
}

template <sender S, stdx::callable F>
[[nodiscard]] constexpr auto upon_stopped(S &&s, F &&f) -> sender auto {
    return std::forward<S>(s) | upon_stopped(std::forward<F>(f));
}
} // namespace async
