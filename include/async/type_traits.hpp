#pragma once

#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/get_completion_signatures.hpp>

#include <stdx/function_traits.hpp>
#include <stdx/tuple.hpp>
#include <stdx/type_traits.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/list.hpp>

#include <type_traits>
#include <utility>

namespace async {
struct set_value_t;
struct set_error_t;
struct set_stopped_t;

template <typename...> struct completion_signatures {};

namespace detail {
template <typename... Tags> struct with_any_tag {
    template <typename Sig>
    using fn =
        std::bool_constant<(... or std::is_same_v<Tags, stdx::return_t<Sig>>)>;
};

template <typename Sigs, typename Tag>
using signatures_by_tag = boost::mp11::mp_copy_if_q<Sigs, with_any_tag<Tag>>;

template <bool> struct indirect_meta_apply {
    template <template <typename...> typename T, typename... As>
    using meta_apply = T<As...>;
};

template <typename...> constexpr auto always_true = true;

template <template <typename...> typename T, typename... As>
using meta_apply = typename indirect_meta_apply<
    always_true<As...>>::template meta_apply<T, As...>;

template <template <typename...> typename T> struct tuplify {
    template <typename... As> using fn_q = meta_apply<T, As...>;
    template <typename Sig> using fn = stdx::args_t<Sig, fn_q>;
};

template <typename> struct meta_transform_t;
template <typename... Sigs>
struct meta_transform_t<completion_signatures<Sigs...>> {
    template <template <typename...> typename F>
    using fn = meta_apply<F, Sigs...>;
};
template <typename List, template <typename...> typename F>
using meta_transform = typename meta_transform_t<List>::template fn<F>;

template <template <typename...> typename V, template <typename...> typename T>
struct variantify {
    template <typename... Sigs>
    using fn_q = meta_apply<V, typename tuplify<T>::template fn<Sigs>...>;
    template <typename List> using fn = meta_transform<List, fn_q>;
};

template <typename Tag, typename Sigs, template <typename...> typename Tuple,
          template <typename...> typename Variant>
using gather_signatures =
    typename variantify<Variant,
                        Tuple>::template fn<signatures_by_tag<Sigs, Tag>>;

template <typename...> struct type_list;
} // namespace detail

template <typename S, typename E = empty_env>
using value_signatures_of_t =
    detail::signatures_by_tag<completion_signatures_of_t<S, E>, set_value_t>;
template <typename S, typename E = empty_env>
using error_signatures_of_t =
    detail::signatures_by_tag<completion_signatures_of_t<S, E>, set_error_t>;
template <typename S, typename E = empty_env>
using stopped_signatures_of_t =
    detail::signatures_by_tag<completion_signatures_of_t<S, E>, set_stopped_t>;

template <typename S, typename E = empty_env,
          template <typename...> typename Tuple = detail::type_list,
          template <typename...> typename Variant = detail::type_list>
using value_types_of_t =
    detail::gather_signatures<set_value_t, completion_signatures_of_t<S, E>,
                              Tuple, Variant>;

template <typename S, typename E = empty_env,
          template <typename...> typename Tuple = detail::type_list,
          template <typename...> typename Variant = detail::type_list>
using error_types_of_t =
    detail::gather_signatures<set_error_t, completion_signatures_of_t<S, E>,
                              Tuple, Variant>;

template <typename S, typename E = empty_env,
          template <typename...> typename Tuple = detail::type_list,
          template <typename...> typename Variant = detail::type_list>
using stopped_types_of_t =
    detail::gather_signatures<set_stopped_t, completion_signatures_of_t<S, E>,
                              Tuple, Variant>;

template <typename S, typename E = empty_env>
constexpr auto sends_stopped =
    not std::is_same_v<detail::type_list<>, stopped_types_of_t<S, E>>;

namespace detail {
template <typename... As>
using default_set_value = completion_signatures<set_value_t(As...)>;
template <typename... Es>
using default_set_error = completion_signatures<set_error_t(Es...)>;

template <typename S, typename E>
using default_set_stopped =
    stdx::conditional_t<sends_stopped<S, E>,
                        completion_signatures<set_stopped_t()>,
                        completion_signatures<>>;

template <typename InputSigs, typename SetStopped> struct stopped_list {
    using sigs_t =
        gather_signatures<set_stopped_t, InputSigs, type_list, type_list>;
    using type = stdx::conditional_t<std::is_same_v<sigs_t, type_list<>>,
                                     completion_signatures<>, SetStopped>;
};
} // namespace detail

template <typename InputSigs, typename AddlSigs = completion_signatures<>,
          template <typename...> typename SetValue = detail::default_set_value,
          template <typename...> typename SetError = detail::default_set_error,
          typename SetStopped = completion_signatures<set_stopped_t()>>
using transform_completion_signatures =
    boost::mp11::mp_unique<boost::mp11::mp_append<
        AddlSigs,
        boost::mp11::mp_flatten<detail::gather_signatures<
            set_value_t, InputSigs, SetValue, completion_signatures>>,
        boost::mp11::mp_flatten<detail::gather_signatures<
            set_error_t, InputSigs, SetError, completion_signatures>>,
        typename detail::stopped_list<InputSigs, SetStopped>::type>>;

template <typename S, typename E = empty_env,
          typename AddlSigs = completion_signatures<>,
          template <typename...> typename SetValue = detail::default_set_value,
          template <typename...> typename SetError = detail::default_set_error,
          typename SetStopped = completion_signatures<set_stopped_t()>>
using transform_completion_signatures_of =
    transform_completion_signatures<completion_signatures_of_t<S, E>, AddlSigs,
                                    SetValue, SetError, SetStopped>;

namespace detail {
template <typename T> struct eat_void {
    template <template <typename...> typename C> using type = C<T>;
};
template <> struct eat_void<void> {
    template <template <typename...> typename C> using type = C<>;
};
template <typename T, template <typename...> typename C>
using eat_void_t = typename eat_void<T>::template type<C>;
} // namespace detail

template <typename Tag> struct channel_holder {
    template <typename... Ts> using base_tuple = stdx::tuple<Ts...>;

    template <typename... Ts> struct tuple : base_tuple<Ts...> {
        using tag_t = Tag;

        template <typename... Args>
        constexpr explicit(true) tuple(Args &&...args)
            : base_tuple<Ts...>{std::forward<Args>(args)...} {}

        template <typename R> auto operator()(R &&r) const & -> void {
            this->apply([&]<typename... Args>(Args &&...args) {
                Tag{}(std::forward<R>(r), std::forward<Args>(args)...);
            });
        }
        template <typename R> auto operator()(R &&r) & -> void {
            this->apply([&]<typename... Args>(Args &&...args) {
                Tag{}(std::forward<R>(r), std::forward<Args>(args)...);
            });
        }
        template <typename R> auto operator()(R &&r) && -> void {
            std::move(*this).apply([&]<typename... Args>(Args &&...args) {
                Tag{}(std::forward<R>(r), std::forward<Args>(args)...);
            });
        }
    };
};

template <typename... Ts>
using value_holder = channel_holder<set_value_t>::tuple<Ts...>;
template <typename... Ts>
using error_holder = channel_holder<set_error_t>::tuple<Ts...>;
template <typename... Ts>
using stopped_holder = channel_holder<set_stopped_t>::tuple<Ts...>;
} // namespace async
