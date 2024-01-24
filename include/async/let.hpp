#pragma once

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/forwarding_query.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/functional.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/list.hpp>

#include <type_traits>
#include <utility>
#include <variant>

namespace async::_let {
template <typename Ops, typename Rcvr> struct second_receiver {
    using is_receiver = void;
    Ops *ops;

  private:
    template <channel_tag Tag, typename... Args>
    friend auto tag_invoke(Tag, second_receiver const &self, Args &&...args)
        -> void {
        Tag{}(self.ops->rcvr, std::forward<Args>(args)...);
    }

    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   second_receiver const &r)
        -> detail::forwarding_env<env_of_t<Rcvr>> {
        return forward_env_of(r.ops->rcvr);
    }
};

template <typename Sndr, typename Rcvr, typename Func,
          typename DependentSenders, template <typename...> typename FirstRcvr>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using first_rcvr = FirstRcvr<Func, op_state, Rcvr>;
    using second_rcvr = second_receiver<op_state, Rcvr>;

    template <typename S, typename R, typename F>
    constexpr op_state(S &&s, R &&r, F &&f)
        : rcvr{std::forward<R>(r)},
          state{std::in_place_index<0>, stdx::with_result_of{[&] {
                    return connect(std::forward<S>(s),
                                   first_rcvr{{this}, std::forward<F>(f)});
                }}} {}
    constexpr op_state(op_state &&) = delete;

    template <typename S> auto complete_first(S &&s) -> void {
        using index =
            boost::mp11::mp_find<DependentSenders, std::remove_cvref_t<S>>;
        static_assert(index::value <
                      boost::mp11::mp_size<DependentSenders>::value);
        auto &op =
            state.template emplace<index::value + 1>(stdx::with_result_of{[&] {
                return connect(std::forward<S>(s), second_rcvr{this});
            }});
        start(std::move(op));
    }

    template <typename S>
    using dependent_connect_result_t = connect_result_t<S, second_rcvr>;
    using dependent_ops = boost::mp11::mp_apply<
        std::variant, boost::mp11::mp_transform<dependent_connect_result_t,
                                                DependentSenders>>;

    using first_ops = connect_result_t<Sndr, first_rcvr>;
    using state_t = boost::mp11::mp_push_front<dependent_ops, first_ops>;

    [[no_unique_address]] Rcvr rcvr;
    state_t state;

  private:
    template <typename O>
        requires std::same_as<op_state, std::remove_cvref_t<O>>
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        start(std::get<0>(std::forward<O>(o).state));
    }
};

template <typename Sndr, typename S, typename F, typename Tag,
          template <typename...> typename FirstRcvr>
struct sender {
    using is_sender = void;

    template <typename... Ts>
    using invoked_type = std::invoke_result_t<F, Ts...>;
    template <typename E>
    using dependent_senders = detail::gather_signatures<Tag, S, E, invoked_type,
                                                        completion_signatures>;

    template <typename E> struct completions_of {
        template <typename T> using fn = completion_signatures_of_t<T, E>;
    };
    template <typename E>
    using dependent_signatures = boost::mp11::mp_unique<boost::mp11::mp_flatten<
        boost::mp11::mp_transform_q<completions_of<E>, dependent_senders<E>>>>;

    template <typename...> using signatures = completion_signatures<>;

    template <receiver_from<S> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Sndr &&s, R &&r)
        -> _let::op_state<S, std::remove_cvref_t<R>, F,
                          dependent_senders<env_of_t<R>>, FirstRcvr> {
        return {std::move(s).s, std::forward<R>(r), std::move(s).f};
    }

    template <typename R> struct is_multishot_sender {
        template <typename T>
        using fn = std::bool_constant<multishot_sender<T, R>>;
    };

    template <typename Self, receiver_from<S> R>
        requires std::same_as<Sndr, std::remove_cvref_t<Self>> and
                 multishot_sender<S, R> and
                 boost::mp11::mp_all_of_q<dependent_senders<env_of_t<R>>,
                                          is_multishot_sender<R>>::value
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r)
        -> _let::op_state<S, std::remove_cvref_t<R>, F,
                          dependent_senders<env_of_t<R>>, FirstRcvr> {
        return {std::forward<Self>(self).s, std::forward<R>(r),
                std::forward<Self>(self).f};
    }

    template <typename Self>
        requires std::same_as<Sndr, std::remove_cvref_t<Self>>
    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   Self &&self) {
        return forward_env_of(self.s);
    }
};

template <typename F, template <typename...> typename Sndr> struct pipeable {
    [[no_unique_address]] F f;

  private:
    template <async::sender S, typename Self>
        requires std::same_as<pipeable, std::remove_cvref_t<Self>>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return Sndr<std::remove_cvref_t<S>, F>{
            {}, std::forward<S>(s), std::forward<Self>(self).f};
    }
};
} // namespace async::_let
