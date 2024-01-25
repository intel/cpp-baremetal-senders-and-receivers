#pragma once

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/stop_token.hpp>
#include <async/tags.hpp>

#include <stdx/concepts.hpp>
#include <stdx/type_traits.hpp>
#include <stdx/utility.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/function.hpp>
#include <boost/mp11/list.hpp>

#include <atomic>
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

  private:
    template <typename... Args>
    friend auto tag_invoke(set_value_t, sub_receiver const &r, Args &&...args)
        -> void {
        r.ops->emplace_value(std::forward<Args>(args)...);
    }
    template <typename... Args>
    friend auto tag_invoke(set_error_t, sub_receiver const &r, Args &&...args)
        -> void {
        r.ops->emplace_error(std::forward<Args>(args)...);
    }
    friend auto tag_invoke(set_stopped_t, sub_receiver const &r) -> void {
        r.ops->emplace_stopped();
    }

    [[nodiscard]] friend constexpr auto tag_invoke(get_env_t,
                                                   sub_receiver const &self)
        -> detail::overriding_env<get_stop_token_t, in_place_stop_token,
                                  typename SubOps::receiver_t> {
        return override_env_with<get_stop_token_t>(self.ops->get_stop_token(),
                                                   self.ops->get_receiver());
    }
};

template <typename S, typename Tag, typename E>
concept single_sender = requires {
    typename async::detail::gather_signatures<Tag, S, E, std::type_identity_t,
                                              std::type_identity_t>;
};

template <typename E, typename S> struct sub_op_storage {
    auto store(auto &&...) -> void {}

    using values_t = detail::type_list<>;
};

template <typename E, single_sender<set_value_t, E> S>
struct sub_op_storage<E, S> {
    template <typename... Args> auto store(Args &&...args) -> void {
        v.emplace(std::forward<Args>(args)...);
    }
    using value_t = value_types_of_t<typename S::sender_t, E, std::optional,
                                     std::type_identity_t>;
    using values_t = detail::type_list<typename value_t::value_type>;

    value_t v{};
};

template <typename Ops, typename R, typename S>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct sub_op_state : sub_op_storage<env_of_t<R>, S> {
    using sender_t = typename S::sender_t;
    using receiver_t = R;

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

    [[nodiscard]] auto get_stop_token() const -> in_place_stop_token {
        return static_cast<Ops const &>(*this).stop_source.get_token();
    }

    using ops_t = connect_result_t<sender_t, sub_receiver<sub_op_state>>;
    ops_t ops;
};

template <typename...> struct error_op_state;

template <typename E> struct error_op_state<E, boost::mp11::mp_list<>> {
    auto release_error(auto &&) const -> void {}
    using signatures = completion_signatures<>;
};

template <typename E, single_sender<set_error_t, E>... Sndrs>
struct error_op_state<E, boost::mp11::mp_list<Sndrs...>> {
    template <typename... Args> auto store_error(Args &&...args) -> void {
        e.emplace(std::forward<Args>(args)...);
    }
    template <typename R> auto release_error(R &&r) -> void {
        set_error(std::forward<R>(r), std::move(*e));
    }

    // All senders should send the same error type
    using error_t = std::common_type_t<error_types_of_t<
        typename Sndrs::sender_t, E, std::optional, std::type_identity_t>...>;
    using signatures =
        completion_signatures<set_error_t(typename error_t::value_type)>;

    error_t e{};
};

template <typename E> struct is_error_sender {
    template <typename S>
    using fn = std::bool_constant<single_sender<S, set_error_t, E>>;
};
template <typename E, typename... Sndrs>
using error_senders = boost::mp11::mp_copy_if_q<boost::mp11::mp_list<Sndrs...>,
                                                is_error_sender<E>>;

template <typename Rcvr, typename... Sndrs>
struct op_state
    : error_op_state<env_of_t<Rcvr>, error_senders<env_of_t<Rcvr>, Sndrs...>>,
      sub_op_state<op_state<Rcvr, Sndrs...>, Rcvr, Sndrs>... {
    struct stop_callback_fn {
        auto operator()() -> void { stop_source->request_stop(); }
        in_place_stop_source *stop_source;
    };

    template <typename S, typename R>
    constexpr op_state(S &&s, R &&r)
        : sub_op_state<op_state<Rcvr, Sndrs...>, Rcvr, Sndrs>{std::forward<S>(
              s)}...,
          rcvr{std::forward<R>(r)} {}

    auto notify() -> void {
        if (--count == 0) {
            complete();
        }
    }

    template <typename... Args> auto notify_error(Args &&...args) -> void {
        if (not have_error.exchange(true)) {
            this->store_error(std::forward<Args>(args)...);
        }
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
        if (have_error) {
            this->release_error(rcvr);
        } else if (stop_source.stop_requested()) {
            set_stopped(rcvr);
        } else {
            using value_senders =
                boost::mp11::mp_copy_if<boost::mp11::mp_list<Sndrs...>,
                                        single_value_sender_t>;
            [&]<typename... Ss>(boost::mp11::mp_list<Ss...>) {
                set_value(
                    rcvr,
                    static_cast<sub_op_state<op_state, Rcvr, Ss> &&>(*this)
                        .v.value()...);
            }(value_senders{});
        }
    }

    using stop_callback_t =
        stop_callback_for_t<stop_token_of_t<env_of_t<Rcvr>>, stop_callback_fn>;

    [[no_unique_address]] Rcvr rcvr;
    std::atomic<std::size_t> count{};
    in_place_stop_source stop_source{};
    std::optional<stop_callback_t> stop_cb{};
    std::atomic<bool> have_error{};

  private:
    template <stdx::same_as_unqualified<op_state> O>
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        o.stop_cb.emplace(get_stop_token(get_env(o.rcvr)),
                          stop_callback_fn{std::addressof(o.stop_source)});
        if (o.stop_source.stop_requested()) {
            set_stopped(std::forward<O>(o).rcvr);
        } else {
            o.count = sizeof...(Sndrs);
            (start(static_cast<stdx::forward_like_t<
                       O, sub_op_state<op_state, Rcvr, Sndrs>>>(o)
                       .ops),
             ...);
        }
    }
};

template <typename... Sndrs> struct sender : Sndrs... {
    using is_sender = void;

  private:
    template <receiver_from<sender> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender &&self,
                                                   R &&r)
        -> op_state<std::remove_cvref_t<R>, Sndrs...> {
        return {std::move(self), std::forward<R>(r)};
    }

    template <stdx::same_as_unqualified<sender> Self, receiver_from<sender> R>
        requires(
            ... and
            multishot_sender<typename Sndrs::sender_t,
                             detail::universal_receiver<detail::overriding_env<
                                 get_stop_token_t, in_place_stop_token,
                                 std::remove_cvref_t<R>>>>)
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r)
        -> op_state<std::remove_cvref_t<R>, Sndrs...> {
        return {std::forward<Self>(self), std::forward<R>(r)};
    }

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
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> signatures<Env> {
        return {};
    }
};

template <typename Rcvr> struct op_state<Rcvr> {
    [[no_unique_address]] Rcvr rcvr;

  private:
    template <stdx::same_as_unqualified<op_state> O>
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        if constexpr (not async::unstoppable_token<
                          async::stop_token_of_t<async::env_of_t<Rcvr>>>) {
            if (async::get_stop_token(async::get_env(o.rcvr))
                    .stop_requested()) {
                set_stopped(std::forward<O>(o).rcvr);
                return;
            }
        }
        set_value(std::forward<O>(o).rcvr);
    }
};

template <> struct sender<> {
    using is_sender = void;

  private:
    template <receiver_from<sender> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender const &,
                                                   R &&r)
        -> op_state<std::remove_cvref_t<R>> {
        return {std::forward<R>(r)};
    }

    template <typename Env>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> completion_signatures<set_value_t(), set_stopped_t()> {
        return {};
    }

    template <typename Env>
        requires unstoppable_token<stop_token_of_t<Env>>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> completion_signatures<set_value_t()> {
        return {};
    }
};
} // namespace _when_all

template <sender... Sndrs>
[[nodiscard]] constexpr auto when_all(Sndrs &&...sndrs) -> sender auto {
    return [&]<auto... Is>(std::index_sequence<Is...>) {
        return _when_all::sender<
            _when_all::sub_sender<std::remove_cvref_t<Sndrs>, Is>...>{
            {std::forward<Sndrs>(sndrs)}...};
    }(std::make_index_sequence<sizeof...(Sndrs)>{});
}
} // namespace async
