#pragma once

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>
#include <conc/concurrency.hpp>

#include <stdx/functional.hpp>
#include <stdx/tuple.hpp>
#include <stdx/utility.hpp>

#include <atomic>
#include <concepts>
#include <cstddef>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>

namespace async {
namespace _when_any {
template <typename S, std::size_t> struct sub_sender : S {
    using sender_t = S;
};

template <typename SubOps> struct sub_receiver {
    using is_receiver = void;

    SubOps *ops;

  private:
    template <channel_tag Tag, typename... Args>
    friend auto tag_invoke(Tag, sub_receiver const &r, Args &&...args) -> void {
        r.ops->template emplace<Tag>(std::forward<Args>(args)...);
    }

    [[nodiscard]] friend constexpr auto tag_invoke(get_env_t,
                                                   sub_receiver const &self)
        -> detail::overriding_env<get_stop_token_t, in_place_stop_token,
                                  typename SubOps::receiver_t> {
        return override_env_with<get_stop_token_t>(self.ops->get_stop_token(),
                                                   self.ops->get_receiver());
    }
};

template <typename Ops, typename R, typename S>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct sub_op_state {
    using sender_t = typename S::sender_t;
    using receiver_t = R;

    constexpr explicit(true) sub_op_state(S &&s)
        : ops{connect(static_cast<sender_t &&>(std::move(s)),
                      sub_receiver<sub_op_state>{this})} {}
    constexpr explicit(true) sub_op_state(S const &s)
        : ops{connect(static_cast<sender_t const &>(s),
                      sub_receiver<sub_op_state>{this})} {}
    constexpr sub_op_state(sub_op_state &&) = delete;

    template <typename Tag, typename... Args>
    auto emplace(Args &&...args) -> void {
        static_cast<Ops &>(*this).template emplace<Tag>(
            std::forward<Args>(args)...);
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

template <typename... Ts>
using decayed_tuple = stdx::tuple<std::remove_cvref_t<Ts>...>;

// Policy: when the first sender completes successfully, all other senders are
// stopped. If no senders complete successfully, the first error is reported.
// Stopped is reported only if all senders are stopped.
struct first_successful {
    template <typename Mutex, typename Tag, typename Completions,
              typename... Args>
    static auto emplace(Completions &completions, auto &stop_source,
                        Args &&...args) -> void {
        using T = decayed_tuple<Tag, Args...>;
        using index = boost::mp11::mp_find<Completions, T>;
        static_assert(index::value < boost::mp11::mp_size<Completions>::value);

        conc::call_in_critical_section<Mutex>([&] {
            auto const should_store = [&] {
                if constexpr (std::same_as<Tag, set_value_t>) {
                    return std::visit(
                        stdx::overload{
                            []<typename Tuple>(Tuple const &) {
                                return not std::same_as<
                                    boost::mp11::mp_first<Tuple>, set_value_t>;
                            },
                            [](std::monostate) { return true; }},
                        completions);
                }
                return completions.index() == 0;
            }();

            if (should_store) {
                completions.template emplace<index::value>(
                    stdx::make_tuple(Tag{}, std::forward<Args>(args)...));
            }
        });
        if constexpr (std::same_as<Tag, set_value_t>) {
            stop_source.request_stop();
        }
    }
};

// Policy: when the first sender completes either with success or error, all
// other senders are stopped. The first success or error is reported. Stopped is
// reported only if all senders are stopped.
struct first_noncancelled {
    template <typename Mutex, typename Tag, typename Completions,
              typename... Args>
    static auto emplace(Completions &completions, auto &stop_source,
                        Args &&...args) -> void {
        using T = decayed_tuple<Tag, Args...>;
        using index = boost::mp11::mp_find<Completions, T>;
        static_assert(index::value < boost::mp11::mp_size<Completions>::value);

        conc::call_in_critical_section<Mutex>([&] {
            auto const should_store = [&] {
                if constexpr (std::same_as<Tag, set_stopped_t>) {
                    return completions.index() == 0;
                }
                return std::visit(
                    stdx::overload{
                        []<typename Tuple>(Tuple const &) {
                            return std::same_as<boost::mp11::mp_first<Tuple>,
                                                set_stopped_t>;
                        },
                        [](std::monostate) { return true; }},
                    completions);
            }();

            if (should_store) {
                completions.template emplace<index::value>(
                    stdx::make_tuple(Tag{}, std::forward<Args>(args)...));
            }
        });
        if constexpr (not std::same_as<Tag, set_stopped_t>) {
            stop_source.request_stop();
        }
    }
};

// Policy: when the first sender completes on any channel, all other senders are
// stopped.
struct first_complete {
    template <typename Mutex, typename Tag, typename Completions,
              typename... Args>
    static auto emplace(Completions &completions, auto &stop_source,
                        Args &&...args) -> void {
        using T = decayed_tuple<Tag, Args...>;
        using index = boost::mp11::mp_find<Completions, T>;
        static_assert(index::value < boost::mp11::mp_size<Completions>::value);

        conc::call_in_critical_section<Mutex>([&] {
            if (completions.index() == 0) {
                completions.template emplace<index::value>(
                    stdx::make_tuple(Tag{}, std::forward<Args>(args)...));
            }
        });
        stop_source.request_stop();
    }
};

template <typename StopPolicy, typename Rcvr, typename... Sndrs>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state
    : sub_op_state<op_state<StopPolicy, Rcvr, Sndrs...>, Rcvr, Sndrs>... {
    struct stop_callback_fn {
        auto operator()() -> void { stop_source->request_stop(); }
        in_place_stop_source *stop_source;
    };

    template <typename S, typename R>
    constexpr op_state(S &&s, R &&r)
        : sub_op_state<op_state, Rcvr, Sndrs>{std::forward<S>(s)}...,
          rcvr{std::forward<R>(r)} {}
    constexpr op_state(op_state &&) = delete;

    template <typename Tag> struct prepend {
        template <typename L> using fn = boost::mp11::mp_push_front<L, Tag>;
    };

    template <typename Tag, typename L>
    using apply_tag = boost::mp11::mp_transform_q<prepend<Tag>, L>;

    using env_t =
        detail::overriding_env<get_stop_token_t, in_place_stop_token, Rcvr>;

    using completions_t = boost::mp11::mp_unique<boost::mp11::mp_append<
        std::variant<std::monostate>,
        apply_tag<set_value_t, value_types_of_t<Sndrs, env_t, decayed_tuple,
                                                std::variant>>...,
        apply_tag<set_error_t, error_types_of_t<Sndrs, env_t, decayed_tuple,
                                                std::variant>>...,
        apply_tag<set_stopped_t, stopped_types_of_t<Sndrs, env_t, decayed_tuple,
                                                    std::variant>>...>>;

    struct mutex;

    template <typename Tag, typename... Args>
    auto emplace(Args &&...args) -> void {
        StopPolicy::template emplace<mutex, Tag>(completions, stop_source,
                                                 std::forward<Args>(args)...);
        if (--count == 0) {
            complete();
        }
    }

    auto complete() -> void {
        stop_cb.reset();
        if constexpr (not async::unstoppable_token<
                          async::stop_token_of_t<async::env_of_t<Rcvr>>>) {
            if (async::get_stop_token(async::get_env(rcvr)).stop_requested()) {
                set_stopped(rcvr);
                return;
            }
        }
        std::visit(stdx::overload{
                       [&]<typename T>(T &&t) {
                           std::forward<T>(t).apply(
                               [&]<typename... Args>(auto tag, Args &&...args) {
                                   tag(rcvr, std::forward<Args>(args)...);
                               });
                       },
                       [](std::monostate) {}},
                   std::move(completions));
    }

    using stop_callback_t =
        stop_callback_for_t<stop_token_of_t<env_of_t<Rcvr>>, stop_callback_fn>;

    [[no_unique_address]] Rcvr rcvr;
    completions_t completions{};
    std::atomic<std::size_t> count{};
    in_place_stop_source stop_source{};
    std::optional<stop_callback_t> stop_cb{};

  private:
    template <typename O>
        requires std::same_as<op_state, std::remove_cvref_t<O>>
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
}; // namespace async

template <typename StopPolicy, typename... Sndrs> struct sender : Sndrs... {
    using is_sender = void;

  private:
    template <typename Env>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> boost::mp11::mp_unique<
            boost::mp11::mp_append<make_completion_signatures<Sndrs, Env>...>> {
        return {};
    }

    template <receiver_from<sender> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender &&self,
                                                   R &&r)
        -> op_state<StopPolicy, std::remove_cvref_t<R>, Sndrs...> {
        return {std::move(self), std::forward<R>(r)};
    }

    template <typename Self, receiver_from<sender> R>
        requires std::same_as<sender, std::remove_cvref_t<Self>> and
                 (... and multishot_sender<
                              typename Sndrs::sender_t,
                              detail::universal_receiver<detail::overriding_env<
                                  get_stop_token_t, in_place_stop_token,
                                  std::remove_cvref_t<R>>>>)
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r)
        -> op_state<StopPolicy, std::remove_cvref_t<R>, Sndrs...> {
        return {std::forward<Self>(self), std::forward<R>(r)};
    }
};

template <typename StopPolicy, typename Rcvr>
struct op_state<StopPolicy, Rcvr> {
    struct stop_callback_fn {
        auto operator()() -> void {
            set_stopped(ops->rcvr);
            ops->stop_cb.reset();
        }
        op_state *ops;
    };
    using stop_callback_t =
        stop_callback_for_t<stop_token_of_t<env_of_t<Rcvr>>, stop_callback_fn>;

    [[no_unique_address]] Rcvr rcvr;
    std::optional<stop_callback_t> stop_cb{};

  private:
    template <typename O>
        requires std::same_as<op_state, std::remove_cvref_t<O>>
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        o.stop_cb.emplace(async::get_stop_token(get_env(o.rcvr)),
                          stop_callback_fn{std::addressof(o)});
    }
};

template <typename StopPolicy> struct sender<StopPolicy> {
    using is_sender = void;

  private:
    template <receiver_from<sender> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender const &,
                                                   R &&r)
        -> op_state<StopPolicy, std::remove_cvref_t<R>> {
        return {std::forward<R>(r)};
    }

    template <typename Env>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> completion_signatures<set_stopped_t()> {
        return {};
    }

    template <typename Env>
        requires unstoppable_token<stop_token_of_t<Env>>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> completion_signatures<> {
        return {};
    }
};

template <typename Sndr> struct pipeable {
    Sndr sndr;

  private:
    template <async::sender S, typename Self>
        requires std::same_as<pipeable, std::remove_cvref_t<Self>>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<first_complete, sub_sender<std::remove_cvref_t<S>, 0>,
                      sub_sender<Sndr, 1>>{std::forward<S>(s),
                                           std::forward<Self>(self).sndr};
    }
};
} // namespace _when_any

template <typename StopPolicy = _when_any::first_noncancelled, sender... Sndrs>
[[nodiscard]] constexpr auto when_any(Sndrs &&...sndrs) -> sender auto {
    return [&]<auto... Is>(std::index_sequence<Is...>) {
        return _when_any::sender<
            StopPolicy,
            _when_any::sub_sender<std::remove_cvref_t<Sndrs>, Is>...>{
            {std::forward<Sndrs>(sndrs)}...};
    }(std::make_index_sequence<sizeof...(Sndrs)>{});
}

template <sender... Sndrs>
[[nodiscard]] constexpr auto first_successful(Sndrs &&...sndrs) -> sender auto {
    return when_any<_when_any::first_successful>(std::forward<Sndrs>(sndrs)...);
}

template <sender Trigger>
[[nodiscard]] constexpr auto stop_when(Trigger &&t)
    -> _when_any::pipeable<std::remove_cvref_t<Trigger>> {
    return {std::forward<Trigger>(t)};
}

template <sender Sndr, sender Trigger>
[[nodiscard]] constexpr auto stop_when(Sndr &&s, Trigger &&t) -> sender auto {
    return std::forward<Sndr>(s) | stop_when(std::forward<Trigger>(t));
}
} // namespace async
