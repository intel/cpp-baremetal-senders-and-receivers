#pragma once

#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/functional.hpp>
#include <stdx/utility.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/list.hpp>

#include <concepts>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>

namespace async {
namespace _split {

template <typename S, typename Uniq> struct op_state_base;

template <typename S, typename Uniq> struct single_receiver {
    using is_receiver = void;
    using op_state_t = op_state_base<S, Uniq>;

    template <typename Tuple, typename... Args>
    static auto store_values(Args &&...args) -> void {
        using index =
            boost::mp11::mp_find<typename op_state_t::completions_t, Tuple>;
        static_assert(
            index::value <
            boost::mp11::mp_size<typename op_state_t::completions_t>::value);
        op_state_t::values.template emplace<index::value>(
            std::forward<Args>(args)...);
    }

    [[nodiscard]] constexpr auto query(get_env_t) const
        -> detail::singleton_env<get_stop_token_t, inplace_stop_token> {
        return {op_state_t::stop_source.get_token()};
    }

    template <typename... Args> static auto set_value(Args &&...args) -> void {
        if (op_state_t::linked_ops) {
            using tuple_t = value_holder<Args...>;
            store_values<tuple_t>(std::forward<Args>(args)...);
            op_state_t::linked_ops->notify();
        }
    }

    template <typename... Args> static auto set_error(Args &&...args) -> void {
        if (op_state_t::linked_ops) {
            using tuple_t = error_holder<Args...>;
            store_values<tuple_t>(std::forward<Args>(args)...);
            op_state_t::linked_ops->notify();
        }
    }

    static auto set_stopped() -> void {
        if (op_state_t::linked_ops) {
            using tuple_t = stopped_holder<>;
            store_values<tuple_t>();
            op_state_t::linked_ops->notify();
        }
    }
};

template <typename S, typename Uniq> struct op_state_base {
    virtual auto notify() -> void = 0;

    static auto reset() -> void {
        single_ops.reset();
        values.template emplace<0>();
        linked_ops = nullptr;
    }

    using E = env_of_t<single_receiver<S, Uniq>>;
    using values_t = value_types_of_t<S, E, value_holder, std::variant>;
    using errors_t = error_types_of_t<S, E, error_holder, std::variant>;
    using stoppeds_t = stopped_types_of_t<S, E, stopped_holder, std::variant>;
    using completions_t = boost::mp11::mp_push_front<
        boost::mp11::mp_unique<
            boost::mp11::mp_append<values_t, errors_t, stoppeds_t>>,
        std::monostate>;
    static inline completions_t values{};
    static inline op_state_base *linked_ops{};
    static inline inplace_stop_source stop_source{};

    using single_op_state_t = connect_result_t<S &&, single_receiver<S, Uniq>>;
    static inline std::optional<single_op_state_t> single_ops{};

    using env_t = env_of_t<S>;
    static inline std::optional<env_t> env{};
};

template <typename S, typename Rcvr, typename Uniq>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state : op_state_base<S, Uniq> {
    using op_state_t = op_state_base<S, Uniq>;

    struct stop_callback_fn {
        auto operator()() -> void { stop_source->request_stop(); }
        inplace_stop_source *stop_source;
    };

    template <stdx::same_as_unqualified<Rcvr> R>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr explicit(true) op_state(R &&r) : rcvr{std::forward<R>(r)} {}
    constexpr op_state(op_state &&) = delete;

    auto notify() -> void final {
        complete();
        if (next_ops) {
            next_ops->notify();
        }
    }

    constexpr auto start() & -> void {
        if (op_state_t::values.index() != 0) {
            complete();
            return;
        }

        stop_cb.emplace(get_stop_token(get_env(rcvr)),
                        stop_callback_fn{std::addressof(this->stop_source)});
        if (this->stop_source.stop_requested()) {
            set_stopped(std::move(rcvr));
            return;
        }

        if (next_ops = std::exchange(op_state_t::linked_ops, this);
            not next_ops) {
            async::start(*op_state_t::single_ops);
        }
    }

  private:
    auto complete() -> void {
        stop_cb.reset();
        std::visit(
            [&]<typename T>(T &t) -> void {
                if constexpr (not std::is_same_v<T, std::monostate>) {
                    std::move(t)(std::move(rcvr));
                }
            },
            op_state_t::values);
    }

    using stop_callback_t =
        stop_callback_for_t<stop_token_of_t<env_of_t<Rcvr>>, stop_callback_fn>;

    [[no_unique_address]] Rcvr rcvr;
    op_state_t *next_ops{};
    std::optional<stop_callback_t> stop_cb{};
};

template <typename Sndr, typename Uniq> struct sender {
    using is_sender = void;
    using op_state_t = op_state_base<Sndr, Uniq>;

    template <typename S>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr explicit(true) sender(S &&s) : sndr{std::forward<S>(s)} {
        if (not op_state_t::env) {
            op_state_t::env.emplace(get_env(sndr));
        }
    }

    [[nodiscard]] constexpr auto query(get_env_t) const ->
        typename op_state_t::env_t & {
        return *op_state_t::env;
    }

    [[no_unique_address]] Sndr sndr;

  private:
    template <typename Env>
    [[nodiscard]] friend constexpr auto
    tag_invoke(get_completion_signatures_t, sender const &,
               Env const &) -> completion_signatures_of_t<Sndr, Env> {
        return {};
    }

    template <stdx::same_as_unqualified<sender> Self, receiver R>
    [[nodiscard]] friend constexpr auto
    tag_invoke(connect_t, Self &&self,
               R &&r) -> op_state<Sndr, std::remove_cvref_t<R>, Uniq> {
        check_connect<Self, R>();
        if (not op_state_t::single_ops) {
            op_state_t::single_ops.emplace(stdx::with_result_of{
                [&]()
                    -> connect_result_t<Sndr &&, single_receiver<Sndr, Uniq>> {
                    return connect(std::move(self.sndr),
                                   single_receiver<Sndr, Uniq>{});
                }});
        }
        return op_state<Sndr, std::remove_cvref_t<R>, Uniq>{std::forward<R>(r)};
    }
};

template <typename Uniq> struct pipeable {
  private:
    template <singleshot_sender S>
        requires(not std::is_reference_v<S>)
    friend constexpr auto operator|(S &&s, pipeable) -> async::sender auto {
        using sender_t = std::remove_cvref_t<S>;
        op_state_base<sender_t, Uniq>::reset();
        return sender<sender_t, Uniq>{std::forward<S>(s)};
    }

    template <multishot_sender S>
    friend constexpr auto operator|(S &&s, pipeable) -> async::sender auto {
        return std::forward<S>(s);
    }
};
} // namespace _split

template <typename Uniq = decltype([] {})>
[[nodiscard]] constexpr auto split() -> _split::pipeable<Uniq> {
    return {};
}

template <sender S, typename Uniq = decltype([] {})>
[[nodiscard]] constexpr auto split(S &&s) -> sender auto {
    return std::forward<S>(s) | split<Uniq>();
}
} // namespace async
