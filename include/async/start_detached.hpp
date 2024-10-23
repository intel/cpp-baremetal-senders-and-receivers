#pragma once

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/stop_token.hpp>
#include <conc/concurrency.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/optional.hpp>

#include <concepts>
#include <functional>
#include <type_traits>
#include <utility>

namespace async {
namespace _start_detached {
template <typename Ops> struct receiver {
    using is_receiver = void;

    Ops *ops;

    [[nodiscard]] constexpr auto query(get_env_t) const -> typename Ops::env_t {
        return ops->query(get_env_t{});
    }

    constexpr auto set_value(auto &&...) const && -> void {
        ops->template die<"set_value">();
    }
    constexpr auto set_error(auto &&...) const && -> void {
        ops->template die<"set_error">();
    }
    constexpr auto set_stopped() const && -> void {
        ops->template die<"set_stopped">();
    }
};

template <typename Uniq> inplace_stop_source *stop_source_for{};

template <typename Uniq, typename A, typename StopSource>
constexpr auto use_single_stop_source =
    std::same_as<StopSource, inplace_stop_source> and
    A::template allocation_limit<Uniq> == 1;

template <typename Uniq, typename A, typename StopSource>
auto set_stop_source(StopSource *p) -> void {
    if constexpr (use_single_stop_source<Uniq, A, StopSource>) {
        conc::call_in_critical_section<Uniq>(
            [p] { stop_source_for<Uniq> = p; });
    }
}

template <typename StopSource, typename Env>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state_base {
    using env_t = env<prop<get_stop_token_t,
                           decltype(std::declval<StopSource>().get_token())>,
                      Env const &>;

    constexpr explicit(true) op_state_base(Env &&env) : e{std::move(env)} {}
    constexpr op_state_base(op_state_base &&) = delete;

    template <stdx::ct_string> auto die() {}

    [[nodiscard]] constexpr auto query(get_env_t) const -> env_t {
        return env{prop{get_stop_token_t{}, stop_src.get_token()},
                   std::cref(e)};
    }

    using stop_source_t = StopSource;
    [[no_unique_address]] stop_source_t stop_src{};
    [[no_unique_address]] Env e;
};

template <typename Uniq, typename Sndr, typename Alloc, typename StopSource,
          typename Env>
struct op_state : op_state_base<StopSource, Env> {
    using receiver_t = receiver<op_state>;
    using ops_t = connect_result_t<Sndr, receiver_t>;

    template <typename S>
    constexpr explicit(true) op_state(S &&s, Env &&env)
        : op_state_base<StopSource, Env>{std::move(env)},
          ops{connect(std::forward<S>(s), receiver_t{this})} {}

    template <stdx::ct_string S> auto die() {
        debug_signal<S, debug::erased_context_for<op_state>>(this->e);
        set_stop_source<Uniq, Alloc, StopSource>(nullptr);
        Alloc::template destruct<Uniq>(this);
    }

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(this->e);
        async::start(ops);
    }

    ops_t ops;
};

template <typename Uniq, typename StopSource, sender S, typename Env>
[[nodiscard]] auto start(S &&s, Env &&e) -> stdx::optional<StopSource *> {
    using sndr_t = std::remove_cvref_t<S>;
    using custom_env_t = std::remove_cvref_t<Env>;

    // to determine the allocator, use a combination of the passed-in
    // environment, the sender's environment, and the environment from the
    // op state resulting from connecting the sender and receiver: this
    // correctly handles senders whose connected behaviour changes with the
    // environment
    using simulated_rcvr_t = receiver<op_state_base<StopSource, custom_env_t>>;
    using ops_env_t = env_of_t<connect_result_t<S, simulated_rcvr_t>>;
    using A = allocator_of_t<env<custom_env_t, env_of_t<sndr_t>, ops_env_t>>;

    using O = op_state<Uniq, sndr_t, A, StopSource, custom_env_t>;
    stdx::optional<StopSource *> stop_src{};
    A::template construct<Uniq, O>(
        [&](O &ops) {
            stop_src = std::addressof(ops.stop_src);
            set_stop_source<Uniq, A>(std::addressof(ops.stop_src));
            async::start(ops);
        },
        std::forward<S>(s), std::forward<Env>(e));
    return stop_src;
}

template <typename Uniq, typename StopSource, typename Env> struct pipeable {
    [[no_unique_address]] Env e;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    [[nodiscard]] friend auto operator|(S &&s, Self &&self) {
        return start<Uniq, StopSource>(std::forward<S>(s),
                                       std::forward<Self>(self).e);
    }
};
} // namespace _start_detached

template <typename Uniq = decltype([] {}), typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] constexpr auto start_detached(Env &&e = {})
    -> _start_detached::pipeable<Uniq, inplace_stop_source, Env> {
    return {std::forward<Env>(e)};
}

template <stdx::ct_string Name, typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] auto start_detached(Env &&e = {}) {
    return start_detached<stdx::cts_t<Name>>(
        env{prop{get_debug_interface_t{}, debug::named_interface<Name>{}},
            std::forward<Env>(e)});
}

template <typename Uniq = decltype([] {}), sender S, typename Env = empty_env>
[[nodiscard]] auto start_detached(S &&s, Env &&e = {}) {
    return std::forward<S>(s) | start_detached<Uniq>(std::forward<Env>(e));
}

template <stdx::ct_string Name, sender S, typename Env = empty_env>
[[nodiscard]] auto start_detached(S &&s, Env &&e = {}) {
    return std::forward<S>(s) | start_detached<Name>(std::forward<Env>(e));
}

template <typename Uniq = decltype([] {}), typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] constexpr auto start_detached_unstoppable(Env &&e = {})
    -> _start_detached::pipeable<Uniq, never_stop_source, Env> {
    return {std::forward<Env>(e)};
}

template <stdx::ct_string Name, typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] auto start_detached_unstoppable(Env &&e = {}) {
    return start_detached_unstoppable<stdx::cts_t<Name>>(
        env{prop{get_debug_interface_t{}, debug::named_interface<Name>{}},
            std::forward<Env>(e)});
}

template <typename Uniq = decltype([] {}), sender S, typename Env = empty_env>
[[nodiscard]] auto start_detached_unstoppable(S &&s, Env &&e = {}) {
    return std::forward<S>(s) |
           start_detached_unstoppable<Uniq>(std::forward<Env>(e));
}

template <stdx::ct_string Name, sender S, typename Env = empty_env>
[[nodiscard]] auto start_detached_unstoppable(S &&s, Env &&e = {}) {
    return std::forward<S>(s) |
           start_detached_unstoppable<Name>(std::forward<Env>(e));
}

template <typename Uniq> auto stop_detached() {
    return conc::call_in_critical_section<Uniq>([] {
        return _start_detached::stop_source_for<Uniq> != nullptr and
               _start_detached::stop_source_for<Uniq>->request_stop();
    });
}

template <stdx::ct_string Name> auto stop_detached() {
    return stop_detached<stdx::cts_t<Name>>();
}

struct start_detached_t;

template <typename... Ts>
struct debug::context_for<_start_detached::op_state<Ts...>> {
    using tag = start_detached_t;
    constexpr static auto name = stdx::ct_string{"start_detached"};
    using children = stdx::type_list<>;
    // context_for<typename _start_detached::op_state<Ts...>::ops_t>>;
    using type = _start_detached::op_state<Ts...>;
};
} // namespace async
