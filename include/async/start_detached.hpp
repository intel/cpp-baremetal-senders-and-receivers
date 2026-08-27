#pragma once

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/schedulers/runloop_scheduler.hpp>
#include <async/stop_token.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/optional.hpp>

#include <conc/concurrency.hpp>

#include <concepts>
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

struct never_stop_handle {
    [[nodiscard]] constexpr auto started() const -> bool { return started_; }
    constexpr explicit operator bool() const { return started_; }

    [[nodiscard]] consteval static auto stop_possible() -> bool {
        return false;
    }
    [[nodiscard]] consteval static auto stop_requested() -> bool {
        return false;
    }
    [[nodiscard]] consteval static auto request_stop() -> bool { return false; }
    [[nodiscard]] consteval static auto is_complete() -> bool { return false; }
    [[nodiscard]] consteval static auto sync_wait() -> bool { return false; }

    bool started_{};
};

template <typename Uniq> inplace_stop_source *stop_source_for{};
template <typename Uniq>
auto synchronizer_for = async::detail::simple_synchronizer{};

template <typename Uniq> struct stop_handle : never_stop_handle {
    [[nodiscard]] constexpr static auto stop_possible() -> bool {
        return conc::call_in_critical_section<Uniq>([&] -> bool {
            auto s = stop_source_for<Uniq>;
            return s != nullptr and s->stop_possible();
        });
    }

    [[nodiscard]] constexpr static auto stop_requested() -> bool {
        return conc::call_in_critical_section<Uniq>([&] -> bool {
            auto s = stop_source_for<Uniq>;
            return s != nullptr and s->stop_requested();
        });
    }

    [[nodiscard]] constexpr static auto request_stop() -> bool {
        return conc::call_in_critical_section<Uniq>([&] -> bool {
            auto s = stop_source_for<Uniq>;
            return s != nullptr and s->request_stop();
        });
    }

    static auto notify() -> void {
        conc::call_in_critical_section<Uniq>(
            [&] -> void { stop_source_for<Uniq> = nullptr; });
        synchronizer_for<Uniq>.notify();
    }
    [[nodiscard]] static auto is_complete() -> bool {
        return synchronizer_for<Uniq>.is_complete();
    }
    [[nodiscard]] static auto sync_wait() -> bool {
        synchronizer_for<Uniq>.wait();
        return true;
    }
};

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
                      Env>;

    template <stdx::same_as_unqualified<Env> E>
    constexpr explicit(true) op_state_base(E &&e) : envmt{std::forward<E>(e)} {}
    constexpr op_state_base(op_state_base &&) = delete;

    template <stdx::ct_string> auto die() {}

    [[nodiscard]] constexpr auto query(get_env_t) const -> env_t {
        return env{prop{get_stop_token_t{}, stop_src.get_token()}, envmt};
    }

    using stop_source_t = StopSource;
    [[no_unique_address]] stop_source_t stop_src{};
    [[no_unique_address]] Env envmt;
};

template <typename Uniq, typename Sndr, typename Alloc, typename StopSource,
          typename Env>
struct op_state : op_state_base<StopSource, Env> {
    using receiver_t = receiver<op_state>;
    using ops_t = connect_result_t<Sndr, receiver_t>;

    template <stdx::same_as_unqualified<Sndr> S,
              stdx::same_as_unqualified<Env> E>
    constexpr explicit(true) op_state(S &&s, E &&e)
        : op_state_base<StopSource, Env>{std::forward<E>(e)},
          ops{connect(std::forward<S>(s), receiver_t{this})} {}

    template <stdx::ct_string S> auto die() {
        debug_signal<S, debug::erased_context_for<op_state>>(this->envmt);
        if constexpr (use_single_stop_source<Uniq, Alloc, StopSource>) {
            stop_handle<Uniq>{}.notify();
        }
        Alloc::template destruct<Uniq>(this);
    }

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(this->envmt);
        async::start(ops);
    }

    ops_t ops;
};

template <typename Uniq, typename StopSource, sender S, typename Env>
[[nodiscard]] auto start(S &&s, Env &&e) {
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
    if constexpr (use_single_stop_source<Uniq, A, StopSource>) {
        return stop_handle<Uniq>{A::template construct<Uniq, O>(
            [&](O &ops) {
                set_stop_source<Uniq, A>(std::addressof(ops.stop_src));
                synchronizer_for<Uniq>.reset();
                async::start(ops);
            },
            std::forward<S>(s), std::forward<Env>(e))};
    } else {
        return never_stop_handle{A::template construct<Uniq, O>(
            [&](O &ops) { async::start(ops); }, std::forward<S>(s),
            std::forward<Env>(e))};
    }
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
[[nodiscard]] constexpr auto start_detached_stoppable(Env &&e = {})
    -> _start_detached::pipeable<Uniq, inplace_stop_source, Env> {
    return {std::forward<Env>(e)};
}

template <stdx::ct_string Name, typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] auto start_detached_stoppable(Env &&e = {}) {
    return start_detached_stoppable<stdx::cts_t<Name>>(
        with_debug_interface<Name>(std::forward<Env>(e)));
}

template <typename Uniq = decltype([] {}), sender S, typename Env = empty_env>
[[nodiscard]] auto start_detached_stoppable(S &&s, Env &&e = {}) {
    return std::forward<S>(s) |
           start_detached_stoppable<Uniq>(std::forward<Env>(e));
}

template <stdx::ct_string Name, sender S, typename Env = empty_env>
[[nodiscard]] auto start_detached_stoppable(S &&s, Env &&e = {}) {
    return std::forward<S>(s) |
           start_detached_stoppable<Name>(std::forward<Env>(e));
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
        with_debug_interface<Name>(std::forward<Env>(e)));
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

template <typename Uniq = decltype([] {}), typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] constexpr auto start_detached(Env &&e = {})
    -> _start_detached::pipeable<Uniq, never_stop_source, Env> {
    return {std::forward<Env>(e)};
}

template <stdx::ct_string Name, typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] auto start_detached(Env &&e = {}) {
    return start_detached<stdx::cts_t<Name>>(
        with_debug_interface<Name>(std::forward<Env>(e)));
}

template <typename Uniq = decltype([] {}), sender S, typename Env = empty_env>
[[nodiscard]] auto start_detached(S &&s, Env &&e = {}) {
    return std::forward<S>(s) | start_detached<Uniq>(std::forward<Env>(e));
}

template <stdx::ct_string Name, sender S, typename Env = empty_env>
[[nodiscard]] auto start_detached(S &&s, Env &&e = {}) {
    return std::forward<S>(s) | start_detached<Name>(std::forward<Env>(e));
}

template <typename Uniq> auto stop_detached() {
    return _start_detached::stop_handle<Uniq>{}.request_stop();
}

template <stdx::ct_string Name> auto stop_detached() {
    return stop_detached<stdx::cts_t<Name>>();
}

template <typename Uniq> auto sync_stop_detached() {
    _start_detached::stop_handle<Uniq>{}.request_stop();
    return _start_detached::stop_handle<Uniq>{}.sync_wait();
}

template <stdx::ct_string Name> auto sync_stop_detached() {
    return sync_stop_detached<stdx::cts_t<Name>>();
}

template <typename Uniq> auto sync_wait_detached() {
    return _start_detached::stop_handle<Uniq>{}.sync_wait();
}

template <stdx::ct_string Name> auto sync_wait_detached() {
    return sync_wait_detached<stdx::cts_t<Name>>();
}

struct start_detached_t;

template <typename... Ts>
struct debug::context_for<_start_detached::op_state<Ts...>> {
    using tag = start_detached_t;
    constexpr static auto name = stdx::ct_string{"start_detached"};
    using children = stdx::type_list<>;
    using type = _start_detached::op_state<Ts...>;
};
} // namespace async
