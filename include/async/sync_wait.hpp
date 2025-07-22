#pragma once

#include <async/completes_synchronously.hpp>
#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/get_scheduler.hpp>
#include <async/schedulers/runloop_scheduler.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/tuple.hpp>

#include <concepts>
#include <functional>
#include <optional>
#include <type_traits>
#include <utility>

namespace async {
namespace _sync_wait {

template <typename V, typename Env> struct receiver {
    using is_receiver = void;

    // NOLINTBEGIN(cppcoreguidelines-avoid-const-or-ref-data-members)
    V &values;
    [[no_unique_address]] Env env;
    // NOLINTEND(cppcoreguidelines-avoid-const-or-ref-data-members)

    [[nodiscard]] constexpr auto query(get_env_t) const noexcept { return env; }

    constexpr auto signal_start() const {
        debug_signal<"start", debug::erased_context_for<receiver>>(env);
    }

    template <typename... Args>
    constexpr auto set_value(Args &&...args) const && -> void {
        debug_signal<"set_value", debug::erased_context_for<receiver>>(env);
        values.emplace(stdx::make_tuple(std::forward<Args>(args)...));
    }
    constexpr auto set_error(auto &&...) const && -> void {
        debug_signal<"set_error", debug::erased_context_for<receiver>>(env);
    }
    constexpr auto set_stopped() const && -> void {
        debug_signal<"set_stopped", debug::erased_context_for<receiver>>(env);
    }
};

template <typename V, typename Env, typename RL>
struct dynamic_receiver : receiver<V, Env> {
    // NOLINTBEGIN(cppcoreguidelines-avoid-const-or-ref-data-members)
    RL &loop;
    // NOLINTEND(cppcoreguidelines-avoid-const-or-ref-data-members)

    template <typename... Args>
    constexpr auto set_value(Args &&...args) const && -> void {
        static_cast<receiver<V, Env> const &&>(*this).set_value(
            std::forward<Args>(args)...);
        loop.finish();
    }
    constexpr auto set_error(auto &&...) const && -> void {
        static_cast<receiver<V, Env> const &&>(*this).set_error();
        loop.finish();
    }
    constexpr auto set_stopped() const && -> void {
        static_cast<receiver<V, Env> const &&>(*this).set_stopped();
        loop.finish();
    }
};

template <typename V, typename Env, typename Uniq>
struct static_receiver : receiver<V, Env> {
    // NOLINTBEGIN(cppcoreguidelines-avoid-const-or-ref-data-members)
    async::detail::synchronizer<Uniq, 0> &sync;
    // NOLINTEND(cppcoreguidelines-avoid-const-or-ref-data-members)

    template <typename... Args>
    constexpr auto set_value(Args &&...args) const && -> void {
        static_cast<receiver<V, Env> const &&>(*this).set_value(
            std::forward<Args>(args)...);
        sync.notify();
    }
    constexpr auto set_error(auto &&...) const && -> void {
        static_cast<receiver<V, Env> const &&>(*this).set_error();
        sync.notify();
    }
    constexpr auto set_stopped() const && -> void {
        static_cast<receiver<V, Env> const &&>(*this).set_stopped();
        sync.notify();
    }
};

namespace detail {
template <typename... Ts>
using decayed_tuple = stdx::tuple<std::remove_cvref_t<Ts>...>;

template <typename S, typename Tag, typename E>
concept single_sender = requires {
    typename async::detail::gather_signatures<
        Tag, completion_signatures_of_t<S, E>, stdx::tuple,
        std::type_identity_t>;
};

template <typename E, sender_in<E> S>
using sync_wait_type = value_types_of_t<S, E, decayed_tuple, std::optional>;
} // namespace detail

struct dynamic_t;
struct static_t;

template <typename Env> struct pipeable_base {
    [[no_unique_address]] Env e;
};

template <typename Uniq, typename Env, typename Flavor> class pipeable;

template <typename Uniq, typename Env>
class pipeable<Uniq, Env, static_t> : public pipeable_base<Env> {
    template <sender S> static auto wait(S &&s, Env const &e) {
        static_assert(detail::single_sender<S, set_value_t, Env>,
                      "sync_wait requires a single set_value completion: "
                      "consider using into_variant");
        static_assert(sender_in<S, Env>,
                      "Sender given to sync_wait_static cannot run with that "
                      "environment: did you mean to use sync_wait_dynamic?");
        async::detail::synchronizer<Uniq, 0> sync{};
        using V = detail::sync_wait_type<Env, S>;
        V values{};
        auto r = static_receiver<V, Env, Uniq>{values, e, sync};

        auto op_state = connect(std::forward<S>(s), r);
        r.signal_start();
        start(op_state);
        sync.wait();
        return values;
    }

    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    [[nodiscard]] friend auto operator|(S &&s, Self &&self) {
        return wait(std::forward<S>(s), std::forward<Self>(self).e);
    }
};

template <typename Uniq, typename Env>
class pipeable<Uniq, Env, dynamic_t> : public pipeable_base<Env> {
    template <sender S> static auto wait(S &&s, Env const &e) {
        run_loop<Uniq> rl{};
        auto sched = rl.get_scheduler();
        auto new_env = env{prop{get_scheduler_t{}, &sched}, e};
        using E = decltype(new_env);
        static_assert(detail::single_sender<S, set_value_t, E>,
                      "sync_wait requires a single set_value completion: "
                      "consider using into_variant");

        using V = detail::sync_wait_type<E, S>;
        V values{};
        auto r = dynamic_receiver<V, E, decltype(rl)>{values,
                                                      std::move(new_env), rl};

        auto op_state = connect(std::forward<S>(s), r);
        r.signal_start();
        start(op_state);
        rl.run();
        return values;
    }

    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    [[nodiscard]] friend auto operator|(S &&s, Self &&self) {
        return wait(std::forward<S>(s), std::forward<Self>(self).e);
    }
};
} // namespace _sync_wait

template <typename Uniq = decltype([] {}), typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] auto sync_wait_dynamic(Env &&e = {})
    -> _sync_wait::pipeable<Uniq, Env, _sync_wait::dynamic_t> {
    return {std::forward<Env>(e)};
}

template <stdx::ct_string Name, typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] auto sync_wait_dynamic(Env &&e = {}) {
    return sync_wait_dynamic<stdx::cts_t<Name>>(
        env{prop{get_debug_interface_t{}, debug::named_interface<Name>{}},
            std::forward<Env>(e)});
}

template <typename Uniq = decltype([] {}), sender S, typename Env = empty_env>
[[nodiscard]] auto sync_wait_dynamic(S &&s, Env &&e = {}) {
    return std::forward<S>(s) | sync_wait_dynamic<Uniq>(std::forward<Env>(e));
}

template <stdx::ct_string Name, sender S, typename Env = empty_env>
[[nodiscard]] auto sync_wait_dynamic(S &&s, Env &&e = {}) {
    return std::forward<S>(s) | sync_wait_dynamic<Name>(std::forward<Env>(e));
}

template <typename Uniq = decltype([] {}), typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] auto sync_wait_static(Env &&e = {})
    -> _sync_wait::pipeable<Uniq, Env, _sync_wait::static_t> {
    return {std::forward<Env>(e)};
}

template <stdx::ct_string Name, typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] auto sync_wait_static(Env &&e = {}) {
    return sync_wait_static<stdx::cts_t<Name>>(
        env{prop{get_debug_interface_t{}, debug::named_interface<Name>{}},
            std::forward<Env>(e)});
}

template <typename Uniq = decltype([] {}), sender S, typename Env = empty_env>
[[nodiscard]] auto sync_wait_static(S &&s, Env &&e = {}) {
    return std::forward<S>(s) | sync_wait_static<Uniq>(std::forward<Env>(e));
}

template <stdx::ct_string Name, sender S, typename Env = empty_env>
[[nodiscard]] auto sync_wait_static(S &&s, Env &&e = {}) {
    return std::forward<S>(s) | sync_wait_static<Name>(std::forward<Env>(e));
}

template <typename Uniq = decltype([] {}), typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] auto sync_wait(Env &&e = {})
    -> _sync_wait::pipeable<Uniq, Env, _sync_wait::static_t> {
    return {std::forward<Env>(e)};
}

template <stdx::ct_string Name, typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] auto sync_wait(Env &&e = {}) {
    return sync_wait<stdx::cts_t<Name>>(
        env{prop{get_debug_interface_t{}, debug::named_interface<Name>{}},
            std::forward<Env>(e)});
}

template <typename Uniq = decltype([] {}), sender S, typename Env = empty_env>
[[nodiscard]] auto sync_wait(S &&s, Env &&e = {}) {
    return std::forward<S>(s) | sync_wait<Uniq>(std::forward<Env>(e));
}

template <stdx::ct_string Name, sender S, typename Env = empty_env>
[[nodiscard]] auto sync_wait(S &&s, Env &&e = {}) {
    return std::forward<S>(s) | sync_wait<Name>(std::forward<Env>(e));
}

struct sync_wait_t;

template <typename... Ts>
struct debug::context_for<_sync_wait::receiver<Ts...>> {
    using tag = sync_wait_t;
    constexpr static auto name = stdx::ct_string{"sync_wait"};
    using children = stdx::type_list<>;
    using type = _sync_wait::receiver<Ts...>;
};

namespace _sync_wait {
template <sender S, typename Env> constexpr auto trivially_waitable() {
    if constexpr (not detail::single_sender<S, set_value_t, Env> or
                  not sender_in<S, Env>) {
        return std::false_type{};
    } else {
        using V = detail::sync_wait_type<Env, S>;
        using O = connect_result_t<S, receiver<V, Env>>;
        return async::synchronous_t<O>{};
    }
}
} // namespace _sync_wait

template <typename S, typename E = empty_env>
concept trivially_sync_waitable = requires {
    { _sync_wait::trivially_waitable<S, E>() } -> std::same_as<std::true_type>;
};
} // namespace async
