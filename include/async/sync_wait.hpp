#pragma once

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

template <typename V, typename RL, typename Env> struct receiver {
    using is_receiver = void;

    // NOLINTBEGIN(cppcoreguidelines-avoid-const-or-ref-data-members)
    V &values;
    RL &loop;
    [[no_unique_address]] Env env;
    // NOLINTEND(cppcoreguidelines-avoid-const-or-ref-data-members)

    [[nodiscard]] constexpr auto query(get_env_t) const noexcept { return env; }

    template <typename... Args>
    constexpr auto set_value(Args &&...args) const && -> void {
        debug_signal<"set_value", debug::erased_context_for<receiver>>(env);
        values.emplace(stdx::make_tuple(std::forward<Args>(args)...));
        loop.finish();
    }
    constexpr auto set_error(auto &&...) const && -> void {
        debug_signal<"set_error", debug::erased_context_for<receiver>>(env);
        loop.finish();
    }
    constexpr auto set_stopped() const && -> void {
        debug_signal<"set_stopped", debug::erased_context_for<receiver>>(env);
        loop.finish();
    }
};

namespace detail {
template <typename... Ts>
using decayed_tuple = stdx::tuple<std::remove_cvref_t<Ts>...>;

template <typename E, sender_in<E> S>
using sync_wait_type = value_types_of_t<S, E, decayed_tuple, std::optional>;
} // namespace detail

template <typename Uniq, sender S, typename Env> auto wait(S &&s, Env &&e) {
    run_loop<Uniq> rl{};
    auto sched = rl.get_scheduler();
    auto new_env = env{prop{get_scheduler_t{}, sched}, std::cref(e)};

    using E = decltype(new_env);
    using V = detail::sync_wait_type<E, S>;
    V values{};
    auto r = receiver<V, decltype(rl), E>{values, rl, std::move(new_env)};

    auto op_state = connect(std::forward<S>(s), r);
    debug_signal<"start", debug::erased_context_for<decltype(r)>>(r.env);
    start(op_state);
    rl.run();
    return values;
}

template <typename Uniq, typename Env> struct pipeable {
    [[no_unique_address]] Env e;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    [[nodiscard]] friend auto operator|(S &&s, Self &&self) {
        return wait<Uniq>(std::forward<S>(s), std::forward<Self>(self).e);
    }
};
} // namespace _sync_wait

template <typename Uniq = decltype([] {}), typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] auto sync_wait(Env &&e = {}) -> _sync_wait::pipeable<Uniq, Env> {
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
} // namespace async
