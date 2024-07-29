#pragma once

#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/get_scheduler.hpp>
#include <async/schedulers/runloop_scheduler.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/tuple.hpp>

#include <concepts>
#include <optional>
#include <type_traits>
#include <utility>

namespace async {
namespace _sync_wait {

template <typename V, typename RL> struct receiver {
    using is_receiver = void;

    // NOLINTBEGIN(cppcoreguidelines-avoid-const-or-ref-data-members)
    V &values;
    RL &loop;
    // NOLINTEND(cppcoreguidelines-avoid-const-or-ref-data-members)

    [[nodiscard]] constexpr auto query(get_env_t) const noexcept {
        return prop{get_scheduler_t{}, loop.get_scheduler()};
    }

    template <typename... Args>
    constexpr auto set_value(Args &&...args) const && -> void {
        values.emplace(stdx::make_tuple(std::forward<Args>(args)...));
        loop.finish();
    }
    constexpr auto set_error(auto &&...) const && -> void { loop.finish(); }
    constexpr auto set_stopped() const && -> void { loop.finish(); }
};

namespace detail {
template <typename... Ts>
using decayed_tuple = stdx::tuple<std::remove_cvref_t<Ts>...>;

template <typename E, sender_in<E> S>
using sync_wait_type = value_types_of_t<S, E, decayed_tuple, std::optional>;
} // namespace detail

template <typename Uniq, sender S> auto wait(S &&s) {
    run_loop<Uniq> rl{};
    auto sched = rl.get_scheduler();
    using E = prop<get_scheduler_t, decltype(sched)>;

    using V = detail::sync_wait_type<E, S>;
    V values{};
    auto r = receiver<V, decltype(rl)>{values, rl};

    auto op_state = connect(std::forward<S>(s), r);
    start(op_state);
    rl.run();
    return values;
}

template <typename Uniq> struct pipeable {
  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend auto operator|(S &&s, Self &&) {
        return wait<Uniq>(std::forward<S>(s));
    }
};
} // namespace _sync_wait

template <typename Uniq = decltype([] {}), sender S>
[[nodiscard]] auto sync_wait(S &&s) {
    return _sync_wait::wait<Uniq>(std::forward<S>(s));
}

template <typename Uniq = decltype([] {})>
[[nodiscard]] constexpr auto sync_wait() -> _sync_wait::pipeable<Uniq> {
    return {};
}
} // namespace async
