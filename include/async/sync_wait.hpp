#pragma once

#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
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

template <typename Sched> struct env {
    [[nodiscard]] constexpr auto
    query(get_scheduler_t) const noexcept -> Sched {
        return s;
    }

    Sched s;
};

template <typename V, typename RL> struct receiver {
    using is_receiver = void;

    // NOLINTBEGIN(cppcoreguidelines-avoid-const-or-ref-data-members)
    V &values;
    RL &loop;
    // NOLINTEND(cppcoreguidelines-avoid-const-or-ref-data-members)

    [[nodiscard]] constexpr auto query(get_env_t) const noexcept
        -> env<decltype(std::declval<RL>().get_scheduler())> {
        return {loop.get_scheduler()};
    }

  private:
    template <typename... Args>
    friend auto tag_invoke(set_value_t, receiver const &r,
                           Args &&...args) -> void {
        r.values.emplace(stdx::make_tuple(std::forward<Args>(args)...));
        r.loop.finish();
    }
    friend auto tag_invoke(set_error_t, receiver const &r, auto &&...) -> void {
        r.loop.finish();
    }
    friend auto tag_invoke(set_stopped_t, receiver const &r) -> void {
        r.loop.finish();
    }
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
    env<decltype(sched)> e{sched};

    using V = detail::sync_wait_type<decltype(e), S>;
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
