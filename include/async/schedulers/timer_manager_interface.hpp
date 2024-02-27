#pragma once

#include <async/schedulers/task.hpp>

#include <stdx/intrusive_list.hpp>
#include <stdx/type_traits.hpp>

#include <concepts>
#include <functional>

namespace async {
template <typename T>
concept timeable_task =
    stdx::double_linkable<T> and std::strict_weak_order<std::less<>, T, T> and
    requires(T *t) {
        { t->run() } -> std::same_as<void>;
        { t->pending } -> std::same_as<bool &>;
        t->expiration_time;
    };

template <typename T>
concept timer_manager =
    timeable_task<typename T::task_t> and
    requires(T &t, typename T::task_t &task, typename T::duration_t d) {
        { t.run_after(task, d) } -> std::convertible_to<bool>;
        { t.service_task() } -> std::same_as<void>;
        { t.is_idle() } -> std::convertible_to<bool>;
    };

namespace detail {
struct undefined_timer_manager {
    struct task_t {
        task_t *next{};
        task_t *prev{};
        bool pending{};
        int expiration_time{};
        auto run() -> void {}

      private:
        [[nodiscard]] friend constexpr auto operator<(task_t const &lhs,
                                                      task_t const &rhs) {
            return lhs.expiration_time < rhs.expiration_time;
        }
    };
    using duration_t = int;

    template <typename... Args> static auto run_after(Args &&...) -> bool {
        static_assert(stdx::always_false_v<Args...>,
                      "Inject a timer manager by specializing "
                      "async::injected_timer_manager.");
        return false;
    }

    template <typename... Args> static auto service_task(Args &&...) -> void {
        static_assert(stdx::always_false_v<Args...>,
                      "Inject a timer manager by specializing "
                      "async::injected_timer_manager.");
    }

    template <typename... Args> static auto is_idle(Args &&...) -> bool {
        static_assert(
            stdx::always_false_v<Args...>,
            "Inject a timer by specializing async::injected_timer_manager.");
        return true;
    }
};
static_assert(timer_manager<undefined_timer_manager>);
} // namespace detail

template <typename...>
inline auto injected_timer_manager = detail::undefined_timer_manager{};

namespace timer_mgr {
namespace detail {
template <typename... DummyArgs, typename... Args>
    requires(sizeof...(DummyArgs) == 0)
auto run_after(Args &&...args) -> bool {
    return injected_timer_manager<DummyArgs...>.run_after(
        std::forward<Args>(args)...);
}
} // namespace detail

template <typename... DummyArgs>
    requires(sizeof...(DummyArgs) == 0)
auto service_task() -> void {
    return injected_timer_manager<DummyArgs...>.template service_task();
}

template <typename... DummyArgs>
    requires(sizeof...(DummyArgs) == 0)
auto is_idle() -> bool {
    return injected_timer_manager<DummyArgs...>.is_idle();
}
} // namespace timer_mgr
} // namespace async
