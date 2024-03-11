#pragma once

#include <async/schedulers/task.hpp>

#include <stdx/intrusive_forward_list.hpp>
#include <stdx/type_traits.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
using priority_t = std::uint8_t;

template <typename T>
concept prioritizable_task = stdx::single_linkable<T> and
                             std::equality_comparable<T> and requires(T *t) {
                                 { t->run() } -> std::same_as<void>;
                                 { t->pending } -> std::same_as<bool &>;
                             };

template <typename T>
concept task_manager = prioritizable_task<typename T::task_t> and
                       requires(T &t, typename T::task_t &task) {
                           {
                               t.enqueue_task(task, priority_t{})
                           } -> std::convertible_to<bool>;
                           {
                               t.template valid_priority<priority_t{}>()
                           } -> std::convertible_to<bool>;
                           {
                               t.template service_tasks<priority_t{}>()
                           } -> std::same_as<void>;
                           { t.is_idle() } -> std::convertible_to<bool>;
                       };

namespace detail {
struct undefined_task_manager {
    struct task_t {
        task_t *next{};
        bool pending{};
        auto run() -> void {}

      private:
        friend constexpr auto operator==(task_t const &, task_t const &)
            -> bool = default;
    };

    template <typename... Args>
    constexpr static auto enqueue_task(Args &&...) -> bool {
        static_assert(stdx::always_false_v<Args...>,
                      "Inject a task manager by specializing "
                      "async::injected_task_manager.");
        return false;
    }

    template <priority_t P> constexpr static auto valid_priority() -> bool {
        static_assert(
            stdx::always_false_v<std::integral_constant<priority_t, P>>,
            "Inject a task manager by specializing "
            "async::injected_task_manager.");
        return false;
    }

    template <priority_t P> constexpr static auto service_tasks() -> void {
        static_assert(
            stdx::always_false_v<std::integral_constant<priority_t, P>>,
            "Inject a task manager by specializing "
            "async::injected_task_manager.");
    }

    template <typename... Args>
    constexpr static auto is_idle(Args &&...) -> bool {
        static_assert(stdx::always_false_v<Args...>,
                      "Inject a task manager by specializing "
                      "async::injected_task_manager.");
        return true;
    }
};
static_assert(task_manager<undefined_task_manager>);
} // namespace detail

template <typename...>
inline auto injected_task_manager = detail::undefined_task_manager{};

namespace task_mgr {
namespace detail {
template <typename... DummyArgs, typename... Args>
    requires(sizeof...(DummyArgs) == 0)
auto enqueue_task(Args &&...args) -> bool {
    return injected_task_manager<DummyArgs...>.enqueue_task(
        std::forward<Args>(args)...);
}

template <priority_t P, typename... DummyArgs>
    requires(sizeof...(DummyArgs) == 0)
constexpr auto valid_priority() -> bool {
    return injected_task_manager<DummyArgs...>.template valid_priority<P>();
}
} // namespace detail

template <priority_t P, typename... DummyArgs>
    requires(sizeof...(DummyArgs) == 0)
auto service_tasks() -> void {
    return injected_task_manager<DummyArgs...>.template service_tasks<P>();
}

template <typename... DummyArgs>
    requires(sizeof...(DummyArgs) == 0)
auto is_idle() -> bool {
    return injected_task_manager<DummyArgs...>.is_idle();
}
} // namespace task_mgr

using priority_task = single_linked_task<task_base>;
} // namespace async
