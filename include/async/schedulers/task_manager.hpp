#pragma once

#include <async/schedulers/task_manager_interface.hpp>
#include <conc/concurrency.hpp>

#include <stdx/function_traits.hpp>
#include <stdx/intrusive_forward_list.hpp>
#include <stdx/tuple.hpp>

#include <array>
#include <atomic>
#include <concepts>
#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>

namespace async {
template <typename F> [[nodiscard]] constexpr auto create_priority_task(F &&f) {
    using func_t = std::remove_cvref_t<F>;
    using args_t = stdx::decayed_args_t<func_t, stdx::tuple>;
    return task<func_t, args_t, single_linked_task>{std::forward<F>(f)};
}

namespace detail {
template <typename T>
concept scheduler_hal = requires {
    { T::schedule(priority_t{}) } -> std::same_as<void>;
};
}

template <detail::scheduler_hal S, std::size_t NumPriorities>
class priority_task_manager {
    struct mutex;
    std::array<stdx::intrusive_forward_list<single_linked_task>, NumPriorities>
        task_queues{};
    std::atomic<int> task_count{};

  public:
    auto enqueue_task(single_linked_task &task, priority_t p) -> bool {
        return conc::call_in_critical_section<mutex>([&]() -> bool {
            auto const added = not std::exchange(task.pending, true);
            if (added) {
                ++task_count;
                task_queues[p].push_back(std::addressof(task));
                S::schedule(p);
            }
            return added;
        });
    }

    template <priority_t P>
    auto service_tasks() -> void
        requires(P < NumPriorities)
    {
        auto &q = task_queues[P];
        while (not std::empty(q)) {
            auto &task = q.front();
            conc::call_in_critical_section<mutex>([&]() {
                q.pop_front();
                task.pending = false;
            });
            task.run();
            --task_count;
        }
    }

    [[nodiscard]] auto is_idle() const -> bool { return task_count == 0; }
};
} // namespace async
