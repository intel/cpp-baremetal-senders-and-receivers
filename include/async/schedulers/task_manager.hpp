#pragma once

#include <async/schedulers/requeue_policy.hpp>
#include <async/schedulers/task_manager_interface.hpp>
#include <conc/concurrency.hpp>

#include <stdx/atomic.hpp>
#include <stdx/function_traits.hpp>
#include <stdx/intrusive_forward_list.hpp>
#include <stdx/tuple.hpp>

#include <array>
#include <concepts>
#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>

namespace async {
namespace detail {
template <typename T>
concept scheduler_hal = requires {
    { T::schedule(priority_t{}) } -> std::same_as<void>;
};
} // namespace detail

namespace archetypes {
struct scheduler_hal {
    constexpr static auto schedule(priority_t) -> void {}
};
} // namespace archetypes
static_assert(detail::scheduler_hal<archetypes::scheduler_hal>);

template <detail::scheduler_hal S, std::size_t NumPriorities,
          prioritizable_task Task = priority_task>
struct priority_task_manager {
    using task_t = Task;

  private:
    struct mutex;
    std::array<stdx::intrusive_forward_list<task_t>, NumPriorities>
        task_queues{};
    stdx::atomic<int> task_count{};

  public:
    constexpr static auto create_task = async::create_task<task_t>;

    auto enqueue_task(task_t &t, priority_t p) -> bool {
        return conc::call_in_critical_section<mutex>([&]() -> bool {
            auto const added = not std::exchange(t.pending, true);
            if (added) {
                ++task_count;
                task_queues[p].push_back(std::addressof(t));
                S::schedule(p);
            }
            return added;
        });
    }

    template <priority_t P> constexpr static auto valid_priority() -> bool {
        return P < NumPriorities;
    }

    template <priority_t P, typename RQP = requeue_policy::deferred>
    auto service_tasks() -> void
        requires(valid_priority<P>())
    {
        decltype(auto) q = RQP::template get_queue<P, mutex>(task_queues);
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
static_assert(
    task_manager<priority_task_manager<archetypes::scheduler_hal, 16>>);
} // namespace async
