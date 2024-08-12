#pragma once

#include <async/schedulers/requeue_policy.hpp>
#include <async/schedulers/task.hpp>
#include <conc/concurrency.hpp>

#include <stdx/ct_string.hpp>
#include <stdx/intrusive_forward_list.hpp>
#include <stdx/type_traits.hpp>

#include <array>
#include <atomic>
#include <concepts>
#include <iterator>
#include <memory>
#include <utility>

namespace async {
template <typename T>
concept triggerable_task = stdx::single_linkable<T> and
                           std::equality_comparable<T> and requires(T *t) {
                               { t->run() } -> std::same_as<void>;
                               { t->pending } -> std::same_as<bool &>;
                           };

using trigger_task = single_linked_task<task_base>;

template <stdx::ct_string Name, triggerable_task Task = trigger_task>
struct trigger_manager {
    using task_t = Task;

  private:
    struct mutex;
    std::array<stdx::intrusive_forward_list<task_t>, 1> tasks{};
    std::atomic<int> task_count{};

  public:
    constexpr static auto create_trigger = async::create_task<task_t>;

    auto enqueue(task_t &t) -> bool {
        return conc::call_in_critical_section<mutex>([&]() -> bool {
            auto const added = not std::exchange(t.pending, true);
            if (added) {
                ++task_count;
                tasks[0].push_back(std::addressof(t));
            }
            return added;
        });
    }

    template <typename RQP = requeue_policy::deferred> auto run() -> void {
        decltype(auto) q = RQP::template get_queue<0, mutex>(tasks);
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

    [[nodiscard]] auto empty() const -> bool { return task_count == 0; }
};

template <stdx::ct_string Name, triggerable_task Task = trigger_task>
auto triggers = trigger_manager<Name, Task>{};
} // namespace async
