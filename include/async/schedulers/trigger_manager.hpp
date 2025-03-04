#pragma once

#include <async/schedulers/requeue_policy.hpp>

#include <stdx/atomic.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/intrusive_list.hpp>
#include <stdx/type_traits.hpp>

#include <conc/concurrency.hpp>

#include <array>
#include <concepts>
#include <iterator>
#include <memory>
#include <utility>

namespace async {
// NOLINTNEXTLINE(*-special-member-functions)
template <typename... Args> struct trigger_task {
    bool pending{};
    trigger_task *prev{};
    trigger_task *next{};

    virtual auto run(Args const &...) -> void = 0;

    constexpr trigger_task() = default;
    constexpr trigger_task(trigger_task &&) = delete;
    virtual ~trigger_task() = default;

  private:
    [[nodiscard]] friend constexpr auto operator==(trigger_task const &lhs,
                                                   trigger_task const &rhs) {
        return std::addressof(lhs) == std::addressof(rhs);
    }
};

template <stdx::ct_string Name, typename... Args> struct trigger_manager {
    using task_t = trigger_task<Args...>;

  private:
    struct mutex;
    std::array<stdx::intrusive_list<task_t>, 1> tasks{};
    stdx::atomic<int> task_count;

  public:
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

    auto dequeue(task_t &t) -> bool {
        return conc::call_in_critical_section<mutex>([&]() -> bool {
            if (std::exchange(t.pending, false)) {
                tasks[0].remove(std::addressof(t));
                --task_count;
                return true;
            }
            return false;
        });
    }

    template <typename RQP = requeue_policy::deferred>
    auto run(Args const &...args) -> void {
        decltype(auto) q = RQP::template get_queue<0, mutex>(tasks);
        while (not std::empty(q)) {
            auto &task = q.front();
            conc::call_in_critical_section<mutex>([&]() {
                q.pop_front();
                task.pending = false;
            });
            task.run(args...);
            --task_count;
        }
    }

    [[nodiscard]] auto empty() const -> bool { return task_count == 0; }
};

template <stdx::ct_string Name, typename... Args>
inline auto triggers = trigger_manager<Name, Args...>{};

template <stdx::ct_string Name, typename RQP = requeue_policy::deferred,
          typename... Args>
auto run_triggers(Args &&...args) -> void {
    triggers<Name, std::remove_cvref_t<Args>...>.template run<RQP>(
        std::forward<Args>(args)...);
}
} // namespace async
