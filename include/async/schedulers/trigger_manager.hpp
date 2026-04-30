#pragma once

#include <async/schedulers/requeue_policy.hpp>

#include <stdx/atomic.hpp>
#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/intrusive_list.hpp>
#include <stdx/type_traits.hpp>

#include <conc/concurrency.hpp>

#include <array>
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
    virtual auto cancel() -> void = 0;

    constexpr trigger_task() = default;
    constexpr trigger_task(trigger_task &&) = delete;

  private:
    [[nodiscard]] friend constexpr auto operator==(trigger_task const &lhs,
                                                   trigger_task const &rhs) {
        return std::addressof(lhs) == std::addressof(rhs);
    }
};

namespace detail {
struct runner {
    template <typename... Args>
    static auto complete(auto task, Args &&...args) -> void {
        task->run(std::forward<Args>(args)...);
    }
};

struct canceller {
    template <typename... Args>
    static auto complete(auto task, Args &&...) -> void {
        task->cancel();
    }
};
} // namespace detail

namespace run_policy {
struct one {
    template <typename, typename Completer, typename M, typename... Args>
    static auto run(auto &&tasks, auto &count, Args &&...args) {
        using RQP = requeue_policy::immediate;
        decltype(auto) q = RQP::template get_queue<0, M>(tasks);
        if (auto task = RQP::template pop<M>(q); task) {
            Completer::complete(task, std::forward<Args>(args)...);
            --count;
        }
    }
};

struct all {
    template <typename RQP, typename Completer, typename M, typename... Args>
    static auto run(auto &&tasks, auto &count, Args &&...args) {
        decltype(auto) q = RQP::template get_queue<0, M>(tasks);
        for (auto task = RQP::template pop<M>(q); task;
             task = RQP::template pop<M>(q)) {
            Completer::complete(task, args...);
            --count;
        }
    }
};
} // namespace run_policy

template <typename Name, typename... Args> struct trigger_manager {
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

    template <typename RQP = requeue_policy::deferred,
              typename RunPolicy = run_policy::all,
              typename Completer = detail::runner, typename... As>
    auto run(As &&...args) -> void {
        static_assert((... and std::same_as<Args, std::remove_cvref_t<As>>));
        RunPolicy::template run<RQP, Completer, mutex>(
            tasks, task_count, std::forward<As>(args)...);
    }

    [[nodiscard]] auto empty() const -> bool { return task_count == 0; }
};

template <typename Name, typename... Args>
inline auto triggers = trigger_manager<Name, Args...>{};

template <typename Name, typename RQP = requeue_policy::deferred,
          typename RunPolicy = run_policy::all, typename... Args>
auto run_triggers(Args &&...args) -> void {
    triggers<Name, std::remove_cvref_t<Args>...>.template run<RQP, RunPolicy>(
        std::forward<Args>(args)...);
}

template <stdx::ct_string Name, typename RQP = requeue_policy::deferred,
          typename RunPolicy = run_policy::all, typename... Args>
auto run_triggers(Args &&...args) -> void {
    run_triggers<stdx::cts_t<Name>, RQP, RunPolicy>(
        std::forward<Args>(args)...);
}

template <typename Name, typename... Args> auto cancel_triggers() -> void {
    triggers<Name, Args...>.template run<requeue_policy::deferred, run_policy::all, detail::canceller>();
}

template <stdx::ct_string Name, typename... Args>
auto cancel_triggers() -> void {
    cancel_triggers<stdx::cts_t<Name>, Args...>();
}

template <typename Name, typename RQP = requeue_policy::deferred,
          typename... Args>
auto run_one_trigger(Args &&...args) -> void {
    run_triggers<Name, RQP, run_policy::one>(std::forward<Args>(args)...);
}

template <stdx::ct_string Name, typename RQP = requeue_policy::deferred,
          typename... Args>
auto run_one_trigger(Args &&...args) -> void {
    run_triggers<stdx::cts_t<Name>, RQP, run_policy::one>(
        std::forward<Args>(args)...);
}
} // namespace async
