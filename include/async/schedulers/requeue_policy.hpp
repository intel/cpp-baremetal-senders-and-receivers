#pragma once

#include <stdx/compiler.hpp>
#include <stdx/concepts.hpp>

#include <conc/concurrency.hpp>

#include <iterator>
#include <utility>

namespace async {
namespace requeue_policy {
struct immediate {
    template <auto P, typename>
    [[nodiscard]] constexpr static auto get_queue(auto &queues LIFETIMEBOUND)
        -> auto & {
        return queues[P];
    }

    template <typename Mutex, typename Q>
    [[nodiscard]] constexpr static auto pop(Q &q) -> typename Q::pointer {
        return conc::call_in_critical_section<Mutex>(
            [&]() -> typename Q::pointer {
                if (not std::empty(q)) {
                    auto task = q.pop_front();
                    task->pending = false;
                    return task;
                }
                return nullptr;
            });
    }

    template <typename Mutex, typename Q,
              stdx::predicate<typename Q::pointer> F>
    [[nodiscard]] constexpr static auto pop_if(Q &q, F &&f) ->
        typename Q::pointer {
        auto task =
            conc::call_in_critical_section<Mutex>([&]() -> typename Q::pointer {
                if (not std::empty(q)) {
                    auto t = q.pop_front();
                    t->pending = false;
                    return t;
                }
                return nullptr;
            });
        if (task and not std::forward<F>(f)(task)) {
            conc::call_in_critical_section<Mutex>([&] {
                task->pending = true;
                q.push_front(task);
            });
        }
        return task;
    }
};

struct deferred {
    template <auto P, typename Mutex>
    [[nodiscard]] constexpr static auto get_queue(auto &queues) {
        return conc::call_in_critical_section<Mutex>(
            [&] { return std::exchange(queues[P], {}); });
    }

    template <typename Mutex, typename Q>
    [[nodiscard]] constexpr static auto pop(Q &q) -> typename Q::pointer {
        if (not std::empty(q)) {
            auto task = q.pop_front();
            conc::call_in_critical_section<Mutex>(
                [&] { task->pending = false; });
            return task;
        }
        return nullptr;
    }

    template <typename Mutex, typename Q,
              stdx::predicate<typename Q::pointer> F>
    [[nodiscard]] constexpr static auto pop_if(Q &q, F &&f, Q &defer_q) ->
        typename Q::pointer {
        if (not std::empty(q)) {
            auto task = q.pop_front();
            conc::call_in_critical_section<Mutex>(
                [&] { task->pending = false; });
            if (not std::forward<F>(f)(task)) {
                conc::call_in_critical_section<Mutex>([&] {
                    task->pending = true;
                    defer_q.push_back(task);
                });
            }
            return task;
        }
        return nullptr;
    }
};
} // namespace requeue_policy
} // namespace async
