#pragma once

#include <conc/concurrency.hpp>

#include <iterator>
#include <utility>

namespace async {
namespace requeue_policy {
struct immediate {
    template <auto P, typename>
    [[nodiscard]] constexpr static auto get_queue(auto &queues) -> auto & {
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
};
} // namespace requeue_policy
} // namespace async
