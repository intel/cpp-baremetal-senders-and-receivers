#pragma once

#include <conc/concurrency.hpp>

#include <utility>

namespace async {
namespace requeue_policy {
struct immediate {
    template <auto P, typename>
    [[nodiscard]] constexpr static auto get_queue(auto &queues) -> auto & {
        return queues[P];
    }
};

struct deferred {
    template <auto P, typename Mutex>
    [[nodiscard]] constexpr static auto get_queue(auto &queues) {
        return conc::call_in_critical_section<Mutex>(
            [&]() { return std::exchange(queues[P], {}); });
    }
};
} // namespace requeue_policy
} // namespace async
