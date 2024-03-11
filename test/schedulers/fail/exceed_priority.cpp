#include <async/just.hpp>
#include <async/on.hpp>
#include <async/schedulers/priority_scheduler.hpp>
#include <async/schedulers/task_manager.hpp>

// EXPECT: fixed_priority_scheduler has invalid priority

namespace {
struct hal {
    static auto schedule(async::priority_t) {}
};

using task_manager_t = async::priority_task_manager<hal, 8>;
} // namespace

template <> inline auto async::injected_task_manager<> = task_manager_t{};

auto main() -> int {
    auto s = async::fixed_priority_scheduler<8>{};
    [[maybe_unused]] async::sender auto sndr = async::on(s, async::just(42));
}
