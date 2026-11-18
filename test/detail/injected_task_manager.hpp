#include <async/schedulers/task_manager.hpp>

struct hal {
    static auto schedule(async::priority_t) {}
};

using task_manager_t = async::priority_task_manager<hal, 8>;

template <> inline auto async::injected_task_manager<> = task_manager_t{};
