#include <async/schedulers/task_manager.hpp>

// EXPECT: bound arguments must be trivially copyable and destructible

namespace {
struct hal {
    static auto schedule(async::priority_t p) {}
};
} // namespace

using task_manager_t = async::priority_task_manager<hal, 8>;

struct no_def_dest {
    explicit(true) no_def_dest(int) {}
    ~no_def_dest() {}
};

auto main() -> int {
    [[maybe_unused]] auto task =
        task_manager_t::create_task([](no_def_dest) {});
}
