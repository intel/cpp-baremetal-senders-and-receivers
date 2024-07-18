#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/schedulers/time_scheduler.hpp>
#include <async/schedulers/timer_manager.hpp>

namespace {
struct hal {
    using time_point_t = int;
    using task_t = async::timer_task<time_point_t>;

    static auto enable() -> void;
    static auto disable() -> void;
    static auto set_event_time(time_point_t tp) -> void;
    static auto now() -> time_point_t;
};

using timer_manager_t = async::generic_timer_manager<hal>;
} // namespace

template <> inline auto async::injected_timer_manager<> = timer_manager_t{};

struct fail_receiver {
    using is_receiver = void;
};
static_assert(async::receiver<fail_receiver>);

auto main() -> int {
    [[maybe_unused]] auto op =
        async::connect(async::time_scheduler{0}.schedule(), fail_receiver{});
}
