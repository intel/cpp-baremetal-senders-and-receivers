#include <async/just.hpp>
#include <async/on.hpp>
#include <async/schedulers/time_scheduler.hpp>
#include <async/schedulers/timer_manager.hpp>
#include <async/start_detached.hpp>

#include <chrono>

// EXPECT: did you forget to specialize async::timer_mgr::time_point_for

using namespace std::chrono_literals;

namespace {
struct hal {
    using time_point_t = std::chrono::steady_clock::time_point;
    using task_t = async::timer_task<time_point_t>;

    static auto enable() -> void;
    static auto disable() -> void;
    static auto set_event_time(time_point_t tp) -> void;
    static auto now() -> time_point_t;
};

using timer_manager_t = async::generic_timer_manager<hal>;
} // namespace

template <> inline auto async::injected_timer_manager<> = timer_manager_t{};

auto main() -> int {
    auto s = async::time_scheduler{10ms};
    [[maybe_unused]] auto x =
        async::on(s, async::just(42)) | async::start_detached();
}
