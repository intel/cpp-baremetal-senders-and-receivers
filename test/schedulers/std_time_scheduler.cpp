#include "detail/common.hpp"

#include <async/just_result_of.hpp>
#include <async/schedulers/time_scheduler.hpp>
#include <async/schedulers/timer_manager.hpp>
#include <async/start_on.hpp>

#include <catch2/catch_test_macros.hpp>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <mutex>
#include <thread>

using namespace std::chrono_literals;

namespace {
struct std_hal {
    using time_point_t = std::chrono::steady_clock::time_point;
    using task_t = async::timer_task<time_point_t>;

    static inline std::mutex m{};
    static inline std::condition_variable cv{};
    static inline std::thread t{};
    static inline time_point_t next_wakeup{};
    static inline std::atomic<bool> running{};
    static inline std::atomic<bool> stopping{};

    static auto enable() -> void {
        if (not running.exchange(true)) {
            t = std::thread{[] {
                while (not stopping) {
                    auto next = [&] {
                        std::unique_lock lock{m};
                        cv.wait_until(lock, next_wakeup);
                        return next_wakeup;
                    }();
                    if (not stopping and now() >= next) {
                        async::timer_mgr::service_task();
                    }
                }
            }};
        }
    }
    static auto disable() -> void { set_event_time(time_point_t::max()); }
    static auto set_event_time(time_point_t tp) -> void {
        {
            std::lock_guard lock{m};
            next_wakeup = tp;
        }
        cv.notify_one();
    }
    static auto now() -> time_point_t {
        return std::chrono::steady_clock::now();
    }

    static auto stop() {
        stopping = true;
        cv.notify_one();
        t.join();
        stopping = false;
        running = false;
    }
};

using std_timer_manager_t = async::generic_timer_manager<std_hal>;
} // namespace

template <typename Rep, typename Period>
struct async::timer_mgr::time_point_for<std::chrono::duration<Rep, Period>> {
    using type = std::chrono::steady_clock::time_point;
};

template <>
[[maybe_unused]] inline auto async::injected_timer_manager<> =
    std_timer_manager_t{};

TEST_CASE("std domain time_scheduler", "[time_scheduler]") {
    auto s = async::time_scheduler{10ms};

    std::mutex m{};
    std::condition_variable cv{};
    int var{};
    async::sender auto sndr = async::start_on(s, async::just_result_of([&] {
                                                  {
                                                      std::lock_guard lock{m};
                                                      var = 42;
                                                  }
                                                  cv.notify_one();
                                              }));
    auto op = async::connect(sndr, universal_receiver{});
    async::start(op);
    {
        std::unique_lock lock{m};
        cv.wait(lock, [&] { return var == 42; });
    }
    std_hal::stop();
}
