#include <async/schedulers/timer_manager.hpp>

#include <catch2/catch_test_macros.hpp>

#include <functional>
#include <iterator>
#include <utility>
#include <vector>

namespace {
struct hal {
    using time_point_t = int;
    using task_t = async::timer_task<time_point_t>;

    static inline time_point_t current_time{};
    static inline bool enabled{};
    static inline std::vector<time_point_t> calls{};

    static auto enable() -> void { enabled = true; }
    static auto disable() -> void { enabled = false; }
    static auto set_event_time(time_point_t tp) -> void { calls.push_back(tp); }
    static auto now() -> time_point_t { return current_time; }
};

using timer_manager_t = async::generic_timer_manager<hal>;

std::function<void()> interrupt_fn{};

struct test_concurrency_policy {
    struct interrupt {
        ~interrupt() {
            if (interrupt_fn) {
                interrupt_fn();
            }
        }
    };

    template <typename = void, typename F, typename... Pred>
    static auto call_in_critical_section(F &&f, Pred &&...) -> decltype(auto) {
        [[maybe_unused]] interrupt raii_interrupt{};
        return std::forward<F>(f)();
    }
};
} // namespace

template <> inline auto conc::injected_policy<> = test_concurrency_policy{};

TEST_CASE("create task with lvalue", "[timer_manager]") {
    int var{};
    auto l = [&] { var = 42; };
    auto task = timer_manager_t::create_task(l);
    task.run();
    CHECK(var == 42);
}

TEST_CASE("create task with rvalue", "[timer_manager]") {
    int var{};
    auto task = timer_manager_t::create_task([&] { var = 42; });
    task.run();
    CHECK(var == 42);
}

TEST_CASE("run a task with bound args", "[timer_manager]") {
    int var{};
    auto task = timer_manager_t::create_task([&](int x) { var = x; });
    task.bind_front(42).run();
    CHECK(var == 42);
}

TEST_CASE("tasks have reference equality", "[timer_manager]") {
    auto t = timer_manager_t::create_task([] {});
    auto u = timer_manager_t::create_task([] {});
    CHECK(t == t);
    CHECK(t != u);
}

TEST_CASE("nothing pending", "[timer_manager]") {
    auto m = timer_manager_t{};
    CHECK(m.is_idle());
}

TEST_CASE("queue a task", "[timer_manager]") {
    hal::calls.clear();
    auto t = timer_manager_t::create_task([] {});

    auto m = timer_manager_t{};
    m.run_after(t, 3);
    CHECK(not m.is_idle());
    REQUIRE(hal::calls.size() == 1);
    CHECK(hal::calls[0] == 3);
}

TEST_CASE("run a queued task", "[timer_manager]") {
    hal::calls.clear();

    auto m = timer_manager_t{};
    int var{};
    auto t = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t, 3);
    CHECK(not m.is_idle());
    REQUIRE(hal::calls.size() == 1);
    CHECK(hal::calls[0] == 3);
    m.service_task();
    CHECK(var == 42);
    CHECK(m.is_idle());
}

TEST_CASE("queueing a queued task fails", "[timer_manager]") {
    hal::calls.clear();

    auto m = timer_manager_t{};
    int var{};
    auto task = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(task, 3);
    CHECK(not m.run_after(task, 4));
    REQUIRE(hal::calls.size() == 1);
    m.service_task();
    CHECK(var == 42);
    CHECK(m.is_idle());
}

TEST_CASE("run tasks in time order", "[timer_manager]") {
    auto m = timer_manager_t{};
    int var{};
    auto t1 = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t1, 3);
    auto t2 = timer_manager_t::create_task([&] { var = 17; });
    m.run_after(t2, 2);

    m.service_task();
    CHECK(var == 17);
    CHECK(not m.is_idle());
    CHECK(not t2.pending);

    m.service_task();
    CHECK(var == 42);
    CHECK(m.is_idle());
    CHECK(not t1.pending);
}

TEST_CASE("run tasks with same expiry time in FIFO order", "[timer_manager]") {
    auto m = timer_manager_t{};
    int var{};
    auto t1 = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t1, 3);
    auto t2 = timer_manager_t::create_task([&] { var = 17; });
    m.run_after(t2, 3);

    m.service_task();
    CHECK(var == 42);
    CHECK(not m.is_idle());
    CHECK(not t1.pending);

    m.service_task();
    CHECK(var == 17);
    CHECK(m.is_idle());
    CHECK(not t2.pending);
}

TEST_CASE("manager is not idle during a running task", "[timer_manager]") {
    auto m = timer_manager_t{};
    int var{};
    auto task = timer_manager_t::create_task([&] {
        CHECK(not m.is_idle());
        var = 42;
    });
    m.run_after(task, 1);
    m.service_task();
    CHECK(var == 42);
    CHECK(m.is_idle());
}

TEST_CASE("task can reschedule itself", "[timer_manager]") {
    auto m = timer_manager_t{};
    int var{};
    auto task = timer_manager_t::create_task([&](hal::task_t *t) {
        CHECK(m.run_after(*t, 1));
        ++var;
    });
    m.run_after(task.bind_front(&task), 1);

    m.service_task();
    CHECK(var == 1);
    CHECK(task.pending);
    CHECK(not m.is_idle());

    m.service_task();
    CHECK(var == 2);
    CHECK(task.pending);
    CHECK(not m.is_idle());
}

TEST_CASE("cancel a scheduled task", "[timer_manager]") {
    auto m = timer_manager_t{};
    int var{};
    auto task = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(task, 1);

    CHECK(m.cancel(task));
    CHECK(not task.pending);
    CHECK(m.is_idle());
}

TEST_CASE("cancelling an unscheduled task fails", "[timer_manager]") {
    auto m = timer_manager_t{};
    int var{};
    auto task = timer_manager_t::create_task([&] { var = 42; });
    CHECK(not m.cancel(task));
    CHECK(m.is_idle());
}

TEST_CASE("cancel with more tasks outstanding", "[timer_manager]") {
    auto m = timer_manager_t{};
    int var{};
    auto t1 = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t1, 3);
    auto t2 = timer_manager_t::create_task([&] { var = 17; });
    m.run_after(t2, 4);

    m.cancel(t1);
    CHECK(not m.is_idle());
    CHECK(not t1.pending);

    m.service_task();
    CHECK(var == 17);
    CHECK(m.is_idle());
    CHECK(not t2.pending);
}

TEST_CASE("cancel during task run", "[timer_manager]") {
    auto m = timer_manager_t{};
    int var{};
    auto task = timer_manager_t::create_task([&](hal::task_t *t) {
        CHECK(not m.cancel(*t));
        var = 42;
    });
    m.run_after(task.bind_front(&task), 1);

    m.service_task();
    CHECK(var == 42);
    CHECK(not task.pending);
    CHECK(m.is_idle());
}

TEST_CASE("interrupt is enabled when a task is queued", "[timer_manager]") {
    hal::enabled = false;
    auto m = timer_manager_t{};
    int var{};
    auto t = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t, 3);
    CHECK(hal::enabled);
}

TEST_CASE("interrupt is disabled after running all tasks", "[timer_manager]") {
    hal::enabled = false;
    auto m = timer_manager_t{};
    int var{};
    auto t = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t, 3);
    CHECK(hal::enabled);
    m.service_task();
    CHECK(not hal::enabled);
}

TEST_CASE("interrupt is disabled after cancelling last task",
          "[timer_manager]") {
    hal::enabled = false;
    auto m = timer_manager_t{};
    int var{};
    auto t = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t, 3);
    CHECK(hal::enabled);
    m.cancel(t);
    CHECK(not hal::enabled);
}

TEST_CASE("interrupt is set for nearest time (updated)", "[timer_manager]") {
    hal::calls.clear();

    auto m = timer_manager_t{};
    int var{};
    auto t1 = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t1, 3);
    REQUIRE(not std::empty(hal::calls));
    CHECK(hal::calls.back() == 3);
    auto t2 = timer_manager_t::create_task([&] { var = 17; });
    m.run_after(t2, 2);
    CHECK(hal::calls.back() == 2);
}

TEST_CASE("interrupt is set for nearest time (not updated)",
          "[timer_manager]") {
    hal::calls.clear();

    auto m = timer_manager_t{};
    int var{};
    auto t1 = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t1, 3);
    REQUIRE(not std::empty(hal::calls));
    CHECK(hal::calls.back() == 3);
    auto t2 = timer_manager_t::create_task([&] { var = 17; });
    m.run_after(t2, 4);
    CHECK(hal::calls.back() == 3);
}

TEST_CASE("interrupt is reset for nearest time after task is run",
          "[timer_manager]") {
    hal::calls.clear();

    auto m = timer_manager_t{};
    int var{};
    auto t1 = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t1, 3);
    auto t2 = timer_manager_t::create_task([&] { var = 17; });
    m.run_after(t2, 4);

    REQUIRE(not std::empty(hal::calls));
    CHECK(hal::calls.back() == 3);
    m.service_task();
    CHECK(hal::calls.back() == 4);
}

TEST_CASE("interrupt is reset for nearest time after task is cancelled",
          "[timer_manager]") {
    hal::calls.clear();

    auto m = timer_manager_t{};
    int var{};
    auto t1 = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t1, 3);
    auto t2 = timer_manager_t::create_task([&] { var = 17; });
    m.run_after(t2, 4);

    REQUIRE(not std::empty(hal::calls));
    CHECK(hal::calls.back() == 3);
    m.cancel(t1);
    CHECK(hal::calls.back() == 4);
}

TEST_CASE("queue a task on interrupt during servicing", "[timer_manager]") {
    hal::enabled = false;
    auto m = timer_manager_t{};
    int var{};
    auto t = timer_manager_t::create_task([&] { var = 42; });
    m.run_after(t, 1);

    interrupt_fn = [&] {
        interrupt_fn = {};
        m.run_after(t, 2);
    };

    CHECK(hal::enabled);
    m.service_task();
    CHECK(var == 42);
    CHECK(not m.is_idle());
    REQUIRE(not std::empty(hal::calls));
    CHECK(hal::calls.back() == 2);
    CHECK(hal::enabled);
}

namespace {
struct interaction_hal {
    using time_point_t = int;
    using task_t = async::timer_task<time_point_t>;

    enum struct call_type { enable, disable, set_event_time, now };

    static inline time_point_t current_time{};
    static inline std::vector<call_type> calls{};

    static auto enable() -> void { calls.push_back(call_type::enable); }
    static auto disable() -> void { calls.push_back(call_type::disable); }
    static auto set_event_time(time_point_t) -> void {
        calls.push_back(call_type::set_event_time);
    }
    static auto now() -> time_point_t {
        calls.push_back(call_type::now);
        return current_time;
    }
};

using interaction_manager_t = async::generic_timer_manager<interaction_hal>;
} // namespace

TEST_CASE("HAL interaction starts with enable", "[timer_manager]") {
    interaction_hal::calls.clear();

    auto m = interaction_manager_t{};
    int var{};
    auto t = interaction_manager_t::create_task([&] { var = 42; });
    m.run_after(t, 3);
    REQUIRE(interaction_hal::calls.size() == 3);
    CHECK(interaction_hal::calls[0] == interaction_hal::call_type::enable);
    CHECK(interaction_hal::calls[1] == interaction_hal::call_type::now);
    CHECK(interaction_hal::calls[2] ==
          interaction_hal::call_type::set_event_time);
}

TEST_CASE("HAL interaction calls now for later task", "[timer_manager]") {
    interaction_hal::calls.clear();

    auto m = interaction_manager_t{};
    int var{};
    auto t1 = interaction_manager_t::create_task([&] { var = 42; });
    m.run_after(t1, 3);
    auto t2 = interaction_manager_t::create_task([&] { var = 42; });
    m.run_after(t2, 4);
    REQUIRE(interaction_hal::calls.size() == 4);
    CHECK(interaction_hal::calls[3] == interaction_hal::call_type::now);
}

TEST_CASE("HAL interaction calls now and set_event_time for earlier task",
          "[timer_manager]") {
    interaction_hal::calls.clear();

    auto m = interaction_manager_t{};
    int var{};
    auto t1 = interaction_manager_t::create_task([&] { var = 42; });
    m.run_after(t1, 3);
    auto t2 = interaction_manager_t::create_task([&] { var = 42; });
    m.run_after(t2, 2);
    REQUIRE(interaction_hal::calls.size() == 5);
    CHECK(interaction_hal::calls[3] == interaction_hal::call_type::now);
    CHECK(interaction_hal::calls[4] ==
          interaction_hal::call_type::set_event_time);
}

TEST_CASE("HAL interaction calls set_event_time for next task",
          "[timer_manager]") {
    interaction_hal::calls.clear();

    auto m = interaction_manager_t{};
    int var{};
    auto t1 = interaction_manager_t::create_task([&] { var = 42; });
    m.run_after(t1, 3);
    auto t2 = interaction_manager_t::create_task([&] { var = 42; });
    m.run_after(t2, 4);
    m.service_task();
    REQUIRE(interaction_hal::calls.size() == 5);
    CHECK(interaction_hal::calls[4] ==
          interaction_hal::call_type::set_event_time);
}

TEST_CASE("HAL interaction ends with disable", "[timer_manager]") {
    interaction_hal::calls.clear();

    auto m = interaction_manager_t{};
    int var{};
    auto t = interaction_manager_t::create_task([&] { var = 42; });
    m.run_after(t, 3);
    m.service_task();
    REQUIRE(not interaction_hal::calls.empty());
    CHECK(interaction_hal::calls.back() == interaction_hal::call_type::disable);
}

namespace {
struct fused_enable_hal {
    using time_point_t = int;
    using task_t = async::timer_task<time_point_t>;

    enum struct call_type {
        enable,
        disable,
        set_event_time,
        now,
        fused_enable
    };

    static inline time_point_t current_time{};
    static inline std::vector<call_type> calls{};

    static auto enable(auto dur) -> time_point_t {
        calls.push_back(call_type::fused_enable);
        return current_time + dur;
    }
    static auto disable() -> void { calls.push_back(call_type::disable); }
    static auto set_event_time(time_point_t) -> void {
        calls.push_back(call_type::set_event_time);
    }
    static auto now() -> time_point_t {
        calls.push_back(call_type::now);
        return current_time;
    }
};

using fused_enable_manager_t = async::generic_timer_manager<fused_enable_hal>;
} // namespace

TEST_CASE("HAL interaction can use enable with duration", "[timer_manager]") {
    fused_enable_hal::calls.clear();

    auto m = fused_enable_manager_t{};
    int var{};
    auto t = fused_enable_manager_t::create_task([&] { var = 42; });
    m.run_after(t, 3);
    REQUIRE(fused_enable_hal::calls.size() == 1);
    CHECK(fused_enable_hal::calls[0] ==
          fused_enable_hal::call_type::fused_enable);
}
