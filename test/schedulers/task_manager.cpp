#include <async/schedulers/task_manager.hpp>

#include <catch2/catch_template_test_macros.hpp>
#include <catch2/catch_test_macros.hpp>

#include <functional>
#include <mutex>
#include <thread>
#include <utility>
#include <vector>

namespace {
struct hal {
    static inline std::vector<async::priority_t> calls{};
    static auto schedule(async::priority_t p) { calls.push_back(p); }
};

using task_manager_t = async::priority_task_manager<hal, 8>;

std::function<void()> interrupt_fn{};

struct test_concurrency_policy {
    static inline std::mutex m{};

    struct interrupt {
        interrupt() { m.lock(); }
        ~interrupt() {
            m.unlock();
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

TEST_CASE("create task with lvalue", "[task_manager]") {
    int var{};
    auto l = [&] { var = 42; };
    auto task = task_manager_t::create_task(l);
    task.run();
    CHECK(var == 42);
}

TEST_CASE("create task with rvalue", "[task_manager]") {
    int var{};
    auto task = task_manager_t::create_task([&] { var = 42; });
    task.run();
    CHECK(var == 42);
}

TEST_CASE("run a task with bound args", "[task_manager]") {
    int var{};
    auto task = task_manager_t::create_task([&](int x) { var = x; });
    task.bind_front(42).run();
    CHECK(var == 42);
}

TEST_CASE("tasks have reference equality", "[task_manager]") {
    auto t = task_manager_t::create_task([] {});
    auto u = task_manager_t::create_task([] {});
    CHECK(t == t);
    CHECK(t != u);
}

TEST_CASE("nothing pending", "[task_manager]") {
    auto m = task_manager_t{};
    CHECK(m.is_idle());
}

TEST_CASE("queue a task", "[task_manager]") {
    hal::calls.clear();
    auto m = task_manager_t{};
    auto task = task_manager_t::create_task([] {});
    m.enqueue_task(task, 3);
    CHECK(not m.is_idle());
    REQUIRE(hal::calls.size() == 1);
    CHECK(hal::calls[0] == 3);
}

TEST_CASE("run a queued task", "[task_manager]") {
    hal::calls.clear();
    auto m = task_manager_t{};
    int var{};
    auto task = task_manager_t::create_task([&] { var = 42; });
    m.enqueue_task(task, 3);
    CHECK(not m.is_idle());
    REQUIRE(hal::calls.size() == 1);
    CHECK(hal::calls[0] == 3);
    m.service_tasks<3>();
    CHECK(var == 42);
}

TEST_CASE("queueing a task is idempotent", "[task_manager]") {
    hal::calls.clear();
    auto m = task_manager_t{};
    int var{};
    auto task = task_manager_t::create_task([&] { var = 42; });
    CHECK(m.enqueue_task(task, 3));
    CHECK(not m.enqueue_task(task, 3));
    REQUIRE(hal::calls.size() == 1);
    m.service_tasks<3>();
    CHECK(var == 42);
    CHECK(m.is_idle());
}

TEST_CASE("run tasks in FIFO order", "[task_manager]") {
    auto m = task_manager_t{};
    int var{1};
    auto task1 = task_manager_t::create_task([&] { var *= 2; });
    auto task2 = task_manager_t::create_task([&] { var += 2; });
    CHECK(m.enqueue_task(task1, 0));
    CHECK(m.enqueue_task(task2, 0));
    m.service_tasks<0>();
    CHECK(var == 4);
    CHECK(m.is_idle());
}

TEST_CASE("manager is not idle during a running task", "[task_manager]") {
    auto m = task_manager_t{};
    int var{};
    auto task = task_manager_t::create_task([&] {
        CHECK(not m.is_idle());
        var = 42;
    });
    CHECK(m.enqueue_task(task, 0));
    m.service_tasks<0>();
    CHECK(var == 42);
    CHECK(m.is_idle());
}

TEST_CASE("task can requeue itself (immediate execution)", "[task_manager]") {
    auto m = task_manager_t{};
    int var{};

    auto task = task_manager_t::create_task(
        [&](task_manager_t *mgr, async::priority_task *t) {
            if (var++ == 0) {
                CHECK(mgr->enqueue_task(*t, 0));
            }
        });
    CHECK(m.enqueue_task(task.bind_front(&m, &task), 0));
    m.service_tasks<0, async::requeue_policy::immediate>();
    CHECK(var == 2);
    CHECK(m.is_idle());
}

TEST_CASE("task can requeue itself (deferred execution)", "[task_manager]") {
    auto m = task_manager_t{};
    int var{};

    auto task = task_manager_t::create_task(
        [&](task_manager_t *mgr, async::priority_task *t) {
            ++var;
            CHECK(mgr->enqueue_task(*t, 0));
        });
    CHECK(m.enqueue_task(task.bind_front(&m, &task), 0));
    m.service_tasks<0, async::requeue_policy::deferred>();
    CHECK(var == 1);
    CHECK(not m.is_idle());
}

TEST_CASE("don't run a queued task of a different priority", "[task_manager]") {
    auto m = task_manager_t{};
    int var{};
    auto task = task_manager_t::create_task([&] { var = 42; });
    m.enqueue_task(task, 1);
    CHECK(not m.is_idle());
    m.service_tasks<0>();
    CHECK(not m.is_idle());
    CHECK(var == 0);
}

TEST_CASE("queue a task on interrupt during servicing (immediate execution)",
          "[task_manager]") {
    auto m = task_manager_t{};
    int var{};
    auto task = task_manager_t::create_task([&] { ++var; });
    m.enqueue_task(task, 1);

    interrupt_fn = [&] {
        interrupt_fn = {};
        m.enqueue_task(task, 1);
    };

    m.service_tasks<1, async::requeue_policy::immediate>();
    CHECK(var == 2);
    CHECK(m.is_idle());
}

TEMPLATE_TEST_CASE("thread safety for execution", "[task_manager]",
                   async::requeue_policy::deferred,
                   async::requeue_policy::immediate) {
    auto m = task_manager_t{};
    auto task1 = task_manager_t::create_task([] {});
    auto task2 = task_manager_t::create_task([] {});
    CHECK(m.enqueue_task(task1, 0));

    auto t1 = std::thread{[&] { m.enqueue_task(task2, 0); }};
    auto t2 = std::thread{[&] { m.service_tasks<0, TestType>(); }};
    t1.join();
    t2.join();
}
