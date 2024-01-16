#include "detail/common.hpp"

#include <async/completion_scheduler.hpp>
#include <async/just.hpp>
#include <async/on.hpp>
#include <async/schedulers/runloop_scheduler.hpp>

#include <catch2/catch_test_macros.hpp>

#include <concepts>

TEST_CASE("runloop_scheduler fulfils concept", "[runloop_scheduler]") {
    static_assert(
        async::scheduler<decltype(async::run_loop{}.get_scheduler())>);
}

TEST_CASE("runloop_schedulers from the same run_loop compare equal",
          "[runloop_scheduler]") {
    async::run_loop rl1{};
    async::run_loop rl2{};

    auto s = rl1.get_scheduler();
    CHECK(s != rl2.get_scheduler());
    CHECK(s == rl1.get_scheduler());
}

TEST_CASE("runloop_sender has runloop_scheduler as its completion scheduler",
          "[runloop_scheduler]") {
    async::run_loop rl{};
    auto s = rl.get_scheduler();
    CHECK(async::get_completion_scheduler<async::set_value_t>(
              async::get_env(s.schedule())) == s);
}

TEST_CASE("runloop operation", "[runloop_scheduler]") {
    int value{};

    async::run_loop rl{};
    auto s = rl.get_scheduler().schedule();
    auto op = async::connect(s, receiver{[&] { value = 42; }});
    rl.push_back(&op);

    rl.finish();
    rl.run();
    CHECK(value == 42);
}
