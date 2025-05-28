#include <async/get_scheduler.hpp>
#include <async/just.hpp>
#include <async/let_value.hpp>
#include <async/read_env.hpp>
#include <async/start_on.hpp>
#include <async/sync_wait.hpp>

// EXPECT: did you mean to use sync_wait_dynamic

auto main() -> int {
    auto s = async::read_env(async::get_scheduler) |
             async::let_value([&](auto sched) {
                 return async::start_on(sched, async::just(42));
             });
    auto value = s | async::sync_wait();
}
