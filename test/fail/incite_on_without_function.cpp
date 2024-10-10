#include <async/incite_on.hpp>
#include <async/just.hpp>
#include <async/schedulers/trigger_scheduler.hpp>
#include <async/sync_wait.hpp>

// EXPECT: Sender passed to incite_on must send a function

auto main() -> int {
    auto const s =
        async::incite_on(async::just(42), async::trigger_scheduler<"basic">{});
    auto x = async::sync_wait(s);
}
