#include <async/incite_on.hpp>
#include <async/just.hpp>
#include <async/schedulers/inline_scheduler.hpp>

// EXPECT: The scheduler passed to incite_on cannot have a synchronous sender

auto main() -> int {
    [[maybe_unused]] auto const s =
        async::incite_on(async::just([] {}), async::inline_scheduler{});
}
