#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/incite_on.hpp>
#include <async/just.hpp>
#include <async/schedulers/trigger_scheduler.hpp>
#include <async/variant_sender.hpp>

// EXPECT: Sender passed to incite_on must send a single function

auto main() -> int {
    auto const v = async::make_variant_sender(
        true, [] { return async::just(42); }, [] { return async::just(1.0f); });
    auto const s = async::incite_on(v, async::trigger_scheduler<"basic">{});
    [[maybe_unused]] auto op =
        async::connect(s, async::detail::universal_receiver{});
}
