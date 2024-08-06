#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/start.hpp>
#include <async/when_any.hpp>

// EXPECT: Starting when_any<> but the connected receiver cannot cancel

auto main() -> int {
    auto op =
        async::connect(async::when_any(), async::detail::universal_receiver{});
    async::start(op);
}
