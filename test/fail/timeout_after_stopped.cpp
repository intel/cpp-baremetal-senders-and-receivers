#include <async/timeout_after.hpp>

#include <chrono>

// EXPECT: set_stopped cannot send values

auto main() -> int {
    using namespace std::chrono_literals;
    [[maybe_unused]] auto s =
        async::timeout_after<struct domain, async::set_stopped_t>(42ms, 1);
}
