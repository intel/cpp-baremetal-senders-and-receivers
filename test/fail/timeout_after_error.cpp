#include <async/timeout_after.hpp>

#include <chrono>

// EXPECT: set_error should send one value only

auto main() -> int {
    using namespace std::chrono_literals;
    [[maybe_unused]] auto s = async::timeout_after(42ms, 1, 2, 3);
}
