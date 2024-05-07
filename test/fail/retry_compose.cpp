#include <async/retry.hpp>
#include <async/then.hpp>

// EXPECT: (invalid operands to binary expression)|(no match for)

auto main() -> int {
    [[maybe_unused]] auto op = async::then([] { return 42; }) | async::retry();
}
