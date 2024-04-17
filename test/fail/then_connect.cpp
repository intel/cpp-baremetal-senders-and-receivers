#include <async/concepts.hpp>
#include <async/just.hpp>
#include <async/tags.hpp>
#include <async/then.hpp>

// EXPECT: Can't connect sender and receiver

struct fail_receiver {
    using is_receiver = void;
};
static_assert(async::receiver<fail_receiver>);

auto main() -> int {
    [[maybe_unused]] auto op = async::connect(
        async::just(42) | async::then([](int x) { return x + 1; }),
        fail_receiver{});
}
