#include <async/just.hpp>
#include <async/sync_wait.hpp>
#include <async/variant_sender.hpp>

// clang-format off
// EXPECT: sync_wait requires a single set_value completion: consider using into_variant
// clang-format on

auto main(int argc, char **) -> int {
    [[maybe_unused]] auto r =
        async::make_optional_sender(argc == 0, [] { return async::just(42); }) |
        async::sync_wait();
}
