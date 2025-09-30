#include <async/just.hpp>
#include <async/sync_wait.hpp>
#include <async/variant_sender.hpp>
#include <async/when_all.hpp>

// clang-format off
// EXPECT: when_all requires each sender to complete with at most one set_value completion
// clang-format on

auto main(int argc, char **) -> int {
    [[maybe_unused]] auto s1 = async::make_variant_sender(
        argc == 0, [] { return async::just(42); },
        [] { return async::just(3.14f); });
    [[maybe_unused]] auto s2 = async::just(42);

    [[maybe_unused]] auto r = async::when_all(s1, s2) | async::sync_wait();
}
