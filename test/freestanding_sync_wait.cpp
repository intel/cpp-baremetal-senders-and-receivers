#include <async/concepts.hpp>
#include <async/just.hpp>
#include <async/let_value.hpp>
#include <async/read.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/start_on.hpp>
#include <async/then.hpp>
#include <async/variant_sender.hpp>

#define SIMULATE_FREESTANDING
#include <async/sync_wait.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("sync_wait for inline scheduler", "[freestanding_sync_wait]") {
    auto value = async::inline_scheduler::schedule() |
                 async::then([] { return 42; }) | async::sync_wait();
    REQUIRE(value.has_value());
    CHECK(get<0>(*value) == 42);
}

TEST_CASE("sync_wait for thread scheduler", "[freestanding_sync_wait]") {
    auto value = async::thread_scheduler::schedule() |
                 async::then([] { return 42; }) | async::sync_wait();
    REQUIRE(value.has_value());
    CHECK(get<0>(*value) == 42);
}

TEST_CASE("sync_wait error completion", "[freestanding_sync_wait]") {
    auto const s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just_error(17); });

    auto value =
        async::start_on(async::thread_scheduler{}, s) | async::sync_wait();
    CHECK(not value.has_value());
}

TEST_CASE("sync_wait stopped completion", "[freestanding_sync_wait]") {
    auto const s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just_stopped(); });

    auto value =
        async::start_on(async::thread_scheduler{}, s) | async::sync_wait();
    CHECK(not value.has_value());
}

TEST_CASE("sync_wait with read (inline scheduler)",
          "[freestanding_sync_wait]") {
    auto s = async::get_scheduler() | async::let_value([&](auto sched) {
                 return async::start_on(sched, async::just(42));
             });

    auto value = async::inline_scheduler::schedule() |
                 async::let_value([&] { return s; }) | async::sync_wait();
    REQUIRE(value.has_value());
    CHECK(get<0>(*value) == 42);
}

TEST_CASE("sync_wait with read (thread scheduler", "[freestanding_sync_wait]") {
    auto s = async::get_scheduler() | async::let_value([&](auto sched) {
                 return async::start_on(sched, async::just(42));
             });

    auto value = async::thread_scheduler::schedule() |
                 async::let_value([&] { return s; }) | async::sync_wait();
    REQUIRE(value.has_value());
    CHECK(get<0>(*value) == 42);
}
