#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/debug.hpp>
#include <async/just.hpp>
#include <async/let_value.hpp>
#include <async/read_env.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/sequence.hpp>
#include <async/start_on.hpp>
#include <async/sync_wait.hpp>
#include <async/then.hpp>
#include <async/variant_sender.hpp>

#include <stdx/ct_format.hpp>
#include <stdx/type_traits.hpp>

#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

TEST_CASE("sync_wait for inline scheduler", "[hosted_sync_wait]") {
    auto value = async::inline_scheduler::schedule() |
                 async::then([] { return 42; }) | async::sync_wait();
    REQUIRE(value.has_value());
    CHECK(get<0>(*value) == 42);
}

TEST_CASE("sync_wait for thread scheduler", "[hosted_sync_wait]") {
    auto value = async::thread_scheduler::schedule() |
                 async::then([] { return 42; }) | async::sync_wait();
    REQUIRE(value.has_value());
    CHECK(get<0>(*value) == 42);
}

TEST_CASE("sync_wait error completion", "[hosted_sync_wait]") {
    auto const s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just_error(17); });

    auto value =
        async::start_on(async::thread_scheduler{}, s) | async::sync_wait();
    CHECK(not value.has_value());
}

TEST_CASE("sync_wait stopped completion", "[hosted_sync_wait]") {
    auto const s = async::make_variant_sender(
        false, [] { return async::just(42); },
        [] { return async::just_stopped(); });

    auto value =
        async::start_on(async::thread_scheduler{}, s) | async::sync_wait();
    CHECK(not value.has_value());
}

TEST_CASE("sync_wait with read (inline scheduler)", "[hosted_sync_wait]") {
    auto s = async::get_scheduler() | async::let_value([&](auto sched) {
                 return async::start_on(sched, async::just(42));
             });

    auto value = async::inline_scheduler::schedule() |
                 async::sequence([&] { return s; }) | async::sync_wait();
    REQUIRE(value.has_value());
    CHECK(get<0>(*value) == 42);
}

TEST_CASE("sync_wait with read (thread scheduler)", "[hosted_sync_wait]") {
    auto s = async::get_scheduler() | async::let_value([&](auto sched) {
                 return async::start_on(sched, async::just(42));
             });

    auto value = async::thread_scheduler::schedule() |
                 async::sequence([&] { return s; }) | async::sync_wait();
    REQUIRE(value.has_value());
    CHECK(get<0>(*value) == 42);
}

TEST_CASE("sync_wait can use a custom environment", "[hosted_sync_wait]") {
    int var{};
    auto s =
        async::read_env(get_fwd_t{}) | async::then([&](int i) { var = i; });
    CHECK(async::sync_wait(s, async::prop{get_fwd_t{}, 42}));
    CHECK(var == 42);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string L, stdx::ct_string S,
              typename Ctx>
    constexpr auto signal(auto &&...) {
        if constexpr (stdx::is_specialization_of_v<
                          Ctx, async::_sync_wait::receiver>) {
            debug_events.push_back(fmt::format("{} {} {}", C, L, S));
        }
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("sync_wait can be named and debugged with a string",
          "[hosted_sync_wait]") {
    using namespace std::string_literals;
    debug_events.clear();
    int var{};
    using S = async::inline_scheduler;
    auto s = S::schedule() | async::then([&] { var = 42; });
    CHECK(async::sync_wait<"op">(s));
    CHECK(var == 42);
    CHECK(debug_events ==
          std::vector{"op sync_wait start"s, "op sync_wait set_value"s});
}
