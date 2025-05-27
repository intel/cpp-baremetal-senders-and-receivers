#include "detail/common.hpp"

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/just.hpp>
#include <async/let_value.hpp>
#include <async/read_env.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/stop_token.hpp>
#include <async/sync_wait.hpp>
#include <async/then.hpp>
#include <async/variant_sender.hpp>
#include <async/when_all.hpp>

#include <stdx/ct_format.hpp>
#include <stdx/tuple_destructure.hpp>

#include <boost/mp11/list.hpp>
#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <algorithm>
#include <array>
#include <chrono>
#include <concepts>
#include <condition_variable>
#include <functional>
#include <iterator>
#include <mutex>
#include <random>
#include <string>
#include <thread>
#include <utility>
#include <vector>

namespace {
[[maybe_unused]] auto get_rng() -> auto & {
    static auto rng = [] {
        std::array<int, std::mt19937::state_size> seed_data;
        std::random_device r;
        std::generate_n(seed_data.data(), seed_data.size(), std::ref(r));
        std::seed_seq seq(std::begin(seed_data), std::end(seed_data));
        return std::mt19937{seq};
    }();
    return rng;
}
} // namespace

TEST_CASE("when_all advertises what it sends", "[when_all]") {
    auto s1 = async::just(42);
    auto s2 = async::just(17);
    [[maybe_unused]] auto w = async::when_all(s1, s2);
    STATIC_REQUIRE(async::sender_of<decltype(w), async::set_value_t(int, int)>);
    STATIC_REQUIRE(not async::sender_of<decltype(w), async::set_error_t()>);
}

TEST_CASE("when_all advertises errors", "[when_all]") {
    auto s1 = async::just(42);
    auto s2 = async::just_error(17);
    [[maybe_unused]] auto w = async::when_all(s1, s2);
    STATIC_REQUIRE(async::sender_of<decltype(w), async::set_error_t(int)>);
}

TEST_CASE("basic operation", "[when_all]") {
    int value{};
    auto s1 = async::just(42);
    auto s2 = async::just(17);
    auto w = async::when_all(s1, s2);

    auto op = async::connect(w, receiver{[&](auto i, auto j) {
                                 CHECK(i == 42);
                                 CHECK(j == 17);
                                 value = i + j;
                             }});
    async::start(op);
    CHECK(value == 59);
}

TEST_CASE("basic operation sending multiple values", "[when_all]") {
    int value{};
    auto s1 = async::just(42, 17);
    auto s2 = async::just(1337);
    STATIC_REQUIRE(
        async::_when_all::single_sender<decltype(s1), async::set_value_t,
                                        async::empty_env>);
    auto w = async::when_all(s1, s2);

    auto op = async::connect(w, receiver{[&](auto i, auto j, auto k) {
                                 CHECK(i == 42);
                                 CHECK(j == 17);
                                 CHECK(k == 1337);
                                 value = i + j + k;
                             }});
    async::start(op);
    CHECK(value == 1396);
}

TEST_CASE("basic operation sending void values", "[when_all]") {
    int value{};
    auto s1 = async::just(42, 17);
    auto s2 = async::just();
    STATIC_REQUIRE(
        async::_when_all::single_sender<decltype(s1), async::set_value_t,
                                        async::empty_env>);
    auto w = async::when_all(s1, s2);

    auto op = async::connect(w, receiver{[&](auto i, auto j) {
                                 CHECK(i == 42);
                                 CHECK(j == 17);
                                 value = i + j;
                             }});
    async::start(op);
    CHECK(value == 59);
}

TEST_CASE("when_all with thread scheduler", "[when_all]") {
    std::uniform_int_distribution<> dis{5, 10};
    auto const d1 = std::chrono::milliseconds{dis(get_rng())};
    auto const d2 = std::chrono::milliseconds{dis(get_rng())};

    auto s1 = async::thread_scheduler<>::schedule() | async::then([&] {
                  std::this_thread::sleep_for(d1);
                  return 42;
              });
    auto s2 = async::thread_scheduler<>::schedule() | async::then([&] {
                  std::this_thread::sleep_for(d2);
                  return 17;
              });
    auto const result = async::when_all(s1, s2) | async::sync_wait();
    REQUIRE(result.has_value());
    auto const [i, j] = *result;
    CHECK(i == 42);
    CHECK(j == 17);
}

TEST_CASE("move-only value", "[when_all]") {
    int value{};
    auto s = async::just(move_only{42});
    auto w = async::when_all(std::move(s));
    STATIC_REQUIRE(async::singleshot_sender<decltype(w), universal_receiver>);
    auto op = async::connect(
        std::move(w), receiver{[&](move_only<int> mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("copy sender", "[when_all]") {
    int value{};
    auto const s = async::just(42);
    auto w = async::when_all(s);
    STATIC_REQUIRE(async::multishot_sender<decltype(w), universal_receiver>);
    auto op = async::connect(w, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move sender", "[when_all]") {
    int value{};
    auto s = async::just(42);
    auto w = async::when_all(s);
    STATIC_REQUIRE(async::multishot_sender<decltype(w), universal_receiver>);
    auto op =
        async::connect(std::move(w), receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("when_all propagates error (order 1)", "[when_all]") {
    int value{};
    auto s1 = async::just(42);
    auto s2 = async::just_error(17);
    auto w = async::when_all(s1, s2);

    auto op = async::connect(w, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 17);
}

TEST_CASE("when_all propagates error (order 2)", "[when_all]") {
    int value{};
    auto s1 = async::just_error(17);
    auto s2 = async::just(42);
    auto w = async::when_all(s1, s2);

    auto op = async::connect(w, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 17);
}

TEST_CASE("when_all does not send values after error", "[when_all]") {
    int value{};
    auto s1 = async::just_error(17);
    auto s2 = async::just(42);
    auto w = async::when_all(s1, s2);

    auto op = async::connect(w, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 0);
}

TEST_CASE("deal with void senders", "[when_all]") {
    int value{};
    auto s1 = async::just();
    auto s2 = async::just(17);
    auto w = async::when_all(s1, s2);

    auto op = async::connect(w, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 17);
}

TEST_CASE("when_all cancellation (before start)", "[when_all]") {
    bool success{};
    bool fail{};
    phase_control ctrl{};

    auto s = async::thread_scheduler<>::schedule() |
             async::then([&] { fail = true; });
    auto const w = async::when_all(s, stoppable_just()) |
                   async::upon_stopped([&] { success = true; });

    auto r = stoppable_receiver{[&] { ctrl.advance(); }};
    auto op = async::connect(w, r);

    r.request_stop();
    async::start(op);

    ctrl.wait_for(1);
    CHECK(success);
    CHECK(not fail);
}

TEST_CASE("when_all cancellation (during operation)", "[when_all]") {
    bool success{};
    phase_control ctrl{};

    auto s = async::thread_scheduler<>::schedule() |
             async::then([&] { ctrl.advance_and_wait(); });
    auto const w = async::when_all(s, stoppable_just()) |
                   async::upon_stopped([&] { success = true; });

    auto r = stoppable_receiver{[&] { ctrl.advance(); }};
    auto op = async::connect(w, r);

    async::start(op);
    ctrl.wait_for(1);
    r.request_stop();

    ctrl.advance_and_wait();
    CHECK(success);
}

TEST_CASE("when_all with zero args completes immediately with set_value_t()",
          "[when_all]") {
    int value{};
    [[maybe_unused]] auto w = async::when_all();
    STATIC_REQUIRE(not async::stoppable_sender<decltype(w)>);
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(w)>,
                     async::completion_signatures<async::set_value_t()>>);

    auto op = async::connect(w, receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("when_all with zero args completes immediately when stopped",
          "[when_all]") {
    int value{};
    [[maybe_unused]] auto w = async::when_all();
    auto r = stoppable_receiver{[&] { value = 42; }};
    STATIC_REQUIRE(
        async::stoppable_sender<decltype(w), async::env_of_t<decltype(r)>>);
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<
                         decltype(w), async::env_of_t<decltype(r)>>,
                     async::completion_signatures<async::set_value_t(),
                                                  async::set_stopped_t()>>);

    auto op = async::connect(w, r);
    r.request_stop();
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("when_all nests", "[when_all]") {
    [[maybe_unused]] auto w = async::when_all(async::when_all());
    [[maybe_unused]] auto op = async::connect(w, receiver{[] {}});
}

TEST_CASE("when_all with one arg is a no-op", "[when_all]") {
    auto s = async::just(42);
    [[maybe_unused]] auto w = async::when_all(s);
    STATIC_REQUIRE(std::same_as<decltype(s), decltype(w)>);
}

TEST_CASE("nullary when_all has a stack allocator", "[when_all]") {
    STATIC_REQUIRE(
        std::is_same_v<
            async::allocator_of_t<async::env_of_t<decltype(async::when_all())>>,
            async::stack_allocator>);
}

TEST_CASE("nullary when_all op_state is synchronous", "[when_all]") {
    [[maybe_unused]] auto op =
        async::connect(async::when_all(), receiver{[] {}});
    STATIC_REQUIRE(async::synchronous<decltype(op)>);
}

TEST_CASE("when_all has a stack allocator when all its subsenders do",
          "[when_all]") {
    STATIC_REQUIRE(
        std::is_same_v<
            async::allocator_of_t<async::env_of_t<decltype(async::when_all(
                async::just(42), async::just(17)))>>,
            async::stack_allocator>);
}

TEST_CASE("when_all op state is synchronous when all its sub-op states are",
          "[when_all]") {
    [[maybe_unused]] auto op = async::connect(
        async::when_all(async::just(42), async::just(17)), receiver{[] {}});
    STATIC_REQUIRE(async::synchronous<decltype(op)>);
}

TEST_CASE("when_all receiver environment is well-formed for synchronous ops",
          "[when_all]") {
    int value{};
    auto op = async::connect(
        async::when_all(async::read_env(async::get_stop_token),
                        async::just(42)),
        receiver{[&](auto st, int x) {
            STATIC_REQUIRE(
                std::is_same_v<decltype(st), async::never_stop_token>);
            value = x;
        }});
    async::start(op);
    CHECK(value == 42);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    std::mutex m{};

    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    auto signal(auto &&...) {
        if constexpr (std::same_as<async::debug::tag_of<Ctx>,
                                   async::when_all_t>) {
            std::lock_guard l{m};
            debug_events.push_back(
                fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
        }
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("nullary when_all can be debugged with a string", "[when_all]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::when_all();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op when_all start"s, "op when_all set_value"s});
}

TEST_CASE("nullary when_all can be named and debugged with a string",
          "[when_all]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::when_all<"when_all_name">();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op when_all_name start"s,
                                      "op when_all_name set_value"s});
}

TEST_CASE("when_all can be debugged with a string", "[when_all]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::when_all(async::just(42), async::just(17));
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op when_all start"s, "op when_all set_value"s});
}

TEST_CASE("when_all can be named and debugged with a string", "[when_all]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::when_all<"when_all_name">(async::just(42), async::just(17));
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op when_all_name start"s,
                                      "op when_all_name set_value"s});
}
