#include "detail/common.hpp"

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/just.hpp>
#include <async/read_env.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/stack_allocator.hpp>
#include <async/stop_token.hpp>
#include <async/sync_wait.hpp>
#include <async/then.hpp>
#include <async/type_traits.hpp>
#include <async/when_any.hpp>

#include <stdx/ct_format.hpp>
#include <stdx/type_traits.hpp>

#include <boost/mp11/list.hpp>
#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <algorithm>
#include <array>
#include <chrono>
#include <condition_variable>
#include <functional>
#include <iterator>
#include <mutex>
#include <random>
#include <string>
#include <thread>
#include <type_traits>
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

TEST_CASE("when_any advertises what it sends", "[when_any]") {
    auto s1 = async::just(42);
    auto s2 = async::just(17);
    [[maybe_unused]] auto w = async::when_any(s1, s2);
    static_assert(async::sender_of<decltype(w), async::set_value_t(int)>);
    static_assert(not async::sender_of<decltype(w), async::set_error_t()>);
}

TEST_CASE("when_any advertises errors", "[when_any]") {
    auto s1 = async::just(42);
    auto s2 = async::just_error(17);
    [[maybe_unused]] auto w = async::when_any(s1, s2);
    static_assert(async::sender_of<decltype(w), async::set_error_t(int)>);
}

TEST_CASE("complete with first success", "[when_any]") {
    int value{};
    auto s1 = async::just(42);
    auto s2 = async::just(17);
    auto w = async::when_any(s1, s2);

    auto op = async::connect(w, receiver{[&](auto i) {
                                 CHECK(value == 0);
                                 value = i;
                             }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("complete with first error", "[when_any]") {
    int value{};
    auto s1 = async::just_error(42);
    auto s2 = async::just_error(17);
    auto w = async::when_any(s1, s2);

    auto op = async::connect(w, error_receiver{[&](auto i) {
                                 CHECK(value == 0);
                                 value = i;
                             }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("complete with all stopped", "[when_any]") {
    int value{};
    auto s1 = async::just_stopped();
    auto s2 = async::just_stopped();
    auto w = async::when_any(s1, s2);

    auto op = async::connect(w, stopped_receiver{[&]() {
                                 CHECK(value == 0);
                                 value = 42;
                             }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("complete with first success (void)", "[when_any]") {
    int value{};
    auto s1 = async::just();
    auto s2 = async::just(17);
    auto w = async::when_any(s1, s2);

    auto op = async::connect(w, receiver{[&](auto...) {
                                 CHECK(value == 0);
                                 value = 42;
                             }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only value", "[when_any]") {
    int value{};
    auto s = async::just(move_only{42});
    auto w = async::when_any(std::move(s));
    static_assert(async::singleshot_sender<decltype(w), universal_receiver>);
    auto op = async::connect(
        std::move(w), receiver{[&](move_only<int> mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("copy sender", "[when_any]") {
    int value{};
    auto const s = async::just(42);
    auto w = async::when_any(s);
    static_assert(async::multishot_sender<decltype(w), universal_receiver>);
    auto op = async::connect(w, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move sender", "[when_any]") {
    int value{};
    auto s = async::just(42);
    auto w = async::when_any(s);
    static_assert(async::multishot_sender<decltype(w), universal_receiver>);
    auto op =
        async::connect(std::move(w), receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("when_any with thread scheduler", "[when_any]") {
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
    auto const result = async::when_any(s1, s2) | async::sync_wait();
    REQUIRE(result.has_value());
    auto const [i] = *result;
    CHECK(((i == 42) or (i == 17)));
}

TEST_CASE("first_successful policy", "[when_any]") {
    int value{};
    auto s1 = async::just_error(42);
    auto s2 = async::just(17);
    auto w = async::first_successful(s1, s2);

    auto op = async::connect(w, receiver{[&](auto i) {
                                 CHECK(value == 0);
                                 value = i;
                             }});
    async::start(op);
    CHECK(value == 17);
}

TEST_CASE("first_complete policy", "[when_any]") {
    int value{};
    auto s1 = async::just_stopped();
    auto s2 = async::just(17);
    auto w = async::stop_when(s1, s2);

    auto op = async::connect(w, stopped_receiver{[&] {
                                 CHECK(value == 0);
                                 value = 42;
                             }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("when_any cancellation (before start)", "[when_any]") {
    bool success{};
    bool fail{};
    phase_control ctrl{};

    auto s = async::thread_scheduler<>::schedule() |
             async::then([&] { fail = true; });
    auto const w = async::when_any(s, async::when_any()) |
                   async::upon_stopped([&] { success = true; });

    auto r = stoppable_receiver{[&] { ctrl.advance(); }};
    auto op = async::connect(w, r);

    r.request_stop();
    async::start(op);

    ctrl.wait_for(1);
    CHECK(success);
    CHECK(not fail);
}

TEST_CASE("when_any cancellation (during operation)", "[when_any]") {
    bool success{};
    phase_control ctrl{};

    auto s = async::thread_scheduler<>::schedule() |
             async::then([&] { ctrl.advance_and_wait(); });
    auto const w = async::when_any(s, async::when_any()) |
                   async::upon_stopped([&] { success = true; });

    auto r = stoppable_receiver{[&] { ctrl.advance(); }};
    auto op = async::connect(w, r);

    async::start(op);
    ctrl.wait_for(1);
    r.request_stop();

    ctrl.advance_and_wait();
    CHECK(success);
}

TEST_CASE("stop_when is pipeable", "[when_any]") {
    int value{};
    auto w = async::just(42) | async::stop_when(async::just(17));
    auto op = async::connect(w, receiver{[&](auto i) {
                                 CHECK(value == 0);
                                 value = i;
                             }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("stop_when is adaptor-pipeable", "[when_any]") {
    int value{};
    auto w = async::then([] { return 42; }) | async::stop_when(async::just(17));
    auto op = async::connect(async::just() | w, receiver{[&](auto i) {
                                 CHECK(value == 0);
                                 value = i;
                             }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("when_any with zero args never completes", "[when_any]") {
    [[maybe_unused]] auto w = async::when_any();
    static_assert(std::same_as<async::completion_signatures_of_t<decltype(w)>,
                               async::completion_signatures<>>);
    auto r = receiver{[] {}};
    auto op = async::connect(w, r);
    static_assert(
        std::is_same_v<decltype(op),
                       async::_when_any::op_state<
                           "when_any", async::_when_any::first_noncancelled,
                           decltype(r)>>);
}

TEST_CASE("when_any with zero args can be stopped (before start)",
          "[when_any]") {
    int value{};
    [[maybe_unused]] auto w = async::when_any();
    auto r = stoppable_receiver{[&] { value = 42; }};
    static_assert(
        std::same_as<async::completion_signatures_of_t<
                         decltype(w), async::env_of_t<decltype(r)>>,
                     async::completion_signatures<async::set_stopped_t()>>);

    auto op = async::connect(w, r);
    r.request_stop();
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("when_any with zero args can be stopped (after start)",
          "[when_any]") {
    int value{};
    [[maybe_unused]] auto w = async::when_any();
    auto r = stoppable_receiver{[&] { value = 42; }};
    static_assert(
        std::same_as<async::completion_signatures_of_t<
                         decltype(w), async::env_of_t<decltype(r)>>,
                     async::completion_signatures<async::set_stopped_t()>>);

    auto op = async::connect(w, r);
    async::start(op);
    CHECK(value == 0);

    r.request_stop();
    CHECK(value == 42);
}

TEST_CASE("when_any nests", "[when_any]") {
    [[maybe_unused]] auto w = async::when_any(async::when_any());
    [[maybe_unused]] auto op = async::connect(w, receiver{[] {}});
}

TEST_CASE("when_any with one arg is a no-op", "[when_all]") {
    auto s = async::just(42);
    [[maybe_unused]] auto w = async::when_any(s);
    static_assert(std::same_as<decltype(s), decltype(w)>);
}

TEST_CASE("nullary when_any has a stack allocator", "[when_any]") {
    static_assert(
        std::is_same_v<
            async::allocator_of_t<async::env_of_t<decltype(async::when_any())>>,
            async::stack_allocator>);
}

TEST_CASE("nullary when_any op_state is synchronous", "[when_any]") {
    [[maybe_unused]] auto op =
        async::connect(async::when_any(), receiver{[] {}});
    static_assert(async::synchronous<decltype(op)>);
}

TEST_CASE("normal, (internally) stoppable op_state", "[when_any]") {
    auto s1 = async::just(42);
    auto s2 = async::when_any();
    auto w = async::when_any(s1, s2);

    [[maybe_unused]] auto op = async::connect(w, receiver{[&](auto...) {}});
    [[maybe_unused]] auto test_op_state =
        []<stdx::ct_string Name, typename P, typename R, typename... Ss>(
            async::_when_any::op_state<Name, P, R, Ss...> const &) {};

    static_assert(requires { test_op_state(op); });
}

TEST_CASE("optimized op_state for unstoppable", "[when_any]") {
    auto s1 = async::just(42);
    auto s2 = async::just(17);
    auto w = async::when_any(s1, s2);

    [[maybe_unused]] auto op =
        async::connect(w, stoppable_receiver{[&](auto) {}});
    [[maybe_unused]] auto test_op_state =
        []<stdx::ct_string Name, typename P, typename R, typename... Ss>(
            async::_when_any::nostop_op_state<Name, P, R, Ss...> const &) {};

    static_assert(requires { test_op_state(op); });
}

TEST_CASE("when_any receiver environment is well-formed for synchronous ops",
          "[when_any]") {
    int value{};
    auto op = async::connect(
        async::when_any(async::get_stop_token(), async::just(42)),
        receiver{[&](auto st) {
            CHECK(std::is_same_v<decltype(st), async::never_stop_token>);
            value = 42;
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
                                   async::when_any_t>) {
            std::lock_guard l{m};
            debug_events.push_back(
                fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
        }
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("nullary when_any can be debugged with a string", "[when_any]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto stop = async::inplace_stop_source{};
    auto s = async::when_any();
    auto r = with_env{
        universal_receiver{},
        async::env{async::prop{async::get_debug_interface_t{},
                               async::debug::named_interface<"op">{}},
                   async::prop{async::get_stop_token_t{}, stop.get_token()}}};
    auto op = async::connect(s, r);
    stop.request_stop();

    async::start(op);
    CHECK(debug_events ==
          std::vector{"op when_any start"s, "op when_any set_stopped"s});
}

TEST_CASE("nullary when_any can be named and debugged with a string",
          "[when_any]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto stop = async::inplace_stop_source{};
    auto s = async::when_any<"when_any_name">();
    auto r = with_env{
        universal_receiver{},
        async::env{async::prop{async::get_debug_interface_t{},
                               async::debug::named_interface<"op">{}},
                   async::prop{async::get_stop_token_t{}, stop.get_token()}}};
    auto op = async::connect(s, r);
    stop.request_stop();

    async::start(op);
    CHECK(debug_events == std::vector{"op when_any_name start"s,
                                      "op when_any_name set_stopped"s});
}

TEST_CASE("when_any can be debugged with a string", "[when_any]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::when_any(async::just(42), async::just(17));
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op when_any start"s, "op when_any set_value"s});
}

TEST_CASE("when_any can be named and debugged with a string", "[when_any]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::when_any<"when_any_name">(async::just(42), async::just(17));
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op when_any_name start"s,
                                      "op when_any_name set_value"s});
}
