#include "detail/common.hpp"
#include "detail/debug_handler.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/let_stopped.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/start_on.hpp>
#include <async/then.hpp>
#include <async/variant_sender.hpp>

#include <catch2/catch_test_macros.hpp>

#include <string>
#include <utility>
#include <vector>

TEST_CASE("let_stopped", "[let_stopped]") {
    int value{};

    auto s = async::just_stopped();
    auto l = async::let_stopped(s, [] { return async::just(42); });
    auto op = async::connect(l, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped error", "[let_stopped]") {
    int value{};

    auto s = async::just_stopped();
    auto l = async::let_stopped(s, [] { return async::just_error(42); });
    auto op = async::connect(l, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped advertises what it sends", "[let_stopped]") {
    auto s = async::just_stopped();
    [[maybe_unused]] auto l =
        async::let_stopped(s, [] { return async::just(42); });
    STATIC_REQUIRE(async::sender_of<decltype(l), async::set_value_t(int)>);
}

TEST_CASE("let_stopped advertises errors", "[let_stopped]") {
    auto s = async::just_stopped();
    [[maybe_unused]] auto l =
        async::let_stopped(s, [] { return async::just_error(42); });
    STATIC_REQUIRE(async::sender_of<decltype(l), async::set_error_t(int)>);
}

TEST_CASE("let_stopped is pipeable", "[let_stopped]") {
    int value{};

    auto l = async::just_stopped() |
             async::let_stopped([] { return async::just(42); });
    auto op = async::connect(l, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped is adaptor-pipeable", "[let_stopped]") {
    int value{};

    auto l = async::let_stopped([] { return async::just_stopped(); }) |
             async::let_stopped([] { return async::just(42); });
    auto op = async::connect(async::just_stopped() | l,
                             receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only value", "[let_stopped]") {
    int value{};

    auto s = async::just_stopped();
    auto l = async::let_stopped(s, [] { return async::just(move_only{42}); });
    STATIC_REQUIRE(async::singleshot_sender<decltype(l), universal_receiver>);
    auto op = async::connect(std::move(l),
                             receiver{[&](auto &&mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("let_stopped propagates value", "[let_stopped]") {
    bool let_called{};
    int value{};

    auto s = async::just(42) | async::let_stopped([&] {
                 let_called = true;
                 return async::just();
             });
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not let_called);
}

TEST_CASE("let_stopped propagates error", "[let_stopped]") {
    bool let_called{};
    int value{};

    auto s = async::just_error(42) | async::let_stopped([&] {
                 let_called = true;
                 return async::just();
             });
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not let_called);
}

TEST_CASE("let_stopped advertises pass-through completions", "[let_stopped]") {
    [[maybe_unused]] auto l = async::just(42) | async::let_stopped([] {});
    STATIC_REQUIRE(async::sender_of<decltype(l), async::set_value_t(int)>);
}

TEST_CASE("let_stopped can be single shot", "[let_stopped]") {
    [[maybe_unused]] auto l = async::just_stopped() | async::let_stopped([] {
                                  return async::just(move_only{42});
                              });
    STATIC_REQUIRE(async::singleshot_sender<decltype(l)>);
}

TEST_CASE("let_stopped can be single shot with passthrough", "[let_stopped]") {
    [[maybe_unused]] auto l = async::just(move_only{42}) |
                              async::let_stopped([](auto) { return 42; });
    STATIC_REQUIRE(async::singleshot_sender<decltype(l)>);
}

TEST_CASE("let_stopped op state may complete synchronously", "[let_stopped]") {
    auto s = async::just_stopped() |
             async::let_stopped([] { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(async::synchronous<decltype(op)>);
}

TEST_CASE("let_stopped op state may not complete synchronously if antecedent "
          "does not",
          "[let_stopped]") {
    auto const s =
        async::start_on(async::thread_scheduler{}, async::just_stopped()) |
        async::let_stopped([] { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(not async::synchronous<decltype(op)>);
}

TEST_CASE("let_stopped op state may not complete synchronously if subsequent "
          "does not",
          "[let_stopped]") {
    auto const s = async::just_stopped() | async::let_stopped([] {
                       return async::thread_scheduler{}.schedule();
                   });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(not async::synchronous<decltype(op)>);
}

template <>
inline auto async::injected_debug_handler<> =
    debug_handler<async::let_t<async::set_stopped_t>>{};

TEST_CASE("let_stopped can be debugged with a string", "[let_stopped]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just_stopped() |
             async::let_stopped([] { return async::just_stopped(); });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op let_stopped start"s, "op let_stopped set_stopped"s});
}

TEST_CASE("let_stopped can be named and debugged with a string",
          "[let_stopped]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just_stopped() | async::let_stopped<"let_stopped_name">(
                                         [] { return async::just_stopped(); });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op let_stopped_name start"s,
                                      "op let_stopped_name set_stopped"s});
}

TEST_CASE("let_stopped produces debug signal on non-handled channel",
          "[let_stopped]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just_error(42) |
             async::let_stopped([](auto) { return async::just_stopped(); });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op let_stopped start"s, "op let_stopped set_error"s});
}
