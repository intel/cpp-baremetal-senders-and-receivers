#include "detail/common.hpp"
#include "detail/debug_handler.hpp"

#include <async/connect.hpp>
#include <async/incite_on.hpp>
#include <async/just.hpp>
#include <async/schedulers/trigger_scheduler.hpp>
#include <async/then.hpp>

#include <catch2/catch_test_macros.hpp>

#include <string>
#include <type_traits>
#include <utility>
#include <vector>

TEST_CASE("incite_on basic functionality", "[incite_on]") {
    int value{};
    auto const s =
        async::incite_on(async::just([] { async::run_triggers<"basic">(); }),
                         async::trigger_scheduler<"basic">{});
    auto op = async::connect(s, receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("incite_on error", "[incite_on]") {
    int value{};
    auto const s = async::incite_on(async::just_error(42),
                                    async::trigger_scheduler<"error">{});
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("incite_on stopped", "[incite_on]") {
    int value{};
    auto const s = async::incite_on(async::just_stopped(),
                                    async::trigger_scheduler<"stopped">{});
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("incite_on send a value with trigger", "[incite_on]") {
    int value{};
    auto const s =
        async::incite_on(async::just([] { async::run_triggers<"value">(42); }),
                         async::trigger_scheduler<"value", int>{});
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("incite_on works being incited by a non-empty lambda",
          "[incite_on]") {
    int value{};
    auto const s = async::incite_on(
        async::just([value = 42] { async::run_triggers<"value">(value); }),
        async::trigger_scheduler<"value", int>{});
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("incite_on advertises what it sends", "[incite_on]") {
    [[maybe_unused]] auto const s =
        async::incite_on(async::just([] { async::run_triggers<"basic">(); }),
                         async::trigger_scheduler<"basic">{});
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_value_t()>);
}

TEST_CASE("incite_on advertises pass-throughs", "[incite_on]") {
    [[maybe_unused]] auto s = async::incite_on(
        async::just_error(17), async::trigger_scheduler<"error">{});
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_error_t(int)>);
}

TEST_CASE("incite_on is pipeable", "[incite_on]") {
    int value{};
    auto s = async::just([] { async::run_triggers<"pipeable">(); }) |
             async::incite_on(async::trigger_scheduler<"pipeable">{});
    auto op = async::connect(s, receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("incite_on is adaptor-pipeable", "[incite_on]") {
    int value{};
    auto a = async::then([] {
                 return [] { async::run_triggers<"adaptor-pipeable">(); };
             }) |
             async::incite_on(async::trigger_scheduler<"adaptor-pipeable">{});
    auto op = async::connect(async::just() | a, receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only first sender", "[incite_on]") {
    int value{};
    auto s =
        async::just([mo = move_only(17)] { async::run_triggers<"mo">(); }) |
        async::incite_on(async::trigger_scheduler<"mo">{});
    STATIC_REQUIRE(async::singleshot_sender<decltype(s)>);
    auto op = async::connect(std::move(s), receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("incite_on does not complete synchronously", "[incite_on]") {
    auto const s = async::just([] { async::run_triggers<"sync">(); }) |
                   async::incite_on(async::trigger_scheduler<"sync">{});
    STATIC_REQUIRE(not async::synchronous<decltype(s)>);
}

template <>
inline auto async::injected_debug_handler<> =
    debug_handler<async::incite_on_t>{};

TEST_CASE("incite_on can be debugged with a string", "[incite_on]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto const s = async::just([] { async::run_triggers<"debug">(); }) |
                   async::incite_on(async::trigger_scheduler<"debug">{});
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op incite_on start"s, "op incite_on set_value"s});
}

TEST_CASE("incite_on_any", "[incite_on]") {
    int value{};
    auto const s = async::just([] { async::run_triggers<"1">(42); }) |
                   async::incite_on_any(async::trigger_scheduler<"0", bool>{},
                                        async::trigger_scheduler<"1", int>{});
    auto op = async::connect(s, receiver{[&](int x) { value = x; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(async::triggers<stdx::cts_t<"0">>.empty());
    CHECK(async::triggers<stdx::cts_t<"1">>.empty());
}
