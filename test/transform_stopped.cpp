#include "detail/common.hpp"
#include "detail/debug_handler.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/just.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/then.hpp>

#include <catch2/catch_test_macros.hpp>

#include <concepts>
#include <memory>
#include <string>
#include <utility>
#include <vector>

TEST_CASE("transform_stopped", "[transform_stopped]") {
    int value{};

    auto s = async::just_stopped();
    auto n = async::transform_stopped(s, [&] { value = 17; });
    auto op = async::connect(n, stopped_receiver{[&] { ++value; }});
    async::start(op);
    CHECK(value == 18);
}

TEST_CASE("transform_stopped advertises what it sends", "[transform_stopped]") {
    auto s = async::just_stopped();
    [[maybe_unused]] auto n = async::transform_stopped(s, [] {});
    STATIC_REQUIRE(async::sender_of<decltype(n), async::set_stopped_t()>);
}

TEST_CASE("transform_stopped is pipeable", "[transform_stopped]") {
    int value{};

    auto n =
        async::just_stopped() | async::transform_stopped([&] { value = 17; });
    auto op = async::connect(n, stopped_receiver{[&] { ++value; }});
    async::start(op);
    CHECK(value == 18);
}

TEST_CASE("transform_stopped is adaptor-pipeable", "[transform_stopped]") {
    int value{};

    auto n = async::transform_stopped([&] { ++value; }) |
             async::transform_stopped([&] { ++value; });
    auto op = async::connect(async::just_stopped() | n,
                             stopped_receiver{[&] { ++value; }});
    async::start(op);
    CHECK(value == 3);
}

TEST_CASE("move-only lambda", "[transform_stopped]") {
    int value{};
    auto n =
        async::just_stopped() |
        async::transform_stopped(
            [mo = move_only{17}, &value] mutable -> void { value = mo.value; });
    STATIC_REQUIRE(async::singleshot_sender<decltype(n), universal_receiver>);
    auto op = async::connect(std::move(n), stopped_receiver{[&] { ++value; }});
    async::start(op);
    CHECK(value == 18);
}

TEST_CASE("single-shot sender", "[transform_stopped]") {
    [[maybe_unused]] auto n = async::inline_scheduler<>::schedule<
                                  async::inline_scheduler<>::singleshot>() |
                              async::transform_stopped([] {});
    STATIC_REQUIRE(async::singleshot_sender<decltype(n), universal_receiver>);
}

TEST_CASE("transform_stopped propagates value", "[transform_stopped]") {
    bool transform_stopped_called{};
    int value{};

    auto s = async::just(42) |
             async::transform_stopped([&] { transform_stopped_called = true; });
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_value_t(int)>>);
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not transform_stopped_called);
}

TEST_CASE("then propagates error", "[then]") {
    bool transform_stopped_called{};
    int value{};

    auto s = async::just_error(42) |
             async::transform_stopped([&] { transform_stopped_called = true; });
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_error_t(int)>>);
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE(
    "transform_stopped propagates forwarding queries to its child environment",
    "[transform_stopped]") {
    auto s = custom_sender{};
    CHECK(get_fwd(async::get_env(s)) == 42);

    auto t = async::transform_stopped(s, [] {});
    CHECK(get_fwd(async::get_env(t)) == 42);
}

template <>
inline auto async::injected_debug_handler<> =
    debug_handler<async::transform_stopped_t>{};

TEST_CASE("transform_stopped can be debugged with a string",
          "[transform_stopped]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just_stopped() | async::transform_stopped([] {});
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op transform_stopped set_stopped"s});
}

TEST_CASE("transform_stopped can be named and debugged with a string",
          "[transform_stopped]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just_stopped() |
             async::transform_stopped<"transform_stopped_name">([] {});
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op transform_stopped_name set_stopped"s});
}
