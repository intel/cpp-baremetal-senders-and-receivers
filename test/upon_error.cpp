#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/then.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("upon_error", "[upon_error]") {
    int value{};

    auto s = async::just_error(42);
    auto n = async::upon_error(s, [](auto i) { return i + 17; });
    auto op = async::connect(n, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 59);
}

TEST_CASE("upon_error advertises what it sends", "[upon_error]") {
    auto s = async::just_error(42);
    [[maybe_unused]] auto n =
        async::upon_error(s, [](auto i) { return i + 17; });
    static_assert(async::sender_of<decltype(n), async::set_value_t(int)>);
}

TEST_CASE("upon_error is pipeable", "[upon_error]") {
    int value{};

    auto s = async::just_error(42);
    auto n = s | async::upon_error([](auto i) { return i + 17; });
    auto op = async::connect(n, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 59);
}

TEST_CASE("upon_error can send nothing", "[upon_error]") {
    int value{};

    auto s = async::just_error(42);
    auto n = async::upon_error(s, [](auto) {});
    static_assert(async::sender_of<decltype(n), async::set_value_t()>);
    auto op = async::connect(n, receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only value", "[upon_error]") {
    int value{};

    auto s = async::just_error(42);
    auto n = s | async::upon_error([](auto i) { return move_only{i}; });
    auto op = async::connect(std::move(n),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("single-shot sender", "[upon_error]") {
    [[maybe_unused]] auto n = async::inline_scheduler::schedule<
                                  async::inline_scheduler::singleshot>() |
                              async::upon_error([] {});
    static_assert(async::singleshot_sender<decltype(n), universal_receiver>);
}

TEST_CASE("upon_error propagates success", "[upon_error]") {
    bool upon_error_called{};
    int value{};

    auto s =
        async::just(42) | async::upon_error([&] { upon_error_called = true; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_value_t(int)>>);
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not upon_error_called);
}

TEST_CASE("upon_error propagates stopped", "[upon_error]") {
    bool upon_error_called{};
    int value{};

    auto s = async::just_stopped() |
             async::upon_error([&] { upon_error_called = true; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_stopped_t()>>);
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not upon_error_called);
}
