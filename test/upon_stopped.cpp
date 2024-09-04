#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/then.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("upon_stopped", "[upon_stopped]") {
    int value{};

    auto s = async::just_stopped();
    auto n = async::upon_stopped(s, [&] { value = 42; });
    auto op = async::connect(n, receiver{[] {}});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("upon_stopped advertises what it sends", "[upon_stopped]") {
    auto s = async::just_stopped();
    [[maybe_unused]] auto n = async::upon_stopped(s, [] { return 42; });
    static_assert(async::sender_of<decltype(n), async::set_value_t(int)>);
}

TEST_CASE("upon_stopped is pipeable", "[upon_stopped]") {
    int value{};

    auto s = async::just_stopped();
    auto n = s | async::upon_stopped([] { return 42; });
    auto op = async::connect(n, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("single-shot sender", "[upon_stopped]") {
    [[maybe_unused]] auto n = async::inline_scheduler::schedule<
                                  async::inline_scheduler::singleshot>() |
                              async::upon_stopped([] {});
    static_assert(async::singleshot_sender<decltype(n), universal_receiver>);
}

TEST_CASE("upon_stopped propagates success", "[upon_stopped]") {
    bool upon_stopped_called{};
    int value{};

    auto s = async::just(42) |
             async::upon_stopped([&] { upon_stopped_called = true; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_value_t(int)>>);
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not upon_stopped_called);
}

TEST_CASE("upon_stopped propagates errors", "[upon_stopped]") {
    bool upon_stopped_called{};
    int value{};

    auto s = async::just_error(42) |
             async::upon_stopped([&] { upon_stopped_called = true; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_error_t(int)>>);
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not upon_stopped_called);
}
