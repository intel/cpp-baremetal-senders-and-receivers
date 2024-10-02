#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/just.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/then.hpp>

#include <stdx/ct_format.hpp>

#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <concepts>
#include <string>
#include <vector>

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
    [[maybe_unused]] auto n = async::inline_scheduler<>::schedule<
                                  async::inline_scheduler<>::singleshot>() |
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

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    constexpr auto signal(auto &&...) {
        using namespace stdx::literals;
        if constexpr (std::is_same_v<async::debug::tag_of<Ctx>,
                                     async::upon_stopped_t>) {
            debug_events.push_back(
                fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
        }
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("upon_stopped can be debugged with a string", "[upon_stopped]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just_stopped() | async::upon_stopped([] { return 42; });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op upon_stopped set_value"s});
}

TEST_CASE("upon_stopped can be named and debugged with a string",
          "[upon_stopped]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just_stopped() |
             async::upon_stopped<"upon_stopped_name">([] { return 42; });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op upon_stopped_name set_value"s});
}
