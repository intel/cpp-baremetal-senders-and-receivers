#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/just.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/then.hpp>

#include <stdx/ct_format.hpp>

#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <concepts>
#include <memory>
#include <string>
#include <utility>
#include <vector>

TEST_CASE("transform_error", "[transform_error]") {
    int value{};

    auto s = async::just_error(17);
    auto n = async::transform_error(s, [] { return 42; });
    auto op = async::connect(n, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("transform_error propagates a value", "[transform_error]") {
    int value{};

    auto s = async::just_error(42);
    auto n = async::transform_error(s, [](auto i) { return i * 2; });
    auto op = async::connect(n, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 84);
}

TEST_CASE("transform_error advertises what it sends", "[transform_error]") {
    auto s = async::just_error(17);
    [[maybe_unused]] auto n = async::transform_error(s, [] { return 42; });
    static_assert(async::sender_of<decltype(n), async::set_error_t(int)>);
}

TEST_CASE("transform_error can send a reference", "[transform_error]") {
    int value{};
    auto s = async::just_error(17);
    [[maybe_unused]] auto n =
        async::transform_error(s, [&]() -> int & { return value; });
    static_assert(async::sender_of<decltype(n), async::set_error_t(int &)>);
}

TEST_CASE("transform_error is pipeable", "[transform_error]") {
    int value{};

    auto n = async::just_error(17) | async::transform_error([] { return 42; });
    auto op = async::connect(n, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("transform_error is adaptor-pipeable", "[transform_error]") {
    int value{};

    auto n = async::transform_error([] { return 42; }) |
             async::transform_error([](int i) { return i * 2; });
    auto op = async::connect(async::just_error(17) | n,
                             error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 84);
}

TEST_CASE("transform_error can send nothing", "[transform_error]") {
    int value{};

    auto s = async::just_error(17);
    auto n1 = async::transform_error(s, [] {});
    static_assert(async::sender_of<decltype(n1), async::set_error_t()>);
    auto n2 = async::transform_error(n1, [] {});
    static_assert(async::sender_of<decltype(n2), async::set_error_t()>);
    auto op = async::connect(n2, error_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only value", "[transform_error]") {
    int value{};

    auto n = async::just_error(17) |
             async::transform_error([] { return move_only{42}; });
    auto op = async::connect(
        std::move(n), error_receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only lambda", "[transform_error]") {
    int value{};
    auto n = async::just_error(17) |
             async::transform_error(
                 [mo = move_only{42}]() -> move_only<int> const && {
                     return std::move(mo);
                 });
    static_assert(async::singleshot_sender<decltype(n), universal_receiver>);
    auto op = async::connect(
        std::move(n), error_receiver{[&](auto &&mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("single-shot sender", "[transform_error]") {
    [[maybe_unused]] auto n = async::inline_scheduler<>::schedule<
                                  async::inline_scheduler<>::singleshot>() |
                              async::transform_error([] {});
    static_assert(async::singleshot_sender<decltype(n), universal_receiver>);
}

TEST_CASE("transform_error propagates value", "[transform_error]") {
    bool transform_error_called{};
    int value{};

    auto s = async::just(42) |
             async::transform_error([&] { transform_error_called = true; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_value_t(int)>>);
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not transform_error_called);
}

TEST_CASE("transform_error propagates stopped", "[transform_error]") {
    bool transform_error_called{};
    int value{};

    auto s = async::just_stopped() |
             async::transform_error([&] { transform_error_called = true; });
    static_assert(
        std::same_as<async::completion_signatures_of_t<decltype(s)>,
                     async::completion_signatures<async::set_stopped_t()>>);
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(not transform_error_called);
}

TEST_CASE(
    "transform_error propagates forwarding queries to its child environment",
    "[transform_error]") {
    auto s = custom_sender{};
    CHECK(get_fwd(async::get_env(s)) == 42);

    auto t = async::transform_error(s, [] {});
    CHECK(get_fwd(async::get_env(t)) == 42);
}

TEST_CASE("transform_error advertises what it sends (variadic)",
          "[transform_error]") {
    auto s = async::just_error(true, false) |
             async::transform_error([](auto) { return 42; },
                                    [](auto) { return 17; });
    static_assert(async::sender_of<decltype(s), async::set_error_t(int, int)>);
}

TEST_CASE("transform_error (variadic)", "[transform_error]") {
    int x{};
    int y{};
    auto s = async::just_error(2, 3) |
             async::transform_error([](auto i) { return i * 2; },
                                    [](auto i) { return i * 3; });
    auto op = async::connect(s, error_receiver{[&](auto i, auto j) {
                                 x = i;
                                 y = j;
                             }});
    async::start(op);
    CHECK(x == 4);
    CHECK(y == 9);
}

TEST_CASE("variadic transform_error can have void-returning functions",
          "[transform_error]") {
    int x{};
    int y{42};
    auto s = async::just_error(2, 3) |
             async::transform_error([](auto i) { return i * 2; }, [](auto) {});
    static_assert(async::sender_of<decltype(s), async::set_error_t(int)>);
    auto op = async::connect(s, error_receiver{[&](auto i) { x = i; }});
    async::start(op);
    CHECK(x == 4);
    CHECK(y == 42);
}

TEST_CASE("move-only value (from transform_error) (variadic)",
          "[transform_error]") {
    int x{};
    auto s = async::just_error(2, 3) |
             async::transform_error([](auto i) { return move_only{i}; },
                                    [](auto) {});
    static_assert(
        async::sender_of<decltype(s), async::set_error_t(move_only<int>)>);
    auto op = async::connect(s, error_receiver{[&](auto i) { x = i.value; }});
    async::start(op);
    CHECK(x == 2);
}

TEST_CASE("move-only value (to transform_error) (variadic)",
          "[transform_error]") {
    int x{};
    auto s =
        async::just_error(move_only{2}, move_only{3}) |
        async::transform_error([](auto i) { return i.value; }, [](auto) {});
    static_assert(async::sender_of<decltype(s), async::set_error_t(int)>);
    auto op =
        async::connect(std::move(s), error_receiver{[&](auto i) { x = i; }});
    async::start(op);
    CHECK(x == 2);
}

TEST_CASE("variadic transform_error can take heteroadic functions",
          "[transform_error]") {
    int x{};
    int y{42};
    bool z{};
    auto s =
        async::just_error(2, 3, 4) |
        async::transform_error([](auto i, auto j) { return i + j; },
                               [](auto k) { return k; }, [] { return true; });
    static_assert(
        async::sender_of<decltype(s), async::set_error_t(int, int, bool)>);
    auto op = async::connect(s, error_receiver{[&](auto i, auto j, auto k) {
                                 x = i;
                                 y = j;
                                 z = k;
                             }});
    async::start(op);
    CHECK(x == 5);
    CHECK(y == 4);
    CHECK(z);
}

TEST_CASE("transform_error can handle a reference", "[transform_error]") {
    int value{};
    auto s = async::just_error() |
             async::transform_error([&]() -> int & { return value; });
    auto op =
        async::connect(s, error_receiver{[&](auto &i) {
                           CHECK(std::addressof(value) == std::addressof(i));
                       }});
    async::start(op);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    constexpr auto signal(auto &&...) {
        using namespace stdx::literals;
        if constexpr (std::is_same_v<async::debug::tag_of<Ctx>,
                                     async::transform_error_t>) {
            static_assert(not boost::mp11::mp_empty<
                          async::debug::children_of<Ctx>>::value);
            debug_events.push_back(
                fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
        }
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("transform_error can be debugged with a string",
          "[transform_error]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just_error() | async::transform_error([] {});
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op transform_error set_error"s});
}

TEST_CASE("transform_error can be named and debugged with a string",
          "[transform_error]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just_error() |
             async::transform_error<"transform_error_name">([] {});
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op transform_error_name set_error"s});
}
