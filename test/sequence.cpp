#include "detail/common.hpp"
#include "detail/debug_handler.hpp"

#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/sequence.hpp>

#include <catch2/catch_test_macros.hpp>

#include <string>
#include <utility>
#include <vector>

TEST_CASE("sequence", "[sequence]") {
    int value{};
    auto s = async::sequence(async::just(), [] { return async::just(42); });
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("sequence discards any values from first sender", "[sequence]") {
    int value{};
    auto s = async::sequence(async::just(17), [](auto &&...args) {
        CHECK(sizeof...(args) == 0);
        return async::just(42);
    });
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("sequence error", "[sequence]") {
    int value{};
    auto s =
        async::sequence(async::just(), [] { return async::just_error(42); });
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("sequence stopped", "[sequence]") {
    int value{};
    auto s =
        async::sequence(async::just(), [] { return async::just_stopped(); });
    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("sequence advertises what it sends", "[sequence]") {
    [[maybe_unused]] auto s =
        async::sequence(async::just(), [] { return async::just(42); });
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_value_t(int)>);
}

TEST_CASE("sequence advertises errors", "[sequence]") {
    [[maybe_unused]] auto s =
        async::sequence(async::just(), [] { return async::just_error(42); });
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_error_t(int)>);
}

TEST_CASE("sequence advertises stopped", "[sequence]") {
    [[maybe_unused]] auto s =
        async::sequence(async::just(), [] { return async::just_stopped(); });
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_stopped_t()>);
}

TEST_CASE("sequence advertises pass-throughs", "[sequence]") {
    [[maybe_unused]] auto s =
        async::sequence(async::just_error(17), [] { return async::just(42); });
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_value_t(int)>);
    STATIC_REQUIRE(async::sender_of<decltype(s), async::set_error_t(int)>);
}

TEST_CASE("sequence is pipeable", "[sequence]") {
    int value{};
    auto s = async::just() | async::sequence([] { return async::just(42); });
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("sequence is adaptor-pipeable", "[sequence]") {
    int value{};
    auto s = async::sequence([] { return async::just(17); }) |
             async::sequence([] { return async::just(42); });
    auto op =
        async::connect(async::just() | s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("move-only first sender", "[sequence]") {
    int value{};
    auto s = async::sequence(async::just(move_only{17}),
                             [] { return async::just(42); });
    STATIC_REQUIRE(async::singleshot_sender<decltype(s)>);
    auto op =
        async::connect(std::move(s), receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

namespace {
struct rvalue_callable {
    auto operator()() && { return async::just(std::move(mo)); }
    move_only<> mo{42};
};
} // namespace

TEST_CASE("move-only callable", "[sequence]") {
    int value{};
    auto s = async::sequence(async::just(), rvalue_callable{});
    STATIC_REQUIRE(async::singleshot_sender<decltype(s)>);
    auto op = async::connect(std::move(s),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("returning move-only sender is not singleshot", "[sequence]") {
    auto s = async::sequence(async::just(),
                             [] { return async::just(move_only{42}); });
    STATIC_REQUIRE(async::multishot_sender<decltype(s)>);
}

TEST_CASE("sequence propagates error", "[sequence]") {
    int value{};

    auto s =
        async::just_error(17) | async::sequence([] { return async::just(42); });
    auto op = async::connect(s, error_receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 17);
}

TEST_CASE("sequence propagates stopped", "[sequence]") {
    int value{};

    auto s =
        async::just_stopped() | async::sequence([] { return async::just(42); });
    auto op = async::connect(s, stopped_receiver{[&] { value = 17; }});
    async::start(op);
    CHECK(value == 17);
}

TEST_CASE("seq(sender) is shorthand for sequence([] { return sender; })",
          "[sequence]") {
    int value{};
    auto s = async::just() | async::seq(async::just(42));
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("seq(s1, s2) is the same as s1 | seq(s2)", "[sequence]") {
    int value{};
    auto s = async::seq(async::just(), async::just(42));
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("seq is variadic", "[sequence]") {
    int value{};
    auto s = async::seq(async::just(), async::just(42), async::just(17));
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 17);
}

TEST_CASE("seq(sender) with move-only sender", "[sequence]") {
    int value{};
    auto s = async::just() | async::seq(async::just(move_only{42}));
    STATIC_REQUIRE(async::singleshot_sender<decltype(s)>);
    auto op = async::connect(std::move(s),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("sequence may complete synchronously", "[sequence]") {
    auto const s =
        async::just() | async::sequence([] { return async::just(); });
    STATIC_REQUIRE(async::synchronous<decltype(s)>);
}

TEST_CASE("sequence may not complete synchronously if antecedent does not",
          "[sequence]") {
    auto const s = async::thread_scheduler{}.schedule() |
                   async::sequence([] { return async::just(); });
    STATIC_REQUIRE(not async::synchronous<decltype(s)>);
}

TEST_CASE("sequence may not complete synchronously if subsequent does not",
          "[sequence]") {
    auto const s = async::just() | async::sequence([] {
                       return async::thread_scheduler{}.schedule();
                   });
    STATIC_REQUIRE(not async::synchronous<decltype(s)>);
}

TEST_CASE("sequence op state may complete synchronously", "[sequence]") {
    auto const s =
        async::just() | async::sequence([] { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(async::synchronous<decltype(op)>);
}

TEST_CASE(
    "sequence op state may not complete synchronously if antecedent does not",
    "[sequence]") {
    auto const s = async::thread_scheduler{}.schedule() |
                   async::sequence([] { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(not async::synchronous<decltype(op)>);
}

TEST_CASE(
    "sequence op state may not complete synchronously if subsequent does not",
    "[sequence]") {
    auto const s = async::just() | async::sequence([] {
                       return async::thread_scheduler{}.schedule();
                   });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(not async::synchronous<decltype(op)>);
}

template <>
inline auto async::injected_debug_handler<> =
    debug_handler<async::sequence_t>{};

TEST_CASE("sequence can be debugged with a string", "[sequence]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just() | async::sequence([] { return async::just(); });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op sequence start"s, "op sequence set_value"s});
}

TEST_CASE("sequence can be named and debugged with a string", "[sequence]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just() |
             async::sequence<"sequence_name">([] { return async::just(); });
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op sequence_name start"s,
                                      "op sequence_name set_value"s});
}

TEST_CASE("seq can be debugged with a string", "[sequence]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just() | async::seq(async::just());
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op seq start"s, "op seq set_value"s});
}

TEST_CASE("seq can be named and debugged with a string", "[sequence]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::just() | async::seq<"seq_name">(async::just());
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op seq_name start"s, "op seq_name set_value"s});
}
