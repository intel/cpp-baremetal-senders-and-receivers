#include "detail/common.hpp"

#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/just.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/sequence.hpp>

#include <stdx/ct_format.hpp>

#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

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
    static_assert(async::sender_of<decltype(s), async::set_value_t(int)>);
}

TEST_CASE("sequence advertises errors", "[sequence]") {
    [[maybe_unused]] auto s =
        async::sequence(async::just(), [] { return async::just_error(42); });
    static_assert(async::sender_of<decltype(s), async::set_error_t(int)>);
}

TEST_CASE("sequence advertises stopped", "[sequence]") {
    [[maybe_unused]] auto s =
        async::sequence(async::just(), [] { return async::just_stopped(); });
    static_assert(async::sender_of<decltype(s), async::set_stopped_t()>);
}

TEST_CASE("sequence advertises pass-throughs", "[sequence]") {
    [[maybe_unused]] auto s =
        async::sequence(async::just_error(17), [] { return async::just(42); });
    static_assert(async::sender_of<decltype(s), async::set_value_t(int)>);
    static_assert(async::sender_of<decltype(s), async::set_error_t(int)>);
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
    static_assert(async::singleshot_sender<decltype(s)>);
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
    static_assert(async::singleshot_sender<decltype(s)>);
    auto op = async::connect(std::move(s),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("returning move-only sender is not singleshot", "[sequence]") {
    auto s = async::sequence(async::just(),
                             [] { return async::just(move_only{42}); });
    static_assert(async::multishot_sender<decltype(s)>);
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

TEST_CASE("seq(sender) with move-only sender", "[sequence]") {
    int value{};
    auto s = async::just() | async::seq(async::just(move_only{42}));
    static_assert(async::singleshot_sender<decltype(s)>);
    auto op = async::connect(std::move(s),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("sequence may complete synchronously", "[sequence]") {
    auto const s =
        async::just() | async::sequence([] { return async::just(); });
    static_assert(async::synchronous<decltype(s)>);
}

TEST_CASE("sequence may not complete synchronously if antecedent does not",
          "[sequence]") {
    auto const s = async::thread_scheduler{}.schedule() |
                   async::sequence([] { return async::just(); });
    static_assert(not async::synchronous<decltype(s)>);
}

TEST_CASE("sequence may not complete synchronously if subsequent does not",
          "[sequence]") {
    auto const s = async::just() | async::sequence([] {
                       return async::thread_scheduler{}.schedule();
                   });
    static_assert(not async::synchronous<decltype(s)>);
}

TEST_CASE("sequence op state may complete synchronously", "[sequence]") {
    auto const s =
        async::just() | async::sequence([] { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(async::synchronous<decltype(op)>);
}

TEST_CASE(
    "sequence op state may not complete synchronously if antecedent does not",
    "[sequence]") {
    auto const s = async::thread_scheduler{}.schedule() |
                   async::sequence([] { return async::just(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(not async::synchronous<decltype(op)>);
}

TEST_CASE(
    "sequence op state may not complete synchronously if subsequent does not",
    "[sequence]") {
    auto const s = async::just() | async::sequence([] {
                       return async::thread_scheduler{}.schedule();
                   });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    static_assert(not async::synchronous<decltype(op)>);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    constexpr auto signal(auto &&...) {
        using namespace stdx::literals;
        if constexpr (std::is_same_v<async::debug::tag_of<Ctx>,
                                     async::sequence_t>) {
            debug_events.push_back(
                fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
        }
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

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
