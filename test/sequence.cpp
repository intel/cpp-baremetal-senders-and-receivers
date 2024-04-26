#include "detail/common.hpp"

#include <async/just.hpp>
#include <async/sequence.hpp>

#include <catch2/catch_test_macros.hpp>

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
    async::start(std::move(op));
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
