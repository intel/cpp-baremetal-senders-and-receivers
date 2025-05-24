#include "detail/common.hpp"

#include <async/completes_synchronously.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/variant_sender.hpp>

#include <catch2/catch_test_macros.hpp>

#include <type_traits>

TEST_CASE("match with predicate", "[variant_sender]") {
    auto const m =
        async::match([](auto i) { return i == 1; }) >> [] { return 17; };
    CHECK(m.test(1));
    CHECK(not m.test(0));
    CHECK(m.invoke() == 17);
}

TEST_CASE("match with bool", "[variant_sender]") {
    auto const i = 1;
    auto const m = async::match(i == 1) >> [] { return 17; };
    CHECK(m.test());
    CHECK(m.invoke() == 17);
}

TEST_CASE("match with two arguments (predicate)", "[variant_sender]") {
    auto const m =
        async::match([](auto i) { return i == 1; }, [] { return 17; });
    CHECK(m.test(1));
    CHECK(not m.test(0));
    CHECK(m.invoke() == 17);
}

TEST_CASE("match with two arguments (bool)", "[variant_sender]") {
    auto const i = 1;
    auto const m = async::match(i == 1, [] { return 17; });
    CHECK(m.test());
    CHECK(m.invoke() == 17);
}

TEST_CASE("match with non-moveable type", "[variant_sender]") {
    auto const i = 1;
    auto const m = async::match(i == 1) >> [] { return non_moveable{}; };
    CHECK(m.test());
    [[maybe_unused]] auto nm = m.invoke();
    STATIC_REQUIRE(std::is_same_v<decltype(nm), non_moveable>);
}

TEST_CASE("matcher returns correct variant", "[variant_sender]") {
    auto const m = async::matcher{
        async::match([](auto i) { return i == 0; }) >> [](auto) { return 17; },
        async::otherwise >> [](auto) { return 3.14f; }};
    auto const v1 = m.run(0);
    REQUIRE(v1.index() == 0);
    CHECK(std::get<0>(v1) == 17);

    auto const v2 = m.run(1);
    REQUIRE(v2.index() == 1);
    CHECK(std::get<1>(v2) == 3.14f);
}

TEST_CASE("matcher with non-moveable type", "[variant_sender]") {
    auto const m =
        async::matcher{async::match([](auto i) { return i == 0; }) >>
                           [](auto) { return non_moveable{}; },
                       async::otherwise >> [](auto) { return 3.14f; }};
    auto const v1 = m.run(0);
    REQUIRE(v1.index() == 0);
    STATIC_REQUIRE(
        std::is_same_v<decltype(std::get<0>(v1)), non_moveable const &>);

    auto const v2 = m.run(1);
    REQUIRE(v2.index() == 1);
    CHECK(std::get<1>(v2) == 3.14f);
}

TEST_CASE("binary select", "[variant_sender]") {
    auto const i = 0;
    auto const v1 =
        async::select(i == 0, [] { return 17; }, [] { return 3.14f; });
    REQUIRE(v1.index() == 0);
    CHECK(std::get<0>(v1) == 17);

    auto const j = 1;
    auto const v2 =
        async::select(j == 0, [] { return 17; }, [] { return 3.14f; });
    REQUIRE(v2.index() == 1);
    CHECK(std::get<1>(v2) == 3.14f);
}

TEST_CASE("binary select with non-moveable type", "[variant_sender]") {
    auto const i = 0;
    auto const v1 = async::select(
        i == 0, [] { return non_moveable{}; }, [] { return 3.14f; });
    REQUIRE(v1.index() == 0);
    STATIC_REQUIRE(
        std::is_same_v<decltype(std::get<0>(v1)), non_moveable const &>);

    auto const j = 1;
    auto const v2 = async::select(
        j == 0, [] { return non_moveable{}; }, [] { return 3.14f; });
    REQUIRE(v2.index() == 1);
    CHECK(std::get<1>(v2) == 3.14f);
}

TEST_CASE("generalized make_variant", "[variant_sender]") {
    auto const v1 = async::make_variant(
        async::match([](auto i, auto j) { return i + j == 3; }) >>
            [](auto...) { return 42; },
        async::otherwise >> [](auto...) { return 3.14f; }, 1, 2);
    REQUIRE(v1.index() == 0);
    CHECK(std::get<0>(v1) == 42);

    auto const v2 = async::make_variant(
        async::match([](auto i, auto j) { return i + j == 2; }) >>
            [](auto...) { return 42; },
        async::otherwise >> [](auto...) { return 3.14f; }, 1, 2);
    REQUIRE(v2.index() == 1);
    CHECK(std::get<1>(v2) == 3.14f);
}

TEST_CASE("make_variant_sender (general choice)", "[variant_sender]") {
    auto const s = async::make_variant_sender(
        async::match([](auto i, auto j) { return i + j == 3; }) >>
            [](auto, auto) { return async::just(42); },
        async::match([](auto i, auto j) { return i + j == 5; }) >>
            [](auto, auto) { return async::just(17); },
        async::otherwise >> [](auto, auto) { return async::just_error(17); }, 1,
        2);

    STATIC_REQUIRE(
        std::is_same_v<async::completion_signatures_of_t<decltype(s)>,
                       async::completion_signatures<async::set_value_t(int),
                                                    async::set_error_t(int)>>);

    int value{};
    auto op = async::connect(s, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("make_optional_sender (unary choice)", "[variant_sender]") {
    auto const i = 0;
    auto const s =
        async::make_optional_sender(i == 0, [] { return async::just(42); });

    STATIC_REQUIRE(
        std::is_same_v<async::completion_signatures_of_t<decltype(s)>,
                       async::completion_signatures<async::set_value_t(int),
                                                    async::set_value_t()>>);

    int value{};
    auto op = async::connect(s, receiver{[&](auto... v) {
                                 if constexpr (sizeof...(v) == 1) {
                                     value = (v, ...);
                                 }
                             }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("make_variant_sender (simplified binary choice)",
          "[variant_sender]") {
    auto const i = 0;
    auto const s = async::make_variant_sender(
        i == 0, [] { return async::just(42); },
        [] { return async::just_error(17); });

    STATIC_REQUIRE(
        std::is_same_v<async::completion_signatures_of_t<decltype(s)>,
                       async::completion_signatures<async::set_value_t(int),
                                                    async::set_error_t(int)>>);

    int value{};
    auto op = async::connect(s, receiver{[&](auto v) { value = v; }});
    async::start(op);
    CHECK(value == 42);
}

TEST_CASE("variant_sender may complete synchronously", "[variant_sender]") {
    auto const i = 0;
    [[maybe_unused]] auto const s = async::make_variant_sender(
        i == 0, [] { return async::just(42); },
        [] { return async::just_error(17); });
    STATIC_REQUIRE(async::synchronous<decltype(s)>);
}

TEST_CASE("variant_sender may not complete synchronously", "[variant_sender]") {
    auto const i = 0;
    [[maybe_unused]] auto const s = async::make_variant_sender(
        i == 0, [] { return async::just(42); },
        [] { return async::thread_scheduler{}.schedule(); });
    STATIC_REQUIRE(not async::synchronous<decltype(s)>);
}

TEST_CASE("variant_sender op state may be synchronous", "[variant_sender]") {
    auto const i = 0;
    auto const s = async::make_variant_sender(
        i == 0, [] { return async::just(42); },
        [] { return async::just_error(17); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(async::synchronous<decltype(op)>);
}

TEST_CASE("variant_sender op state may not be synchronous",
          "[variant_sender]") {
    auto const i = 0;
    auto const s = async::make_variant_sender(
        i == 0, [] { return async::just(42); },
        [] { return async::thread_scheduler{}.schedule(); });
    [[maybe_unused]] auto op = async::connect(s, receiver{[] {}});
    STATIC_REQUIRE(not async::synchronous<decltype(op)>);
}
