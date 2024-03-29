#include <async/concepts.hpp>
#include <async/stop_token.hpp>
#include <conc/concurrency.hpp>

#include <catch2/catch_test_macros.hpp>

#include <memory>

TEST_CASE("concepts", "[stop_token]") {
    static_assert(async::stoppable_token<async::inplace_stop_token>);
    static_assert(async::stoppable_token<async::never_stop_token>);
    static_assert(async::unstoppable_token<async::never_stop_token>);
    static_assert(
        async::stoppable_token_for<async::inplace_stop_token, decltype([] {})>);
}

TEST_CASE("never_stop_token", "[stop_token]") {
    constexpr auto t = async::never_stop_token{};
    static_assert(not t.stop_possible());
    static_assert(not t.stop_requested());
}

TEST_CASE("request_stop returns true once", "[stop_token]") {
    auto s = async::inplace_stop_source{};

    REQUIRE(s.stop_possible());
    CHECK(s.request_stop());
    CHECK(s.stop_requested());
    CHECK(not s.request_stop());
    CHECK(s.stop_requested());
}

TEST_CASE("stop request is visible to all tokens", "[stop_token]") {
    auto s = async::inplace_stop_source{};

    auto const t1 = s.get_token();
    CHECK(s.request_stop());
    CHECK(t1.stop_requested());

    auto const t2 = s.get_token();
    CHECK(t2.stop_requested());
}

TEST_CASE("stop callbacks are called on request_stop", "[stop_token]") {
    auto s = async::inplace_stop_source{};
    auto const t = s.get_token();

    auto stopped = false;
    auto cb = async::inplace_stop_callback{t, [&] { stopped = true; }};

    CHECK(s.request_stop());
    CHECK(stopped);
}

TEST_CASE("destroyed stop callbacks are handled properly", "[stop_token]") {
    auto s = async::inplace_stop_source{};
    auto const t = s.get_token();

    auto stopped = 2;
    auto l = [&] { --stopped; };

    auto cb1 = async::inplace_stop_callback{t, l};
    auto cb2 =
        std::make_unique<async::inplace_stop_callback<decltype(l)>>(t, l);
    auto cb3 = async::inplace_stop_callback{t, l};

    cb2.reset();
    CHECK(s.request_stop());
    CHECK(stopped == 0);
}

TEST_CASE("after request_stop, stop callback construction causes callback",
          "[stop_token]") {
    auto s = async::inplace_stop_source{};
    auto const t = s.get_token();

    auto stopped = 2;
    auto l = [&] { --stopped; };

    auto cb1 = async::inplace_stop_callback{t, l};
    CHECK(s.request_stop());
    CHECK(stopped == 1);

    auto cb2 = async::inplace_stop_callback{t, l};
    CHECK(stopped == 0);
}

TEST_CASE("stop callback can register another callback", "[stop_token]") {
    auto s = async::inplace_stop_source{};
    auto const t = s.get_token();

    auto stopped = 2;
    auto cb1 = async::inplace_stop_callback{
        t, [&] {
            --stopped;
            auto cb2 = async::inplace_stop_callback{t, [&] { --stopped; }};
        }};
    CHECK(s.request_stop());
    CHECK(stopped == 0);
}
