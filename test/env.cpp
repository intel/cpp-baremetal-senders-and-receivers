#include "detail/common.hpp"

#include <async/env.hpp>

#include <catch2/catch_test_macros.hpp>

#include <concepts>
#include <functional>

TEST_CASE("empty env", "[env]") {
    auto r = receiver{[] {}};
    static_assert(std::same_as<async::env_of_t<decltype(r)>, async::empty_env>);
}

TEST_CASE("non-empty env", "[env]") {
    auto r = stoppable_receiver{[] {}};
    static_assert(
        std::same_as<async::env_of_t<decltype(r)>, typename decltype(r)::env>);
}

TEST_CASE("single prop", "[env]") {
    auto e = async::prop{get_fwd, 42};
    CHECK(get_fwd(e) == 42);
}

TEST_CASE("singleton env", "[env]") {
    auto e = async::env{async::prop{get_fwd, 42}};
    CHECK(get_fwd(e) == 42);
}

TEST_CASE("doubleton env", "[env]") {
    auto e = async::env{async::prop{get_fwd, 42}, async::prop{get_nofwd, 17}};
    CHECK(get_fwd(e) == 42);
    CHECK(get_nofwd(e) == 17);
}

TEST_CASE("env with duplicate property looks up the first one", "[env]") {
    auto e = async::env{async::prop{get_fwd, 42}, async::prop{get_fwd, 17}};
    CHECK(get_fwd(e) == 42);
}

TEST_CASE("forwarding_env screens non-forwarding queries", "[env]") {
    auto e = async::env{async::prop{get_fwd, 42},
                        async::forwarding_env{async::prop{get_nofwd, 17}}};
    CHECK(get_nofwd(e) == none{});
}

TEST_CASE("env composition", "[env]") {
    auto e1 = async::env{async::prop{get_fwd, 42}};
    auto e2 = async::env{async::prop{get_fwd, 17}};
    auto e = async::env{e1, e2};
    CHECK(get_fwd(e) == 42);
}

TEST_CASE("env composition (by ref)", "[env]") {
    auto e1 = async::env{async::prop{get_fwd, 42}};
    auto e2 = async::env{async::prop{get_nofwd, 17}};
    auto e = async::env{std::cref(e1), std::cref(e2)};
    CHECK(get_fwd(e) == 42);
    CHECK(get_nofwd(e) == 17);
}

namespace {
struct test_queryable {
    [[nodiscard]] constexpr static auto
    query(async::get_env_t) noexcept -> custom_env {
        return {};
    }
};
} // namespace

TEST_CASE("forwarding_env_of", "[env]") {
    auto e = async::forward_env_of(test_queryable{});
    CHECK(get_fwd(e) == 42);
    CHECK(get_nofwd(e) == none{});
}
