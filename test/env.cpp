#include "detail/common.hpp"

#include <async/env.hpp>

#include <catch2/catch_test_macros.hpp>

#include <concepts>

TEST_CASE("empty env", "[env]") {
    auto r = receiver{[] {}};
    static_assert(std::same_as<async::env_of_t<decltype(r)>, async::empty_env>);
}

TEST_CASE("non-empty env", "[env]") {
    auto r = stoppable_receiver{[] {}};
    static_assert(
        std::same_as<async::env_of_t<decltype(r)>, typename decltype(r)::env>);
}

namespace {
struct test_receiver {
    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   test_receiver const &)
        -> custom_env {
        return {};
    }
};
} // namespace

TEST_CASE("singleton env", "[env]") {
    auto e = async::singleton_env<get_fwd_t>(42);
    CHECK(get_fwd(e) == 42);
}

TEST_CASE("forwarding env forwards forwarding queries", "[env]") {
    auto r = test_receiver{};
    CHECK(get_fwd(async::get_env(r)) == 42);
    CHECK(get_nofwd(async::get_env(r)) == 17);

    static_assert(async::forwarding_query(get_fwd));
    auto f = async::forward_env_of(r);
    CHECK(get_fwd(f) == 42);
    CHECK(get_nofwd(f) == none{});
}

TEST_CASE("override env with one value", "[env]") {
    auto r = test_receiver{};
    CHECK(get_fwd(async::get_env(r)) == 42);
    auto e = async::override_env_with<get_fwd_t>(1729, r);
    CHECK(get_fwd(e) == 1729);
}
