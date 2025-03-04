#include "detail/common.hpp"

#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>

#include <catch2/catch_test_macros.hpp>

#include <concepts>
#include <functional>

TEST_CASE("empty env", "[env]") {
    auto const r = receiver{[] {}};
    static_assert(std::same_as<async::env_of_t<decltype(r)>, async::empty_env>);
}

TEST_CASE("nonexistent query", "[env]") {
    auto const e = async::empty_env{};
    CHECK(get_fwd(e) == none{});
}

TEST_CASE("non-empty env", "[env]") {
    auto const r = stoppable_receiver{[] {}};
    static_assert(
        not std::same_as<async::env_of_t<decltype(r)>, async::empty_env>);
}

TEST_CASE("single prop", "[env]") {
    auto const e = async::prop{get_fwd, 42};
    CHECK(get_fwd(e) == 42);
}

TEST_CASE("prop with move-only type", "[env]") {
    auto const e = async::prop{get_fwd, move_only{42}};
    CHECK(get_fwd(e).value == 42);
}

TEST_CASE("singleton env", "[env]") {
    auto const e = async::env{async::prop{get_fwd, 42}};
    CHECK(get_fwd(e) == 42);
}

TEST_CASE("doubleton env", "[env]") {
    auto const e =
        async::env{async::prop{get_fwd, 42}, async::prop{get_nofwd, 17}};
    CHECK(get_fwd(e) == 42);
    CHECK(get_nofwd(e) == 17);
}

TEST_CASE("env with duplicate property looks up the first one", "[env]") {
    auto const e =
        async::env{async::prop{get_fwd, 42}, async::prop{get_fwd, 17}};
    CHECK(get_fwd(e) == 42);
}

TEST_CASE("forwarding_env screens non-forwarding queries", "[env]") {
    auto const e =
        async::env{async::prop{get_fwd, 42},
                   async::forwarding_env{async::prop{get_nofwd, 17}}};
    CHECK(get_fwd(e) == 42);
    CHECK(get_nofwd(e) == none{});
}

TEST_CASE("env composition", "[env]") {
    auto const e1 = async::env{async::prop{get_fwd, 42}};
    auto const e2 = async::env{async::prop{get_fwd, 17}};
    auto const e = async::env{e1, e2};
    CHECK(get_fwd(e) == 42);
}

TEST_CASE("env composition (by ref)", "[env]") {
    auto const e1 = async::env{async::prop{get_fwd, 42}};
    auto const e2 = async::env{async::prop{get_nofwd, 17}};
    auto const e = async::env{std::cref(e1), std::cref(e2)};
    CHECK(get_fwd(e) == 42);
    CHECK(get_nofwd(e) == 17);
}

namespace {
struct test_queryable {
    [[nodiscard]] constexpr static auto query(async::get_env_t) noexcept
        -> custom_env {
        return {};
    }
};
} // namespace

TEST_CASE("forwarding_env_of", "[env]") {
    auto const e = async::forward_env_of(test_queryable{});
    CHECK(get_fwd(e) == 42);
    CHECK(get_nofwd(e) == none{});
}

TEST_CASE("template query prop", "[env]") {
    auto const e =
        async::make_template_prop<async::get_completion_scheduler_t,
                                  async::set_value_t, async::set_error_t,
                                  async::set_stopped_t>(42);
    CHECK(async::get_completion_scheduler<async::set_value_t>(e) == 42);
    CHECK(async::get_completion_scheduler<async::set_error_t>(e) == 42);
    CHECK(async::get_completion_scheduler<async::set_stopped_t>(e) == 42);
}

TEST_CASE("valid_query_for", "[env]") {
    static_assert(
        async::detail::valid_query_for<async::get_env_t, test_queryable>);
    static_assert(
        not async::detail::valid_query_for<async::get_env_t, async::empty_env>);
}

TEST_CASE("valid_query_over", "[env]") {
    static_assert(
        async::detail::valid_query_over<async::get_env_t, async::empty_env,
                                        test_queryable>);
    static_assert(not async::detail::valid_query_over<async::get_env_t,
                                                      async::empty_env>);
}
