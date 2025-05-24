#include <async/debug_context.hpp>

#include <catch2/catch_test_macros.hpp>

namespace {
template <typename...> struct list;

struct leaf_context {
    struct tag;
    constexpr static auto name = stdx::ct_string{"name"};
    using children = list<>;
    using type = int;
};

struct branch_context {
    struct tag;
    constexpr static auto name = stdx::ct_string{"name"};
    using children = list<leaf_context>;
    using type = int;
};

struct bad_context_no_name {
    struct tag;
    using children = list<>;
    using type = int;
};

struct bad_context_no_tag {
    constexpr static auto name = stdx::ct_string{"name"};
    using children = list<>;
    using type = int;
};

struct bad_context_no_children {
    struct tag;
    constexpr static auto name = stdx::ct_string{"name"};
    using type = int;
};

struct bad_context_bad_children {
    struct tag;
    constexpr static auto name = stdx::ct_string{"name"};
    using children = list<int>;
    using type = int;
};

struct bad_context_no_type {
    struct tag;
    constexpr static auto name = stdx::ct_string{"name"};
    using children = list<>;
};
} // namespace

TEST_CASE("leaf context", "[debug_context]") {
    STATIC_REQUIRE(async::debug::contextlike<leaf_context>);
}

TEST_CASE("branch context", "[debug_context]") {
    STATIC_REQUIRE(async::debug::contextlike<branch_context>);
}

TEST_CASE("not a context (no name)", "[debug_context]") {
    STATIC_REQUIRE(not async::debug::contextlike<bad_context_no_name>);
}

TEST_CASE("not a context (no tag)", "[debug_context]") {
    STATIC_REQUIRE(not async::debug::contextlike<bad_context_no_tag>);
}

TEST_CASE("not a context (no children)", "[debug_context]") {
    STATIC_REQUIRE(not async::debug::contextlike<bad_context_no_children>);
}

TEST_CASE("not a context (children are not contexts)", "[debug_context]") {
    STATIC_REQUIRE(not async::debug::contextlike<bad_context_bad_children>);
}

TEST_CASE("not a context (no type)", "[debug_context]") {
    STATIC_REQUIRE(not async::debug::contextlike<bad_context_no_type>);
}
