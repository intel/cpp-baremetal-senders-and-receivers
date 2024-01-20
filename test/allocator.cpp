#include "detail/common.hpp"

#include <async/allocator.hpp>

#include <stdx/functional.hpp>

#include <catch2/catch_test_macros.hpp>

namespace {
struct domain;
struct multi_domain;

struct S {
    S(int x) : i{x} {}
    int i;
};
} // namespace

TEST_CASE("allocate with default limit 1", "[allocator]") {
    auto &alloc = async::get_allocator<domain, S>();
    auto p = alloc.construct(42);
    REQUIRE(p);
    CHECK(p->i == 42);
    alloc.destruct(p);
}

TEST_CASE("allocate fails when static limit is reached", "[allocator]") {
    auto &alloc = async::get_allocator<domain, S>();
    auto p = alloc.construct(42);
    REQUIRE(p);
    auto q = alloc.construct(17);
    CHECK(not q);
    alloc.destruct(p);
}

template <>
constexpr inline auto async::allocation_limit<multi_domain> = std::size_t{2};

TEST_CASE("allocate more than 1", "[allocator]") {
    auto &alloc = async::get_allocator<multi_domain, S>();
    auto p = alloc.construct(42);
    REQUIRE(p);
    auto q = alloc.construct(17);
    REQUIRE(q);
    auto x = alloc.construct(1);
    CHECK(not x);

    CHECK(p->i == 42);
    CHECK(q->i == 17);
    alloc.destruct(p);
    alloc.destruct(q);
}

namespace {
struct nm : non_moveable {
    int i;
};
} // namespace

TEST_CASE("allocate non-movable objects", "[allocator]") {
    auto &alloc = async::get_allocator<domain, nm>();
    auto p = alloc.construct(stdx::with_result_of{[] { return nm{{}, 42}; }});
    REQUIRE(p);
    CHECK(p->i == 42);
    alloc.destruct(p);
}
