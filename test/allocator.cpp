#include "detail/common.hpp"

#include <async/allocator.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/sequence.hpp>
#include <async/stack_allocator.hpp>
#include <async/static_allocator.hpp>
#include <async/then.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("default allocator is static_allocator", "[allocator]") {
    static_assert(std::is_same_v<async::allocator_of_t<async::empty_env>,
                                 async::static_allocator>);
}

TEST_CASE("allocator is forwarded through senders", "[allocator]") {
    [[maybe_unused]] auto s1 =
        async::inline_scheduler{}.schedule() | async::then([] {});
    static_assert(
        std::is_same_v<async::allocator_of_t<async::env_of_t<decltype(s1)>>,
                       async::stack_allocator>);

    [[maybe_unused]] auto s2 =
        async::thread_scheduler{}.schedule() | async::then([] {});
    static_assert(
        std::is_same_v<async::allocator_of_t<async::env_of_t<decltype(s2)>>,
                       async::static_allocator>);
}

namespace {
struct domain;
struct multi_domain;

struct S {
    S(int x) : i{x} {}
    int i;
};
} // namespace

TEST_CASE("static allocate with default limit 1", "[allocator]") {
    auto alloc = async::static_allocator{};
    CHECK(alloc.construct<domain, S>(
        [&](auto &&s) {
            CHECK(s.i == 42);
            alloc.destruct<domain>(&s);
        },
        42));
}

TEST_CASE("static allocate fails when limit is reached", "[allocator]") {
    auto alloc = async::static_allocator{};
    CHECK(alloc.construct<domain, S>(
        [&](auto &&s) {
            CHECK(s.i == 42);
            auto q =
                alloc.construct<domain, S>([](auto &&) { CHECK(false); }, 17);
            CHECK(not q);
            alloc.destruct<domain>(&s);
        },
        42));
}

template <>
constexpr inline auto async::static_allocation_limit<multi_domain> =
    std::size_t{2};

TEST_CASE("static allocate more than 1", "[allocator]") {
    auto alloc = async::static_allocator{};
    CHECK(alloc.construct<multi_domain, S>(
        [&](auto &&p) {
            CHECK(p.i == 42);

            CHECK(alloc.construct<multi_domain, S>(
                [&](auto &&q) {
                    CHECK(q.i == 17);
                    auto x = alloc.construct<multi_domain, S>(
                        [](auto &&) { CHECK(false); }, 0);
                    CHECK(not x);
                    alloc.destruct<multi_domain>(&q);
                },
                17));
            alloc.destruct<multi_domain>(&p);
        },
        42));
}

namespace {
struct nm : non_moveable {
    nm(int x) : i{x} {}
    nm(nm &&) = delete;
    int i;
};
} // namespace

TEST_CASE("static allocate non-movable objects", "[allocator]") {
    auto alloc = async::static_allocator{};
    CHECK(alloc.construct<domain, nm>(
        [&](auto &&n) {
            CHECK(n.i == 42);
            alloc.destruct<domain>(&n);
        },
        42));
}

namespace {
int construction_count{};
struct counter {
    counter(int x) : i{x} { ++construction_count; }
    int i;
};
} // namespace

TEST_CASE("stack allocate lvalues", "[allocator]") {
    construction_count = 0;
    auto alloc = async::stack_allocator{};
    CHECK(alloc.construct<domain, counter>(
        [&](counter &) { CHECK(construction_count == 1); }, 42));
}

TEST_CASE("stack allocate rvalues", "[allocator]") {
    construction_count = 0;
    auto alloc = async::stack_allocator{};
    CHECK(alloc.construct<domain, counter>(
        [&](counter) { CHECK(construction_count == 1); }, 42));
}

TEST_CASE("static allocator is an allocator", "[allocator]") {
    static_assert(async::allocator<async::static_allocator>);
}

TEST_CASE("stack allocator is an allocator", "[allocator]") {
    static_assert(async::allocator<async::stack_allocator>);
}

TEST_CASE("sender that completes synchronously defaults to a stack allocator",
          "[allocator]") {
    [[maybe_unused]] auto s = async::inline_scheduler{}.schedule();
    static_assert(
        std::is_same_v<async::allocator_of_t<async::env_of_t<decltype(s)>>,
                       async::stack_allocator>);
}

TEST_CASE("sender that completes asynchronously defaults to a static allocator",
          "[allocator]") {
    [[maybe_unused]] auto s = async::thread_scheduler{}.schedule();
    static_assert(
        std::is_same_v<async::allocator_of_t<async::env_of_t<decltype(s)>>,
                       async::static_allocator>);
}
