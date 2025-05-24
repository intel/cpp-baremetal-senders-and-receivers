#include <async/forwarding_query.hpp>

#include <catch2/catch_test_macros.hpp>

namespace {
struct query1 {};

struct query2 {
    [[nodiscard]] constexpr static auto query(async::forwarding_query_t)
        -> bool {
        return true;
    }
};

struct query3 : async::forwarding_query_t {};
} // namespace

TEST_CASE("non-forwarding", "[forwarding_query]") {
    STATIC_REQUIRE(not async::forwarding_query(query1{}));
}

TEST_CASE("forwarding by query", "[forwarding_query]") {
    STATIC_REQUIRE(async::forwarding_query(query2{}));
}

TEST_CASE("forwarding by inheritance", "[forwarding_query]") {
    STATIC_REQUIRE(async::forwarding_query(query3{}));
}
