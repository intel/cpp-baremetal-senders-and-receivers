#include <async/forwarding_query.hpp>

#include <catch2/catch_test_macros.hpp>

namespace {
struct query1 {};

struct query2 {
    [[nodiscard]] friend constexpr auto tag_invoke(async::forwarding_query_t,
                                                   query2) -> bool {
        return true;
    }
};

struct query3 : async::forwarding_query_t {};
} // namespace

TEST_CASE("non-forwarding", "[forwarding_query]") {
    static_assert(not async::forwarding_query(query1{}));
}

TEST_CASE("forwarding by tag_invoke", "[forwarding_query]") {
    static_assert(async::forwarding_query(query2{}));
}

TEST_CASE("forwarding by inheritance", "[forwarding_query]") {
    static_assert(async::forwarding_query(query3{}));
}
