#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/type_traits.hpp>

#include <catch2/catch_template_test_macros.hpp>
#include <catch2/catch_test_macros.hpp>

#include <concepts>
#include <type_traits>

TEST_CASE("connect_result_t", "[type_traits]") {
    auto s = async::inline_scheduler{}.schedule();
    auto r = receiver{[] {}};
    STATIC_REQUIRE(async::operation_state<
                   async::connect_result_t<decltype(s), decltype(r)>>);
}

namespace {
template <typename...> struct variant;
template <typename...> struct tuple;
template <typename T> struct unary_tuple;
template <typename T> struct unary_variant;
} // namespace

TEST_CASE("gather signatures", "[type_traits]") {
    using sigs = async::completion_signatures<async::set_value_t(int),
                                              async::set_error_t(float),
                                              async::set_stopped_t()>;
    STATIC_REQUIRE(
        std::is_same_v<variant<tuple<int>>,
                       async::detail::gather_signatures<async::set_value_t,
                                                        sigs, tuple, variant>>);
    STATIC_REQUIRE(
        std::is_same_v<variant<tuple<float>>,
                       async::detail::gather_signatures<async::set_error_t,
                                                        sigs, tuple, variant>>);
    STATIC_REQUIRE(
        std::is_same_v<variant<tuple<>>,
                       async::detail::gather_signatures<async::set_stopped_t,
                                                        sigs, tuple, variant>>);
}

TEMPLATE_TEST_CASE("gather signatures (no signature for tag)", "[type_traits]",
                   async::set_value_t, async::set_error_t,
                   async::set_stopped_t) {
    using sigs = async::completion_signatures<>;
    STATIC_REQUIRE(
        std::is_same_v<variant<>, async::detail::gather_signatures<
                                      TestType, sigs, tuple, variant>>);
}

TEST_CASE("gather signatures (unary template)", "[type_traits]") {
    using sigs = async::completion_signatures<async::set_value_t(int)>;
    STATIC_REQUIRE(
        std::is_same_v<
            unary_variant<unary_tuple<int>>,
            async::detail::gather_signatures<async::set_value_t, sigs,
                                             unary_tuple, unary_variant>>);
}

namespace {
struct typed_sender1 {
    using completion_signatures =
        async::completion_signatures<async::set_value_t(int),
                                     async::set_error_t(float),
                                     async::set_stopped_t()>;
};
struct typed_sender2 {
    using completion_signatures = async::completion_signatures<>;
};
} // namespace

TEST_CASE("completion signatures with exposed type", "[type_traits]") {
    STATIC_REQUIRE(
        std::same_as<typename typed_sender1::completion_signatures,
                     async::completion_signatures_of_t<typed_sender1>>);
    STATIC_REQUIRE(
        std::same_as<typename typed_sender2::completion_signatures,
                     async::completion_signatures_of_t<typed_sender2>>);
}

TEST_CASE("typed completion signatures by channel", "[type_traits]") {
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures<async::set_value_t(int)>,
                     async::value_signatures_of_t<typed_sender1>>);
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures<async::set_error_t(float)>,
                     async::error_signatures_of_t<typed_sender1>>);
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures<async::set_stopped_t()>,
                     async::stopped_signatures_of_t<typed_sender1>>);
}

namespace {
struct queryable_sender1 {
    [[nodiscard]] constexpr static auto
    get_completion_signatures(auto &&) noexcept
        -> async::completion_signatures<async::set_value_t(int),
                                        async::set_error_t(float),
                                        async::set_stopped_t()> {
        return {};
    }
};

struct queryable_sender2 {
    [[nodiscard]] constexpr static auto
    get_completion_signatures(auto &&) noexcept
        -> async::completion_signatures<> {
        return {};
    }

    // query takes precedence over the exposed type
    using completion_signatures =
        async::completion_signatures<async::set_value_t(int),
                                     async::set_error_t(float),
                                     async::set_stopped_t()>;
};
} // namespace

TEST_CASE("completion signatures with exposed query", "[type_traits]") {
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<queryable_sender1>,
                     async::completion_signatures<async::set_value_t(int),
                                                  async::set_error_t(float),
                                                  async::set_stopped_t()>>);
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<queryable_sender2>,
                     async::completion_signatures<>>);
}

TEST_CASE("queryable completion signatures by channel", "[type_traits]") {
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures<async::set_value_t(int)>,
                     async::value_signatures_of_t<queryable_sender1>>);
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures<async::set_error_t(float)>,
                     async::error_signatures_of_t<queryable_sender1>>);
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures<async::set_stopped_t()>,
                     async::stopped_signatures_of_t<queryable_sender1>>);
}

namespace {
template <typename T> struct dependent_env {
    using type = T;
};

struct queryable_sender3 {
    template <typename Env>
    [[nodiscard]] constexpr static auto
    get_completion_signatures(Env const &) noexcept
        -> async::completion_signatures<
            async::set_value_t(typename Env::type)> {
        return {};
    }
};
} // namespace

TEST_CASE("queryable completion signatures dependent on environment",
          "[type_traits]") {
    STATIC_REQUIRE(
        std::same_as<async::completion_signatures_of_t<queryable_sender3,
                                                       dependent_env<int>>,
                     async::completion_signatures<async::set_value_t(int)>>);
}

TEST_CASE("types by channel (exposed types)", "[type_traits]") {
    STATIC_REQUIRE(
        std::same_as<variant<tuple<int>>,
                     async::value_types_of_t<typed_sender1, async::empty_env,
                                             tuple, variant>>);
    STATIC_REQUIRE(
        std::same_as<variant<tuple<float>>,
                     async::error_types_of_t<typed_sender1, async::empty_env,
                                             tuple, variant>>);
    STATIC_REQUIRE(
        std::same_as<variant<tuple<>>,
                     async::stopped_types_of_t<typed_sender1, async::empty_env,
                                               tuple, variant>>);
    STATIC_REQUIRE(async::sends_stopped<typed_sender1>);

    STATIC_REQUIRE(
        std::same_as<variant<>,
                     async::value_types_of_t<typed_sender2, async::empty_env,
                                             tuple, variant>>);
    STATIC_REQUIRE(
        std::same_as<variant<>,
                     async::error_types_of_t<typed_sender2, async::empty_env,
                                             tuple, variant>>);
    STATIC_REQUIRE(
        std::same_as<variant<>,
                     async::stopped_types_of_t<typed_sender2, async::empty_env,
                                               tuple, variant>>);
    STATIC_REQUIRE(not async::sends_stopped<typed_sender2>);
}

TEST_CASE("types by channel (queries with empty env)", "[type_traits]") {
    STATIC_REQUIRE(
        std::same_as<variant<tuple<int>>,
                     async::value_types_of_t<
                         queryable_sender1, async::empty_env, tuple, variant>>);
    STATIC_REQUIRE(
        std::same_as<variant<tuple<float>>,
                     async::error_types_of_t<
                         queryable_sender1, async::empty_env, tuple, variant>>);
    STATIC_REQUIRE(
        std::same_as<variant<tuple<>>,
                     async::stopped_types_of_t<
                         queryable_sender1, async::empty_env, tuple, variant>>);
    STATIC_REQUIRE(async::sends_stopped<queryable_sender1>);

    STATIC_REQUIRE(
        std::same_as<variant<>,
                     async::value_types_of_t<
                         queryable_sender2, async::empty_env, tuple, variant>>);
    STATIC_REQUIRE(
        std::same_as<variant<>,
                     async::error_types_of_t<
                         queryable_sender2, async::empty_env, tuple, variant>>);
    STATIC_REQUIRE(
        std::same_as<variant<>,
                     async::stopped_types_of_t<
                         queryable_sender2, async::empty_env, tuple, variant>>);
    STATIC_REQUIRE(not async::sends_stopped<queryable_sender2>);
}

TEST_CASE("types by channel (queries with dependent env)", "[type_traits]") {
    STATIC_REQUIRE(
        std::same_as<
            variant<tuple<int>>,
            async::value_types_of_t<queryable_sender3, dependent_env<int>,
                                    tuple, variant>>);
    STATIC_REQUIRE(
        std::same_as<
            variant<tuple<float>>,
            async::value_types_of_t<queryable_sender3, dependent_env<float>,
                                    tuple, variant>>);
}

TEST_CASE("types by channel (non-variadic templates)", "[type_traits]") {
    STATIC_REQUIRE(
        std::same_as<tuple<int>,
                     async::value_types_of_t<typed_sender1, async::empty_env,
                                             tuple, std::type_identity_t>>);
    STATIC_REQUIRE(
        std::same_as<tuple<float>,
                     async::error_types_of_t<typed_sender1, async::empty_env,
                                             tuple, std::type_identity_t>>);
    STATIC_REQUIRE(
        std::same_as<variant<unary_tuple<int>>,
                     async::value_types_of_t<typed_sender1, async::empty_env,
                                             unary_tuple, variant>>);

    STATIC_REQUIRE(
        std::same_as<int, async::value_types_of_t<
                              typed_sender1, async::empty_env,
                              std::type_identity_t, std::type_identity_t>>);
}

namespace {
template <typename S, typename Tag>
concept single_sender = requires {
    typename async::detail::gather_signatures<
        Tag, async::completion_signatures_of_t<S, async::empty_env>,
        unary_tuple, unary_variant>;
};
} // namespace

TEST_CASE("non-variadic templates in concept", "[type_traits]") {
    STATIC_REQUIRE(single_sender<typed_sender1, async::set_value_t>);
    STATIC_REQUIRE(single_sender<typed_sender1, async::set_error_t>);
    STATIC_REQUIRE(not single_sender<typed_sender2, async::set_value_t>);
}

TEST_CASE("channel holder (values)", "[type_traits]") {
    int value{};
    auto r = receiver{[&](auto i) { value = i; }};
    auto h = async::value_holder<int>{42};
    h(std::move(r));
    CHECK(value == 42);
}

TEST_CASE("channel holder (error)", "[type_traits]") {
    int value{};
    auto r = error_receiver{[&](auto i) { value = i; }};
    auto h = async::error_holder<int>{42};
    h(std::move(r));
    CHECK(value == 42);
}

TEST_CASE("channel holder (stopped)", "[type_traits]") {
    int value{};
    auto r = stopped_receiver{[&] { value = 42; }};
    auto h = async::stopped_holder<>{};
    h(std::move(r));
    CHECK(value == 42);
}

namespace {
template <typename Tag> struct set_lvalue {
    template <typename... As>
    using fn =
        async::completion_signatures<Tag(std::add_lvalue_reference_t<As>...)>;
};
} // namespace

TEST_CASE("transform completion signatures (values)", "[type_traits]") {
    using sigs = async::completion_signatures<async::set_value_t(int),
                                              async::set_value_t(float, bool)>;
    using transformed_sigs = async::transform_completion_signatures<
        sigs, async::completion_signatures<>,
        set_lvalue<async::set_value_t>::template fn>;

    STATIC_REQUIRE(
        std::is_same_v<
            async::completion_signatures<async::set_value_t(int &),
                                         async::set_value_t(float &, bool &)>,
            transformed_sigs>);
}

TEST_CASE("transform completion signatures (errors)", "[type_traits]") {
    using sigs = async::completion_signatures<async::set_error_t(int),
                                              async::set_error_t(float)>;
    using transformed_sigs = async::transform_completion_signatures<
        sigs, async::completion_signatures<>, async::detail::default_set_value,
        set_lvalue<async::set_error_t>::template fn>;

    STATIC_REQUIRE(std::is_same_v<
                   async::completion_signatures<async::set_error_t(int &),
                                                async::set_error_t(float &)>,
                   transformed_sigs>);
}

TEST_CASE("transform completion signatures (stopped)", "[type_traits]") {
    {
        using sigs = async::completion_signatures<async::set_stopped_t()>;
        using transformed_sigs = async::transform_completion_signatures<
            sigs, async::completion_signatures<>,
            async::detail::default_set_value, async::detail::default_set_error,
            async::completion_signatures<async::set_value_t()>>;
        STATIC_REQUIRE(
            std::is_same_v<async::completion_signatures<async::set_value_t()>,
                           transformed_sigs>);
    }
    {
        using sigs = async::completion_signatures<>;
        using transformed_sigs = async::transform_completion_signatures<sigs>;
        STATIC_REQUIRE(
            std::is_same_v<async::completion_signatures<>, transformed_sigs>);
    }
}
