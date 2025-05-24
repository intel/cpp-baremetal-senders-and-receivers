#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/let.hpp>

#include <stdx/ct_string.hpp>
#include <stdx/type_traits.hpp>

#include <catch2/catch_test_macros.hpp>

#include <type_traits>
#include <utility>

namespace {
template <stdx::ct_string Name, typename S, typename F>
using multilet_sender =
    async::_let::sender<Name, S, F, async::set_value_t, async::set_stopped_t>;

template <typename F>
[[nodiscard]] constexpr auto multilet(F &&f)
    -> async::_let::pipeable<"", std::remove_cvref_t<F>, multilet_sender> {
    return {std::forward<F>(f)};
}
struct wrapped_int {
    int value;
};
} // namespace

TEST_CASE("multi-channel let", "[let_multichannel]") {
    int value{};
    {
        [[maybe_unused]] auto l = async::just_stopped() | multilet([] {
                                      return async::just(wrapped_int{42});
                                  });
        auto op = async::connect(
            l, receiver{[&](wrapped_int i) { value = i.value; }});
        async::start(op);
        CHECK(value == 42);
    }
    {
        [[maybe_unused]] auto l = async::just(17) | multilet([](auto i) {
                                      return async::just(wrapped_int{i});
                                  });
        auto op = async::connect(
            l, receiver{[&](wrapped_int i) { value = i.value; }});
        async::start(op);
        CHECK(value == 17);
    }
}

TEST_CASE("multi-channel let advertises what it sends", "[let_multichannel]") {
    [[maybe_unused]] auto l = async::just(42) | multilet([](auto...) {
                                  return async::just(wrapped_int{42});
                              });
    STATIC_REQUIRE(
        async::sender_of<decltype(l), async::set_value_t(wrapped_int)>);
}

TEST_CASE("multi-channel let advertises pass-through completions",
          "[let_multichannel]") {
    [[maybe_unused]] auto l = async::just_error(42) | multilet([](auto) {});
    STATIC_REQUIRE(async::sender_of<decltype(l), async::set_error_t(int)>);
}

TEST_CASE("multi-channel let can be single shot", "[let_multichannel]") {
    [[maybe_unused]] auto l = async::just(42) | multilet([](auto i) {
                                  return async::just(move_only{i});
                              });
    STATIC_REQUIRE(async::singleshot_sender<decltype(l)>);
}

TEST_CASE("multi-channel let can be single shot with passthrough",
          "[let_multichannel]") {
    [[maybe_unused]] auto l =
        async::just_error(move_only{42}) | multilet([](auto) { return 42; });
    STATIC_REQUIRE(async::singleshot_sender<decltype(l)>);
}

namespace {
template <stdx::ct_string Name, typename S, typename F>
using allchannel_sender =
    async::_let::sender<Name, S, F, async::set_value_t, async::set_error_t,
                        async::set_stopped_t>;

template <typename F>
[[nodiscard]] constexpr auto all_let(F &&f)
    -> async::_let::pipeable<"", std::remove_cvref_t<F>, allchannel_sender> {
    return {std::forward<F>(f)};
}
} // namespace

TEST_CASE("multi-channel let supports compile-time channel differentiation",
          "[let_multichannel]") {
    int value{};

    auto fn = [&]<async::channel_tag Tag>(auto...) {
        if constexpr (std::is_same_v<Tag, async::set_value_t>) {
            return async::just(1);
        } else if constexpr (std::is_same_v<Tag, async::set_error_t>) {
            return async::just(2);
        } else if constexpr (std::is_same_v<Tag, async::set_stopped_t>) {
            return async::just(3);
        } else {
            STATIC_REQUIRE(stdx::always_false_v<Tag>);
        }
    };
    {
        auto l = async::just() | all_let(fn);
        auto op = async::connect(l, receiver{[&](auto i) { value = i; }});
        async::start(op);
        CHECK(value == 1);
    }
    {
        auto l = async::just_error(0) | all_let(fn);
        auto op = async::connect(l, receiver{[&](auto i) { value = i; }});
        async::start(op);
        CHECK(value == 2);
    }
    {
        auto l = async::just_stopped() | all_let(fn);
        auto op = async::connect(l, receiver{[&](auto i) { value = i; }});
        async::start(op);
        CHECK(value == 3);
    }
}
