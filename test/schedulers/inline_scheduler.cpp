#include "detail/common.hpp"

#include <async/allocator.hpp>
#include <async/completes_synchronously.hpp>
#include <async/concepts.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/stack_allocator.hpp>

#include <stdx/ct_format.hpp>
#include <stdx/type_traits.hpp>

#include <boost/mp11/list.hpp>
#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <concepts>
#include <string>
#include <type_traits>
#include <vector>

TEST_CASE("inline_scheduler fulfils concept", "[inline_scheduler]") {
    static_assert(async::scheduler<async::inline_scheduler<>>);
}

TEST_CASE("inline_scheduler start immediately completes",
          "[inline_scheduler]") {
    bool recvd{};
    auto s = async::inline_scheduler<>::schedule();
    auto op = async::connect(s, receiver{[&] { recvd = true; }});
    async::start(op);
    CHECK(recvd);
}

TEST_CASE("inline_scheduler sender advertises nothing", "[inline_scheduler]") {
    static_assert(
        async::sender_of<decltype(async::inline_scheduler<>::schedule()),
                         async::set_value_t()>);
}

TEST_CASE("singleshot inline_scheduler", "[inline_scheduler]") {
    [[maybe_unused]] auto s = async::inline_scheduler<>::schedule<
        async::inline_scheduler<>::singleshot>();
    static_assert(async::singleshot_sender<decltype(s), universal_receiver>);
}

TEST_CASE("multishot inline_scheduler", "[inline_scheduler]") {
    [[maybe_unused]] auto s = async::inline_scheduler<>::schedule<
        async::inline_scheduler<>::multishot>();
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);

    [[maybe_unused]] auto s_default = async::inline_scheduler<>::schedule();
    static_assert(
        async::multishot_sender<decltype(s_default), universal_receiver>);
}

TEST_CASE("sender has the inline_scheduler as its completion scheduler",
          "[inline_scheduler]") {
    auto s1 = async::inline_scheduler<>::schedule();
    auto cs1 =
        async::get_completion_scheduler<async::set_value_t>(async::get_env(s1));
    static_assert(std::same_as<decltype(cs1), async::inline_scheduler<>>);

    auto s2 = async::inline_scheduler<>::schedule<
        async::inline_scheduler<>::singleshot>();
    auto cs2 =
        async::get_completion_scheduler<async::set_value_t>(async::get_env(s2));
    static_assert(std::same_as<decltype(cs2), async::inline_scheduler<>>);
}

TEST_CASE("sender has a stack allocator", "[inline_scheduler]") {
    static_assert(
        std::is_same_v<async::allocator_of_t<async::env_of_t<
                           decltype(async::inline_scheduler<>::schedule())>>,
                       async::stack_allocator>);
}

TEST_CASE("op state is synchronous", "[inline_scheduler]") {
    [[maybe_unused]] auto op =
        async::connect(async::inline_scheduler<>::schedule(), receiver{[] {}});
    static_assert(async::synchronous<decltype(op)>);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    constexpr auto signal(auto &&...) {
        static_assert(std::is_same_v<async::debug::tag_of<Ctx>,
                                     async::inline_scheduler_sender_t>);
        static_assert(
            boost::mp11::mp_empty<async::debug::children_of<Ctx>>::value);
        debug_events.push_back(
            fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("inline_scheduler can be debugged with a string",
          "[inline_scheduler]") {
    using namespace std::string_literals;
    debug_events.clear();
    auto s = async::inline_scheduler<>::schedule();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events == std::vector{"op inline_scheduler start"s,
                                      "op inline_scheduler set_value"s});
}

TEST_CASE("inline_scheduler can be named and debugged with a string",
          "[inline_scheduler]") {
    using namespace std::string_literals;
    debug_events.clear();
    auto s = async::inline_scheduler<"sched">::schedule();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);
    CHECK(debug_events ==
          std::vector{"op sched start"s, "op sched set_value"s});
}
