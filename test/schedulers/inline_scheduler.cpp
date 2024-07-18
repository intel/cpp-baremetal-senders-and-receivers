#include "detail/common.hpp"

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/stack_allocator.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("inline_scheduler fulfils concept", "[inline_scheduler]") {
    static_assert(async::scheduler<async::inline_scheduler>);
}

TEST_CASE("inline_scheduler start immediately completes",
          "[inline_scheduler]") {
    bool recvd{};
    auto s = async::inline_scheduler::schedule();
    auto op = async::connect(s, receiver{[&] { recvd = true; }});
    async::start(op);
    CHECK(recvd);
}

TEST_CASE("inline_scheduler sender advertises nothing", "[inline_scheduler]") {
    static_assert(
        async::sender_of<decltype(async::inline_scheduler::schedule()),
                         async::set_value_t()>);
}

TEST_CASE("singleshot inline_scheduler", "[inline_scheduler]") {
    [[maybe_unused]] auto s = async::inline_scheduler::schedule<
        async::inline_scheduler::singleshot>();
    static_assert(async::singleshot_sender<decltype(s), universal_receiver>);
}

TEST_CASE("multishot inline_scheduler", "[inline_scheduler]") {
    [[maybe_unused]] auto s =
        async::inline_scheduler::schedule<async::inline_scheduler::multishot>();
    static_assert(async::multishot_sender<decltype(s), universal_receiver>);

    [[maybe_unused]] auto s_default = async::inline_scheduler::schedule();
    static_assert(
        async::multishot_sender<decltype(s_default), universal_receiver>);
}

TEST_CASE("sender has the inline_scheduler as its completion scheduler",
          "[inline_scheduler]") {
    auto s1 = async::inline_scheduler::schedule();
    auto cs1 =
        async::get_completion_scheduler<async::set_value_t>(async::get_env(s1));
    static_assert(std::same_as<decltype(cs1), async::inline_scheduler>);

    auto s2 = async::inline_scheduler::schedule<
        async::inline_scheduler::singleshot>();
    auto cs2 =
        async::get_completion_scheduler<async::set_value_t>(async::get_env(s2));
    static_assert(std::same_as<decltype(cs2), async::inline_scheduler>);
}

TEST_CASE("sender has a stack allocator", "[inline_scheduler]") {
    static_assert(
        std::is_same_v<async::allocator_of_t<async::env_of_t<
                           decltype(async::inline_scheduler::schedule())>>,
                       async::stack_allocator>);
}
