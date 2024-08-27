#include "detail/common.hpp"

#include <async/allocator.hpp>
#include <async/completes_synchronously.hpp>
#include <async/concepts.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/schedulers/thread_scheduler.hpp>

#include <stdx/ct_format.hpp>
#include <stdx/type_traits.hpp>

#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <concepts>
#include <condition_variable>
#include <mutex>
#include <string>
#include <thread>
#include <type_traits>
#include <vector>

TEST_CASE("thread_scheduler fulfils concept", "[thread_scheduler]") {
    static_assert(async::scheduler<async::thread_scheduler<>>);
}

TEST_CASE("thread_scheduler sender advertises nothing", "[thread_scheduler]") {
    static_assert(
        async::sender_of<decltype(async::thread_scheduler<>::schedule()),
                         async::set_value_t()>);
}

TEST_CASE("thread_scheduler sender completes on a separate thread",
          "[thread_scheduler]") {
    auto const this_thread_id = std::this_thread::get_id();
    std::thread::id other_thread_id{};

    bool recvd{};
    std::mutex m{};
    std::condition_variable cv{};

    auto s = async::thread_scheduler<>::schedule();
    auto op = async::connect(s, receiver{[&] {
                                 other_thread_id = std::this_thread::get_id();
                                 std::lock_guard l{m};
                                 recvd = true;
                                 cv.notify_one();
                             }});
    async::start(op);

    std::unique_lock l{m};
    cv.wait(l, [&] { return recvd; });
    CHECK(this_thread_id != other_thread_id);
}

TEST_CASE("sender has the thread_scheduler as its completion scheduler",
          "[inline_scheduler]") {
    auto s = async::thread_scheduler<>::schedule();
    auto cs =
        async::get_completion_scheduler<async::set_value_t>(async::get_env(s));
    static_assert(std::same_as<decltype(cs), async::thread_scheduler<>>);
}

TEST_CASE("sender has a static allocator", "[inline_scheduler]") {
    static_assert(
        std::is_same_v<async::allocator_of_t<async::env_of_t<
                           decltype(async::thread_scheduler<>::schedule())>>,
                       async::static_allocator>);
}

TEST_CASE("op state is not synchronous", "[inline_scheduler]") {
    [[maybe_unused]] auto op =
        async::connect(async::thread_scheduler<>::schedule(), receiver{[] {}});
    static_assert(not async::synchronous<decltype(op)>);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string L, stdx::ct_string S,
              typename Ctx>
    constexpr auto signal(auto &&...) {
        debug_events.push_back(fmt::format("{} {} {}", C, L, S));
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("thread_scheduler can be debugged with a string",
          "[thread_scheduler]") {
    using namespace std::string_literals;
    debug_events.clear();

    bool recvd{};
    std::mutex m{};
    std::condition_variable cv{};

    auto s = async::thread_scheduler<>::schedule();
    auto op = async::connect(
        s, with_env{receiver{[&] {
                        std::lock_guard l{m};
                        recvd = true;
                        cv.notify_one();
                    }},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    std::unique_lock l{m};
    cv.wait(l, [&] { return recvd; });
    CHECK(debug_events == std::vector{"op thread_scheduler start"s,
                                      "op thread_scheduler set_value"s});
}

TEST_CASE("thread_scheduler can be named and debugged with a string",
          "[thread_scheduler]") {
    using namespace std::string_literals;
    debug_events.clear();

    bool recvd{};
    std::mutex m{};
    std::condition_variable cv{};

    auto s = async::thread_scheduler<"sched">::schedule();
    auto op = async::connect(
        s, with_env{receiver{[&] {
                        std::lock_guard l{m};
                        recvd = true;
                        cv.notify_one();
                    }},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});

    async::start(op);
    std::unique_lock l{m};
    cv.wait(l, [&] { return recvd; });
    CHECK(debug_events ==
          std::vector{"op sched start"s, "op sched set_value"s});
}
