#include <async/completes_synchronously.hpp>
#include <async/env.hpp>
#include <async/just.hpp>
#include <async/just_result_of.hpp>
#include <async/read_env.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/schedulers/thread_scheduler.hpp>
#include <async/then.hpp>

#include <catch2/catch_test_macros.hpp>

TEST_CASE("empty env doesn't complete synchronously",
          "[completes_synchronously]") {
    static_assert(not async::completes_synchronously(async::empty_env{}));
}

TEST_CASE("just completes synchronously", "[completes_synchronously]") {
    constexpr auto s = async::just(42);
    static_assert(async::completes_synchronously(async::get_env(s)));
}

TEST_CASE("just_result_of completes synchronously",
          "[completes_synchronously]") {
    constexpr auto s = async::just_result_of([] { return 42; });
    static_assert(async::completes_synchronously(async::get_env(s)));
}

TEST_CASE("read_env completes synchronously", "[completes_synchronously]") {
    constexpr auto s = async::get_stop_token();
    static_assert(async::completes_synchronously(async::get_env(s)));
}

TEST_CASE("complete_synchronously is a forwarding query",
          "[completes_synchronously]") {
    constexpr auto s = async::just(42) | async::then([](auto v) { return v; });
    static_assert(async::completes_synchronously(async::get_env(s)));
}

TEST_CASE("inline_scheduler sender completes synchronously",
          "[completes_synchronously]") {
    constexpr auto s = async::inline_scheduler{}.schedule();
    static_assert(async::completes_synchronously(async::get_env(s)));
}

TEST_CASE("thread_scheduler sender does not complete synchronously",
          "[completes_synchronously]") {
    constexpr auto s = async::thread_scheduler{}.schedule();
    static_assert(not async::completes_synchronously(async::get_env(s)));
}
