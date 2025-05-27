#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/get_scheduler.hpp>
#include <async/just.hpp>
#include <async/let_value.hpp>
#include <async/read_env.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/start_on.hpp>
#include <async/stop_token.hpp>
#include <async/sync_wait.hpp>
#include <async/then.hpp>
#include <async/type_traits.hpp>

#include <stdx/ct_format.hpp>
#include <stdx/type_traits.hpp>

#include <boost/mp11/list.hpp>
#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <string>
#include <type_traits>
#include <vector>

TEST_CASE("read_env advertises what it sends", "[read_env]") {
    [[maybe_unused]] auto r = stoppable_receiver{[] {}};
    using E = async::env_of_t<decltype(r)>;
    using ST = async::stop_token_of_t<E>;
    STATIC_REQUIRE(
        async::sender_of<decltype(async::read_env(async::get_stop_token_t{})),
                         async::set_value_t(ST const &), E>);
    STATIC_REQUIRE(std::is_same_v<
                   async::completion_signatures_of_t<decltype(async::read_env(
                       async::get_stop_token_t{}))>,
                   async::completion_signatures<async::set_value_t(
                       async::never_stop_token)>>);
}

TEST_CASE("read_env sends a value", "[read_env]") {
    int value{};
    auto r = stoppable_receiver{[&] { ++value; }};

    auto s = async::read_env(async::get_stop_token) |
             async::then([&](async::inplace_stop_token) { value = 42; });
    auto op = async::connect(s, r);
    async::start(op);
    CHECK(value == 43);
}

TEST_CASE("read_env with sync_wait_dynamic", "[read_env]") {
    auto s = async::read_env(async::get_scheduler) |
             async::let_value([&](auto sched) {
                 return async::start_on(sched, async::just(42));
             });

    auto value = s | async::sync_wait_dynamic();
    REQUIRE(value.has_value());
    CHECK(get<0>(*value) == 42);
}

TEST_CASE("read_env completes synchronously", "[read_env]") {
    [[maybe_unused]] auto const s = async::read_env(async::get_scheduler);
    STATIC_REQUIRE(async::synchronous<decltype(s)>);
}

TEST_CASE("read_env op state is synchronous", "[read_env]") {
    auto const s = async::read_env(async::get_stop_token);
    [[maybe_unused]] auto const op =
        async::connect(s, stoppable_receiver{[] {}});
    STATIC_REQUIRE(async::synchronous<decltype(op)>);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    constexpr auto signal(auto &&...) {
        if constexpr (stdx::is_specialization_of_v<async::debug::tag_of<Ctx>,
                                                   async::read_env_t>) {
            static_assert(
                boost::mp11::mp_empty<async::debug::children_of<Ctx>>::value);
            debug_events.push_back(
                fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
        }
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("read_env tag provides a debug name", "[read_env]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto stop = async::inplace_stop_source{};
    auto s = async::read_env(async::get_stop_token);
    auto r = with_env{
        universal_receiver{},
        async::env{async::prop{async::get_debug_interface_t{},
                               async::debug::named_interface<"op">{}},
                   async::prop{async::get_stop_token_t{}, stop.get_token()}}};
    auto op = async::connect(s, r);

    async::start(op);
    CHECK(debug_events == std::vector{"op get_stop_token start"s,
                                      "op get_stop_token set_value"s});
}

TEST_CASE("read_env can customize the debug name", "[read_env]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto stop = async::inplace_stop_source{};
    auto s = async::read_env<"GST">(async::get_stop_token);
    auto r = with_env{
        universal_receiver{},
        async::env{async::prop{async::get_debug_interface_t{},
                               async::debug::named_interface<"op">{}},
                   async::prop{async::get_stop_token_t{}, stop.get_token()}}};
    auto op = async::connect(s, r);

    async::start(op);
    CHECK(debug_events == std::vector{"op GST start"s, "op GST set_value"s});
}

TEST_CASE("read_env provides a fallback name for tags that don't",
          "[read_env]") {
    using namespace std::string_literals;
    debug_events.clear();

    auto s = async::read_env(get_fwd);
    auto r =
        with_env{universal_receiver{},
                 async::env{async::prop{async::get_debug_interface_t{},
                                        async::debug::named_interface<"op">{}},
                            async::prop{get_fwd_t{}, 42}}};
    auto op = async::connect(s, r);

    async::start(op);
    CHECK(debug_events ==
          std::vector{"op read_env start"s, "op read_env set_value"s});
}
