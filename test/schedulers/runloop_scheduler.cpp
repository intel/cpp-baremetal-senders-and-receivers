#include "detail/common.hpp"

#include <async/debug.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/schedulers/runloop_scheduler.hpp>

#include <stdx/ct_format.hpp>
#include <stdx/type_traits.hpp>

#include <boost/mp11/list.hpp>
#include <catch2/catch_test_macros.hpp>
#include <fmt/format.h>

#include <string>
#include <vector>

TEST_CASE("runloop_scheduler fulfils concept", "[runloop_scheduler]") {
    static_assert(
        async::scheduler<decltype(async::run_loop{}.get_scheduler())>);
}

TEST_CASE("runloop_schedulers from the same run_loop compare equal",
          "[runloop_scheduler]") {
    async::run_loop rl1{};
    async::run_loop rl2{};

    auto s = rl1.get_scheduler();
    CHECK(s != rl2.get_scheduler());
    CHECK(s == rl1.get_scheduler());
}

TEST_CASE("runloop_sender has runloop_scheduler as its completion scheduler",
          "[runloop_scheduler]") {
    async::run_loop rl{};
    auto s = rl.get_scheduler();
    CHECK(async::get_completion_scheduler<async::set_value_t>(
              async::get_env(s.schedule())) == s);
}

TEST_CASE("runloop operation", "[runloop_scheduler]") {
    int value{};

    async::run_loop rl{};
    auto s = rl.get_scheduler().schedule();
    auto op = async::connect(s, receiver{[&] { value = 42; }});
    rl.push_back(&op);

    rl.finish();
    rl.run();
    CHECK(value == 42);
}

namespace {
std::vector<std::string> debug_events{};

struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    constexpr auto signal(auto &&...) {
        static_assert(std::is_same_v<async::debug::tag_of<Ctx>,
                                     async::runloop_scheduler_sender_t>);
        static_assert(
            boost::mp11::mp_empty<async::debug::children_of<Ctx>>::value);
        debug_events.push_back(
            fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
    }
};
} // namespace

template <> inline auto async::injected_debug_handler<> = debug_handler{};

TEST_CASE("runloop_scheduler can be debugged with a string",
          "[runloop_scheduler]") {
    using namespace std::string_literals;
    debug_events.clear();

    async::run_loop rl{};
    auto s = rl.get_scheduler().schedule();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);

    rl.finish();
    rl.run();
    CHECK(debug_events == std::vector{"op runloop_scheduler start"s,
                                      "op runloop_scheduler set_value"s});
}

TEST_CASE("runloop_scheduler can be named and debugged with a string",
          "[runloop_scheduler]") {
    using namespace std::string_literals;
    debug_events.clear();

    async::run_loop<stdx::cts_t<"sched">> rl{};
    auto s = rl.get_scheduler().schedule();
    auto op = async::connect(
        s, with_env{universal_receiver{},
                    async::prop{async::get_debug_interface_t{},
                                async::debug::named_interface<"op">{}}});
    async::start(op);

    rl.finish();
    rl.run();
    CHECK(debug_events ==
          std::vector{"op sched start"s, "op sched set_value"s});
}
