#include "detail/common.hpp"

#include <async/schedulers/trigger_manager.hpp>
#include <async/schedulers/trigger_scheduler.hpp>
#include <async/then.hpp>

#include <catch2/catch_test_macros.hpp>

extern auto run_test_triggers() -> void;

TEST_CASE("trigger_scheduler works across translation units",
          "[trigger_scheduler]") {
    int var{};

    auto s = async::trigger_scheduler<"test">{};
    async::sender auto sndr = s.schedule() | async::then([&] { var = 42; });
    auto op = async::connect(sndr, universal_receiver{});

    async::start(op);
    run_test_triggers();

    CHECK(var == 42);
    CHECK(async::triggers<async::trigger_name<"test">>.empty());
}
