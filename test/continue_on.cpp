#include "detail/common.hpp"

#include <async/concepts.hpp>
#include <async/continue_on.hpp>
#include <async/just.hpp>
#include <async/let_stopped.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/start_on.hpp>
#include <async/tags.hpp>
#include <async/then.hpp>

#include <stdx/concepts.hpp>

#include <catch2/catch_test_macros.hpp>

namespace {
template <auto> class test_scheduler {
    template <typename R> struct op_state {
        [[no_unique_address]] R receiver;

      private:
        template <stdx::same_as_unqualified<op_state> O>
        friend constexpr auto tag_invoke(async::start_t, O &&o) -> void {
            async::set_value(std::forward<O>(o).receiver);
        }
    };

    struct env {
        template <typename Tag>
        [[nodiscard]] constexpr static auto query(
            async::get_completion_scheduler_t<Tag>) noexcept -> test_scheduler {
            return {};
        }
    };

    struct sender {
        using is_sender = void;
        using completion_signatures =
            async::completion_signatures<async::set_value_t()>;

        [[nodiscard]] constexpr auto
        query(async::get_env_t) const noexcept -> env {
            return {};
        }

      private:
        template <stdx::same_as_unqualified<sender> S,
                  async::receiver_from<sender> R>
        [[nodiscard]] friend constexpr auto tag_invoke(async::connect_t, S &&,
                                                       R &&r) -> op_state<R> {
            return {std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto
    operator==(test_scheduler, test_scheduler) -> bool = default;

  public:
    auto schedule() {
        ++schedule_calls;
        return sender{};
    }
    static inline int schedule_calls{};
};
} // namespace

TEST_CASE("continue_on", "[continue_on]") {
    static_assert(async::scheduler<test_scheduler<0>>);
    test_scheduler<1>::schedule_calls = 0;
    test_scheduler<2>::schedule_calls = 0;
    int value{};

    auto sched1 = test_scheduler<1>{};
    auto sched2 = test_scheduler<2>{};

    auto s = sched1.schedule();
    auto n1 = async::then(s, [] { return 42; });
    auto t = async::continue_on(n1, sched2);
    auto n2 = async::then(t, [](auto i) { return i + 17; });
    auto op = async::connect(n2, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 59);

    CHECK(test_scheduler<1>::schedule_calls == 1);
    CHECK(test_scheduler<2>::schedule_calls == 1);
}

TEST_CASE("continue_on advertises what it sends", "[continue_on]") {
    auto sched = test_scheduler<1>{};

    auto n1 = async::just(42);
    [[maybe_unused]] auto t = async::continue_on(n1, sched);
    static_assert(async::sender_of<decltype(t), async::set_value_t(int)>);
}

TEST_CASE("continue_on is pipeable", "[continue_on]") {
    test_scheduler<1>::schedule_calls = 0;
    test_scheduler<2>::schedule_calls = 0;
    int value{};

    auto sched1 = test_scheduler<1>{};
    auto sched2 = test_scheduler<2>{};

    auto s = sched1.schedule();
    auto n1 = async::then(s, [] { return 42; }) | async::continue_on(sched2);
    auto n2 = async::then(n1, [](auto i) { return i + 17; });
    auto op = async::connect(n2, receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 59);

    CHECK(test_scheduler<1>::schedule_calls == 1);
    CHECK(test_scheduler<2>::schedule_calls == 1);
}

TEST_CASE("continue_on is adaptor-pipeable", "[continue_on]") {
    test_scheduler<1>::schedule_calls = 0;
    test_scheduler<2>::schedule_calls = 0;
    int value{};

    auto sched1 = test_scheduler<1>{};
    auto sched2 = test_scheduler<2>{};

    auto n = async::then([] { return 42; }) | async::continue_on(sched2) |
             async::then([](auto i) { return i + 17; });
    auto op = async::connect(sched1.schedule() | n,
                             receiver{[&](auto i) { value = i; }});
    async::start(op);
    CHECK(value == 59);

    CHECK(test_scheduler<1>::schedule_calls == 1);
    CHECK(test_scheduler<2>::schedule_calls == 1);
}

TEST_CASE("continue_on advertises pass-through completions", "[continue_on]") {
    auto sched = test_scheduler<1>{};
    [[maybe_unused]] auto t = async::just_error(42) | async::continue_on(sched);
    static_assert(async::sender_of<decltype(t), async::set_error_t(int)>);
}

TEST_CASE("move-only value", "[continue_on]") {
    test_scheduler<1>::schedule_calls = 0;
    test_scheduler<2>::schedule_calls = 0;
    int value{};

    auto sched1 = test_scheduler<1>{};
    auto sched2 = test_scheduler<2>{};

    auto s = sched1.schedule();
    auto n1 = async::then(s, [] { return move_only{42}; });
    auto trans = async::continue_on(n1, sched2);
    auto n2 =
        async::then(trans, [](auto mo) { return move_only{mo.value + 17}; });
    auto op = async::connect(std::move(n2),
                             receiver{[&](auto mo) { value = mo.value; }});
    async::start(op);
    CHECK(value == 59);

    CHECK(test_scheduler<1>::schedule_calls == 1);
    CHECK(test_scheduler<2>::schedule_calls == 1);
}

TEST_CASE("singleshot continue_on", "[continue_on]") {
    auto sched1 = test_scheduler<1>{};
    [[maybe_unused]] auto n = async::inline_scheduler::schedule<
                                  async::inline_scheduler::singleshot>() |
                              async::continue_on(sched1);
    static_assert(async::singleshot_sender<decltype(n), universal_receiver>);
}

TEST_CASE("continue_on cancellation", "[continue_on]") {
    test_scheduler<1>::schedule_calls = 0;
    test_scheduler<2>::schedule_calls = 0;
    int value{};

    auto sched1 = test_scheduler<1>{};
    auto sched2 = test_scheduler<2>{};

    auto s = async::start_on(sched1, async::just_stopped()) |
             async::continue_on(sched2);

    auto op = async::connect(s, stopped_receiver{[&] { value = 42; }});
    async::start(op);
    CHECK(value == 42);
    CHECK(test_scheduler<1>::schedule_calls == 1);
    CHECK(test_scheduler<2>::schedule_calls == 0);
}
