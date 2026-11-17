#pragma once

#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/schedulers/task_manager_interface.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <type_traits>
#include <utility>

namespace async {
namespace _pri {
template <auto Name, priority_t P, typename Rcvr, typename Task>
struct op_state final : Task {
    template <stdx::same_as_unqualified<Rcvr> R>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr explicit(true) op_state(R &&r) : rcvr{std::forward<R>(r)} {}

    auto run() -> void final {
        if (not check_stopped()) {
            debug_signal<"set_value", debug::erased_context_for<op_state>>(
                get_env(rcvr));
            set_value(std::move(rcvr));
        }
    }

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        if (not check_stopped()) {
            task_mgr::enqueue_task(*this, P);
        }
    }

    [[no_unique_address]] Rcvr rcvr;

  private:
    auto check_stopped() -> bool {
        if constexpr (not unstoppable_token<stop_token_of_t<env_of_t<Rcvr>>>) {
            if (get_stop_token(get_env(rcvr)).stop_requested()) {
                debug_signal<"set_stopped",
                             debug::erased_context_for<op_state>>(
                    get_env(rcvr));
                set_stopped(std::move(rcvr));
                return true;
            }
        }
        return false;
    }
};

template <priority_t P, auto Name, typename Task = priority_task>
class scheduler {
    struct sender {
        using is_sender = void;

        [[nodiscard]] constexpr auto query(get_env_t) const noexcept {
            return prop{get_completion_scheduler_t<set_value_t>{}, scheduler{}};
        }

        template <typename Env>
        [[nodiscard]] constexpr static auto
        get_completion_signatures(Env const &) noexcept
            -> completion_signatures<set_value_t(), set_stopped_t()> {
            return {};
        }

        template <typename Env>
            requires unstoppable_token<stop_token_of_t<Env>>
        [[nodiscard]] constexpr static auto
        get_completion_signatures(Env const &) noexcept
            -> completion_signatures<set_value_t()> {
            return {};
        }

        template <receiver R>
        [[nodiscard]] constexpr auto connect(R &&r) const {
            check_connect<sender, R>();
            return op_state<Name, P, std::remove_cvref_t<R>, Task>{
                std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto operator==(scheduler, scheduler)
        -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender {
        static_assert(task_mgr::valid_priority<P>(),
                      "fixed_priority_scheduler has invalid priority for the "
                      "injected task manager");
        return {};
    }
};
} // namespace _pri

template <priority_t P, stdx::ct_string Name = "fixed_priority_scheduler",
          typename Task = priority_task>
using fixed_priority_scheduler = _pri::scheduler<P, stdx::shrink<Name>(), Task>;

struct priority_scheduler_sender_t;

template <auto Name, priority_t P, typename Rcvr, typename Task>
struct debug::context_for<_pri::op_state<Name, P, Rcvr, Task>>
    : stdx::with_shrunk_name<Name> {
    using tag = priority_scheduler_sender_t;
    using children = stdx::type_list<>;
    using type = _pri::op_state<Name, P, Rcvr, Task>;
};
} // namespace async
