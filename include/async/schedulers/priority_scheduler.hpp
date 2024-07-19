#pragma once

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/schedulers/task_manager_interface.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace task_mgr {
template <priority_t P, typename Rcvr, typename Task>
struct op_state final : Task {
    template <stdx::same_as_unqualified<Rcvr> R>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr explicit(true) op_state(R &&r) : rcvr{std::forward<R>(r)} {}

    auto run() -> void final {
        if (not check_stopped()) {
            set_value(std::move(rcvr));
        }
    }

    constexpr auto start() & -> void {
        if (not check_stopped()) {
            detail::enqueue_task(*this, P);
        }
    }

    [[no_unique_address]] Rcvr rcvr;

  private:
    auto check_stopped() -> bool {
        if constexpr (not unstoppable_token<stop_token_of_t<env_of_t<Rcvr>>>) {
            if (get_stop_token(get_env(rcvr)).stop_requested()) {
                set_stopped(std::move(rcvr));
                return true;
            }
        }
        return false;
    }
};
} // namespace task_mgr

template <priority_t P, typename Task = priority_task>
class fixed_priority_scheduler {
    struct env {
        [[nodiscard]] constexpr static auto
        query(get_completion_scheduler_t<set_value_t>) noexcept
            -> fixed_priority_scheduler {
            return {};
        }
    };

    struct sender {
        using is_sender = void;

        [[nodiscard]] constexpr auto query(get_env_t) const noexcept -> env {
            return {};
        }

      private:
        template <stdx::same_as_unqualified<sender> S, receiver R>
        [[nodiscard]] friend constexpr auto tag_invoke(connect_t, S &&, R &&r) {
            check_connect<S, R>();
            return task_mgr::op_state<P, std::remove_cvref_t<R>, Task>{
                std::forward<R>(r)};
        }

        template <typename Env>
        [[nodiscard]] friend constexpr auto
        tag_invoke(get_completion_signatures_t, sender, Env const &) noexcept
            -> completion_signatures<set_value_t(), set_stopped_t()> {
            return {};
        }

        template <typename Env>
            requires unstoppable_token<stop_token_of_t<Env>>
        [[nodiscard]] friend constexpr auto
        tag_invoke(get_completion_signatures_t, sender, Env const &) noexcept
            -> completion_signatures<set_value_t()> {
            return {};
        }
    };

    [[nodiscard]] friend constexpr auto
    operator==(fixed_priority_scheduler,
               fixed_priority_scheduler) -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender {
        static_assert(task_mgr::detail::valid_priority<P>(),
                      "fixed_priority_scheduler has invalid priority for the "
                      "injected task manager");
        return {};
    }
};
} // namespace async
