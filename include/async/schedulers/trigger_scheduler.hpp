#pragma once

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/schedulers/task.hpp>
#include <async/schedulers/trigger_manager.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>

#include <stdx/ct_string.hpp>

namespace async {
namespace trigger_mgr {
template <stdx::ct_string Name, typename Rcvr, typename Task>
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
            triggers<Name>.enqueue(*this);
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
} // namespace trigger_mgr

template <stdx::ct_string Name, typename Task = trigger_task>
class trigger_scheduler {
    struct sender {
        using is_sender = void;

        [[nodiscard]] constexpr auto query(get_env_t) const noexcept {
            return prop{get_completion_scheduler_t<set_value_t>{},
                        trigger_scheduler{}};
        }

        template <typename Env>
        [[nodiscard]] constexpr static auto
        get_completion_signatures(Env const &) noexcept
            -> completion_signatures<set_value_t(), set_stopped_t()> {
            return {};
        }

        template <typename Env>
            requires unstoppable_token<stop_token_of_t<Env>>
        [[nodiscard]] constexpr static auto get_completion_signatures(
            Env const &) noexcept -> completion_signatures<set_value_t()> {
            return {};
        }

        template <receiver R>
        [[nodiscard]] constexpr auto connect(R &&r) const {
            check_connect<sender, R>();
            return trigger_mgr::op_state<Name, std::remove_cvref_t<R>, Task>{
                std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto
    operator==(trigger_scheduler, trigger_scheduler) -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender { return {}; }
};
} // namespace async
