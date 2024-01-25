#pragma once

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/schedulers/task_manager_interface.hpp>
#include <async/stop_token.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace task_mgr {
template <priority_t P, typename Rcvr> struct op_state : single_linked_task {
    template <stdx::same_as_unqualified<Rcvr> R>
    constexpr explicit(true) op_state(R &&r) : rcvr{std::forward<R>(r)} {}

    auto run() -> void final {
        if (not check_stopped()) {
            set_value(std::move(rcvr));
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

    template <stdx::same_as_unqualified<op_state> O>
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        if (not std::forward<O>(o).check_stopped()) {
            detail::enqueue_task(o, P);
        }
    }
};
} // namespace task_mgr

template <priority_t P> class fixed_priority_scheduler {
    class env {
        [[nodiscard]] friend constexpr auto
        tag_invoke(get_completion_scheduler_t<set_value_t>, env) noexcept
            -> fixed_priority_scheduler {
            return {};
        }
    };

    struct sender {
        using is_sender = void;

      private:
        template <stdx::same_as_unqualified<sender> S, receiver_from<sender> R>
        [[nodiscard]] friend constexpr auto tag_invoke(connect_t, S &&, R &&r) {
            return task_mgr::op_state<P, std::remove_cvref_t<R>>{
                std::forward<R>(r)};
        }

        [[nodiscard]] friend constexpr auto tag_invoke(get_env_t,
                                                       sender) noexcept -> env {
            return {};
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

    [[nodiscard]] friend constexpr auto operator==(fixed_priority_scheduler,
                                                   fixed_priority_scheduler)
        -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender { return {}; }
};
} // namespace async
