#pragma once

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/schedulers/timer_manager_interface.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <memory>
#include <optional>
#include <type_traits>
#include <utility>

namespace async {
namespace timer_mgr {
template <typename Rcvr, typename Task> struct op_state_base : Task {
    template <stdx::same_as_unqualified<Rcvr> R>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr explicit(true) op_state_base(R &&r) : rcvr{std::forward<R>(r)} {}

    auto run() -> void final { set_value(std::move(rcvr)); }

    [[no_unique_address]] Rcvr rcvr;
};

template <typename Domain, typename Duration, typename Rcvr, typename Task>
struct op_state;

template <typename Domain, typename Duration, typename Rcvr, typename Task>
    requires unstoppable_token<stop_token_of_t<env_of_t<Rcvr>>>
struct op_state<Domain, Duration, Rcvr, Task> final
    : op_state_base<Rcvr, Task> {
    template <stdx::same_as_unqualified<Rcvr> R>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr explicit(true) op_state(R &&r, Duration dur)
        : op_state_base<Rcvr, Task>{std::forward<R>(r)}, d{dur} {}

    constexpr auto start() & -> void { detail::run_after<Domain>(*this, d); }

    [[no_unique_address]] Duration d{};
};

template <typename Domain, typename Duration, typename Rcvr, typename Task>
    requires(not unstoppable_token<stop_token_of_t<env_of_t<Rcvr>>>)
struct op_state<Domain, Duration, Rcvr, Task> final
    : op_state_base<Rcvr, Task> {
    template <stdx::same_as_unqualified<Rcvr> R>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr explicit(true) op_state(R &&r, Duration dur)
        : op_state_base<Rcvr, Task>{std::forward<R>(r)}, d{dur} {}

    constexpr auto start() & -> void {
        auto token = get_stop_token(get_env(this->rcvr));
        if (token.stop_requested()) {
            set_stopped(std::move(this->rcvr));
        } else {
            detail::run_after<Domain>(*this, d);
            stop_cb.emplace(token, stop_callback_fn{this});
        }
    }

  private:
    struct stop_callback_fn {
        auto operator()() -> void {
            if (detail::cancel<Domain>(*ops)) {
                set_stopped(std::move(ops->rcvr));
            }
        }
        op_state *ops;
    };
    using stop_callback_t =
        stop_callback_for_t<stop_token_of_t<env_of_t<Rcvr>>, stop_callback_fn>;

    [[no_unique_address]] Duration d{};
    std::optional<stop_callback_t> stop_cb{};
};
} // namespace timer_mgr

template <typename Domain, typename Duration,
          typename Task = timer_task<timer_mgr::time_point_for_t<Duration>>>
class time_scheduler {
    struct sender {
        using is_sender = void;
        [[no_unique_address]] Duration d{};

        [[nodiscard]] constexpr auto query(get_env_t) const noexcept {
            return prop{get_completion_scheduler_t<set_value_t>{},
                        time_scheduler{d}};
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
        [[nodiscard]] constexpr auto connect(R &&r) const & {
            check_connect<sender, R>();
            return timer_mgr::op_state<Domain, Duration, std::remove_cvref_t<R>,
                                       Task>{std::forward<R>(r), d};
        }
    };

    [[nodiscard]] friend constexpr auto
    operator==(time_scheduler, time_scheduler) -> bool = default;

  public:
    [[nodiscard]] constexpr auto schedule() -> sender {
        static_assert(timer_mgr::detail::valid_duration<Duration, Domain>(),
                      "time_scheduler has invalid duration type for the "
                      "injected timer manager");
        return {d};
    }

    [[no_unique_address]] Duration d{};
};

template <typename D>
time_scheduler(D) -> time_scheduler<timer_mgr::default_domain, D>;

template <typename Domain = timer_mgr::default_domain>
constexpr auto time_scheduler_factory =
    []<typename D>(D d) -> time_scheduler<Domain, D> { return {d}; };
} // namespace async
