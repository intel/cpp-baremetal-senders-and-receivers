#pragma once

#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/schedulers/task.hpp>
#include <async/schedulers/trigger_manager.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>

#include <stdx/ct_conversions.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/panic.hpp>

#include <concepts>
#include <cstdint>
#include <memory>
#include <optional>
#include <type_traits>
#include <utility>

namespace async {
namespace trigger_mgr {
template <typename T>
constexpr auto name_of(T)
    -> stdx::ct_string<stdx::type_as_string<T>().size() + 1> {
    return stdx::ct_string<stdx::type_as_string<T>().size() + 1>{
        stdx::type_as_string<T>()};
}

template <stdx::ct_string S>
constexpr auto name_of(stdx::cts_t<S>) -> decltype(S) {
    return S;
}

// There are two ways a trigger_scheduler sender may be cancelled:
// 1. By cooperative cancellation: calling request_stop on a stop source whose
//    token is exposed through the receiver's environment
// 2. By force: calling cancel_triggers

//  If cancel_triggers MAY NOT be called, the advertisement of a set_stopped
//  completion depends on the connected receiver's environment.
struct coop_cancel_policy {
    template <typename... Args, typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> completion_signatures<set_value_t(Args const &...),
                                 set_stopped_t()> {
        return {};
    }

    template <typename... Args, typename Env>
        requires unstoppable_token<stop_token_of_t<Env>>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> completion_signatures<set_value_t(Args const &...)> {
        return {};
    }
};

//  If cancel_triggers MAY be called, we must always advertise a set_stopped
//  completion.
struct force_cancel_policy {
    template <typename... Args, typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> completion_signatures<set_value_t(Args const &...),
                                 set_stopped_t()> {
        return {};
    }
};

//
// 4 cases are possible for the op state:
//
// 1. cancel_triggers MAY be called, request_stop MAY be called
// 2. cancel_triggers MAY NOT be called, request_stop MAY be called
// 3. cancel_triggers MAY be called, request_stop MAY NOT be called
// 4. cancel_triggers MAY NOT be called, request_stop MAY NOT be called
//
// These choices correspond to 2 orthogonal choices:
// - the receiver's environment may or may not contain a stop_token
// - the cancel policy could be force_cancel_policy or coop_cancel_policy

// When request_stop MAY be called, we have stop callback machinery

template <typename Rcvr, typename Ops> struct op_state_coop {
    auto init_stop_cb() -> void {
        auto self = static_cast<Ops *>(this);
        self->stop_cb.emplace(async::get_stop_token(get_env(self->rcvr)),
                              stop_callback_fn{self});
    }

    struct stop_callback_fn {
        auto operator()() -> void { ops->request_stop(); }
        Ops *ops;
    };

    using stop_token_t = stop_token_of_t<env_of_t<Rcvr>>;
    using stop_callback_t = stop_callback_for_t<stop_token_t, stop_callback_fn>;
    std::optional<stop_callback_t> stop_cb{};
};

// When request_stop MAY NOT be called, we can omit the stop callback machinery

template <typename Rcvr, typename Ops>
    requires unstoppable_token<stop_token_of_t<env_of_t<Rcvr>>>
struct op_state_coop<Rcvr, Ops> {
    constexpr static auto init_stop_cb() -> void {}
};

// When the CancelPolicy is force_cancel_policy, OR the connected receiver's
// environment has a stop token (so the sender advertised that it may complete
// with set_stopped) it's safe to call cancel. If this was a
// coop_trigger_scheduler, the programmer got lucky: they said they wouldn't
// call cancel_triggers, and then they called it. It happens to work because the
// environment's stop token means a set_stopped() completion is possible anyway.

template <typename Rcvr, typename Ops, typename CancelPolicy>
struct op_state_force {
    auto on_cancel() -> void { static_cast<Ops *>(this)->complete_stopped(); }
};

// When the CancelPolicy is coop_cancel_policy (so the programmer warranted that
// they would not call cancel_triggers), AND the connected receiver's
// environment has no stop token, a call to cancel must cause a panic: the
// sender did not have set_stopped() in its completion signatures, but now we're
// asking it to complete with set_stopped()!

template <typename Rcvr, typename Ops>
    requires unstoppable_token<stop_token_of_t<env_of_t<Rcvr>>>
struct op_state_force<Rcvr, Ops, coop_cancel_policy> {
    constexpr static auto on_cancel() -> void {
        using stdx::ct_string_literals::operator""_cts;
        stdx::panic<
            "cancel_triggers called on coop_trigger_scheduler sender"_cts>(
            debug::erased_context_for<Ops>{});
    }
};

template <typename Rcvr, typename Ops, typename CancelPolicy>
struct op_state_impl : op_state_force<Rcvr, Ops, CancelPolicy>,
                       op_state_coop<Rcvr, Ops> {};

// The op state has events that happen in order:
// 1. start: add self to the trigger queue, set up the stop callback (if any)
// then one of:
// 2a. run (run_triggers was called): set_value
// 2b. cancel (cancel_triggers was called): set_stopped
// 2c. stop callback called: set_stopped (if not run/cancelled already)
// Note: in 2a and 2b, the op state has already been dequeued, so a stop request
// cannot have any effect.

template <typename Name, typename Rcvr, typename QueuePolicy,
          typename CancelPolicy, typename... Args>
struct op_state final
    : op_state_impl<Rcvr,
                    op_state<Name, Rcvr, QueuePolicy, CancelPolicy, Args...>,
                    CancelPolicy>,
      trigger_task<Args...> {
    template <stdx::same_as_unqualified<Rcvr> R>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr explicit(true) op_state(R &&r) : rcvr{std::forward<R>(r)} {}

    auto run(Args const &...args) -> void final {
        debug_signal<"set_value", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        set_value(std::move(rcvr), args...);
    }

    auto cancel() -> void final { this->on_cancel(); }

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        triggers<Name, Args...>.template enqueue<QueuePolicy>(*this);
        // setting the stop callback will result in an immediate request_stop()
        // call if a stop was already requested
        this->init_stop_cb();
    }

    auto request_stop() -> void {
        if (triggers<Name, Args...>.dequeue(*this)) {
            complete_stopped();
        }
    }

    auto complete_stopped() -> void {
        debug_signal<"set_stopped", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        set_stopped(std::move(rcvr));
    }

    [[no_unique_address]] Rcvr rcvr;
};

template <typename S, typename Name, typename QueuePolicy,
          typename CancelPolicy, typename... Args>
class scheduler {
    struct sender {
        using is_sender = void;

        template <typename Env>
        [[nodiscard]] constexpr static auto
        get_completion_signatures(Env const &e) {
            return CancelPolicy::template get_completion_signatures<Args...>(e);
        }

        [[nodiscard]] constexpr auto query(get_env_t) const noexcept {
            return env{prop{get_completion_scheduler_t<set_value_t>{}, S{}},
                       prop{get_completion_scheduler_t<set_stopped_t>{}, S{}}};
        }

        template <receiver R>
        [[nodiscard]] constexpr auto connect(R &&r) const {
            check_connect<sender, R>();
            return trigger_mgr::op_state<Name, std::remove_cvref_t<R>,
                                         QueuePolicy, CancelPolicy, Args...>{
                std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto operator==(scheduler, scheduler)
        -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender { return {}; }
};
} // namespace trigger_mgr

namespace detail {
template <stdx::ct_string Name, typename... Args>
class trigger_scheduler
    : public trigger_mgr::scheduler<trigger_scheduler<Name, Args...>,
                                    stdx::cts_t<Name>, Args...> {
    [[nodiscard]] friend constexpr auto operator==(trigger_scheduler,
                                                   trigger_scheduler)
        -> bool = default;
};
} // namespace detail

template <stdx::ct_string Name, typename... Args>
using trigger_scheduler =
    detail::trigger_scheduler<Name, trigger_mgr::queue_at_back,
                              trigger_mgr::force_cancel_policy, Args...>;

template <stdx::ct_string Name, typename... Args>
using coop_trigger_scheduler =
    detail::trigger_scheduler<Name, trigger_mgr::queue_at_back,
                              trigger_mgr::coop_cancel_policy, Args...>;

template <stdx::ct_string Name, typename... Args>
using urgent_trigger_scheduler =
    detail::trigger_scheduler<Name, trigger_mgr::queue_at_front,
                              trigger_mgr::force_cancel_policy, Args...>;

struct trigger_scheduler_sender_t;

template <typename Name, typename Rcvr, typename... Args>
struct debug::context_for<trigger_mgr::op_state<Name, Rcvr, Args...>> {
    using tag = trigger_scheduler_sender_t;
    constexpr static auto name = trigger_mgr::name_of(Name{});
    using children = stdx::type_list<>;
    using type = trigger_mgr::op_state<Name, Rcvr, Args...>;
};
} // namespace async
