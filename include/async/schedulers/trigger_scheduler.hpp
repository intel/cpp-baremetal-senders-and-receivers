#pragma once

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/schedulers/task.hpp>
#include <async/schedulers/trigger_manager.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>

#include <stdx/ct_string.hpp>

#include <optional>

namespace async {
namespace trigger_mgr {
template <typename Rcvr, typename Ops> struct op_state_base {
    auto check_stopped() -> bool {
        return get_stop_token(get_env(as_derived().rcvr)).stop_requested();
    }

    auto emplace_stop_cb() -> void {
        auto &self = as_derived();
        stop_cb.emplace(async::get_stop_token(get_env(self.rcvr)),
                        stop_callback_fn{std::addressof(self)});
    }

    auto clear_stop_cb() -> void { stop_cb.reset(); }

  private:
    auto as_derived() -> Ops & { return static_cast<Ops &>(*this); }

    struct stop_callback_fn {
        auto operator()() -> void {
            if (ops->stop()) {
                ops->complete_stopped();
            }
        }
        Ops *ops;
    };

    using stop_token_t = stop_token_of_t<env_of_t<Rcvr>>;
    using stop_callback_t = stop_callback_for_t<stop_token_t, stop_callback_fn>;
    std::optional<stop_callback_t> stop_cb{};
};

template <typename Rcvr, typename Ops>
    requires unstoppable_token<stop_token_of_t<env_of_t<Rcvr>>>
struct op_state_base<Rcvr, Ops> {
    auto check_stopped() -> bool { return false; }
    auto emplace_stop_cb() -> void {}
    auto clear_stop_cb() -> void {}
};

template <stdx::ct_string Name, typename Rcvr, typename... Args>
struct op_state final : op_state_base<Rcvr, op_state<Name, Rcvr, Args...>>,
                        trigger_task<Args...> {
    template <stdx::same_as_unqualified<Rcvr> R>
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    constexpr explicit(true) op_state(R &&r) : rcvr{std::forward<R>(r)} {}

    auto run(Args const &...args) -> void final {
        this->clear_stop_cb();
        debug_signal<"set_value", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        set_value(std::move(rcvr), args...);
    }

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        if (this->check_stopped()) {
            complete_stopped();
            return;
        }
        triggers<Name, Args...>.enqueue(*this);
        this->emplace_stop_cb();
    }

    auto stop() -> bool {
        this->clear_stop_cb();
        return triggers<Name, Args...>.dequeue(*this);
    }

    auto complete_stopped() {
        debug_signal<"set_stopped", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        set_stopped(std::move(rcvr));
    }

    [[no_unique_address]] Rcvr rcvr;
};
} // namespace trigger_mgr

template <stdx::ct_string Name, typename... Args> class trigger_scheduler {
    struct sender {
        using is_sender = void;

        [[nodiscard]] constexpr auto query(get_env_t) const noexcept {
            return prop{get_completion_scheduler_t<set_value_t>{},
                        trigger_scheduler{}};
        }

        template <typename Env>
        [[nodiscard]] constexpr static auto
        get_completion_signatures(Env const &) noexcept
            -> completion_signatures<set_value_t(Args const &...),
                                     set_stopped_t()> {
            return {};
        }

        template <typename Env>
            requires unstoppable_token<stop_token_of_t<Env>>
        [[nodiscard]] constexpr static auto
        get_completion_signatures(Env const &) noexcept
            -> completion_signatures<set_value_t(Args const &...)> {
            return {};
        }

        template <receiver R>
        [[nodiscard]] constexpr auto connect(R &&r) const {
            check_connect<sender, R>();
            return trigger_mgr::op_state<Name, std::remove_cvref_t<R>, Args...>{
                std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto operator==(trigger_scheduler,
                                                   trigger_scheduler)
        -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender { return {}; }
};

struct trigger_scheduler_sender_t;

template <stdx::ct_string Name, typename Rcvr, typename... Args>
struct debug::context_for<trigger_mgr::op_state<Name, Rcvr, Args...>> {
    using tag = trigger_scheduler_sender_t;
    constexpr static auto name = Name;
    using children = stdx::type_list<>;
    using type = trigger_mgr::op_state<Name, Rcvr, Args...>;
};
} // namespace async
