#pragma once

#if not __has_include(<thread>)
#error async::thread_scheduler is unavailable: <thread> does not exist
#endif

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>

#include <concepts>
#include <thread>
#include <type_traits>
#include <utility>

namespace async {
namespace _thread_scheduler {
template <stdx::ct_string Name, typename R> struct op_state {
    [[no_unique_address]] R receiver;

    auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(receiver));
        std::thread{[&] {
            debug_signal<"set_value", debug::erased_context_for<op_state>>(
                get_env(receiver));
            set_value(std::move(receiver));
        }}.detach();
    }
};
} // namespace _thread_scheduler

template <stdx::ct_string Name = "thread_scheduler"> class thread_scheduler {
    struct sender {
        using is_sender = void;
        using completion_signatures =
            async::completion_signatures<set_value_t()>;

        [[nodiscard]] static constexpr auto query(get_env_t) noexcept {
            return prop{get_completion_scheduler<set_value_t>,
                        thread_scheduler{}};
        }

        template <receiver R>
        [[nodiscard]] constexpr static auto connect(R &&r)
            -> _thread_scheduler::op_state<Name, std::remove_cvref_t<R>> {
            check_connect<sender, R>();
            return {std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto operator==(thread_scheduler,
                                                   thread_scheduler)
        -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender { return {}; }
};

struct thread_scheduler_sender_t;

template <stdx::ct_string Name, typename R>
struct debug::context_for<_thread_scheduler::op_state<Name, R>> {
    using tag = thread_scheduler_sender_t;
    constexpr static auto name = Name;
    using children = stdx::type_list<>;
    using type = _thread_scheduler::op_state<Name, R>;
};
} // namespace async
