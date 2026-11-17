#pragma once

#include <async/detail/freestanding.hpp>

#ifdef ASYNC_FREESTANDING
#define HAS_THREAD 0
#else
#define HAS_THREAD __has_include(<thread>)
#endif

#if HAS_THREAD

#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>

#include <thread>
#include <type_traits>
#include <utility>

namespace async {
namespace _thread {
template <auto Name, typename R> struct op_state {
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

template <auto Name> class scheduler {
    struct sender {
        using is_sender = void;
        using completion_signatures =
            async::completion_signatures<set_value_t()>;

        [[nodiscard]] constexpr static auto query(get_env_t) noexcept {
            return prop{get_completion_scheduler<set_value_t>, scheduler{}};
        }

        template <receiver R>
        [[nodiscard]] constexpr static auto connect(R &&r)
            -> op_state<Name, std::remove_cvref_t<R>> {
            check_connect<sender, R>();
            return {std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto operator==(scheduler, scheduler)
        -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender { return {}; }
};

scheduler() -> scheduler<stdx::shrink<"thread_scheduler">()>;
} // namespace _thread

template <stdx::ct_string Name = "thread_scheduler">
using thread_scheduler = _thread::scheduler<stdx::shrink<Name>()>;

struct thread_scheduler_sender_t;

template <auto Name, typename R>
struct debug::context_for<_thread::op_state<Name, R>>
    : stdx::with_shrunk_name<Name> {
    using tag = thread_scheduler_sender_t;
    using children = stdx::type_list<>;
    using type = _thread::op_state<Name, R>;
};
} // namespace async

#endif

#undef HAS_THREAD
