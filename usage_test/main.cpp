#include <async/allocator.hpp>
#include <async/completes_synchronously.hpp>
#include <async/completion_tags.hpp>
#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/continue_on.hpp>
#include <async/debug.hpp>
#include <async/debug_context.hpp>
#include <async/env.hpp>
#include <async/forwarding_query.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/get_completion_signatures.hpp>
#include <async/get_scheduler.hpp>
#include <async/incite_on.hpp>
#include <async/into_variant.hpp>
#include <async/just.hpp>
#include <async/just_result_of.hpp>
#include <async/let.hpp>
#include <async/let_error.hpp>
#include <async/let_stopped.hpp>
#include <async/let_value.hpp>
#include <async/periodic.hpp>
#include <async/read_env.hpp>
#include <async/repeat.hpp>
#include <async/retry.hpp>
#include <async/schedulers/get_expiration.hpp>
#include <async/schedulers/inline_scheduler.hpp>
#include <async/schedulers/priority_scheduler.hpp>
#include <async/schedulers/requeue_policy.hpp>
#include <async/schedulers/runloop_scheduler.hpp>
#include <async/schedulers/task.hpp>
#include <async/schedulers/task_manager.hpp>
#include <async/schedulers/task_manager_interface.hpp>
#ifndef ASYNC_FREESTANDING
#include <async/schedulers/thread_scheduler.hpp>
#endif
#include <async/schedulers/time_scheduler.hpp>
#include <async/schedulers/timer_manager.hpp>
#include <async/schedulers/timer_manager_interface.hpp>
#include <async/schedulers/trigger_manager.hpp>
#include <async/schedulers/trigger_scheduler.hpp>
#include <async/sequence.hpp>
#include <async/split.hpp>
#include <async/stack_allocator.hpp>
#include <async/start.hpp>
#include <async/start_detached.hpp>
#include <async/start_on.hpp>
#include <async/static_allocator.hpp>
#include <async/stop_token.hpp>
#include <async/sync_wait.hpp>
#include <async/then.hpp>
#include <async/timeout_after.hpp>
#include <async/type_traits.hpp>
#include <async/variant_sender.hpp>
#include <async/when_all.hpp>
#include <async/when_any.hpp>

#if __STDC_HOSTED__ == 0
extern "C" auto main() -> int;
#endif

#ifdef ASYNC_FREESTANDING
namespace {
struct conc_policy {
    template <typename = void, std::invocable F, std::predicate... Pred>
        requires(sizeof...(Pred) < 2)
    static inline auto call_in_critical_section(F &&f, auto &&...pred)
        -> decltype(auto) {
        while (true) {
            if ((... and pred())) {
                return std::forward<F>(f)();
            }
        }
    }
};
} // namespace

template <> inline auto conc::injected_policy<> = conc_policy{};
#endif

auto main() -> int { return 0; }
