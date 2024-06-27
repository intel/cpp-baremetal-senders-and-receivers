#pragma once

#if not __has_include(<thread>)
#error async::thread_scheduler is unavailable: <thread> does not exist
#endif

#include <async/completion_scheduler.hpp>
#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <concepts>
#include <thread>
#include <type_traits>
#include <utility>

namespace async {
class thread_scheduler {
    template <typename R> struct op_state {
        [[no_unique_address]] R receiver;

      private:
        template <stdx::same_as_unqualified<op_state> O>
        friend auto tag_invoke(start_t, O &&o) -> void {
            std::thread{[&] {
                set_value(std::forward<O>(o).receiver);
            }}.detach();
        }
    };

    class env {
        [[nodiscard]] friend constexpr auto
        tag_invoke(get_completion_scheduler_t<set_value_t>,
                   env) noexcept -> thread_scheduler {
            return {};
        }
    };

    struct sender {
        using is_sender = void;
        using completion_signatures =
            async::completion_signatures<set_value_t()>;

        [[nodiscard]] constexpr static auto query(get_env_t) noexcept -> env {
            return {};
        }

      private:
        template <stdx::same_as_unqualified<sender> S, receiver R>
        [[nodiscard]] friend constexpr auto tag_invoke(connect_t, S &&,
                                                       R &&r) -> op_state<R> {
            check_connect<S, R>();
            return {std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto
    operator==(thread_scheduler, thread_scheduler) -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender { return {}; }
};
} // namespace async
