#pragma once

#if not __has_include(<thread>)
#error async::thread_scheduler is unavailable: <thread> does not exist
#endif

#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
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

        auto start() & -> void {
            std::thread{[&] { set_value(std::move(receiver)); }}.detach();
        }
    };

    struct env {
        [[nodiscard]] constexpr static auto
        query(get_completion_scheduler_t<set_value_t>) noexcept
            -> thread_scheduler {
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

        template <receiver R>
        [[nodiscard]] constexpr static auto
        connect(R &&r) -> op_state<std::remove_cvref_t<R>> {
            check_connect<sender, R>();
            return {std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto
    operator==(thread_scheduler, thread_scheduler) -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender { return {}; }
};
} // namespace async
