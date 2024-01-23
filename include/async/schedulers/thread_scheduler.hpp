#pragma once

#if not __has_include(<thread>)
#error async::thread_scheduler is unavailable: <thread> does not exist
#endif

#include <async/completion_scheduler.hpp>
#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <concepts>
#include <thread>
#include <type_traits>
#include <utility>

namespace async {
class thread_scheduler {
    template <typename R> struct op_state {
        auto start() -> void {
            std::thread{[&] { set_value(std::move(receiver)); }}.detach();
        }

        [[no_unique_address]] R receiver;
    };

    class env {
        [[nodiscard]] friend constexpr auto
        tag_invoke(get_completion_scheduler_t<set_value_t>, env) noexcept
            -> thread_scheduler {
            return {};
        }
    };

    struct sender {
        using is_sender = void;
        using completion_signatures =
            async::completion_signatures<set_value_t()>;

      private:
        [[nodiscard]] friend constexpr auto tag_invoke(get_env_t,
                                                       sender) noexcept -> env {
            return {};
        }

        template <typename S, receiver_from<sender> R>
            requires std::same_as<sender, std::remove_cvref_t<S>>
        [[nodiscard]] friend constexpr auto tag_invoke(connect_t, S &&, R &&r)
            -> op_state<R> {
            return {std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto operator==(thread_scheduler,
                                                   thread_scheduler)
        -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender { return {}; }
};
} // namespace async
