#pragma once

#include <async/allocator.hpp>
#include <async/completes_synchronously.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/stack_allocator.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
class inline_scheduler {
    template <typename R> struct op_state {
        [[no_unique_address]] R receiver;

        constexpr auto start() & -> void { set_value(std::move(receiver)); }
    };

    struct env {
        [[nodiscard]] constexpr static auto
        query(get_allocator_t) noexcept -> stack_allocator {
            return {};
        }

        [[nodiscard]] constexpr static auto
        query(get_completion_scheduler_t<set_value_t>) noexcept
            -> inline_scheduler {
            return {};
        }

        [[nodiscard]] constexpr static auto
        query(completes_synchronously_t) noexcept -> bool {
            return true;
        }
    };

    struct sender_base {
        using is_sender = void;
        using completion_signatures =
            async::completion_signatures<set_value_t()>;

        [[nodiscard]] static constexpr auto query(get_env_t) noexcept -> env {
            return {};
        }
    };

    struct multishot_sender : sender_base {
        template <receiver R>
        [[nodiscard]] constexpr auto connect(R &&r) const -> op_state<R> {
            check_connect<multishot_sender, R>();
            return {std::forward<R>(r)};
        }
    };

    struct singleshot_sender : sender_base {
        template <receiver R>
        [[nodiscard]] constexpr auto connect(R &&r) && -> op_state<R> {
            check_connect<singleshot_sender &&, R>();
            return {std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto
    operator==(inline_scheduler, inline_scheduler) -> bool = default;

  public:
    struct singleshot;
    struct multishot;

    template <typename T = multishot>
    [[nodiscard]] constexpr static auto schedule() {
        if constexpr (std::same_as<T, multishot>) {
            return multishot_sender{};
        } else {
            return singleshot_sender{};
        }
    }
};
} // namespace async
