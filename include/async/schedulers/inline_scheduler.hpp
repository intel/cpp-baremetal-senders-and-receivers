#pragma once

#include <async/completion_scheduler.hpp>
#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
class inline_scheduler {
    template <typename R> struct op_state {
        auto start() -> void { std::move(receiver).set_value(); }
        [[no_unique_address]] R receiver;
    };

    class env {
        [[nodiscard]] friend constexpr auto
        tag_invoke(get_completion_scheduler_t<set_value_t>, env) noexcept
            -> inline_scheduler {
            return {};
        }
    };

    struct sender_base {
        using is_sender = void;
        using completion_signatures =
            async::completion_signatures<set_value_t()>;

        [[nodiscard]] friend constexpr auto tag_invoke(get_env_t,
                                                       sender_base) noexcept
            -> env {
            return {};
        }
    };

    class multishot_sender : public sender_base {
        template <typename S, receiver_from<multishot_sender> R>
            requires std::same_as<multishot_sender, std::remove_cvref_t<S>>
        [[nodiscard]] friend constexpr auto tag_invoke(connect_t, S &&, R &&r)
            -> op_state<R> {
            return {std::forward<R>(r)};
        }
    };

    class singleshot_sender : public sender_base {
        template <receiver_from<singleshot_sender> R>
        [[nodiscard]] friend constexpr auto
        // NOLINTNEXTLINE(cppcoreguidelines-rvalue-reference-param-not-moved)
        tag_invoke(connect_t, singleshot_sender &&, R &&r) -> op_state<R> {
            return {std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto operator==(inline_scheduler,
                                                   inline_scheduler)
        -> bool = default;

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
