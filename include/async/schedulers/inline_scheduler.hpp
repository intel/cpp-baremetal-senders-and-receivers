#pragma once

#include <async/completes_synchronously.hpp>
#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _in_line {
template <auto Name, typename R> struct op_state {
    [[no_unique_address]] R receiver;

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(receiver));
        debug_signal<"set_value", debug::erased_context_for<op_state>>(
            get_env(receiver));
        set_value(std::move(receiver));
    }

    [[nodiscard]] constexpr auto query(get_env_t) const noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }
};

template <auto Name> class scheduler {
    struct sender_base {
        using is_sender = void;
        using completion_signatures =
            async::completion_signatures<set_value_t()>;

        [[nodiscard]] constexpr static auto query(get_env_t) noexcept {
            return env{
                prop{completes_synchronously_t{}, std::true_type{}},
                prop{get_completion_scheduler<set_value_t>, scheduler{}}};
        }
    };

    struct multishot_sender : sender_base {
        template <receiver R>
        [[nodiscard]] constexpr auto connect(R &&r) const -> op_state<Name, R> {
            check_connect<multishot_sender, R>();
            return {std::forward<R>(r)};
        }
    };

    struct singleshot_sender : sender_base {
        template <receiver R>
        [[nodiscard]] constexpr auto connect(R &&r) && -> op_state<Name, R> {
            check_connect<singleshot_sender &&, R>();
            return {std::forward<R>(r)};
        }
    };

    [[nodiscard]] friend constexpr auto operator==(scheduler, scheduler)
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

scheduler() -> scheduler<stdx::shrink<"inline_scheduler">()>;
} // namespace _in_line

template <stdx::ct_string Name = "inline_scheduler">
using inline_scheduler = _in_line::scheduler<stdx::shrink<Name>()>;

struct inline_scheduler_sender_t;

template <auto Name, typename R>
struct debug::context_for<_in_line::op_state<Name, R>>
    : stdx::with_shrunk_name<Name> {
    using tag = inline_scheduler_sender_t;
    using type = _in_line::op_state<Name, R>;
    using children = stdx::type_list<>;
};
} // namespace async
