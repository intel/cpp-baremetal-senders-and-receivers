#pragma once

#include <async/completes_synchronously.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/get_scheduler.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _read_env {
template <typename R, typename Tag> struct op_state {
    [[no_unique_address]] R receiver;

    constexpr auto start() & -> void {
        set_value(std::move(receiver), Tag{}(get_env(receiver)));
    }
};

struct env {
    [[nodiscard]] constexpr static auto
    query(completes_synchronously_t) noexcept -> bool {
        return true;
    }
};

template <typename Tag> struct sender {
    using is_sender = void;

    template <typename Env>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> completion_signatures<
            set_value_t(decltype(std::declval<Tag>()(std::declval<Env>())))> {
        return {};
    }

    [[nodiscard]] constexpr auto query(get_env_t) const noexcept -> env {
        return {};
    }

  private:
    template <stdx::same_as_unqualified<sender> Self, receiver R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&, R &&r)
        -> op_state<std::remove_cvref_t<R>, Tag> {
        check_connect<Self, R>();
        return {std::forward<R>(r)};
    }
};
} // namespace _read_env

template <typename Tag>
[[nodiscard]] constexpr auto read_env(Tag) -> sender auto {
    return _read_env::sender<Tag>{};
}

[[nodiscard]] constexpr auto get_stop_token() -> sender auto {
    return read_env(get_stop_token_t{});
}
[[nodiscard]] constexpr auto get_scheduler() -> sender auto {
    return read_env(get_scheduler_t{});
}
} // namespace async
