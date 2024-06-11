#pragma once

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/stop_token.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _read_env {
template <typename R, typename Tag> struct op_state {
    [[no_unique_address]] R receiver;
    [[no_unique_address]] Tag t;

  private:
    template <stdx::same_as_unqualified<op_state> O>
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        set_value(std::forward<O>(o).receiver, Tag{}(get_env(o.receiver)));
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

    [[no_unique_address]] Tag t;

  private:
    template <receiver R>
    [[nodiscard]] friend constexpr auto
    tag_invoke(connect_t, sender &&self,
               R &&r) -> op_state<std::remove_cvref_t<R>, Tag> {
        check_connect<sender &&, R>();
        return {std::forward<R>(r), std::move(self).t};
    }

    template <stdx::same_as_unqualified<sender> Self, receiver R>
        requires std::copy_constructible<Tag>
    [[nodiscard]] friend constexpr auto
    tag_invoke(connect_t, Self &&self,
               R &&r) -> op_state<std::remove_cvref_t<R>, Tag> {
        check_connect<Self, R>();
        return {std::forward<R>(r), std::forward<Self>(self).t};
    }
};
} // namespace _read_env

template <typename Tag>
[[nodiscard]] constexpr auto read_env(Tag &&t) -> sender auto {
    return _read_env::sender<Tag>{{std::forward<Tag>(t)}};
}

[[nodiscard]] constexpr auto get_stop_token() -> sender auto {
    return read_env(get_stop_token_t{});
}
[[nodiscard]] constexpr auto get_scheduler() -> sender auto {
    return read_env(get_scheduler_t{});
}
} // namespace async
