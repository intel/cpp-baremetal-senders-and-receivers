#pragma once

#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/stop_token.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _read {
template <typename R, typename Tag> struct op_state {
    [[no_unique_address]] R receiver;
    [[no_unique_address]] Tag t;

  private:
    template <typename O>
        requires std::same_as<op_state, std::remove_cvref_t<O>>
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
    template <receiver_from<sender> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender &&self,
                                                   R &&r)
        -> op_state<std::remove_cvref_t<R>, Tag> {
        return {std::forward<R>(r), std::move(self).t};
    }

    template <typename Self, receiver_from<sender> R>
        requires std::same_as<sender, std::remove_cvref_t<Self>> and
                 std::copy_constructible<Tag>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r)
        -> op_state<std::remove_cvref_t<R>, Tag> {
        return {std::forward<R>(r), std::forward<Self>(self).t};
    }
};
} // namespace _read

template <typename Tag>
[[nodiscard]] constexpr auto read(Tag &&t) -> sender auto {
    return _read::sender<Tag>{{std::forward<Tag>(t)}};
}

[[nodiscard]] constexpr auto get_stop_token() -> sender auto {
    return read(get_stop_token_t{});
}
[[nodiscard]] constexpr auto get_scheduler() -> sender auto {
    return read(get_scheduler_t{});
}
} // namespace async
