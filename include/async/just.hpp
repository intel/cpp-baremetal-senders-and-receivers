#pragma once

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/stack_allocator.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/tuple.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _just {
template <typename Tag, typename R, typename... Vs> struct op_state {
    [[no_unique_address]] R receiver;
    [[no_unique_address]] stdx::tuple<Vs...> values;

  private:
    friend constexpr auto tag_invoke(start_t, op_state &&o) -> void {
        std::move(o).values.apply([&]<typename... Ts>(Ts &&...ts) {
            Tag{}(std::move(o).receiver, std::forward<Ts>(ts)...);
        });
    }

    template <stdx::same_as_unqualified<op_state> O>
        requires(... and std::copy_constructible<Vs>)
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        std::forward<O>(o).values.apply([&]<typename... Ts>(Ts &&...ts) {
            Tag{}(std::forward<O>(o).receiver, std::forward<Ts>(ts)...);
        });
    }
};

template <typename Tag, typename... Vs> struct sender {
    using is_sender = void;
    using completion_signatures = async::completion_signatures<Tag(Vs...)>;
    [[no_unique_address]] stdx::tuple<Vs...> values;

  private:
    struct env {
        [[nodiscard]] constexpr static auto
        query(get_allocator_t) noexcept -> stack_allocator {
            return {};
        }
    };

    template <receiver R>
    [[nodiscard]] friend constexpr auto
    tag_invoke(connect_t, sender &&self,
               R &&r) -> op_state<Tag, std::remove_cvref_t<R>, Vs...> {
        check_connect<sender &&, R>();
        return {std::forward<R>(r), std::move(self).values};
    }

    template <stdx::same_as_unqualified<sender> Self, receiver R>
        requires std::copy_constructible<decltype(values)>
    [[nodiscard]] friend constexpr auto
    tag_invoke(connect_t, Self &&self,
               R &&r) -> op_state<Tag, std::remove_cvref_t<R>, Vs...> {
        check_connect<Self, R>();
        return {std::forward<R>(r), std::forward<Self>(self).values};
    }

  public:
    [[nodiscard]] constexpr auto query(get_env_t) const noexcept -> env {
        return {};
    }
};
} // namespace _just

template <typename... Vs>
[[nodiscard]] constexpr auto just(Vs &&...vs) -> sender auto {
    return _just::sender<set_value_t, std::remove_cvref_t<Vs>...>{
        {std::forward<Vs>(vs)...}};
}

template <typename V>
[[nodiscard]] constexpr auto just_error(V &&v) -> sender auto {
    return _just::sender<set_error_t, std::remove_cvref_t<V>>{
        {std::forward<V>(v)}};
}

[[nodiscard]] constexpr auto just_stopped() -> sender auto {
    return _just::sender<set_stopped_t>{};
}
} // namespace async
