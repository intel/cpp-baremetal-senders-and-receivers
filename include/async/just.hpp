#pragma once

#include <async/concepts.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/tuple.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _just {
template <typename Tag, typename R, typename... Vs> struct op_state {
    auto start() -> void {
        std::move(values).apply([&]<typename... Ts>(Ts &&...ts) {
            Tag{}(receiver, std::forward<Ts>(ts)...);
        });
    }

    [[no_unique_address]] R receiver;
    [[no_unique_address]] stdx::tuple<Vs...> values;
};

template <typename Tag, typename... Vs> struct sender {
    using is_sender = void;
    using completion_signatures = async::completion_signatures<Tag(Vs...)>;
    [[no_unique_address]] stdx::tuple<Vs...> values;

  private:
    template <receiver_from<sender> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender &&self,
                                                   R &&r)
        -> op_state<Tag, std::remove_cvref_t<R>, Vs...> {
        return {std::forward<R>(r), std::move(self).values};
    }

    template <typename Self, receiver_from<sender> R>
        requires std::same_as<sender, std::remove_cvref_t<Self>> and
                 std::copy_constructible<decltype(values)>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r)
        -> op_state<Tag, std::remove_cvref_t<R>, Vs...> {
        return {std::forward<R>(r), std::forward<Self>(self).values};
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
