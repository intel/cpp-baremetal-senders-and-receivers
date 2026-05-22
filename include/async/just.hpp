#pragma once

#include <async/completes_synchronously.hpp>
#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/tuple.hpp>
#include <stdx/type_traits.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _just {
template <stdx::ct_string Name, typename Tag, typename R, typename... Vs>
struct op_state {
    [[no_unique_address]] R receiver;
    [[no_unique_address]] stdx::tuple<Vs...> values;

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(receiver));
        std::move(values).apply([&]<typename... Ts>(Ts &&...ts) {
            debug_signal<Tag::name, debug::erased_context_for<op_state>>(
                get_env(receiver));
            Tag{}(std::move(receiver), std::forward<Ts>(ts)...);
        });
    }

    [[nodiscard]] constexpr auto query(get_env_t) const noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }
};

template <stdx::ct_string Name, typename Tag, typename... Vs> struct sender {
    using is_sender = void;
    using completion_signatures = async::completion_signatures<Tag(Vs...)>;
    [[no_unique_address]] stdx::tuple<Vs...> values;

    template <receiver R>
    [[nodiscard]] constexpr auto
    connect(R &&r) && -> op_state<Name, Tag, std::remove_cvref_t<R>, Vs...> {
        check_connect<sender &&, R>();
        return {std::forward<R>(r), std::move(values)};
    }

    template <receiver R>
        requires std::copy_constructible<decltype(values)>
    [[nodiscard]] constexpr auto connect(
        R &&r) const & -> op_state<Name, Tag, std::remove_cvref_t<R>, Vs...> {
        check_connect<sender const &, R>();
        return {std::forward<R>(r), values};
    }

    [[nodiscard]] constexpr auto query(get_env_t) const noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }
};

template <channel_tag Tag> constexpr inline auto name = stdx::ct_string{""};

template <>
constexpr inline auto name<set_value_t> = stdx::ct_string{"just_value"};
template <>
constexpr inline auto name<set_error_t> = stdx::ct_string{"just_error"};
template <>
constexpr inline auto name<set_stopped_t> = stdx::ct_string{"just_stopped"};
} // namespace _just

template <channel_tag Tag = set_value_t,
          stdx::ct_string Name = _just::name<Tag>, typename... Vs>
[[nodiscard]] constexpr auto just(Vs &&...vs) -> sender auto {
    static_assert(sizeof...(Vs) == 0 or not std::same_as<Tag, set_stopped_t>,
                  "Can't send any values on the stopped channel");

    return _just::sender<Name, Tag, std::remove_cvref_t<Vs>...>{
        {std::forward<Vs>(vs)...}};
}

template <stdx::ct_string Name = _just::name<set_value_t>, typename... Vs>
[[nodiscard]] constexpr auto just_value(Vs &&...vs) -> sender auto {
    return just<set_value_t, Name>(std::forward<Vs>(vs)...);
}

template <stdx::ct_string Name = _just::name<set_error_t>, typename... Vs>
[[nodiscard]] constexpr auto just_error(Vs &&...vs) -> sender auto {
    return just<set_error_t, Name>(std::forward<Vs>(vs)...);
}

template <stdx::ct_string Name = _just::name<set_stopped_t>>
[[nodiscard]] constexpr auto just_stopped() -> sender auto {
    return just<set_stopped_t, Name>();
}

struct just_value_t;
struct just_error_t;
struct just_stopped_t;

template <typename Tag>
using just_tag_for =
    stdx::conditional_t<std::same_as<Tag, set_value_t>, just_value_t,
                        stdx::conditional_t<std::same_as<Tag, set_error_t>,
                                            just_error_t, just_stopped_t>>;

template <stdx::ct_string Name, typename Tag, typename R, typename... Vs>
struct debug::context_for<_just::op_state<Name, Tag, R, Vs...>> {
    using tag = just_tag_for<Tag>;
    constexpr static auto name = Name;
    using children = stdx::type_list<>;
    using type = _just::op_state<Name, Tag, R, Vs...>;
};
} // namespace async
