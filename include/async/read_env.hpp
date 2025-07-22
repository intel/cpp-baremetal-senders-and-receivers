#pragma once

#include <async/completes_synchronously.hpp>
#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/ct_conversions.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/tuple.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _read_env {
namespace detail {
template <stdx::ct_string Name, typename... Tags> constexpr auto get_name() {
    if constexpr (not Name.empty()) {
        return Name;
    } else {
        auto const f = []<typename Tag>() {
            if constexpr (requires {
                              []<auto N>(stdx::ct_string<N>) {}(Tag::name);
                          }) {
                return Tag::name;
            } else {
                constexpr auto s = stdx::type_as_string<Tag>();
                return stdx::ct_string<s.size() + 1>{s};
            }
        };
        using namespace stdx::literals;
        if constexpr (sizeof...(Tags) == 1) {
            return f.template operator()<Tags...>();
        } else {
            return "read_env<"_cts +
                   stdx::tuple{f.template operator()<Tags>()...}.join(
                       ""_cts,
                       [](auto lhs, auto rhs) { return lhs + ","_cts + rhs; }) +
                   ">"_cts;
        }
    }
}
} // namespace detail

template <stdx::ct_string Name, typename R, typename... Tags> struct op_state {
    [[no_unique_address]] R receiver;

    constexpr auto start() & -> void {
        auto e = get_env(receiver);
        debug_signal<"start", debug::erased_context_for<op_state>>(e);
        debug_signal<"set_value", debug::erased_context_for<op_state>>(e);
        set_value(std::move(receiver), Tags{}(e)...);
    }

    [[nodiscard]] constexpr static auto query(get_env_t) noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }
};

template <stdx::ct_string Name, typename... Tags> struct sender {
    using is_sender = void;

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> completion_signatures<set_value_t(
            decltype(std::declval<Tags>()(std::declval<Env>()))...)> {
        return {};
    }

    [[nodiscard]] constexpr static auto query(get_env_t) noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }

    template <receiver R>
    [[nodiscard]] constexpr static auto connect(R &&r)
        -> op_state<detail::get_name<Name, Tags...>(), std::remove_cvref_t<R>,
                    Tags...> {
        check_connect<sender, R>();
        return {std::forward<R>(r)};
    }
};
} // namespace _read_env

template <stdx::ct_string Name = "", typename... Tags>
    requires(sizeof...(Tags) > 0)
[[nodiscard]] constexpr auto read_env(Tags...) -> sender auto {
    return _read_env::sender<Name, Tags...>{};
}

template <typename...> struct read_env_t;

template <stdx::ct_string Name, typename R, typename... Tags>
struct debug::context_for<_read_env::op_state<Name, R, Tags...>> {
    using tag = read_env_t<Tags...>;
    constexpr static auto name = Name;
    using children = stdx::type_list<>;
    using type = _read_env::op_state<Name, R, Tags...>;
};
} // namespace async
