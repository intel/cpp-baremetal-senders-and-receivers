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

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _read_env {
namespace detail {
template <stdx::ct_string Name, typename Tag> constexpr auto get_name() {
    if constexpr (not Name.empty()) {
        return Name;
    } else if constexpr (requires {
                             []<auto N>(stdx::ct_string<N>) {}(Tag::name);
                         }) {
        return Tag::name;
    } else {
        return stdx::ct_string{"read_env"};
    }
}
} // namespace detail

template <stdx::ct_string Name, typename R, typename Tag> struct op_state {
    [[no_unique_address]] R receiver;

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(receiver));
        debug_signal<"set_value", debug::erased_context_for<op_state>>(
            get_env(receiver));
        set_value(std::move(receiver), Tag{}(get_env(receiver)));
    }

    [[nodiscard]] constexpr static auto query(get_env_t) noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }
};

template <stdx::ct_string Name, typename Tag> struct sender {
    using is_sender = void;

    template <typename Env>
    [[nodiscard]] constexpr static auto get_completion_signatures(Env const &)
        -> completion_signatures<
            set_value_t(decltype(std::declval<Tag>()(std::declval<Env>())))> {
        return {};
    }

    [[nodiscard]] constexpr static auto query(get_env_t) noexcept {
        return prop{completes_synchronously_t{}, std::true_type{}};
    }

    template <receiver R>
    [[nodiscard]] constexpr static auto connect(R &&r)
        -> op_state<detail::get_name<Name, Tag>(), std::remove_cvref_t<R>,
                    Tag> {
        check_connect<sender, R>();
        return {std::forward<R>(r)};
    }
};
} // namespace _read_env

template <stdx::ct_string Name = "", typename Tag>
[[nodiscard]] constexpr auto read_env(Tag) -> sender auto {
    return _read_env::sender<Name, Tag>{};
}

template <typename> struct read_env_t;

template <stdx::ct_string Name, typename R, typename Tag>
struct debug::context_for<_read_env::op_state<Name, R, Tag>> {
    using tag = read_env_t<Tag>;
    constexpr static auto name = Name;
    using children = stdx::type_list<>;
    using type = _read_env::op_state<Name, R, Tag>;
};
} // namespace async
