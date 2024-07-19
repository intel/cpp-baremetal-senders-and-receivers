#pragma once
#include <async/forwarding_query.hpp>
#include <async/stop_token.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
struct empty_env {};

constexpr inline struct get_env_t {
    template <typename T>
        requires true // more constrained
    [[nodiscard]] constexpr auto operator()(T &&t) const
        noexcept(noexcept(std::forward<T>(t).query(std::declval<get_env_t>())))
            -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }

    [[nodiscard]] constexpr auto operator()(auto &&) const -> empty_env {
        return {};
    }
} get_env{};

template <typename T> using env_of_t = decltype(get_env(std::declval<T>()));

namespace detail {
template <typename Tag, typename T, typename E = empty_env>
struct singleton_env : E {
    [[nodiscard]] constexpr auto query(Tag) const -> T { return value; }

    [[no_unique_address]] T value{};
};

template <typename E> struct forwarding_env {
    template <typename ForwardTag>
        requires(forwarding_query(ForwardTag{}))
    [[nodiscard]] constexpr auto query(ForwardTag tag) const -> decltype(auto) {
        return tag(child_env);
    }

    [[no_unique_address]] E child_env;
};
template <typename E> forwarding_env(E) -> forwarding_env<E>;

template <typename Tag, typename T, typename Q>
struct overriding_env : singleton_env<Tag, T, forwarding_env<env_of_t<Q>>> {};
} // namespace detail

template <typename Tag, typename T>
constexpr auto
singleton_env(T &&t) -> detail::singleton_env<Tag, std::remove_cvref_t<T>> {
    return {{}, std::forward<T>(t)};
}

template <typename Q>
constexpr auto forward_env_of(Q &&q) -> detail::forwarding_env<env_of_t<Q>> {
    return {get_env(std::forward<Q>(q))};
}

template <typename Tag, typename T, typename Q>
constexpr auto override_env_with(T &&t, Q &&q)
    -> detail::overriding_env<Tag, std::remove_cvref_t<T>,
                              std::remove_cvref_t<Q>> {
    return {get_env(std::forward<Q>(q)), std::forward<T>(t)};
}
} // namespace async
