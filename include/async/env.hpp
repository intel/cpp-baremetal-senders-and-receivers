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
    constexpr auto operator()(T &&t) const
        noexcept(noexcept(tag_invoke(std::declval<get_env_t>(),
                                     std::forward<T>(t))))
            -> decltype(tag_invoke(*this, std::forward<T>(t))) {
        return tag_invoke(*this, std::forward<T>(t));
    }

    constexpr auto operator()(auto &&) const -> empty_env { return {}; }
} get_env{};

template <typename T> using env_of_t = decltype(get_env(std::declval<T>()));

namespace detail {
template <typename Tag, typename T> struct singleton_env {
    [[nodiscard]] friend constexpr auto
    tag_invoke(Tag, singleton_env const &self) -> T {
        return self.value;
    }
    [[no_unique_address]] T value{};
};

template <typename E> struct forwarding_env {
    template <typename ForwardTag>
        requires(forwarding_query(ForwardTag{}))
    [[nodiscard]] friend constexpr auto
    tag_invoke(ForwardTag tag, forwarding_env const &self) -> decltype(auto) {
        return tag(self.child_env);
    }

    [[no_unique_address]] E child_env;
};
template <typename E> forwarding_env(E) -> forwarding_env<E>;

template <typename Tag, typename T, typename Q>
struct overriding_env : singleton_env<Tag, T>, forwarding_env<env_of_t<Q>> {};
} // namespace detail

template <typename Tag, typename T>
constexpr auto
singleton_env(T &&t) -> detail::singleton_env<Tag, std::remove_cvref_t<T>> {
    return {std::forward<T>(t)};
}

template <typename Q>
constexpr auto forward_env_of(Q &&q) -> detail::forwarding_env<env_of_t<Q>> {
    return {get_env(std::forward<Q>(q))};
}

template <typename Tag, typename T, typename Q>
constexpr auto override_env_with(T &&t, Q &&q)
    -> detail::overriding_env<Tag, std::remove_cvref_t<T>,
                              std::remove_cvref_t<Q>> {
    return {std::forward<T>(t), get_env(std::forward<Q>(q))};
}
} // namespace async
