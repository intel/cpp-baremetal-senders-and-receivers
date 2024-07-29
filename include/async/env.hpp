#pragma once
#include <async/forwarding_query.hpp>

#include <stdx/tuple.hpp>

#include <boost/mp11/algorithm.hpp>

#include <type_traits>
#include <utility>

namespace async {
template <typename Query, typename Value> struct prop {
    [[nodiscard]] constexpr auto query(Query) const noexcept -> Value const & {
        return value;
    }

    [[no_unique_address]] Query _{};
    [[no_unique_address]] Value value{};
};
template <typename Query, typename Value>
prop(Query, Value) -> prop<Query, Value>;

template <template <typename> typename Query, typename Value, typename... Ts>
struct template_prop {
    template <typename T>
        requires(... or std::same_as<T, Ts>)
    [[nodiscard]] constexpr auto
    query(Query<T>) const noexcept -> Value const & {
        return value;
    }

    [[no_unique_address]] Value value{};
};

template <template <typename> typename Query, typename... Ts>
constexpr auto make_template_prop = []<typename Value>(Value &&v) {
    return template_prop<Query, std::remove_cvref_t<Value>, Ts...>{
        std::forward<Value>(v)};
};

namespace detail {
template <typename Query, typename Env>
concept valid_query_for = requires(Env const &e) { e.query(Query{}); };

template <typename Query, typename... Envs>
concept valid_query_over = (... or valid_query_for<Query, Envs>);

template <typename Query> struct has_query {
    template <typename Env>
    using fn = std::bool_constant<valid_query_for<Query, Env>>;
};
} // namespace detail

template <typename... Envs> struct env : stdx::tuple<Envs...> {
    template <detail::valid_query_over<Envs...> Query>
    constexpr auto query(Query q) const -> decltype(auto) {
        using I = boost::mp11::mp_find_if_q<stdx::tuple<Envs...>,
                                            detail::has_query<Query>>;
        return q(this->operator[](I{}));
    }
};
template <typename... Envs>
env(Envs...) -> env<std::unwrap_reference_t<Envs>...>;

using empty_env = env<>;

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

template <typename Env> struct forwarding_env : Env {
    template <detail::valid_query_for<Env> Query>
        requires(forwarding_query(Query{}))
    [[nodiscard]] constexpr auto query(Query q) const -> decltype(auto) {
        return q(static_cast<Env const &>(*this));
    }
};
template <typename Env> forwarding_env(Env) -> forwarding_env<Env>;

template <typename Q>
constexpr auto forward_env_of(Q &&q) -> forwarding_env<env_of_t<Q>> {
    return {get_env(std::forward<Q>(q))};
}

template <typename Query, typename Value, typename Q>
using overriding_env = env<prop<Query, Value>, env_of_t<Q>>;

template <typename Query, typename Value, typename Q>
constexpr auto override_env_with(Value &&v, Q &&q)
    -> overriding_env<Query, std::remove_cvref_t<Value>, Q> {
    return env{prop{Query{}, std::forward<Value>(v)},
               get_env(std::forward<Q>(q))};
}
} // namespace async
