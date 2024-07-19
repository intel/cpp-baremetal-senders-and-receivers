#pragma once

#include <async/env.hpp>

#include <type_traits>
#include <utility>

namespace async {
constexpr inline struct get_completion_signatures_t {
    template <typename S, typename E>
    constexpr auto operator()(S &&s, E &&e) const noexcept(noexcept(
        std::forward<S>(s).get_completion_signatures(std::forward<E>(e))))
        -> decltype(std::forward<S>(s).get_completion_signatures(
            std::forward<E>(e))) {
        return std::forward<S>(s).get_completion_signatures(std::forward<E>(e));
    }

    template <typename S, typename E>
    constexpr auto operator()(S const &, E const &) const noexcept ->
        typename std::remove_cvref_t<S>::completion_signatures {
        return {};
    }
} get_completion_signatures{};

template <typename S, typename E = empty_env>
using completion_signatures_of_t =
    std::invoke_result_t<get_completion_signatures_t, S, E>;
} // namespace async
