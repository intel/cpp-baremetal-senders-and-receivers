#pragma once

#include <async/concepts.hpp>

#include <stdx/concepts.hpp>
#include <stdx/tuple.hpp>
#include <stdx/tuple_algorithms.hpp>
#include <stdx/utility.hpp>

#include <utility>

namespace async {
namespace _compose {
template <typename... As> struct adaptor {
    template <typename... Ts>
    constexpr explicit(true) adaptor(Ts &&...ts)
        : as{std::forward<Ts>(ts)...} {}

    using is_adaptor_composition = void;
    stdx::tuple<As...> as;

  private:
    template <async::sender S, stdx::same_as_unqualified<adaptor> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> auto {
        return std::forward<Self>(self).as.apply(
            [&]<typename... Ts>(Ts &&...ts) {
                return (std::forward<S>(s) | ... | std::forward<Ts>(ts));
            });
    }
};

template <typename... Ts> adaptor(Ts...) -> adaptor<Ts...>;
template <typename... Ts> adaptor(stdx::tuple<Ts...>) -> adaptor<Ts...>;

template <typename S>
concept is_adaptor_composition =
    requires { typename S::is_adaptor_composition; };

template <is_adaptor_composition A, is_adaptor_composition B>
constexpr auto operator|(A &&a, B &&b) {
    return adaptor{stdx::tuple_cat(FWD(a).as, FWD(b).as)};
}
} // namespace _compose
} // namespace async
