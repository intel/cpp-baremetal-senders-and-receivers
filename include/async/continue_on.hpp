#pragma once

#include <async/concepts.hpp>
#include <async/just.hpp>
#include <async/let_value.hpp>
#include <async/sequence.hpp>

#include <stdx/concepts.hpp>

#include <type_traits>
#include <utility>

namespace async {
namespace _continue_on {
template <typename Sched> struct pipeable {
    Sched sched;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return std::forward<S>(s) |
               let_value(
                   [sch = std::forward<Self>(self).sched]<typename... Args>(
                       Args &&...args) mutable {
                       return sch.schedule() |
                              seq(async::just(std::forward<Args>(args)...));
                   });
    }
};
} // namespace _continue_on

template <typename Sched>
[[nodiscard]] constexpr auto continue_on(Sched &&sched)
    -> _continue_on::pipeable<std::remove_cvref_t<Sched>> {
    return {std::forward<Sched>(sched)};
}

template <sender S, typename Sched>
[[nodiscard]] constexpr auto continue_on(S &&s, Sched &&sched) -> sender auto {
    return std::forward<S>(s) | continue_on(std::forward<Sched>(sched));
}
} // namespace async
