#pragma once

#include <async/concepts.hpp>
#include <async/let_value.hpp>

#include <type_traits>
#include <utility>

namespace async {
namespace _on {
template <sender S> struct next_sender {
    constexpr auto operator()() const & -> S
        requires std::copy_constructible<S>
    {
        return s;
    }
    constexpr auto operator()() && -> S
        requires(not std::copy_constructible<S>)
    {
        return std::move(s);
    }

    S s;
};
} // namespace _on

template <scheduler Sched, sender S>
[[nodiscard]] constexpr auto on(Sched &&sched, S &&s) -> sender auto {
    return std::forward<Sched>(sched).schedule() |
           let_value(
               _on::next_sender<std::remove_cvref_t<S>>{std::forward<S>(s)});
}
} // namespace async
