#pragma once

#include <async/concepts.hpp>
#include <async/sequence.hpp>

#include <utility>

namespace async {
template <scheduler Sched, sender S>
[[nodiscard]] constexpr auto start_on(Sched &&sched, S &&s) -> sender auto {
    return std::forward<Sched>(sched).schedule() | seq(std::forward<S>(s));
}
} // namespace async
