#pragma once

#include <utility>

namespace async {
constexpr inline struct start_t {
    template <typename O>
    constexpr auto operator()(O &o) const noexcept(noexcept(o.start()))
        -> decltype(o.start()) {
        return o.start();
    }
} start{};
} // namespace async
