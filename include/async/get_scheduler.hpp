#pragma once

#include <async/forwarding_query.hpp>

#include <stdx/ct_string.hpp>
#include <stdx/type_traits.hpp>

#include <utility>

namespace async {
constexpr inline struct get_scheduler_t : forwarding_query_t {
    constexpr static auto name = stdx::ct_string{"get_scheduler"};

    template <typename T>
    constexpr auto operator()(T &&t) const noexcept(
        noexcept(std::forward<T>(t).query(std::declval<get_scheduler_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }
} get_scheduler{};
} // namespace async
