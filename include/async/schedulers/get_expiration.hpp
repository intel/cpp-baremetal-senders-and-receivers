#pragma once

#include <async/forwarding_query.hpp>

#include <stdx/ct_string.hpp>

#include <utility>

namespace async {
namespace timer_mgr {
constexpr inline struct get_expiration_t : forwarding_query_t {
    constexpr static auto name = stdx::ct_string{"get_expiration"};

    template <typename T>
    constexpr auto operator()(T &&t) const noexcept(
        noexcept(std::forward<T>(t).query(std::declval<get_expiration_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }
} get_expiration{};
} // namespace timer_mgr
} // namespace async
