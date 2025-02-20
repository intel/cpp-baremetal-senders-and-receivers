#pragma once

#include <async/forwarding_query.hpp>

#include <stdx/ct_string.hpp>
#include <stdx/type_traits.hpp>

#include <utility>

namespace async {
namespace timer_mgr {
constexpr inline struct get_expiration_t : forwarding_query_t {
    constexpr static auto name = stdx::ct_string{"get_expiration"};

    template <typename T>
        requires true
    constexpr auto operator()(T &&t) const noexcept(
        noexcept(std::forward<T>(t).query(std::declval<get_expiration_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }

    struct default_expiration_provider {
        struct time_point_t {};
    };

    template <typename T>
    constexpr auto operator()(T &&) const -> default_expiration_provider {
        static_assert(stdx::always_false_v<T>,
                      "Attempting to connect an externally controlled "
                      "time_scheduler sender to a receiver whose environment "
                      "does not provide an expiration provider");
        return {};
    }
} get_expiration{};
} // namespace timer_mgr
} // namespace async
