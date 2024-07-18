#pragma once

#include <stdx/type_traits.hpp>

#include <utility>

namespace async {
constexpr inline struct start_t {
    template <typename> struct failure_t {};

    template <typename T>
        requires true
    constexpr auto operator()(T &&t) const
        noexcept(noexcept(tag_invoke(std::declval<start_t>(),
                                     std::forward<T>(t))))
            -> decltype(tag_invoke(*this, std::forward<T>(t))) {
        return tag_invoke(*this, std::forward<T>(t));
    }

    template <typename T>
    constexpr auto operator()(T &&) const -> failure_t<T> {
        static_assert(stdx::always_false_v<T>,
                      "No function call for start: does the argument "
                      "provide a tag_invoke(start_t)?");
        return {};
    }
} start{};
} // namespace async
