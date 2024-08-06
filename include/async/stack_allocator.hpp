#pragma once

#include <utility>

namespace async {
struct stack_allocator {
    template <typename, typename T, typename F, typename... Args>
        requires std::is_constructible_v<T, Args...>
    static auto construct(F &&f, Args &&...args) -> bool {
        if constexpr (requires {
                          std::forward<F>(f)(T{std::forward<Args>(args)...});
                      }) {
            std::forward<F>(f)(T{std::forward<Args>(args)...});
        } else {
            T t{std::forward<Args>(args)...};
            std::forward<F>(f)(t);
        }
        return true;
    }

    template <typename, typename T> static auto destruct(T const *) -> void {}

    template <typename> constexpr static auto allocation_limit = std::size_t{};
};
} // namespace async
