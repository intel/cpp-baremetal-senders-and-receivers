#pragma once

#include <stdx/concepts.hpp>
#include <stdx/function_traits.hpp>
#include <stdx/tuple.hpp>

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <utility>

namespace async {
// NOLINTNEXTLINE(*-special-member-functions)
struct task_base {
    bool pending{};
    virtual auto run() -> void = 0;
    virtual ~task_base() = default;

  private:
    [[nodiscard]] friend constexpr auto operator==(task_base const &lhs,
                                                   task_base const &rhs) {
        return std::addressof(lhs) == std::addressof(rhs);
    }
};

// NOLINTNEXTLINE(*-special-member-functions)
template <std::derived_from<task_base> Base> struct single_linked_task : Base {
    constexpr single_linked_task() = default;
    constexpr single_linked_task(single_linked_task &&) = delete;
    single_linked_task *next{};
};

// NOLINTNEXTLINE(*-special-member-functions)
template <std::derived_from<task_base> Base> struct double_linked_task : Base {
    constexpr double_linked_task() = default;
    constexpr double_linked_task(double_linked_task &&) = delete;
    double_linked_task *next{};
    double_linked_task *prev{};
};

template <stdx::callable F, typename ArgTuple, typename Base>
struct task : Base {
    constexpr explicit(true) task(F const &f) : func(f) {}
    constexpr explicit(true) task(F &&f) : func(std::move(f)) {}

    template <typename... Args> auto bind_front(Args &&...args) -> task & {
        [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            ((get<Is>(bound_args) = std::forward<Args>(args)), ...);
        }(std::index_sequence_for<Args...>{});
        return *this;
    }

    auto run() -> void final { bound_args.apply(func); }

    [[no_unique_address]] F func;
    [[no_unique_address]] ArgTuple bound_args{};
};

template <typename T>
constexpr auto create_task = []<typename F>(F &&f) {
    using func_t = std::remove_cvref_t<F>;
    using args_t = stdx::decayed_args_t<func_t, stdx::tuple>;
    return task<func_t, args_t, T>{std::forward<F>(f)};
};
} // namespace async
