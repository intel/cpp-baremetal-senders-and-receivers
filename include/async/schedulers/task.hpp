#pragma once

#include <stdx/concepts.hpp>
#include <stdx/function_traits.hpp>
#include <stdx/tuple.hpp>

#include <concepts>
#include <memory>
#include <type_traits>
#include <utility>

namespace async {
// NOLINTNEXTLINE(*-special-member-functions, *-virtual-class-destructor)
struct task_base {
    bool pending{};
    virtual auto run() -> void = 0;

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

template <typename...> struct task;

template <stdx::callable F, typename Base>
struct task<F, Base, stdx::tuple<>> : Base {
    constexpr explicit(true) task(F const &f) : func{f} {}
    constexpr explicit(true) task(F &&f) : func{std::move(f)} {}

    auto run() -> void final { func(); }

    [[no_unique_address]] F func;
};

template <stdx::callable F, typename Base, typename... Args>
struct bound_task : Base {
    constexpr explicit(true) bound_task(F const &f) : func{f} {}
    constexpr explicit(true) bound_task(F &&f) : func{std::move(f)} {}

    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-union-access)
    auto run() -> void final { args.apply(func); }

    [[no_unique_address]] F func;
    union {
        char unused{};
        [[no_unique_address]] stdx::tuple<Args...> args;
    };
};

namespace task_detail {
template <typename T>
concept union_usable = std::is_trivially_copy_constructible_v<T> and
                       std::is_trivially_destructible_v<T>;

struct poisoned_run {
    template <typename... CantRunWithUnboundArguments> auto run() const -> void;
};
} // namespace task_detail

template <stdx::callable F, typename Base, typename... Args>
struct task<F, Base, stdx::tuple<Args...>> {
    static_assert(
        stdx::always_false_v<task>,
        "task: bound arguments must be trivially copyable and destructible");
};

template <stdx::callable F, typename Base, task_detail::union_usable... Args>
struct task<F, Base, stdx::tuple<Args...>> : bound_task<F, Base, Args...>,
                                             task_detail::poisoned_run {
    using bound_t = bound_task<F, Base, Args...>;

    using bound_t::bound_t;

    template <typename... As>
    constexpr auto bind_front(As &&...as) -> bound_t & {
        this->args = {std::forward<As>(as)...};
        return *this;
    }
};

template <typename T>
constexpr auto create_task = []<typename F>(F &&f) {
    using func_t = std::remove_cvref_t<F>;
    using args_t = stdx::decayed_args_t<func_t, stdx::tuple>;
    return task<func_t, T, args_t>{std::forward<F>(f)};
};
} // namespace async
