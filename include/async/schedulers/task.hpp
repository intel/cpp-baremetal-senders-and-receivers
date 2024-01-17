#pragma once

#include <stdx/concepts.hpp>

#include <cstddef>
#include <cstdint>
#include <memory>
#include <utility>

namespace async {
// NOLINTNEXTLINE(cppcoreguidelines-virtual-class-destructor)
struct task_base {
    bool pending{};
    virtual auto run() -> void = 0;
};

// NOLINTNEXTLINE(*-virtual-class-destructor,*-special-member-functions)
struct single_linked_task : task_base {
    constexpr single_linked_task() = default;
    constexpr single_linked_task(single_linked_task &&) = delete;
    single_linked_task *next{};

  private:
    [[nodiscard]] friend constexpr auto
    operator==(single_linked_task const &lhs, single_linked_task const &rhs) {
        return std::addressof(lhs) == std::addressof(rhs);
    }
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

using priority_t = std::uint8_t;
} // namespace async
