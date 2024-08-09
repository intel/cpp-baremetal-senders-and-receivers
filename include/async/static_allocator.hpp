#pragma once

#include <conc/concurrency.hpp>

#include <stdx/bit.hpp>
#include <stdx/bitset.hpp>
#include <stdx/compiler.hpp>

#include <array>
#include <atomic>
#include <cstddef>
#include <iterator>
#include <memory>
#include <type_traits>
#include <utility>

namespace async {
namespace detail {
template <typename Name, typename T, std::size_t N> struct static_allocator_t {
    constexpr static inline auto alignment = alignof(T);
    constexpr static inline auto size = sizeof(T);
    constexpr static inline auto aligned_size =
        size % alignment == 0 ? size : size + alignment - (size % alignment);

    using storage_t = std::array<std::byte, aligned_size * N>;

    alignas(alignment) storage_t data{};
    stdx::bitset<N> used{};
    struct mutex;

    template <typename... Args>
    auto construct(Args &&...args) LIFETIMEBOUND -> T * {
        auto const idx = conc::call_in_critical_section<mutex>([&] {
            auto const i = used.lowest_unset();
            if (i != N) {
                used.set(i);
            }
            return i;
        });
        if (idx == N) {
            return nullptr;
        }
        auto const ptr = std::data(data) + idx * aligned_size;
        return std::construct_at(stdx::bit_cast<T *>(ptr),
                                 std::forward<Args>(args)...);
    }

    auto destruct(T const *t) -> void {
        std::destroy_at(t);
        auto const ptr = stdx::bit_cast<std::byte *>(t);
        auto const idx =
            static_cast<std::size_t>(ptr - std::data(data)) / aligned_size;
        conc::call_in_critical_section<mutex>([&] { used.reset(idx); });
    }
};

template <typename Name, typename T> struct static_allocator_t<Name, T, 1> {
    constexpr static inline auto alignment = alignof(T);
    constexpr static inline auto size = sizeof(T);

    alignas(alignment) std::array<std::byte, size> data{};
    std::atomic<bool> used{};

    template <typename... Args>
    auto construct(Args &&...args) LIFETIMEBOUND -> T * {
        if (not used.exchange(true)) {
            return std::construct_at(stdx::bit_cast<T *>(std::data(data)),
                                     std::forward<Args>(args)...);
        } else {
            return nullptr;
        }
    }

    auto destruct(T const *t) -> void {
        std::destroy_at(t);
        used = false;
    }
};

template <typename Name, typename T, std::size_t N>
inline auto static_allocator_v = static_allocator_t<Name, T, N>{};
} // namespace detail

template <typename Name>
constexpr inline auto static_allocation_limit = std::size_t{1};

struct static_allocator {
    template <typename Name, typename T, typename F, typename... Args>
        requires std::is_constructible_v<T, Args...>
    static auto construct(F &&f, Args &&...args) -> bool {
        auto &a =
            detail::static_allocator_v<Name, T, static_allocation_limit<Name>>;
        if (auto t = a.construct(std::forward<Args>(args)...); t != nullptr) {
            if constexpr (requires { std::forward<F>(f)(std::move(*t)); }) {
                std::forward<F>(f)(std::move(*t));
            } else {
                std::forward<F>(f)(*t);
            }
            return true;
        }
        return false;
    }

    template <typename Name, typename T>
    static auto destruct(T const *t) -> void {
        auto &a =
            detail::static_allocator_v<Name, T, static_allocation_limit<Name>>;
        a.destruct(t);
    }
};
} // namespace async
