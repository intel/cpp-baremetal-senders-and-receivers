#pragma once

#include <stdx/bit.hpp>
#include <stdx/bitset.hpp>

#include <array>
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

    template <typename... Args> auto construct(Args &&...args) -> T * {
        auto const idx = used.lowest_unset();
        if (idx == N) {
            return nullptr;
        }
        auto const ptr = std::data(data) + idx * aligned_size;
        used.set(idx);
        return std::construct_at(stdx::bit_cast<T *>(ptr),
                                 std::forward<Args>(args)...);
    }

    auto destruct(T const *t) -> void {
        std::destroy_at(t);
        auto const ptr = stdx::bit_cast<std::byte *>(t);
        auto const idx =
            static_cast<std::size_t>(ptr - std::data(data)) / aligned_size;
        used.reset(idx);
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
            std::forward<F>(f)(std::move(*t));
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
