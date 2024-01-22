#pragma once

#include <stdx/bit.hpp>
#include <stdx/bitset.hpp>

#include <array>
#include <cstddef>
#include <iterator>
#include <memory>
#include <utility>

namespace async {
template <typename T, std::size_t N> struct static_allocator {
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

template <typename Name>
constexpr inline auto allocation_limit = std::size_t{1};
template <typename T, std::size_t N>
static inline static_allocator<T, N> alloc{};

template <typename Name, typename T> constexpr auto get_allocator() -> auto & {
    return alloc<T, allocation_limit<Name>>;
}
} // namespace async
