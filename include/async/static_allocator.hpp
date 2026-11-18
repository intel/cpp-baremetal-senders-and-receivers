#pragma once

#include <stdx/atomic.hpp>
#include <stdx/bit.hpp>
#include <stdx/bitset.hpp>
#include <stdx/compiler.hpp>

#include <conc/concurrency.hpp>

#include <array>
#include <cstddef>
#include <iterator>
#include <memory>
#include <type_traits>
#include <utility>

namespace async {
namespace detail {
template <typename Name, std::size_t Size, std::size_t Align, std::size_t N>
struct sa_data_t {
    constexpr static inline auto size = Size;

    using storage_t = std::array<std::byte, Size * N>;
    alignas(Align) storage_t buffer{};
    stdx::bitset<N> used{};
};

template <typename Name, std::size_t Size, std::size_t Align>
struct sa_data_t<Name, Size, Align, 1> {
    constexpr static inline auto size = Size;

    // NOLINTNEXTLINE(bugprone-throwing-static-initialization)
    static inline stdx::atomic<bool> used{};
    alignas(Align) static inline std::array<std::byte, Size> buffer{};
};

template <typename Name, std::size_t Size, std::size_t Align, std::size_t N>
inline auto sa_data_v = sa_data_t<Name, Size, Align, N>{};

template <typename Name, std::size_t N> struct static_allocator_t {
    struct mutex;

    template <typename T> constexpr auto data_for() -> auto & {
        constexpr auto alignment = alignof(T);
        constexpr auto size = sizeof(T);
        constexpr auto aligned_size =
            size % alignment == 0 ? size
                                  : size + alignment - (size % alignment);

        return sa_data_v<Name, aligned_size, alignment, N>;
    }

    template <typename T, typename... Args>
    auto construct(Args &&...args) LIFETIMEBOUND -> T * {
        auto &data = data_for<T>();
        auto const idx = conc::call_in_critical_section<mutex>([&] {
            auto const i = data.used.lowest_unset();
            if (i != N) {
                data.used.set(i);
            }
            return i;
        });
        if (idx == N) {
            return nullptr;
        }
        constexpr auto size = std::remove_cvref_t<decltype(data)>::size;
        auto const ptr = std::data(data.buffer) + (idx * size);
        return std::construct_at(stdx::bit_cast<T *>(ptr),
                                 std::forward<Args>(args)...);
    }

    template <typename T> auto destruct(T const *t) -> void {
        std::destroy_at(t);
        auto &data = data_for<T>();
        auto const ptr = stdx::bit_cast<std::byte *>(t);
        constexpr auto size = std::remove_cvref_t<decltype(data)>::size;
        auto const idx =
            static_cast<std::size_t>(ptr - std::data(data.buffer)) / size;
        conc::call_in_critical_section<mutex>([&] { data.used.reset(idx); });
    }
};

template <typename Name> struct static_allocator_t<Name, 1> {
    template <typename T> constexpr auto data_for() -> auto & {
        constexpr auto alignment = alignof(T);
        constexpr auto size = sizeof(T);
        return sa_data_v<Name, size, alignment, 1>;
    }

    template <typename T, typename... Args>
    auto construct(Args &&...args) LIFETIMEBOUND -> T * {
        auto &data = data_for<T>();
        if (not data.used.exchange(true)) {
            return std::construct_at(
                stdx::bit_cast<T *>(std::data(data.buffer)),
                std::forward<Args>(args)...);
        }
        return nullptr;
    }

    template <typename T> auto destruct(T const *t) -> void {
        std::destroy_at(t);
        auto &data = data_for<T>();
        data.used = false;
    }
};

template <typename Name, std::size_t N>
inline auto static_allocator_v = static_allocator_t<Name, N>{};
} // namespace detail

template <typename Name>
constexpr inline auto static_allocation_limit = std::size_t{1};

struct static_allocator {
    template <typename Name, typename T, typename F, typename... Args>
        requires std::is_constructible_v<T, Args...>
    static auto construct(F &&f, Args &&...args) -> bool {
        auto &a =
            detail::static_allocator_v<Name, static_allocation_limit<Name>>;
        if (auto t = a.template construct<T>(std::forward<Args>(args)...);
            t != nullptr) {
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
            detail::static_allocator_v<Name, static_allocation_limit<Name>>;
        a.destruct(t);
    }

    template <typename Name>
    constexpr static auto allocation_limit = static_allocation_limit<Name>;
};
} // namespace async
