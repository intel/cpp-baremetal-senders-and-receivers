#pragma once

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/tags.hpp>

#include <stdx/concepts.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _start_detached {
template <typename Ops> struct receiver {
    using is_receiver = void;

    Ops *ops;

  private:
    friend auto tag_invoke(channel_tag auto, receiver const &r, auto &&...)
        -> void {
        r.ops->die();
    }
};

template <typename Uniq, typename Sndr, typename Alloc>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using Ops = connect_result_t<Sndr, receiver<op_state>>;

    template <typename S>
    constexpr explicit(true) op_state(S &&s)
        : ops{connect(std::forward<S>(s), receiver<op_state>{this})} {}
    constexpr op_state(op_state &&) = delete;

    auto die() { Alloc::template destruct<Uniq>(this); }

    Ops ops;

  private:
    template <stdx::same_as_unqualified<op_state> O>
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        start(std::forward<O>(o).ops);
    }
};

template <typename Uniq, sender S> [[nodiscard]] auto start(S &&s) -> bool {
    using Sndr = std::remove_cvref_t<S>;
    using A = allocator_of_t<env_of_t<Sndr>>;
    using O = op_state<Uniq, Sndr, A>;
    return A::template construct<Uniq, O>(async::start, std::forward<S>(s));
}

template <typename Uniq> struct pipeable {
  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend auto operator|(S &&s, Self &&) {
        return start<Uniq>(std::forward<S>(s));
    }
};
} // namespace _start_detached

template <typename Uniq = decltype([] {}), sender S>
[[nodiscard]] auto start_detached(S &&s) {
    return _start_detached::start<Uniq>(std::forward<S>(s));
}

template <typename Uniq = decltype([] {})>
[[nodiscard]] constexpr auto start_detached()
    -> _start_detached::pipeable<Uniq> {
    return {};
}
} // namespace async
