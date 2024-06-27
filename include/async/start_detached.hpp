#pragma once

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/stop_token.hpp>
#include <async/tags.hpp>

#include <stdx/concepts.hpp>
#include <stdx/optional.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _start_detached {
template <typename Ops> struct receiver {
    using is_receiver = void;

    Ops *ops;

    [[nodiscard]] constexpr auto query(get_env_t) const
        -> detail::singleton_env<
            get_stop_token_t,
            decltype(std::declval<typename Ops::stop_source_t>().get_token())> {
        return singleton_env<get_stop_token_t>(ops->stop_src.get_token());
    }

  private:
    friend auto tag_invoke(channel_tag auto, receiver const &r,
                           auto &&...) -> void {
        r.ops->die();
    }
};

template <typename Uniq, typename Sndr, typename Alloc, typename StopSource>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using receiver_t = receiver<op_state>;
    using stop_source_t = StopSource;
    using Ops = connect_result_t<Sndr, receiver_t>;

    template <typename S>
    constexpr explicit(true) op_state(S &&s)
        : ops{connect(std::forward<S>(s), receiver<op_state>{this})} {}
    constexpr op_state(op_state &&) = delete;

    auto die() { Alloc::template destruct<Uniq>(this); }

    [[no_unique_address]] stop_source_t stop_src;
    Ops ops;

  private:
    template <stdx::same_as_unqualified<op_state> O>
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        start(std::forward<O>(o).ops);
    }
};

template <typename Uniq, typename StopSource, sender S>
[[nodiscard]] auto start(S &&s) -> stdx::optional<StopSource *> {
    using Sndr = std::remove_cvref_t<S>;
    using A = allocator_of_t<env_of_t<Sndr>>;
    using O = op_state<Uniq, Sndr, A, StopSource>;
    stdx::optional<StopSource *> stop_src{};
    A::template construct<Uniq, O>(
        [&](O &&ops) {
            stop_src = std::addressof(ops.stop_src);
            async::start(std::move(ops));
        },
        std::forward<S>(s));
    return stop_src;
}

template <typename Uniq, typename StopSource> struct pipeable {
  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    [[nodiscard]] friend auto operator|(S &&s, Self &&) {
        return start<Uniq, StopSource>(std::forward<S>(s));
    }
};
} // namespace _start_detached

template <typename Uniq = decltype([] {})>
[[nodiscard]] constexpr auto
start_detached() -> _start_detached::pipeable<Uniq, inplace_stop_source> {
    return {};
}

template <typename Uniq = decltype([] {}), sender S>
[[nodiscard]] auto start_detached(S &&s) {
    return std::forward<S>(s) | start_detached<Uniq>();
}

template <typename Uniq = decltype([] {})>
[[nodiscard]] constexpr auto start_detached_unstoppable()
    -> _start_detached::pipeable<Uniq, never_stop_source> {
    return {};
}

template <typename Uniq = decltype([] {}), sender S>
[[nodiscard]] auto start_detached_unstoppable(S &&s) {
    return std::forward<S>(s) | start_detached_unstoppable<Uniq>();
}
} // namespace async
