#pragma once

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/stop_token.hpp>

#include <stdx/concepts.hpp>
#include <stdx/optional.hpp>

#include <concepts>
#include <functional>
#include <type_traits>
#include <utility>

namespace async {
namespace _start_detached {
template <typename Ops, typename Env> struct receiver {
    using is_receiver = void;

    Ops *ops;
    [[no_unique_address]] Env e;

    [[nodiscard]] constexpr auto query(get_env_t) const
        -> env<prop<get_stop_token_t,
                    decltype(std::declval<typename Ops::stop_source_t>()
                                 .get_token())>,
               Env const &> {
        return env{prop{get_stop_token_t{}, ops->stop_src.get_token()},
                   std::cref(e)};
    }

    constexpr auto set_value(auto &&...) const && -> void { ops->die(); }
    constexpr auto set_error(auto &&...) const && -> void { ops->die(); }
    constexpr auto set_stopped() const && -> void { ops->die(); }
};

template <typename Uniq, typename Sndr, typename Alloc, typename StopSource,
          typename Env>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using receiver_t = receiver<op_state, Env>;
    using stop_source_t = StopSource;
    using Ops = connect_result_t<Sndr, receiver_t>;

    template <typename S>
    constexpr explicit(true) op_state(S &&s, Env &&e)
        : ops{connect(std::forward<S>(s), receiver_t{this, std::move(e)})} {}
    constexpr op_state(op_state &&) = delete;

    auto die() { Alloc::template destruct<Uniq>(this); }

    constexpr auto start() & -> void { async::start(ops); }

    [[no_unique_address]] stop_source_t stop_src;
    Ops ops;
};

template <typename Uniq, typename StopSource, sender S, typename Env>
[[nodiscard]] auto start(S &&s, Env &&e) -> stdx::optional<StopSource *> {
    using Sndr = std::remove_cvref_t<S>;
    auto composite_env = env{std::forward<Env>(e), get_env(s)};
    using E = decltype(composite_env);
    using A = allocator_of_t<E>;
    using O = op_state<Uniq, Sndr, A, StopSource, E>;
    stdx::optional<StopSource *> stop_src{};
    A::template construct<Uniq, O>(
        [&](O &ops) {
            stop_src = std::addressof(ops.stop_src);
            async::start(ops);
        },
        std::forward<S>(s), std::move(composite_env));
    return stop_src;
}

template <typename Uniq, typename StopSource, typename Env> struct pipeable {
    [[no_unique_address]] Env e;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    [[nodiscard]] friend auto operator|(S &&s, Self &&self) {
        return start<Uniq, StopSource>(std::forward<S>(s),
                                       std::forward<Self>(self).e);
    }
};
} // namespace _start_detached

template <typename Uniq = decltype([] {}), typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] constexpr auto start_detached(Env &&e = {})
    -> _start_detached::pipeable<Uniq, inplace_stop_source, Env> {
    return {std::forward<Env>(e)};
}

template <typename Uniq = decltype([] {}), sender S, typename Env = empty_env>
[[nodiscard]] auto start_detached(S &&s, Env &&e = {}) {
    return std::forward<S>(s) | start_detached<Uniq>(std::forward<Env>(e));
}

template <typename Uniq = decltype([] {}), typename Env = empty_env>
    requires(not sender<Env>)
[[nodiscard]] constexpr auto start_detached_unstoppable(Env &&e = {})
    -> _start_detached::pipeable<Uniq, never_stop_source, Env> {
    return {std::forward<Env>(e)};
}

template <typename Uniq = decltype([] {}), sender S, typename Env = empty_env>
[[nodiscard]] auto start_detached_unstoppable(S &&s, Env &&e = {}) {
    return std::forward<S>(s) |
           start_detached_unstoppable<Uniq>(std::forward<Env>(e));
}
} // namespace async
