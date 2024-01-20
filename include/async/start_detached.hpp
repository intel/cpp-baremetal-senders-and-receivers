#pragma once

#include <async/allocator.hpp>
#include <async/concepts.hpp>
#include <async/tags.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _start_detached {
template <typename Ops> struct receiver {
    using is_receiver = void;

    template <typename... Args> auto set_value(Args &&...) const -> void {
        ops->die();
    }
    auto set_error(auto &&...) const -> void { ops->die(); }
    auto set_stopped() const -> void { ops->die(); }

    Ops *ops;
};

template <typename Uniq, typename Sndr>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using Ops = connect_result_t<Sndr, receiver<op_state>>;

    template <typename S>
    constexpr explicit(true) op_state(S &&s)
        : ops{connect(std::forward<S>(s), receiver<op_state>{this})} {}
    constexpr op_state(op_state &&) = delete;

    auto start() -> void { ops.start(); }

    auto die() {
        auto &alloc = get_allocator<Uniq, op_state>();
        alloc.destruct(this);
    }

    Ops ops;
};

template <typename Uniq, sender S> [[nodiscard]] auto start(S &&s) -> bool {
    using O = op_state<Uniq, std::remove_cvref_t<S>>;
    auto &alloc = get_allocator<Uniq, O>();
    if (auto op_state = alloc.construct(std::forward<S>(s)); op_state) {
        op_state->start();
        return true;
    }
    return false;
}

template <typename Uniq> struct pipeable {
  private:
    template <async::sender S, typename Self>
        requires std::same_as<pipeable, std::remove_cvref_t<Self>>
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
