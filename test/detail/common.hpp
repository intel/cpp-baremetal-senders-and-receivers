#pragma once

#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/forwarding_query.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/start.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>

#include <condition_variable>
#include <iterator>
#include <mutex>
#include <optional>
#include <type_traits>
#include <utility>

namespace {
template <typename T = int> struct move_only {
    move_only(T t) : value{std::move(t)} {}
    move_only(move_only &&) = default;
    move_only &operator=(move_only &&) = default;
    T value;
};

struct non_moveable {
    non_moveable() = default;
    non_moveable(non_moveable &&) = delete;
};

using universal_receiver = async::detail::universal_receiver<>;

template <typename F> struct receiver {
    using is_receiver = void;

    template <typename... Args>
    constexpr auto set_value(Args &&...args) const && -> void {
        f(std::forward<Args>(args)...);
    }
    constexpr auto set_error(auto &&...) const && -> void {}
    constexpr auto set_stopped() const && -> void {}

    F f;
};
template <typename F> receiver(F) -> receiver<F>;

template <typename F> struct error_receiver {
    using is_receiver = void;

    constexpr auto set_value(auto &&...) const && -> void {}
    template <typename... Args>
    constexpr auto set_error(Args &&...args) const && -> void {
        f(std::forward<Args>(args)...);
    }
    constexpr auto set_stopped() const && -> void {}

    F f;
};
template <typename F> error_receiver(F) -> error_receiver<F>;

template <typename F> struct stopped_receiver {
    using is_receiver = void;

    constexpr auto set_value(auto &&...) const && -> void {}
    constexpr auto set_error(auto &&...) const && -> void {}
    constexpr auto set_stopped() const && -> void { f(); }

    F f;
};
template <typename F> stopped_receiver(F) -> stopped_receiver<F>;

template <typename T> std::optional<async::inplace_stop_source> stop_source{};

template <typename F> struct stoppable_receiver {
    using is_receiver = void;

    explicit stoppable_receiver(F &&fn) : f{std::move(fn)} {
        stop_source<stoppable_receiver>.emplace();
    }
    ~stoppable_receiver() { stop_source<stoppable_receiver>.reset(); }

    auto request_stop() { stop_source<stoppable_receiver>->request_stop(); }

    [[nodiscard]] constexpr auto query(async::get_env_t) const {
        return async::prop{async::get_stop_token_t{},
                           stop_source<stoppable_receiver>->get_token()};
    }

    constexpr auto set_value(auto &&...) const && -> void { f(); }
    constexpr auto set_error(auto &&...) const && -> void { f(); }
    constexpr auto set_stopped() const && -> void { f(); }

    F f;
};
template <typename F> stoppable_receiver(F) -> stoppable_receiver<F>;

class singleshot_scheduler {
    template <typename R> struct op_state {
        [[no_unique_address]] R receiver;

        constexpr auto start() & -> void {
            async::set_value(std::move(receiver));
        }
    };

    struct sender {
        using is_sender = void;
        using completion_signatures =
            async::completion_signatures<async::set_value_t()>;

        [[nodiscard, maybe_unused]] constexpr static auto
        query(async::get_env_t) noexcept {
            return async::make_template_prop<
                async::get_completion_scheduler_t, async::set_value_t,
                async::set_error_t, async::set_stopped_t>(
                singleshot_scheduler{});
        }

        template <async::receiver_from<sender> R>
        [[nodiscard]] constexpr auto
        connect(R &&r) && -> op_state<std::remove_cvref_t<R>> {
            return {std::forward<R>(r)};
        }
    };

    [[nodiscard, maybe_unused]] friend constexpr auto
    operator==(singleshot_scheduler, singleshot_scheduler) -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender { return {}; }
};

struct none {
    [[nodiscard]] friend constexpr auto operator==(none,
                                                   none) -> bool = default;
};

constexpr inline struct get_fwd_t : async::forwarding_query_t {
    template <typename T>
    constexpr auto operator()(T &&t) const
        noexcept(noexcept(std::forward<T>(t).query(std::declval<get_fwd_t>())))
            -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }

    constexpr auto operator()(...) const { return none{}; }
} get_fwd{};

constexpr inline struct get_nofwd_t {
    template <typename T>
    constexpr auto operator()(T &&t) const noexcept(
        noexcept(std::forward<T>(t).query(std::declval<get_nofwd_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }

    constexpr auto operator()(...) const { return none{}; }
} get_nofwd{};

struct custom_env {
    [[nodiscard]] constexpr static auto query(get_fwd_t) -> int { return 42; }
    [[nodiscard]] constexpr static auto query(get_nofwd_t) -> int { return 17; }
};

struct custom_sender {
    using is_sender = void;
    using completion_signatures =
        async::completion_signatures<async::set_value_t()>;

    [[nodiscard]] constexpr static auto
    query(async::get_env_t) noexcept -> custom_env {
        return {};
    }

    template <typename R> struct op_state {
        auto start() -> void { async::set_value(std::move(r)); }
        [[no_unique_address]] R r;
    };

    template <typename R>
    [[nodiscard]] constexpr static auto
    connect(R &&r) -> op_state<std::remove_cvref_t<R>> {
        return {std::forward<R>(r)};
    }
};

struct phase_control {
    auto advance() -> int {
        std::lock_guard l{m};
        auto p = ++phase;
        cv.notify_one();
        return p;
    }

    auto wait_for(int p) -> void {
        std::unique_lock l{m};
        cv.wait(l, [&] { return phase == p; });
    }

    auto advance_and_wait() -> void {
        auto p = advance();
        wait_for(p + 1);
    }

  private:
    std::mutex m{};
    std::condition_variable cv{};
    int phase{};
};

template <typename R> struct stoppable_just_op_state {
    [[no_unique_address]] R receiver;

    constexpr auto start() & -> void {
        if constexpr (not async::unstoppable_token<
                          async::stop_token_of_t<async::env_of_t<R>>>) {
            if (async::get_stop_token(async::get_env(receiver))
                    .stop_requested()) {
                async::set_stopped(std::move(receiver));
                return;
            }
        }
        async::set_value(std::move(receiver));
    }
};

struct stoppable_just {
    using is_sender = void;
    using completion_signatures =
        async::completion_signatures<async::set_value_t(),
                                     async::set_stopped_t()>;

    template <async::receiver R>
    [[nodiscard]] constexpr static auto
    connect(R &&r) -> stoppable_just_op_state<std::remove_cvref_t<R>> {
        return {std::forward<R>(r)};
    }
};
} // namespace
