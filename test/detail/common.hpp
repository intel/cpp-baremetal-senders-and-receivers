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

template <typename F> struct receiver : F {
    using is_receiver = void;

  private:
    template <stdx::same_as_unqualified<receiver> R, typename... Args>
    friend constexpr auto tag_invoke(async::set_value_t, R &&r,
                                     Args &&...args) -> void {
        std::forward<R>(r)(std::forward<Args>(args)...);
    }
    friend constexpr auto tag_invoke(async::channel_tag auto, receiver const &,
                                     auto &&...) -> void {}
};
template <typename F> receiver(F) -> receiver<F>;

template <typename F> struct error_receiver : F {
    using is_receiver = void;

  private:
    template <stdx::same_as_unqualified<error_receiver> R, typename... Args>
    friend constexpr auto tag_invoke(async::set_error_t, R &&r,
                                     Args &&...args) -> void {
        std::forward<R>(r)(std::forward<Args>(args)...);
    }
    friend constexpr auto tag_invoke(async::channel_tag auto,
                                     error_receiver const &,
                                     auto &&...) -> void {}
};
template <typename F> error_receiver(F) -> error_receiver<F>;

template <typename F> struct stopped_receiver : F {
    using is_receiver = void;

  private:
    template <stdx::same_as_unqualified<stopped_receiver> R>
    friend constexpr auto tag_invoke(async::set_stopped_t, R &&r) -> void {
        std::forward<R>(r)();
    }
    friend constexpr auto tag_invoke(async::channel_tag auto,
                                     stopped_receiver const &,
                                     auto &&...) -> void {}
};
template <typename F> stopped_receiver(F) -> stopped_receiver<F>;

template <typename T> std::optional<async::inplace_stop_source> stop_source{};

template <typename F> struct stoppable_receiver : F {
    using is_receiver = void;

    explicit stoppable_receiver(F &&f) : F{std::move(f)} {
        stop_source<stoppable_receiver>.emplace();
    }
    ~stoppable_receiver() { stop_source<stoppable_receiver>.reset(); }

    auto request_stop() { stop_source<stoppable_receiver>->request_stop(); }

    struct env {
        async::inplace_stop_token stop_token;

        [[nodiscard]] constexpr auto
        query(async::get_stop_token_t) const -> async::inplace_stop_token {
            return stop_token;
        }
    };

    [[nodiscard]] constexpr auto query(async::get_env_t) const -> env {
        return {stop_source<stoppable_receiver>->get_token()};
    }

  private:
    template <stdx::same_as_unqualified<stoppable_receiver> R>
    friend constexpr auto tag_invoke(async::channel_tag auto, R &&r,
                                     auto &&...) -> void {
        std::forward<R>(r)();
    }
};
template <typename F> stoppable_receiver(F) -> stoppable_receiver<F>;

template <typename F> struct only_stoppable_receiver : stoppable_receiver<F> {
    explicit only_stoppable_receiver(F &&f)
        : stoppable_receiver<F>{std::move(f)} {}

  private:
    template <stdx::same_as_unqualified<only_stoppable_receiver> R>
    friend constexpr auto tag_invoke(async::channel_tag auto, R &&r,
                                     auto &&...) -> void {
        std::forward<R>(r)();
    }
    template <stdx::same_as_unqualified<only_stoppable_receiver> R>
    friend constexpr auto tag_invoke(async::set_stopped_t, R &&r) -> void {
        std::forward<R>(r)();
    }
};
template <typename F> only_stoppable_receiver(F) -> only_stoppable_receiver<F>;

class singleshot_scheduler {
    template <typename R> struct op_state {
        [[no_unique_address]] R receiver;

      private:
        template <stdx::same_as_unqualified<op_state> O>
        friend constexpr auto tag_invoke(async::start_t, O &&o) -> void {
            async::set_value(std::forward<O>(o).receiver);
        }
    };

    struct env {
        template <typename Tag>
        [[nodiscard]] static constexpr auto
        query(async::get_completion_scheduler_t<Tag>) noexcept
            -> singleshot_scheduler {
            return {};
        }
    };

    struct sender {
        using is_sender = void;
        using completion_signatures =
            async::completion_signatures<async::set_value_t()>;

        [[nodiscard, maybe_unused]] constexpr static auto
        query(async::get_env_t) noexcept -> env {
            return {};
        }

      private:
        template <async::receiver_from<sender> R>
        [[nodiscard]] friend constexpr auto
        tag_invoke(async::connect_t, sender &&, R &&r) -> op_state<R> {
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
        noexcept(noexcept(tag_invoke(std::declval<get_fwd_t>(),
                                     std::forward<T>(t))))
            -> decltype(tag_invoke(*this, std::forward<T>(t))) {
        return tag_invoke(*this, std::forward<T>(t));
    }

    constexpr auto operator()(...) const { return none{}; }
} get_fwd{};

constexpr inline struct get_nofwd_t {
    template <typename T>
    constexpr auto operator()(T &&t) const
        noexcept(noexcept(tag_invoke(std::declval<get_nofwd_t>(),
                                     std::forward<T>(t))))
            -> decltype(tag_invoke(*this, std::forward<T>(t))) {
        return tag_invoke(*this, std::forward<T>(t));
    }

    constexpr auto operator()(...) const { return none{}; }
} get_nofwd{};

struct custom_env {
    [[nodiscard]] friend constexpr auto tag_invoke(get_fwd_t,
                                                   custom_env const &) -> int {
        return 42;
    }
    [[nodiscard]] friend constexpr auto tag_invoke(get_nofwd_t,
                                                   custom_env const &) -> int {
        return 17;
    }
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
    [[nodiscard]] friend constexpr auto
    tag_invoke(async::connect_t, custom_sender &&,
               R &&r) -> op_state<std::remove_cvref_t<R>> {
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

  private:
    template <stdx::same_as_unqualified<stoppable_just_op_state> O>
    friend constexpr auto tag_invoke(async::start_t, O &&o) -> void {
        if constexpr (not async::unstoppable_token<
                          async::stop_token_of_t<async::env_of_t<R>>>) {
            if (async::get_stop_token(async::get_env(o.receiver))
                    .stop_requested()) {
                async::set_stopped(std::move(o.receiver));
                return;
            }
        }
        async::set_value(std::move(o.receiver));
    }
};

struct stoppable_just {
    using is_sender = void;
    using completion_signatures =
        async::completion_signatures<async::set_value_t(),
                                     async::set_stopped_t()>;

  private:
    template <async::receiver R>
    [[nodiscard]] friend constexpr auto
    tag_invoke(async::connect_t, stoppable_just const &,
               R &&r) -> stoppable_just_op_state<std::remove_cvref_t<R>> {
        return {std::forward<R>(r)};
    }
};
} // namespace
