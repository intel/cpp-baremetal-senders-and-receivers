#pragma once

#include <async/completion_scheduler.hpp>
#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/forwarding_query.hpp>
#include <async/stop_token.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

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

struct universal_receiver {
    using is_receiver = void;
    auto set_value(auto &&...) const -> void {}
    auto set_error(auto &&...) const -> void {}
    auto set_stopped() const -> void {}
};

template <typename F> struct receiver : F {
    using is_receiver = void;
    template <typename... Args> auto set_value(Args &&...args) -> void {
        this->F::operator()(std::forward<Args>(args)...);
    }
    auto set_error(auto &&...) -> void {}
    auto set_stopped() -> void {}
};
template <typename F> receiver(F) -> receiver<F>;

template <typename F> struct error_receiver : F {
    using is_receiver = void;
    auto set_value(auto &&...) -> void {}
    template <typename... Args> auto set_error(Args &&...args) -> void {
        this->F::operator()(std::forward<Args>(args)...);
    }
    auto set_stopped() -> void {}
};
template <typename F> error_receiver(F) -> error_receiver<F>;

template <typename F> struct stopped_receiver : F {
    using is_receiver = void;
    auto set_value(auto &&...) -> void {}
    auto set_error(auto &&...) -> void {}
    auto set_stopped() -> void { this->F::operator()(); }
};
template <typename F> stopped_receiver(F) -> stopped_receiver<F>;

template <typename T> std::optional<async::in_place_stop_source> stop_source{};

template <typename F> struct stoppable_receiver : F {
    using is_receiver = void;
    stoppable_receiver(F &&f) : F{std::forward<F>(f)} {
        stop_source<stoppable_receiver>.emplace();
    }
    ~stoppable_receiver() { stop_source<stoppable_receiver>.reset(); }

    auto set_value(auto &&...) -> void { this->F::operator()(); }
    auto set_error(auto &&...) -> void { this->F::operator()(); }
    auto set_stopped() -> void { this->F::operator()(); }

    struct env {
        [[nodiscard]] friend constexpr auto tag_invoke(async::get_stop_token_t,
                                                       env const &self) {
            return self.stop_token;
        }
        async::in_place_stop_token stop_token;
    };

    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   stoppable_receiver const &)
        -> env {
        return {stop_source<stoppable_receiver>->get_token()};
    }

    auto request_stop() { stop_source<stoppable_receiver>->request_stop(); }
};
template <typename F> stoppable_receiver(F) -> stoppable_receiver<F>;

template <typename F> struct only_stoppable_receiver : stoppable_receiver<F> {
    only_stoppable_receiver(F &&f)
        : stoppable_receiver<F>{std::forward<F>(f)} {}

    auto set_value(auto &&...) -> void {}
    auto set_error(auto &&...) -> void {}
    auto set_stopped() -> void { this->F::operator()(); }
};
template <typename F> only_stoppable_receiver(F) -> only_stoppable_receiver<F>;

class singleshot_scheduler {
    template <typename R> struct op_state {
        auto start() -> void { std::move(receiver).set_value(); }
        [[no_unique_address]] R receiver;
    };

    class env {
        template <typename Tag>
        [[nodiscard]] friend constexpr auto
        tag_invoke(async::get_completion_scheduler_t<Tag>, env) noexcept
            -> singleshot_scheduler {
            return {};
        }
    };

    struct sender {
        using is_sender = void;
        using completion_signatures =
            async::completion_signatures<async::set_value_t()>;

      private:
        [[nodiscard, maybe_unused]] friend constexpr auto
        tag_invoke(async::get_env_t, sender) noexcept -> env {
            return {};
        }

        template <async::receiver_from<sender> R>
        [[nodiscard]] friend constexpr auto tag_invoke(async::connect_t,
                                                       sender &&, R &&r)
            -> op_state<R> {
            return {std::forward<R>(r)};
        }
    };

    [[nodiscard, maybe_unused]] friend constexpr auto
    operator==(singleshot_scheduler, singleshot_scheduler) -> bool = default;

  public:
    [[nodiscard]] constexpr static auto schedule() -> sender { return {}; }
};

struct none {
    [[nodiscard]] friend constexpr auto operator==(none, none)
        -> bool = default;
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

    [[nodiscard]] friend constexpr auto tag_invoke(async::get_env_t,
                                                   custom_sender const &)
        -> custom_env {
        return {};
    }

    template <typename R> struct op_state {
        auto start() -> void { std::move(r).set_value(); }
        [[no_unique_address]] R r;
    };

    template <typename R>
    [[nodiscard]] friend constexpr auto tag_invoke(async::connect_t,
                                                   custom_sender &&, R &&r)
        -> op_state<std::remove_cvref_t<R>> {
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
} // namespace
