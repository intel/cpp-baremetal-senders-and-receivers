#pragma once

#include <async/forwarding_query.hpp>
#include <conc/concurrency.hpp>

#include <stdx/atomic.hpp>
#include <stdx/ct_string.hpp>
#include <stdx/intrusive_list.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
template <class T, class CB>
using stop_callback_for_t = typename T::template callback_type<CB>;

template <class T>
concept stoppable_token =
    std::copyable<T> and std::equality_comparable<T> and requires(T const t) {
        { T(t) } noexcept;
        { t.stop_requested() } noexcept -> std::same_as<bool>;
        { t.stop_possible() } noexcept -> std::same_as<bool>;
        typename stop_callback_for_t<T, decltype([] {})>;
    };

template <class T, class CB, class Initializer = CB>
concept stoppable_token_for =
    stoppable_token<T> and std::invocable<CB> and
    std::constructible_from<CB, Initializer> and
    requires { typename stop_callback_for_t<T, CB>; } and
    std::constructible_from<stop_callback_for_t<T, CB>, T const &, Initializer>;

template <class T>
concept unstoppable_token = stoppable_token<T> and requires {
    {
        std::bool_constant<T::stop_possible()>{}
    } -> std::same_as<std::false_type>;
};

struct inplace_stop_source;
template <typename F> struct inplace_stop_callback;

template <typename Source> struct stop_token {
    template <class T> using callback_type = inplace_stop_callback<T>;

    [[nodiscard]] auto stop_requested() const noexcept -> bool {
        return source != nullptr and source->stop_requested();
    }
    [[nodiscard]] constexpr auto stop_possible() const noexcept -> bool {
        return source != nullptr;
    }

    Source const *source{};

  private:
    [[nodiscard]] friend constexpr auto
    operator==(stop_token, stop_token) noexcept -> bool = default;
};

// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct stop_callback_base {
    virtual auto run() -> void = 0;

    constexpr stop_callback_base() = default;
    constexpr stop_callback_base(stop_callback_base &&) = delete;
    virtual ~stop_callback_base() = default;

    bool pending{};
    stop_callback_base *prev{};
    stop_callback_base *next{};
};

struct inplace_stop_source {
    struct mutex;

    constexpr inplace_stop_source() = default;
    constexpr explicit(true) inplace_stop_source(bool b) : requested{b} {}

    [[nodiscard]] auto stop_requested() const noexcept -> bool {
        return requested.load();
    }
    [[nodiscard]] constexpr static auto stop_possible() noexcept -> bool {
        return true;
    }

    [[nodiscard]] constexpr auto
    get_token() const noexcept -> stop_token<inplace_stop_source> {
        return {this};
    }

    auto request_stop() -> bool {
        if (not requested.exchange(true)) {
            auto get_next_cb = [&] {
                return conc::call_in_critical_section<mutex>(
                    [&]() -> stop_callback_base * {
                        if (callbacks.empty()) {
                            return nullptr;
                        }
                        auto cb = callbacks.pop_front();
                        cb->pending = false;
                        return cb;
                    });
            };
            for (auto cb = get_next_cb(); cb != nullptr; cb = get_next_cb()) {
                cb->run();
            }
            return true;
        }
        return false;
    }

    auto register_callback(stop_callback_base *cb) -> bool {
        return conc::call_in_critical_section<mutex>([&] {
            if (not requested) {
                callbacks.push_back(cb);
                cb->pending = true;
                return true;
            }
            return false;
        });
    }
    auto unregister_callback(stop_callback_base *cb) -> void {
        conc::call_in_critical_section<mutex>([&] {
            if (std::exchange(cb->pending, false)) {
                callbacks.remove(cb);
            }
        });
    }

  private:
    stdx::atomic<bool> requested;
    stdx::intrusive_list<stop_callback_base> callbacks{};
};

using inplace_stop_token = stop_token<inplace_stop_source>;

struct never_stop_token {
    template <class T> using callback_type = inplace_stop_callback<T>;

    [[nodiscard]] constexpr static auto stop_requested() noexcept -> bool {
        return false;
    }
    [[nodiscard]] constexpr static auto stop_possible() noexcept -> bool {
        return false;
    }

  private:
    [[nodiscard]] friend constexpr auto
    operator==(never_stop_token, never_stop_token) noexcept -> bool = default;
};

struct never_stop_source {
    [[nodiscard]] constexpr static auto stop_requested() noexcept -> bool {
        return false;
    }
    [[nodiscard]] constexpr static auto stop_possible() noexcept -> bool {
        return false;
    }

    [[nodiscard]] constexpr static auto
    get_token() noexcept -> never_stop_token {
        return {};
    }

    constexpr static auto request_stop() -> bool { return false; }
    constexpr static auto register_callback(stop_callback_base *) -> bool {
        return false;
    }
    constexpr static auto unregister_callback(stop_callback_base *) -> void {}
};

// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
template <typename F> struct inplace_stop_callback : stop_callback_base {
    inplace_stop_callback(never_stop_token, F const &f) : callback(f) {}
    inplace_stop_callback(never_stop_token, F &&f) : callback(std::move(f)) {}

    inplace_stop_callback(inplace_stop_token t, F const &f)
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-const-cast)
        : source{const_cast<inplace_stop_source *>(t.source)}, callback{f} {
        if (not source->register_callback(this)) {
            callback();
        }
    }
    inplace_stop_callback(inplace_stop_token t, F &&f)
        // NOLINTNEXTLINE(cppcoreguidelines-pro-type-const-cast)
        : source{const_cast<inplace_stop_source *>(t.source)},
          callback{std::move(f)} {
        if (not source->register_callback(this)) {
            callback();
        }
    }
    ~inplace_stop_callback() override {
        if (source != nullptr) {
            source->unregister_callback(this);
        }
    }

    auto run() -> void final { callback(); }

    inplace_stop_source *source{};
    F callback;
};

struct get_stop_token_t : forwarding_query_t {
    constexpr static auto name = stdx::ct_string{"get_stop_token"};

    template <typename T>
        requires true // more constrained
    constexpr auto operator()(T &&t) const noexcept(
        noexcept(std::forward<T>(t).query(std::declval<get_stop_token_t>())))
        -> decltype(std::forward<T>(t).query(*this)) {
        return std::forward<T>(t).query(*this);
    }

    constexpr auto operator()(auto &&) const -> never_stop_token { return {}; }
};

template <typename E>
auto get_stop_token(E &&e) -> decltype(get_stop_token_t{}(std::forward<E>(e))) {
    return get_stop_token_t{}(std::forward<E>(e));
}

template <typename T>
using stop_token_of_t =
    std::remove_cvref_t<decltype(get_stop_token(std::declval<T>()))>;
} // namespace async
