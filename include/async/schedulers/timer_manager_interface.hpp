#pragma once

#include <async/schedulers/task.hpp>

#include <stdx/intrusive_list.hpp>
#include <stdx/type_traits.hpp>

#include <concepts>
#include <functional>

namespace async {
namespace timer_mgr {
template <typename D> struct time_point_for {
    using type = D;
};
template <typename D> using time_point_for_t = typename time_point_for<D>::type;
} // namespace timer_mgr

template <typename T>
concept timeable_task =
    stdx::double_linkable<T> and std::strict_weak_order<std::less<>, T, T> and
    requires(T *t) {
        { t->run() } -> std::same_as<void>;
        { t->pending } -> std::same_as<bool &>;
        t->expiration_time;
    };

namespace archetypes {
template <typename D> struct expiration_provider {
    using time_point_t = timer_mgr::time_point_for_t<D>;

    template <typename>
    [[nodiscard]] auto compute_expiration() const -> time_point_t {
        return {};
    }
};
} // namespace archetypes

template <typename T>
concept timer_manager =
    timeable_task<typename T::task_t> and
    requires(T &t, typename T::task_t &task, typename T::duration_t d) {
        { t.run_after(task, d) } -> std::convertible_to<bool>;
        {
            t.run_at(task,
                     archetypes::expiration_provider<typename T::duration_t>{})
        } -> std::convertible_to<bool>;
        { t.service_task() } -> std::same_as<void>;
        { t.is_idle() } -> std::convertible_to<bool>;
        typename T::time_point_t;
    };

namespace detail {
// NOLINTNEXTLINE(cppcoreguidelines-virtual-class-destructor)
template <typename T> struct default_timer_task : task_base {
    T expiration_time{};

  private:
    [[nodiscard]] friend constexpr auto
    operator<(default_timer_task const &lhs, default_timer_task const &rhs) {
        return lhs.expiration_time < rhs.expiration_time;
    }
};
} // namespace detail

template <typename T>
using timer_task = double_linked_task<detail::default_timer_task<T>>;

namespace detail {
struct undefined_timer_manager {
    using time_point_t = int;
    using task_t = timer_task<time_point_t>;
    using duration_t = decltype(time_point_t{} - time_point_t{});

    template <typename... Args> static auto run_after(Args &&...) -> bool {
        static_assert(stdx::always_false_v<Args...>,
                      "Inject a timer manager by specializing "
                      "async::injected_timer_manager.");
        return false;
    }

    template <typename... Args> static auto run_at(Args &&...) -> bool {
        static_assert(stdx::always_false_v<Args...>,
                      "Inject a timer manager by specializing "
                      "async::injected_timer_manager.");
        return false;
    }

    template <typename... Args> static auto service_task(Args &&...) -> void {
        static_assert(stdx::always_false_v<Args...>,
                      "Inject a timer manager by specializing "
                      "async::injected_timer_manager.");
    }

    template <typename... Args> static auto is_idle(Args &&...) -> bool {
        static_assert(
            stdx::always_false_v<Args...>,
            "Inject a timer by specializing async::injected_timer_manager.");
        return true;
    }
};
static_assert(timer_manager<undefined_timer_manager>);
} // namespace detail

template <typename...>
inline auto injected_timer_manager = detail::undefined_timer_manager{};

namespace timer_mgr {
struct default_domain;

template <typename Domain, typename... DummyArgs>
    requires(sizeof...(DummyArgs) == 0)
[[nodiscard]] constexpr auto get_injected_manager() -> auto & {
    if constexpr (std::is_same_v<Domain, default_domain>) {
        return injected_timer_manager<DummyArgs...>;
    } else {
        return injected_timer_manager<Domain, DummyArgs...>;
    }
}

template <typename Domain = default_domain, typename... DummyArgs,
          typename... Args>
    requires(sizeof...(DummyArgs) == 0)
auto run_after(Args &&...args) -> bool {
    return get_injected_manager<Domain, DummyArgs...>().run_after(
        std::forward<Args>(args)...);
}

template <typename Domain = default_domain, typename... DummyArgs,
          typename... Args>
    requires(sizeof...(DummyArgs) == 0)
auto run_at(Args &&...args) -> bool {
    return get_injected_manager<Domain, DummyArgs...>().run_at(
        std::forward<Args>(args)...);
}

template <typename Domain = default_domain, typename... DummyArgs,
          typename... Args>
    requires(sizeof...(DummyArgs) == 0)
auto cancel(Args &&...args) -> bool {
    return get_injected_manager<Domain, DummyArgs...>().cancel(
        std::forward<Args>(args)...);
}

template <typename D, typename Domain = default_domain, typename... DummyArgs>
    requires(sizeof...(DummyArgs) == 0)
constexpr auto valid_duration() -> bool {
    return std::convertible_to<
        D, typename std::remove_cvref_t<
               decltype(get_injected_manager<Domain, DummyArgs...>())>::
               duration_t>;
}

template <typename Domain = default_domain, typename... DummyArgs>
    requires(sizeof...(DummyArgs) == 0)
auto service_task() -> void {
    return get_injected_manager<Domain, DummyArgs...>().service_task();
}

template <typename Domain = default_domain, typename... DummyArgs>
    requires(sizeof...(DummyArgs) == 0)
auto is_idle() -> bool {
    return get_injected_manager<Domain, DummyArgs...>().is_idle();
}
} // namespace timer_mgr
} // namespace async
