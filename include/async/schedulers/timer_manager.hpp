#pragma once

#include <async/schedulers/timer_manager_interface.hpp>

#include <stdx/atomic.hpp>
#include <stdx/function_traits.hpp>
#include <stdx/intrusive_list.hpp>
#include <stdx/tuple.hpp>

#include <conc/concurrency.hpp>

#include <algorithm>
#include <concepts>
#include <iterator>
#include <memory>
#include <type_traits>
#include <utility>

namespace async {
namespace detail {
template <typename T>
concept basic_timer_hal =
    timeable_task<typename T::task_t> and
    requires(typename T::time_point_t tp, typename T::task_t &task) {
        { T::now() } -> std::same_as<typename T::time_point_t>;
        { T::disable() } -> std::same_as<void>;
        { T::set_event_time(tp) } -> std::same_as<void>;
        { task.expiration_time } -> std::same_as<typename T::time_point_t &>;
    };

template <typename T>
concept separate_enable_timer_hal = basic_timer_hal<T> and requires {
    { T::enable() } -> std::same_as<void>;
};

template <typename T>
concept fused_enable_timer_hal =
    basic_timer_hal<T> and requires(typename T::time_point_t tp) {
        { T::enable(tp - tp) } -> std::same_as<typename T::time_point_t>;
    };

template <typename T>
concept timer_hal = separate_enable_timer_hal<T> or fused_enable_timer_hal<T>;
} // namespace detail

template <detail::timer_hal H> struct generic_timer_manager {
    using time_point_t = typename H::time_point_t;
    using duration_t =
        decltype(std::declval<time_point_t>() - std::declval<time_point_t>());
    using task_t = typename H::task_t;

  private:
    struct mutex;
    stdx::intrusive_list<task_t> task_queue{};
    stdx::atomic<int> task_count;

    auto enqueue(task_t *t) -> void {
        auto pos = std::find_if(std::begin(task_queue), std::end(task_queue),
                                [&](auto const &task) { return *t < task; });
        if (pos == std::begin(task_queue)) {
            H::set_event_time(t->expiration_time);
        }
        task_queue.insert(pos, t);
    }

    auto schedule_at(task_t *t, time_point_t tp) -> void {
        t->expiration_time = tp;
        if (std::empty(task_queue)) {
            task_queue.push_back(t);
            H::enable();
            H::set_event_time(t->expiration_time);
        } else {
            enqueue(t);
        }
    }

    auto schedule_after(task_t *t, duration_t d) -> void {
        if (std::empty(task_queue)) {
            task_queue.push_back(t);
            if constexpr (detail::fused_enable_timer_hal<H>) {
                t->expiration_time = H::enable(d);
            } else {
                H::enable();
                t->expiration_time = H::now() + d;
                H::set_event_time(t->expiration_time);
            }
        } else {
            t->expiration_time = H::now() + d;
            enqueue(t);
        }
    }

    auto compute_next_event() -> void {
        if (std::empty(task_queue)) {
            H::disable();
        } else {
            H::set_event_time(std::begin(task_queue)->expiration_time);
        }
    }

  public:
    constexpr static auto create_task = async::create_task<task_t>;

    template <std::derived_from<task_t> T, std::convertible_to<duration_t> D>
    auto run_after(T &t, D d) -> bool {
        return conc::call_in_critical_section<mutex>([&]() -> bool {
            if (auto const added = not std::exchange(t.pending, true); added) {
                ++task_count;
                schedule_after(std::addressof(t), d);
                return true;
            }
            return false;
        });
    }

    template <std::derived_from<task_t> T, std::convertible_to<time_point_t> TP>
    auto run_at(T &t, TP tp) -> bool {
        return conc::call_in_critical_section<mutex>([&]() -> bool {
            if (auto const added = not std::exchange(t.pending, true); added) {
                ++task_count;
                schedule_at(std::addressof(t), tp);
                return true;
            }
            return false;
        });
    }

    template <typename T, typename D> auto run_after(T const &, D) -> bool {
        static_assert(stdx::always_false_v<D>,
                      "Invalid duration type: did you forget to specialize "
                      "async::timer_mgr::time_point_for?");
        return false;
    }

    auto cancel(task_t &t) -> bool {
        return conc::call_in_critical_section<mutex>([&]() -> bool {
            if (t.pending) {
                task_queue.remove(std::addressof(t));
                t.pending = false;
                --task_count;
                compute_next_event();
                return true;
            }
            return false;
        });
    }

    auto service_task() -> void {
        if (auto t = conc::call_in_critical_section<mutex>([&]() -> task_t * {
                if (std::empty(task_queue)) {
                    return nullptr;
                }
                auto n = task_queue.pop_front();
                n->pending = false;
                compute_next_event();
                return n;
            });
            t != nullptr) {
            t->run();
            --task_count;
        }
    }

    [[nodiscard]] auto is_idle() const -> bool { return task_count == 0; }
};
} // namespace async
