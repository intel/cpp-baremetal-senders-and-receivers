#pragma once

#include <async/schedulers/timer_manager_interface.hpp>
#include <conc/concurrency.hpp>

#include <stdx/function_traits.hpp>
#include <stdx/intrusive_list.hpp>
#include <stdx/tuple.hpp>

#include <algorithm>
#include <atomic>
#include <concepts>
#include <iterator>
#include <memory>
#include <type_traits>
#include <utility>

namespace async {
namespace detail {
template <typename T>
concept timer_hal =
    timeable_task<typename T::task_t> and
    requires(typename T::time_point_t tp, typename T::task_t &task) {
        { T::now() } -> std::same_as<typename T::time_point_t>;
        { T::enable() } -> std::same_as<void>;
        { T::disable() } -> std::same_as<void>;
        { T::set_event_time(tp) } -> std::same_as<void>;
        { task.expiration_time } -> std::same_as<typename T::time_point_t &>;
    };
} // namespace detail

namespace archetypes {
struct timer_hal {
    using time_point_t = int;
    using task_t = timer_task<time_point_t>;

    static auto now() -> time_point_t { return {}; }
    static auto enable() -> void {}
    static auto disable() -> void {}
    static auto set_event_time(time_point_t) -> void {}
};
} // namespace archetypes
static_assert(detail::timer_hal<archetypes::timer_hal>);

template <detail::timer_hal H> struct generic_timer_manager {
    using time_point_t = typename H::time_point_t;
    using duration_t =
        decltype(std::declval<time_point_t>() - std::declval<time_point_t>());
    using task_t = typename H::task_t;

  private:
    struct mutex;
    stdx::intrusive_list<task_t> task_queue{};
    std::atomic<int> task_count{};

    auto schedule(task_t *t) -> void {
        if (std::empty(task_queue)) {
            task_queue.push_back(t);
            H::set_event_time(t->expiration_time);
            H::enable();
        } else {
            auto pos = std::find_if(
                std::begin(task_queue), std::end(task_queue),
                [&](auto const &task) {
                    return task.expiration_time > t->expiration_time;
                });
            if (pos == std::begin(task_queue)) {
                H::set_event_time(t->expiration_time);
            }
            task_queue.insert(pos, t);
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
                t.expiration_time = H::now() + static_cast<duration_t>(d);
                schedule(std::addressof(t));
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
static_assert(timer_manager<generic_timer_manager<archetypes::timer_hal>>);
} // namespace async
