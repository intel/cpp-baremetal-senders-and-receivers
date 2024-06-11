#pragma once

#ifdef SIMULATE_FREESTANDING
#define HAS_CONDITION_VARIABLE 0
#else
#define HAS_CONDITION_VARIABLE __has_include(<condition_variable>)
#endif

#include <async/completion_scheduler.hpp>
#include <async/concepts.hpp>
#include <async/env.hpp>
#include <async/stop_token.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>
#include <conc/concurrency.hpp>

#include <stdx/concepts.hpp>
#include <stdx/intrusive_list.hpp>

#include <atomic>
#if HAS_CONDITION_VARIABLE
#include <condition_variable>
#include <mutex>
#endif

namespace async {
namespace _run_loop {
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
template <typename Uniq = decltype([] {})> class run_loop {
    struct op_state_base {
        virtual auto execute() -> void = 0;
        op_state_base *next{};
        op_state_base *prev{};
    };

    // NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
    template <typename Rcvr> struct op_state : op_state_base {
        template <typename R>
        op_state(run_loop *rl, R &&r) : loop{rl}, rcvr{std::forward<R>(r)} {}
        op_state(op_state &&) = delete;

        auto execute() -> void override {
            if (get_stop_token(get_env(rcvr)).stop_requested()) {
                set_stopped(std::move(rcvr));
            } else {
                set_value(std::move(rcvr));
            }
        }

        run_loop *loop{};
        [[no_unique_address]] Rcvr rcvr;

      private:
        template <stdx::same_as_unqualified<op_state> O>
        friend constexpr auto tag_invoke(start_t, O &&o) -> void {
            std::forward<O>(o).loop->push_back(std::addressof(o));
        }
    };

    struct scheduler {
        struct env {
            [[nodiscard]] friend constexpr auto
            tag_invoke(get_completion_scheduler_t<set_value_t>,
                       env e) noexcept -> scheduler {
                return e.loop->get_scheduler();
            }
            run_loop *loop;
        };

        struct sender {
            using is_sender = void;
            using completion_signatures =
                async::completion_signatures<set_value_t(), set_stopped_t()>;

            [[nodiscard]] friend constexpr auto
            tag_invoke(get_env_t, sender s) noexcept -> env {
                return {s.loop};
            }

            template <stdx::same_as_unqualified<sender> S, receiver R>
            [[nodiscard]] friend constexpr auto
            tag_invoke(connect_t, S &&s, R &&r) -> op_state<R> {
                check_connect<S, R>();
                return {s.loop, std::forward<R>(r)};
            }

            run_loop *loop;
        };

        [[nodiscard]] constexpr auto schedule() -> sender { return {loop}; }

        template <typename T>
        [[nodiscard]] friend constexpr auto operator==(scheduler x,
                                                       T const &y) -> bool {
            if constexpr (std::same_as<T, scheduler>) {
                return x.loop == y.loop;
            }
            return false;
        }

        run_loop *loop;
    };

    template <typename Pred> auto wait_until(Pred &&p) {
#if HAS_CONDITION_VARIABLE
        std::unique_lock l{m};
        cv.wait(l, std::forward<Pred>(p));
#else
        while (not p()) {
        }
#endif
    }

    template <typename F> auto notify(F &&f) {
#if HAS_CONDITION_VARIABLE
        std::unique_lock l{m};
        std::forward<F>(f)();
        cv.notify_one();
#else
        std::forward<F>(f)();
#endif
    }

  public:
    run_loop() noexcept = default;
    run_loop(run_loop &&) = delete;

    auto get_scheduler() -> scheduler { return {this}; }

    auto finish() -> void {
        notify([&] {
            conc::call_in_critical_section<Uniq>(
                [&] { state = state_t::finishing; });
        });
    }

    auto run() -> void {
        while (auto op = pop_front()) {
            op->execute();
        }
    }

    auto push_back(op_state_base *task) -> void {
        notify([&] {
            conc::call_in_critical_section<Uniq>(
                [&] { tasks.push_back(task); });
        });
    }

    auto pop_front() -> op_state_base * {
        wait_until([&] {
            return conc::call_in_critical_section<Uniq>([&] {
                return not tasks.empty() or state == state_t::finishing;
            });
        });
        return conc::call_in_critical_section<Uniq>([&]() -> op_state_base * {
            return tasks.empty() ? nullptr : tasks.pop_front();
        });
    }

  private:
    enum struct state_t { starting, running, finishing };

    state_t state{state_t::starting};
    stdx::intrusive_list<op_state_base> tasks{};
#if HAS_CONDITION_VARIABLE
    std::mutex m{};
    std::condition_variable cv{};
#endif
};
} // namespace _run_loop

using _run_loop::run_loop;
} // namespace async

#undef HAS_CONDITION_VARIABLE
