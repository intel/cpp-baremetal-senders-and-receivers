#pragma once

#ifdef ASYNC_FREESTANDING
#define HAS_CONDITION_VARIABLE 0
#else
#define HAS_CONDITION_VARIABLE __has_include(<condition_variable>)
#endif

#include <async/completion_tags.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/debug.hpp>
#include <async/env.hpp>
#include <async/get_completion_scheduler.hpp>
#include <async/stop_token.hpp>
#include <async/type_traits.hpp>

#include <stdx/atomic.hpp>
#include <stdx/concepts.hpp>
#include <stdx/intrusive_list.hpp>

#include <conc/concurrency.hpp>

#include <cstdint>
#if HAS_CONDITION_VARIABLE
#include <condition_variable>
#include <mutex>
#endif

namespace async {
namespace detail {
#if HAS_CONDITION_VARIABLE
template <typename Uniq, std::size_t N> struct synchronizer {
    template <typename... Fs> auto notify(Fs &&...fs) {
        static_assert(sizeof...(Fs) <= N);
        std::unique_lock l{m};
        (std::forward<Fs>(fs)(), ...);
        if constexpr (sizeof...(Fs) == 0) {
            done = true;
        }
        cv.notify_one();
    }

    template <typename... Ps> auto wait(Ps &&...ps) {
        static_assert(sizeof...(Ps) <= N);
        std::unique_lock l{m};
        cv.wait(l, [&] { return (ps() or ... or done); });
    }

    std::mutex m;
    std::condition_variable cv;
    bool done{};
};
#else
template <typename Uniq, std::size_t N> struct synchronizer {
    template <typename... Fs> auto notify(Fs &&...fs) {
        static_assert(sizeof...(Fs) <= N);
        conc::call_in_critical_section<Uniq>([&] {
            (std::forward<Fs>(fs)(), ...);
            if constexpr (sizeof...(Fs) == 0) {
                done = true;
            }
        });
    }

    template <typename... Ps> auto wait(Ps &&...ps) {
        static_assert(sizeof...(Ps) <= N);
        conc::call_in_critical_section<Uniq>(
            [] {}, [&] { return (ps() or ... or done); });
    }

    bool done{};
};

template <typename Uniq> struct synchronizer<Uniq, 0> {
    auto notify() { done = true; }

    auto wait() {
        while (!done) {
        }
    }

    stdx::atomic<bool> done{};
};
#endif
} // namespace detail

namespace _run_loop {
namespace detail {
template <typename Name> constexpr auto get_name() {
    if constexpr (requires {
                      []<auto N>(stdx::ct_string<N>) {}(Name::value);
                  }) {
        return Name::value;
    } else {
        return stdx::ct_string{"runloop_scheduler"};
    }
}
} // namespace detail

// NOLINTNEXTLINE(cppcoreguidelines-virtual-class-destructor)
struct op_state_base {
    virtual auto execute() -> void = 0;
    op_state_base *next{};
    op_state_base *prev{};
};

template <typename> class run_loop;

// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
template <typename Uniq, typename Rcvr> struct op_state : op_state_base {
    template <typename R>
    op_state(run_loop<Uniq> *rl, R &&r) : loop{rl}, rcvr{std::forward<R>(r)} {}
    op_state(op_state &&) = delete;

    auto execute() -> void override {
        if (get_stop_token(get_env(rcvr)).stop_requested()) {
            debug_signal<"set_stopped", debug::erased_context_for<op_state>>(
                get_env(rcvr));
            set_stopped(std::move(rcvr));
        } else {
            debug_signal<"set_value", debug::erased_context_for<op_state>>(
                get_env(rcvr));
            set_value(std::move(rcvr));
        }
    }

    constexpr auto start() & -> void {
        debug_signal<"start", debug::erased_context_for<op_state>>(
            get_env(rcvr));
        loop->push_back(this);
    }

    run_loop<Uniq> *loop{};
    [[no_unique_address]] Rcvr rcvr;
};

// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
template <typename Uniq = decltype([] {})> class run_loop {
    struct scheduler {
        struct sender {
            using is_sender = void;
            using completion_signatures =
                async::completion_signatures<set_value_t(), set_stopped_t()>;

            [[nodiscard]] constexpr auto query(get_env_t) const noexcept {
                return make_template_prop<get_completion_scheduler_t,
                                          set_value_t, set_stopped_t>(
                    loop->get_scheduler());
            }

            template <receiver R>
            [[nodiscard]] constexpr auto connect(R &&r) const
                -> op_state<Uniq, std::remove_cvref_t<R>> {
                check_connect<sender, R>();
                return {loop, std::forward<R>(r)};
            }

            run_loop *loop;
        };

        [[nodiscard]] constexpr auto schedule() -> sender { return {loop}; }

        template <typename T>
        [[nodiscard]] friend constexpr auto operator==(scheduler x, T const &y)
            -> bool {
            if constexpr (std::same_as<T, scheduler>) {
                return x.loop == y.loop;
            }
            return false;
        }

        run_loop *loop;
    };

  public:
    run_loop() noexcept = default;
    run_loop(run_loop &&) = delete;

    auto get_scheduler() -> scheduler { return {this}; }

    auto finish() -> void { sync.notify(); }

    auto run() -> void {
        while (auto op = pop_front()) {
            op->execute();
        }
    }

    auto push_back(op_state_base *task) -> void {
        sync.notify([&] { tasks.push_back(task); });
    }

    auto pop_front() -> op_state_base * {
        sync.wait([&] { return not tasks.empty(); });
        return conc::call_in_critical_section<Uniq>([&]() -> op_state_base * {
            return tasks.empty() ? nullptr : tasks.pop_front();
        });
    }

  private:
    async::detail::synchronizer<Uniq, 1> sync{};
    stdx::intrusive_list<op_state_base> tasks{};
};
} // namespace _run_loop

using _run_loop::run_loop;

struct runloop_scheduler_sender_t;

template <typename Uniq, typename R>
struct debug::context_for<_run_loop::op_state<Uniq, R>> {
    using tag = runloop_scheduler_sender_t;
    constexpr static auto name = _run_loop::detail::get_name<Uniq>();
    using children = stdx::type_list<>;
    using type = _run_loop::op_state<Uniq, R>;
};
} // namespace async

#undef HAS_CONDITION_VARIABLE
