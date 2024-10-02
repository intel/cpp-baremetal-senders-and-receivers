#pragma once

#include <async/compose.hpp>
#include <async/concepts.hpp>
#include <async/just.hpp>
#include <async/schedulers/time_scheduler.hpp>
#include <async/schedulers/timer_manager_interface.hpp>
#include <async/start_on.hpp>
#include <async/when_any.hpp>

#include <stdx/concepts.hpp>
#include <stdx/tuple.hpp>

namespace async {
namespace _timeout_after {
template <typename Domain, channel_tag Tag, typename Duration, typename... Vs>
struct pipeable {
    Duration d;
    [[no_unique_address]] stdx::tuple<Vs...> values;

  private:
    template <sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> sender auto {
        return std::forward<S>(s) |
               stop_when(start_on(
                   time_scheduler_factory<Domain>(std::forward<Self>(self).d),
                   std::forward<Self>(self).values.apply(
                       []<typename... Ts>(Ts &&...ts) {
                           return _just::sender<"timeout", Tag, Vs...>{
                               std::forward<Ts>(ts)...};
                       })));
    }
};
} // namespace _timeout_after

template <typename Domain = timer_mgr::default_domain,
          channel_tag Tag = set_error_t, typename Duration, typename... Vs>
[[nodiscard]] constexpr auto timeout_after(Duration &&d, Vs &&...vs) {
    static_assert(not std::is_same_v<Tag, set_stopped_t> or sizeof...(Vs) == 0,
                  "set_stopped cannot send values");
    static_assert(not std::is_same_v<Tag, set_error_t> or sizeof...(Vs) == 1,
                  "set_error should send one value only");
    return _compose::adaptor{
        _timeout_after::pipeable<Domain, Tag, std::remove_cvref_t<Duration>,
                                 std::remove_cvref_t<Vs>...>{
            std::forward<Duration>(d), {std::forward<Vs>(vs)...}}};
}

template <typename Domain = timer_mgr::default_domain,
          channel_tag Tag = set_error_t, sender Sndr, typename Duration,
          typename... Vs>
[[nodiscard]] constexpr auto timeout_after(Sndr &&s, Duration &&d,
                                           Vs &&...vs) -> sender auto {
    return std::forward<Sndr>(s) |
           timeout_after<Domain, Tag>(std::forward<Duration>(d),
                                      std::forward<Vs>(vs)...);
}
} // namespace async
