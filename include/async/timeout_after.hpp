#pragma once

#include <async/compose.hpp>
#include <async/just.hpp>
#include <async/schedulers/time_scheduler.hpp>
#include <async/schedulers/timer_manager_interface.hpp>
#include <async/start_on.hpp>
#include <async/when_any.hpp>

#include <stdx/concepts.hpp>
#include <stdx/tuple.hpp>

namespace async {
namespace _timeout_after {
template <typename Domain, typename Duration, typename Error> struct pipeable {
    Duration d;
    [[no_unique_address]] Error e;

  private:
    template <sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> sender auto {
        return std::forward<S>(s) |
               stop_when(start_on(
                   time_scheduler_factory<Domain>(std::forward<Self>(self).d),
                   just_error(std::forward<Self>(self).e)));
    }
};
} // namespace _timeout_after

template <typename Domain = timer_mgr::default_domain, typename Duration,
          typename Error>
[[nodiscard]] constexpr auto timeout_after(Duration &&d, Error &&e) {
    return _compose::adaptor{stdx::tuple{
        _timeout_after::pipeable<Domain, std::remove_cvref_t<Duration>,
                                 std::remove_cvref_t<Error>>{
            std::forward<Duration>(d), std::forward<Error>(e)}}};
}

template <typename Domain = timer_mgr::default_domain, sender Sndr,
          typename Duration, typename Error>
[[nodiscard]] constexpr auto timeout_after(Sndr &&s, Duration &&d,
                                           Error &&e) -> sender auto {
    return std::forward<Sndr>(s) |
           timeout_after<Domain>(std::forward<Duration>(d),
                                 std::forward<Error>(e));
}
} // namespace async
