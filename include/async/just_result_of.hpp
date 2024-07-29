#pragma once

#include <async/allocator.hpp>
#include <async/completes_synchronously.hpp>
#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/env.hpp>
#include <async/stack_allocator.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/tuple.hpp>
#include <stdx/utility.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/list.hpp>

#include <concepts>
#include <type_traits>
#include <utility>

namespace async {
namespace _just_result_of {
template <typename Tag, typename R, typename... Fs> struct op_state : Fs... {
    template <typename F>
    using has_void_result = std::is_void<std::invoke_result_t<F>>;

    [[no_unique_address]] R receiver;

    auto start() & -> void {
        using split_returns =
            boost::mp11::mp_partition<boost::mp11::mp_list<Fs...>,
                                      has_void_result>;

        [&]<typename... Ts>(boost::mp11::mp_list<Ts...>) {
            (static_cast<Ts &>(*this)(), ...);
        }(boost::mp11::mp_front<split_returns>{});

        [&]<typename... Ts>(boost::mp11::mp_list<Ts...>) {
            Tag{}(std::move(receiver), static_cast<Ts &>(*this)()...);
        }(boost::mp11::mp_back<split_returns>{});
    }
};

template <typename Tag, std::invocable... Fs> struct sender : Fs... {
    template <receiver R>
    [[nodiscard]] constexpr auto
    connect(R &&r) && -> op_state<Tag, std::remove_cvref_t<R>, Fs...> {
        check_connect<sender &&, R>();
        return {{static_cast<Fs &&>(std::move(*this))}..., std::forward<R>(r)};
    }

    template <receiver R>
        requires(... and std::copy_constructible<Fs>)
    [[nodiscard]] constexpr auto
    connect(R &&r) const & -> op_state<Tag, std::remove_cvref_t<R>, Fs...> {
        check_connect<sender const &, R>();
        return {{static_cast<Fs const &>(*this)}..., std::forward<R>(r)};
    }

    template <typename... Ts>
    using make_signature = async::completion_signatures<Tag(Ts...)>;

    using is_sender = void;
    using completion_signatures = boost::mp11::mp_apply<
        make_signature,
        boost::mp11::mp_copy_if_q<
            async::completion_signatures<std::invoke_result_t<Fs>...>,
            boost::mp11::mp_not_fn<std::is_void>>>;

    [[nodiscard]] constexpr auto query(get_env_t) const noexcept {
        return env{prop{get_allocator_t{}, stack_allocator{}},
                   prop{completes_synchronously_t{}, true}};
    }
};
} // namespace _just_result_of

template <std::invocable... Fs>
[[nodiscard]] constexpr auto just_result_of(Fs &&...fs) -> sender auto {
    return _just_result_of::sender<set_value_t, std::remove_cvref_t<Fs>...>{
        std::forward<Fs>(fs)...};
}

template <std::invocable... Fs>
[[nodiscard]] constexpr auto just_error_result_of(Fs &&...fs) -> sender auto {
    return _just_result_of::sender<set_error_t, std::remove_cvref_t<Fs>...>{
        std::forward<Fs>(fs)...};
}
} // namespace async
