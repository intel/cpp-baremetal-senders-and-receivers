#pragma once

#include <async/concepts.hpp>
#include <async/tags.hpp>
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

  private:
    template <stdx::same_as_unqualified<op_state> O>
    friend auto tag_invoke(start_t, O &&o) -> void {
        using split_returns =
            boost::mp11::mp_partition<boost::mp11::mp_list<Fs...>,
                                      has_void_result>;

        [&]<typename... Ts>(boost::mp11::mp_list<Ts...>) {
            (static_cast<stdx::forward_like_t<O, Ts>>(o)(), ...);
        }(boost::mp11::mp_front<split_returns>{});

        [&]<typename... Ts>(boost::mp11::mp_list<Ts...>) {
            Tag{}(std::forward<O>(o).receiver,
                  static_cast<stdx::forward_like_t<O, Ts>>(o)()...);
        }(boost::mp11::mp_back<split_returns>{});
    }
};

template <typename Tag, std::invocable... Fs> class sender : public Fs... {
    template <receiver_from<sender> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender &&self,
                                                   R &&r)
        -> op_state<Tag, std::remove_cvref_t<R>, Fs...> {
        return {{static_cast<Fs>(std::move(self))}..., std::forward<R>(r)};
    }

    template <stdx::same_as_unqualified<sender> Self, receiver_from<sender> R>
        requires(... and std::copy_constructible<Fs>)
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r)
        -> op_state<Tag, std::remove_cvref_t<R>, Fs...> {
        return {{static_cast<Fs>(std::forward<Self>(self))}...,
                std::forward<R>(r)};
    }

    template <typename... Ts>
    using make_signature = async::completion_signatures<Tag(Ts...)>;

  public:
    using is_sender = void;
    using completion_signatures = boost::mp11::mp_apply<
        make_signature,
        boost::mp11::mp_copy_if_q<
            async::completion_signatures<std::invoke_result_t<Fs>...>,
            boost::mp11::mp_not_fn<std::is_void>>>;
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
