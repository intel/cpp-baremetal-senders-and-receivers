#pragma once

#include <async/concepts.hpp>
#include <async/tags.hpp>
#include <async/type_traits.hpp>

#include <stdx/concepts.hpp>
#include <stdx/functional.hpp>
#include <stdx/tuple.hpp>
#include <stdx/utility.hpp>

#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/list.hpp>

#include <concepts>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>

namespace async {
namespace _transfer {
template <typename Ops> struct first_receiver {
    using is_receiver = void;

    Ops *ops;

  private:
    template <channel_tag Tag, typename... Args>
    friend auto tag_invoke(Tag, first_receiver const &r, Args &&...args)
        -> void {
        r.ops->template complete_first<Tag>(std::forward<Args>(args)...);
    }
};

template <typename Ops> struct second_receiver {
    using is_receiver = void;

    Ops *ops;

  private:
    template <channel_tag Tag, typename... Args>
    friend auto tag_invoke(Tag, second_receiver const &r, Args &&...args)
        -> void {
        Tag{}(r.ops->rcvr, std::forward<Args>(args)...);
    }

    friend auto tag_invoke(set_value_t, second_receiver const &r) -> void {
        r.ops->complete_second();
    }
};

template <typename Sched, typename Sndr, typename Rcvr>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct op_state {
    using sched_sender = decltype(std::declval<Sched>().schedule());
    using first_rcvr = first_receiver<op_state>;
    using second_rcvr = second_receiver<op_state>;

    template <typename Sch, typename S, typename R>
    constexpr op_state(Sch &&sch, S &&s, R &&r)
        : sched{std::forward<Sch>(sch)}, rcvr{std::forward<R>(r)},
          state{std::in_place_index<0>, stdx::with_result_of{[&] {
                    return connect(std::forward<S>(s), first_rcvr{this});
                }}} {}
    constexpr op_state(op_state &&) = delete;

    using values_t =
        value_types_of_t<Sndr, env_of_t<Rcvr>, value_holder, std::variant>;
    using errors_t =
        error_types_of_t<Sndr, env_of_t<Rcvr>, error_holder, std::variant>;
    using stoppeds_t =
        stopped_types_of_t<Sndr, env_of_t<Rcvr>, stopped_holder, std::variant>;
    using completions_t = boost::mp11::mp_unique<
        boost::mp11::mp_append<values_t, errors_t, stoppeds_t>>;

    template <typename Tag, typename... Args> struct matching_completion {
        template <typename C>
        using fn = boost::mp11::mp_and<std::is_same<typename C::tag_t, Tag>,
                                       std::is_constructible<C, Args &&...>>;
    };

    template <channel_tag Tag, typename... Args>
    auto complete_first(Args &&...args) -> void {
        using index =
            boost::mp11::mp_find_if_q<completions_t,
                                      matching_completion<Tag, Args...>>;
        static_assert(index::value <
                      boost::mp11::mp_size<completions_t>::value);
        values.template emplace<index::value + 1>(std::forward<Args>(args)...);

        auto &op = state.template emplace<1>(stdx::with_result_of{
            [&] { return connect(sched.schedule(), second_rcvr{this}); }});
        start(std::move(op));
    }

    auto complete_second() -> void {
        std::visit(
            [&]<typename T>(T &&t) -> void {
                if constexpr (not std::is_same_v<std::remove_cvref_t<T>,
                                                 std::monostate>) {
                    std::forward<T>(t)(rcvr);
                }
            },
            std::move(values));
    }

    [[no_unique_address]] Sched sched;
    [[no_unique_address]] Rcvr rcvr;

    boost::mp11::mp_push_front<completions_t, std::monostate> values{};

    using first_ops = connect_result_t<Sndr, first_rcvr>;
    using second_ops = connect_result_t<sched_sender, second_rcvr>;
    std::variant<first_ops, second_ops> state;

  private:
    template <stdx::same_as_unqualified<op_state> O>
    friend constexpr auto tag_invoke(start_t, O &&o) -> void {
        start(std::get<0>(std::forward<O>(o).state));
    }
};

template <typename Sched, typename S> class sender {
    template <receiver_from<sender> R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, sender &&self,
                                                   R &&r)
        -> op_state<Sched, S, std::remove_cvref_t<R>> {
        return {std::move(self).sched, std::move(self).s, std::forward<R>(r)};
    }

    template <stdx::same_as_unqualified<sender> Self, receiver_from<sender> R>
        requires multishot_sender<S, R>
    [[nodiscard]] friend constexpr auto tag_invoke(connect_t, Self &&self,
                                                   R &&r)
        -> op_state<Sched, S, std::remove_cvref_t<R>> {
        return {std::forward<Self>(self).sched, std::forward<Self>(self).s,
                std::forward<R>(r)};
    }

    using sched_sender = decltype(std::declval<Sched>().schedule());

    template <typename Env>
    [[nodiscard]] friend constexpr auto tag_invoke(get_completion_signatures_t,
                                                   sender const &, Env const &)
        -> make_completion_signatures<S, Env> {
        return {};
    }

  public:
    using is_sender = void;

    [[no_unique_address]] Sched sched;
    [[no_unique_address]] S s;
};

template <typename Sched> struct pipeable {
    Sched sched;

  private:
    template <async::sender S, stdx::same_as_unqualified<pipeable> Self>
    friend constexpr auto operator|(S &&s, Self &&self) -> async::sender auto {
        return sender<Sched, std::remove_cvref_t<S>>{
            std::forward<Self>(self).sched, std::forward<S>(s)};
    }
};
} // namespace _transfer

template <typename Sched>
[[nodiscard]] constexpr auto transfer(Sched &&sched)
    -> _transfer::pipeable<std::remove_cvref_t<Sched>> {
    return {std::forward<Sched>(sched)};
}

template <sender S, typename Sched>
[[nodiscard]] constexpr auto transfer(S &&s, Sched &&sched) -> sender auto {
    return std::forward<S>(s) | transfer(std::forward<Sched>(sched));
}
} // namespace async
