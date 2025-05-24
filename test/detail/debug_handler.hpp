#pragma once

#include <async/debug.hpp>

#include <stdx/ct_format.hpp>

#include <boost/mp11/list.hpp>
#include <fmt/format.h>

#include <string>
#include <type_traits>
#include <vector>

namespace {
std::vector<std::string> debug_events{};

template <typename Tag, auto Empty = false> struct debug_handler {
    template <stdx::ct_string C, stdx::ct_string S, typename Ctx>
    constexpr auto signal(auto &&...) {
        if constexpr (std::is_same_v<async::debug::tag_of<Ctx>, Tag>) {
            static_assert(
                Empty ==
                boost::mp11::mp_empty<async::debug::children_of<Ctx>>::value);
            debug_events.push_back(
                fmt::format("{} {} {}", C, async::debug::name_of<Ctx>, S));
        }
    }
};
} // namespace
