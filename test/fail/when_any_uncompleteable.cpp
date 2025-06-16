#include <async/concepts.hpp>
#include <async/connect.hpp>
#include <async/just.hpp>
#include <async/start.hpp>
#include <async/when_any.hpp>

// EXPECT: Connect: when_any<> has a subsender that never completes

struct uncompleteable_sender {
    using is_sender = void;
    using completion_signatures = async::completion_signatures<>;

    struct op_state {
        constexpr auto start() & -> void {}
    };

    [[nodiscard]] constexpr auto connect(async::receiver auto &&) -> op_state {
        return {};
    }
};

auto main() -> int {
    auto s = async::when_any(uncompleteable_sender{}, async::just());
    auto op = async::connect(s, async::detail::universal_receiver{});
}
