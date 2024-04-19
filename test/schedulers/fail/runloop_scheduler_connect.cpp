#include <async/concepts.hpp>
#include <async/schedulers/runloop_scheduler.hpp>
#include <async/tags.hpp>

struct fail_receiver {
    using is_receiver = void;
};
static_assert(async::receiver<fail_receiver>);

auto main() -> int {
    async::run_loop<void> rl{};

    [[maybe_unused]] auto op =
        async::connect(rl.get_scheduler().schedule(), fail_receiver{});
}
