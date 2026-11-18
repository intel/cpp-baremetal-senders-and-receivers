#include "detail/injected_task_manager.hpp"

#include <async/schedulers/priority_scheduler.hpp>
#include <async/schedulers/task_manager.hpp>
#include <async/start_detached.hpp>

#include <stdx/ct_string.hpp>

auto try_second_start() -> bool {
    using S = async::fixed_priority_scheduler<0>;
    return static_cast<bool>(async::start_detached<"test">(S::schedule()));
}
