#include <async/schedulers/trigger_manager.hpp>

auto run_test_triggers() -> void { async::run_triggers<"test">(); }
