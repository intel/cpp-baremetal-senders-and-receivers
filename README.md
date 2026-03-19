# C++ Bare Metal Senders and Receivers

[![Unit Tests](https://github.com/intel/cpp-baremetal-senders-and-receivers/actions/workflows/unit_tests.yml/badge.svg)](https://github.com/intel/cpp-baremetal-senders-and-receivers/actions/workflows/unit_tests.yml)

*C++ Bare Metal Senders and Receivers* is a C++ header-only library that
partially implements [P2300](https://wg21.link/p2300), the proposed C++ standard
design for asynchronous computation.

This implementation is intended for use on bare metal microcontrollers and
implements a subset of the standard design. P2300 does encourage implementations
not to do any runtime allocation, and that is fine for bare metal. But some
notable differences here from the standard proposal are:

- This implementation does not deal with exceptions in any way. An error is just
  an error value.
- Coroutines are not considered at all. This implementation does not preclude
  coroutines as senders, but in a bare metal environment, the lack of
  non-allocation guarantees currently makes using coroutines an uncertain
  proposition at best.
- Some functions are missing, some have different names, and some are
  non-standard.

In general, although the P2300 design allows mechanism-agnostic handling of
concurrency, the main mechanism of concurrency targeted by this library is
interrupts. A careful reading of
\[[intro.multithread](https://eel.is/c++draft/intro.multithread)\] is
recommended to understand how the C++ memory model defines well-formed
concurrent execution.

C++ standard support is as follows:

- C++23: [main branch](https://github.com/intel/cpp-baremetal-senders-and-receivers/tree/main) (active development)
- C++20: [cpp20 branch](https://github.com/intel/cpp-baremetal-senders-and-receivers/tree/cpp20) (supported)

See the [full documentation](https://intel.github.io/cpp-baremetal-senders-and-receivers/).

Compiler support:

| Branch | GCC versions | Clang versions |
| --- | --- | --- |
| [main](https://github.com/intel/cpp-baremetal-senders-and-receivers/tree/main) | 12 thru 14 | 19 thru 22 |
| [cpp20](https://github.com/intel/cpp-baremetal-senders-and-receivers/tree/cpp20) | 12 thru 14 | 14 thru 21 |
