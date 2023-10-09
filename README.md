# C++ Bare Metal Senders and Receivers

[![Unit Tests](https://github.com/intel/cpp-baremetal-senders-and-receivers/actions/workflows/unit_tests.yml/badge.svg)](https://github.com/intel/cpp-baremetal-senders-and-receivers/actions/workflows/unit_tests.yml)

## WARNING: Work in Progress!

This library is under active development. It is a work in progress and has not
yet been proved in production. Proceed at your own risk.

## Abstract

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
- Generalized tag dispatch may be missing for some functions.

In general, although the P2300 design allows mechanism-agnostic handling of
concurrency, the main mechanism of concurrency targeted by this library is
interrupts. A careful reading of
\[[intro.multithread](https://eel.is/c++draft/intro.multithread)\] is
recommended to understand how the C++ memory model defines well-formed
concurrent execution.

C++20 is required. The following compilers are supported:
 
- clang 14 through 17
- gcc 12 through 13

See the [full documentation](https://intel.github.io/cpp-baremetal-senders-and-receivers/).
