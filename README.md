# arcs-hipot
**A**synchronous-Rust **R**emote **C**ontrol for **S**CI **Hipot** testers

As the name would imply, this is a library for controlling
[SCI electrical safety testers](https://www.hipot.com/products.html) using their
serial remote control interfaces. Specifically, it is designed for use with the
[Tokio async framework](https://tokio.rs) for the Rust programming
language. It exposes SCI's serial command set in an API-friendly, composable
way allowing for easy integration into automated, programmatic testing.

## DISCLAIMER
This library **IS** a wrapper around a serial command set allowing programmatic
interaction with a suite of devices.

This library **IS NOT** an authority on how to use said devices correctly and
accordingly neither is it an authority on how to do electrical safety tests correctly.
Behaviors of the API are designed to prevent **programmatically impossible**
operations but **NOT** to prevent **unsafe**, **unusual**, or **legally noncompliant**
operations. All documentation is strictly for the purposes of explaing what the
device will be commanded to do. Any testing regiment built on this library
should be designed and reviewed by qualified safety codes and test engineers.

**Simply put:** This library does not stop you from doing stupid and/or wrong things
with your tests.

Electrical safety is no joke. People can die from testing done stupidly or wrong.
_Don't mess around!_

## Device Support
This library currently supports the following SCI devices:
  - SCI 4520

## License
This library is free software and may be used and redistributed under the terms
of the BSD 3-Clause license.
