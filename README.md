# arcs-hipot
**A**synchronous-Rust **R**emote **C**ontrol for **S**CI **Hipot** testers

As the name would imply, this is a library for controlling
[SCI electrical safety testers](https://www.hipot.com/products.html) using their
serial remote control interfaces. Specifically, it is designed for use with the
[Tokio async framework](https://tokio.rs) for the Rust programming
language. It exposes SCI's serial command set in an API-friendly, composable
way allowing for easy integration into automated, programmatic testing.

**This is currently a work in progress and does not have a working version**

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

## Goals
I started working on this as part of a project for my day job which requires
automated testing using SCI 4520 and 448 instruments and some Associated Research 7704s as
well. Associated Research devices have almost an almost identical remote command
set and test behavior. By analogy to CPU instruction set architecture, the
opcodes are different between SCI and Associated Research but basic types of
instruction are the same right down to structure and semantics. Associated
Research and SCI are sister companies and have technology exchange agreements
which is probably why this similarity exists.

My hard goals are thus largely driven by the requirements of my job. They are as
follows:
  - Support for SCI 4520, SCI 448, and Associated Research 7704
  - Configure devices to run:
    - AC hipot tests
    - Ground bond tests
  - Loading and running tests
  - Parsing of test results stored on the device's memory
  - Device-tailored API to prevent impossible test regiments from even being
    compiled

### Soft Goals
These are things which I don't currently have need for but that I probably will do
because if there's one thing I've learned, its that doing 10% more work upfront
can save you double or triple the work later:
  - Configure devices to run:
    - DC hipot tests
    - Insulation resistance tests
  - Support for SCI's entire current suite of testers (i.e. the 200, 300 and 400
    series testers)

### Dream Goals
Things that are firmly in the "would be nice" category but as a matter of
practicality don't strongly affect this library's usefullness:
  - Zero allocations on the library backend
  - Fully unified, trait-based API to allow for dynamic dispatch and polymorphism
  - Synchronous frontend
  - Full command set implementation for supported devices

## License
This library is free (libre) software and may be used, modified, and
redistributed under the terms of the BSD 3-Clause license.
