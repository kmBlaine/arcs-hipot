//! **A**synchronous-Rust **R**emote **C**ontrol for **S**CI **Hipot** testers
//!
//! # About
//! As the name would imply, this is a library for controlling
//! [SCI electrical safety testers](https://www.hipot.com/products.html) using their serial remote
//! control interfaces. Specifically, it is designed for use with the
//! [Tokio async framework](https://tokio.rs). It wraps SCI's serial command set in a type-safe,
//! composable API allowing for easy integration into automated, programmatic testing.
//!
//! Because of the needs of the original author of this library, there is also support for some
//! Associated Research devices. SCI and Associated Research are sister companies and have
//! technology exchange agreements, hence they share an almost identical remote control command set.
//! This makes integration with them more a matter of substituting some opcodes rather than
//! implementing a totally different protocol and wrapper API.
//!
//! # Notice on Usage
//! This library **IS** a wrapper around a serial command set allowing programmatic
//! interaction with a suite of devices.
//! 
//! This library **IS NOT** an authority on how to use said devices correctly. Accordingly, neither
//! is it an authority on how to do electrical safety tests correctly. Behaviors of the API are
//! designed to prevent programmatically impossible operations but **NOT** to prevent
//! **unsafe**, **unusual**, or **legally noncompliant** operations. All documentation is strictly
//! for the purposes of explaing what the device will be commanded to do. Any testing regiment built
//! on this library should be designed and reviewed by qualified safety codes and test engineers.
//!
//! This library is open source. It is **strongly** encouraged that adopters review the source code
//! for themselves to ensure compliance in their test regiments.
//!
//! # Quick Start
//! ```
//! #[macro_use]
//! use arcs_hipot::{ Sci4520, units::* };
//!
//! // Open a serial port and then hand to the device API wrapper
//! let port_settings = tokio_serial::SerialPortSettings {
//!     baud_rate: 9600,
//!     parity: tokio_serial::Parity::None,
//!     stop_bits: tokio_serial::StopBits::One,
//!     data_bits: tokio_serial::DataBits::Eight,
//!     flow_control: tokio_serial::FlowControl::None,
//!     ..Default::default()
//! };
//! let mut device = Sci4520::with(tokio_serial::Serial::from_path(
//!     "/dev/ttyS0",
//!     &port_settings
//! ).unwrap());
//!
//! // Create a test sequence on the device
//! // Will perform a ground bond followed by an AC hipot
//! device
//!     .edit_sequence(4)
//!     .step(1)
//!     .gnd_bond(GndBondTestSpec::new()
//!         .check_current(Ampere::from_base(25))
//!         .resistance_max(Ohm::from::<Milli>(150))
//!         .resistance_min(Ohm::from::<Milli>(5))
//!         .dwell_time(Second::from_base(2))
//!     )
//!     .continue_to_next(true)
//!     .step(2)
//!     .continue_to_next(false)
//!     .ac_hipot(AcHipotTestSpec::new()
//!         .voltage(ival!(1250, Volt))
//!         .leak_current_min(ival!(800, Micro Ampere))
//!         .leak_current_max(ival!(25_000, Micro Ampere))
//!         .ac_frequency(AcFrequency::Hz60)
//!         .dwell_time(fval!(3.0, Second))
//!         .ramp_time(fval!(0.9, Second))
//!     )
//!     .commit()
//!     .await
//!     .unwrap();
//!
//! // Select the appropriate step and run the test
//! device.select_step(1).await.unwrap();
//! device.start_test().await.unwrap();
//!
//! tokio::time::delay_for(tokio::time::Duration::from_secs(10)).await;
//!
//! // Retrieve the test results
//! let ac_hipot_results = device.get_test_data(1).await.unwrap();
//! let gnd_bond_results = device.get_test_data(2).await.unwrap();
//! ```
//!
//! # Interface Agnosticism
//! You may have noticed that the quick start was using
//! [tokio_serial](https://crates.io/crates/tokio-serial) to open a serial port on the computer and
//! then hands it to a device wrapper. The reason for this is fairly straightforward; a protocol
//! implementation really doesn't care if it is reading and writing bytes from a file, a TCP socket,
//! serial port, or some other yet-to-be-defined interface. A byte stream is a byte stream. Any
//! thread-safe implementor of Tokio's `AsyncRead` and `AsyncWrite` traits will do.
//!
//! This is especially relevant to the most likely use case of this library: factory supervisory and
//! control applications. It is not uncommon to put IP serial bridges in factories, such as a
//! [Moxa NPort 5100](https://moxa.com/en/products/industrial-edge-connectivity/serial-device-servers/general-device-servers/nport-5100-series),
//! which convert an RS232 or RS422 device into a TCP or UDP endpoint, allowing access from anywhere
//! on the plant network. This is naturally far more scalable and flexible than everything being
//! attached locally.
//! 
//! By not specifying the hardware interface to be used, this library allows you to handle such a
//! case trivially by merely replacing the serial port with a TCP socket or whatever read-write
//! interface happens to suit you:
//!
//! ```
//! let sock_addr = "10.3.20.14:4529".parse::<std::net::SocketAddr>();
//! let mut device = Sci4520::with(tokio::net::TcpStream::connect(sock_addr).await.unwrap());
//! ```
//!
//! Integrating interface definition into the library is convenient, yes, but restricts its possible
//! uses.
//!
//! # Correctness and Completeness
//! This library has been well-tested against the SCI 4520 and the Associated Research HypotMax 7704.
//! It is hoped that this library will eventually support entirely all current-generation SCI
//! testers.
//!
//! However, both of these companies' user manuals are lacking in substance, specificity, and examples when
//! it comes to documenting their device APIs. Thus, it's very hard to know exactly how much of this
//! library will or will not work with various offerings. Adopters are encouraged to document and
//! communicate their findings.

pub mod test_data;
#[macro_use]
pub mod units;
pub mod devices;

pub use devices::{ ArHypotMax7704, Sci4520, Sci448 };

mod cmd;
mod executor;
mod test_edit;

pub use test_edit::{ AcHipotTestSpec, GndBondTestSpec };
