//! Device definition and APIs
//!
//! # Purpose
//! This module defines a set of handles to an active I/O stream which provide high-level RPCs
//! for:
//!   - Selecting, editing, and running tests
//!   - Retrieving structured test result data
//!   - Inquiring about the status of the device and synchronizing with it
//!
//! Not all devices support the same functions so the API will be slightly different from device to
//! device.
//!
//! # Device API/RPC Differences
//! There are two basic ways in which devices differ from each other:
//!   - **Single sequence and multi Sequence devices**. Devices may support multiple sequences with
//!     multiple steps in each sequence, or only support multiple steps.
//!
//!   - **Single purpose and multipurpose devices**. Devices may support only one kind of test e.g.
//!     ground bond, or may support some combination of ground bond, AC hipot, DC hipot, and insulation
//!     resistance.
//!
//! Taken together, this will result in some differences in the API of each device. Some are subltle,
//! such as single sequence devices having an `edit_test()` method which takes no arguments while
//! multiple sequence devices taken argument for the sequence number. Others are major such as a
//! complete lack of a `gnd_bond()` method on the test editor struct of a device which doesn't
//! support ground bond tests.
//!
//! The purpose of these differences is to make impossible test sequences, well, impossible. If a
//! device doesn't support doing ground bond tests, you should not be able to ask it to do one. If
//! a device doesn't support multiple sequences, you should not be able to ask it to load one.
//!
//! # Cancel Safety
//! **Unless otherwise stated, the device RPCs are not cancel safe,** i.e. they should not be used in a `tokio::select!`
//! call. This is because each RPC has both a write and a read, so cancelling after a write but
//! before a read will result in a misalignment with the data stream. The acknowledgement from the
//! first command will be interpreted as the acknowledgement for any subsequent RPC.
//!
//! All test editor `commit()`s are _extremely_ non-cancel safe because they are actually a long
//! chains of commands, meaning you may have only executed half of them when the task is cancelled.
//!
//! This limitation exists presently because of time limitations while implementing the library. It
//! may change in the future.
//!
//! # Supported Devices
//! All supported devices have struct in this module which is named closely to a camel-case version
//! of brand and model number. For instance, an SCI 4520 would be `Sci4520`. Currently supported
//! devices are:
//!   - SCI 4520
//!   - SCI 448
//!   - Associated Research HypotMax 7704
//!
//! ## What if my device isn't supported?
//! You may be able to use a handle for a device already defined. The device handles mostly function
//! to assure you don't ask for something impossible such as:
//!   - A hipot test on a ground-bond-only device (this won't even compile)
//!   - A max acceptable hipot leakage of 50mA on a 20mA device (this will result in a runtime error
//!     and no command(s) sent to the device)
//!
//! Ultimately though, all these APIs map to basically the same ASCII commands on the serial line.
//! Thus it is highly likely you could instantiate a handle for a device very similar to yours and
//! use that instead.

use crate::{
    executor::{ ArExecutor, SciMultiExecutor, SciSingleExecutor },
    test_edit::{ AcHipotDeviceLimits, GndBondDeviceLimits},
};

pub trait TestSupport
{
    fn ac_hipot_test(&self) -> Option<AcHipotDeviceLimits>
    {
        None
    }

    fn gnd_bond_test(&self) -> Option<GndBondDeviceLimits>
    {
        None
    }
}

macro_rules! impl_test_editor
{
    {ac_hipot_test} => {
        /// Make the currently selected step into an AC hipot test with the given parameters
        ///
        /// Note this will clobber any spec for the current step. Only the last specification
        /// will be sent to the device.
        pub fn ac_hipot(mut self, hipot_params: AcHipotTestSpec) -> Self
        {
            self.editor.test_spec(&mut self.steps, TestParams::AcHipot(hipot_params));
            self
        }
    };
    {gnd_bond_test} => {
        /// Make the currently selected step into a ground bond test with the given parameters
        ///
        /// Note that this will clobber any spec for the current step. Only the last specification
        /// will be sent to the device.
        pub fn gnd_bond(mut self, gnd_bond_params: GndBondTestSpec) -> Self
        {
            self.editor.test_spec(&mut self.steps, TestParams::GndBond(gnd_bond_params));
            self
        }
    };
}

macro_rules! impl_device
{
    (executor_type Sci No) => { super::SciSingleExecutor<T> };
    (executor_type Sci Yes) => { super::SciMultiExecutor<T> };
    (executor_type AssociatedResearch Yes) => { super::ArExecutor<T> };
    (create_delegate Sci No $handle:expr) => { super::SciSingleExecutor::with($handle) };
    (create_delegate Sci Yes $handle:expr) => { super::SciMultiExecutor::with($handle) };
    (create_delegate AssociatedResearch Yes $handle:expr) => { super::ArExecutor::with($handle) };
    {edit_sequence Sci No} => {
        /// Edit the test sequence on the device
        ///
        /// This will creates a test builder object which caches all edits and test parameters and
        /// then submits them to the device all at once.
        pub fn edit_sequence<'h>(&'h mut self) -> TestEditor<'h, T>
        {
            TestEditor::new(self, 1)
        }
    };
    {edit_sequence $brand:ident Yes} => {
        /// Edit the specified test sequence on the device
        ///
        /// This will creates a test builder object which caches all edits and test parameters and
        /// then submits them to the device all at once.
        pub fn edit_sequence<'h>(&'h mut self, sequence_num: u32) -> TestEditor<'h, T>
        {
            TestEditor::new(self, sequence_num)
        }
    };
    {load_sequence No} => {
        // intentionally left blank
    };
    {load_sequence Yes} => {
        /// Readies the given sequence for editing and execution
        pub async fn load_sequence(&mut self, sequence_num: u32) -> Result<(), std::io::Error>
        {
            self.io_handle.exec_cmd(CmdSet::LoadSequence(sequence_num)).await?;
            Ok(())
        }
    };
    {sequences No} => {
        /// Return the number of sequences supported by this device
        pub fn sequences(&self) -> u32
        {
            1
        }
    };
    {sequences Yes($steps:literal)} => {
        /// Return the number of sequences supported by this device
        pub fn sequences(&self) -> u32
        {
            $steps
        }
    };
}

/// Defines an SCI device by model number, file & step count limits, and supported test types
///
/// The model will be used as a module name to create a namespace for associated data types and thus
/// should be a module-friendly name e.g. `sci_4520`.
macro_rules! define_device
{
    {
        brand: $brand:ident,
        multi_sequence: $qualifier:ident $(($sequences:literal))?,
        steps_per_sequence: $steps:literal,
        supported_tests: [$($test_type:ident {limits: $limit_type:ident $lims:tt}),+]
    } => {
        use super::TestSupport;
        use crate::{
            cmd::{ CmdSet },
            test_edit::{
                TestEditor as TestEditorDelegate, StepInfo, GndBondDeviceLimits, AcHipotDeviceLimits,
                TestParams, AcHipotTestSpec, GndBondTestSpec
            },
            test_data::{ TestData, ParseTestDataErr },
            units::{ Ampere, Volt, Ohm, Second, scalar::{ Milli, Kilo, Micro }}
        };
        use tokio::io::{ AsyncWriteExt, AsyncReadExt };

        impl <T> Device<T>
        {
            impl_device!{sequences $qualifier$(($sequences))*}

            pub fn steps_per_sequence(&self) -> u32
            {
                $steps
            }
        }

        impl <T> Device<T>
            where T: AsyncReadExt + AsyncWriteExt + Unpin + Send
        {
            /// Construct a new device handle from an async I/O stream
            ///
            /// Creating I/O handles is not handled by this library so that you are not restricted to connecting to
            /// the device via a particular hardware interface. For instance, it may be desirable to use a TCP/IP
            /// serial bridge so that the device can be controlled via the Internet instead of a local RS232 line.
            pub fn with(io_handle: T) -> Self
            {
                Device {
                    io_handle: impl_device!(create_delegate $brand $qualifier io_handle),
                }
            }

            impl_device!{edit_sequence $brand $qualifier}

            impl_device!{load_sequence $qualifier}

            pub async fn start_test(&mut self) -> Result<(), std::io::Error>
            {
                self.io_handle.exec_cmd(CmdSet::RunTest).await?;
                Ok(())
            }
        
            pub async fn soft_reset(&mut self) -> Result<(), std::io::Error>
            {
                self.io_handle.exec_cmd(CmdSet::SoftReset).await?;
                Ok(())
            }

            pub async fn get_test_data(&mut self, step_num: u32) -> Result<TestData, ParseTestDataErr>
            {
                self.io_handle.get_test_data(step_num).await
            }

            pub async fn select_step(&mut self, step_num: u32) -> Result<(), std::io::Error>
            {
                self.io_handle.exec_cmd(CmdSet::SelectStep(step_num)).await?;
                Ok(())
            }
        }

        impl <T> TestSupport for Device<T>
        {
            $(fn $test_type(&self) -> Option<$limit_type>
            {
                Some($limit_type $lims)
            })+
        }

        pub struct TestEditor<'h, T>
        {
            dev: &'h mut Device<T>,
            editor: TestEditorDelegate,
            steps: [Option<StepInfo>; $steps],
        }

        impl <'h, T> TestEditor<'h, T>
            where T: AsyncReadExt + AsyncWriteExt + Unpin + Send,
        {
            fn new(dev: &'h mut Device<T>, sequence_num: u32) -> Self
            {
                Self {
                    dev: dev,
                    editor: TestEditorDelegate::edit_sequence(sequence_num),
                    // TODO instatiate this array differently
                    // This only works for a limited number of values. I think up to 32 due limitations in the Rust compiler
                    // And I don't want to implement Copy for the StepInfo type to be able to use [expr; size] notation
                    // It's quite a sizeable type
                    steps: Default::default(),
                }
            }

            /// Select the step to be edited
            ///
            /// Until a different step is selected, all changes will clobber each other on the current step. For
            /// instance, if a ground bond test is specified and then subsequently an AC hipot test is specified,
            /// only the AC hipot spec will be submitted to the device.
            ///
            /// The step number must be within limits of what this device allows. If it is not, a runtime error will
            /// occur when the test regiment is compiled and submitted to the device.
            pub fn step(mut self, step_num: u32) -> Self
            {
                self.editor.step(step_num);
                self
            }

            pub fn continue_to_next(mut self, cont: bool) -> Self
            {
                self.editor.continue_to_next(&mut self.steps, cont);
                self
            }

            /// Send the test specification to the device
            ///
            /// The selected tests and parameters will be overwritten on the device's memory at the requested
            /// locations.
            pub async fn commit(self) -> Result<(), std::io::Error>
            {
                // TODO create the actual error kinds for the various ways this can fail
                let cmds = self
                    .editor
                    .compile(
                        &self.steps,
                        self.dev.ac_hipot_test(),
                        self.dev.gnd_bond_test(),
                        self.dev.sequences(),
                        self.dev.steps_per_sequence(),
                    )
                    .ok_or(std::io::Error::from(std::io::ErrorKind::InvalidInput))?;
                
                self.dev.io_handle.exec_all(&cmds).await
            }

            $(impl_test_editor!{$test_type})+
        }
    }
}

mod sci_4520
{
    /// A connected SCI 4520 device
    ///
    /// Supports AC/DC hipot, insulation resistance, and ground bond
    pub struct Device<T>
    {
        io_handle: super::SciMultiExecutor<T>,
    }

    define_device!{
        brand: Sci,
        multi_sequence: Yes(6),
        steps_per_sequence: 6,
        supported_tests: [
            ac_hipot_test {
                limits: AcHipotDeviceLimits {
                    voltage_min: fval!(1000.0, Volt),
                    voltage_max: ival!(5, Kilo Volt),
                    dwell_min: ival!(500, Milli Second),
                    dwell_max: ival!(999, Second) + ival!(900, Milli Second),
                    leak_current_min: ival!(0, Ampere),
                    leak_current_max: ival!(99_900, Micro Ampere),
                    ramp_min: ival!(100, Milli Second),
                    ramp_max: ival!(999, Second) + ival!(900, Milli Second),
                }
            },
            gnd_bond_test {
                limits: GndBondDeviceLimits {
                    check_current_min: ival!(3, Ampere),
                    check_current_max: ival!(30, Ampere),
                    dwell_min: ival!(500, Milli Second),
                    dwell_max: ival!(999, Second) + ival!(900, Milli Second),
                    offset_min: ival!(0, Ohm),
                    offset_max: ival!(100, Milli Ohm),
                    resistance_min: ival!(0, Ohm),
                    resistance_max: ival!(510, Milli Ohm),
                }
            }
        ]
    }
}

mod sci_448
{
    /// A connected SCI 448 device
    ///
    /// Supports AC/DC hipot, insulation resistance, and ground bond
    pub struct Device<T>
    {
        io_handle: super::SciSingleExecutor<T>,
    }

    define_device!{
        brand: Sci,
        multi_sequence: No,
        steps_per_sequence: 20,
        supported_tests: [
            ac_hipot_test {
                limits: AcHipotDeviceLimits {
                    voltage_min: fval!(1000.0, Volt),
                    voltage_max: ival!(5, Kilo Volt),
                    dwell_min: ival!(500, Milli Second),
                    dwell_max: ival!(999, Second) + ival!(900, Milli Second),
                    leak_current_min: ival!(0, Ampere),
                    leak_current_max: ival!(99, Milli Ampere) + ival!(900, Micro Ampere),
                    ramp_min: ival!(100, Milli Second),
                    ramp_max: ival!(999, Second) + ival!(900, Milli Second),
                }
            },
            gnd_bond_test {
                limits: GndBondDeviceLimits {
                    check_current_min: ival!(3, Ampere),
                    check_current_max: ival!(30, Ampere),
                    dwell_min: ival!(500, Milli Second),
                    dwell_max: ival!(999, Second) + ival!(900, Milli Second),
                    offset_min: ival!(0, Ohm),
                    offset_max: ival!(100, Milli Ohm),
                    resistance_min: ival!(0, Ohm),
                    resistance_max: ival!(510, Milli Ohm),
                }
            }
        ]
    }
}

mod ar_hm_7704
{
    /// A connected Associated Research HypotMax 7704 device
    ///
    /// Supports AC/DC hipot, insulation resistance, and ground bond
    pub struct Device<T>
    {
        io_handle: super::ArExecutor<T>,
    }

    define_device!{
        brand: AssociatedResearch,
        multi_sequence: Yes(3),
        steps_per_sequence: 8,
        supported_tests: [
            ac_hipot_test {
                limits: AcHipotDeviceLimits {
                    voltage_min: fval!(1000.0, Volt),
                    voltage_max: ival!(5, Kilo Volt),
                    dwell_min: ival!(500, Milli Second),
                    dwell_max: ival!(999, Second) + ival!(900, Milli Second),
                    leak_current_min: ival!(0, Ampere),
                    leak_current_max: ival!(99, Milli Ampere) + ival!(900, Micro Ampere),
                    ramp_min: ival!(100, Milli Second),
                    ramp_max: ival!(999, Second) + ival!(900, Milli Second),
                }
            },
            gnd_bond_test {
                limits: GndBondDeviceLimits {
                    check_current_min: ival!(3, Ampere),
                    check_current_max: ival!(30, Ampere),
                    dwell_min: ival!(500, Milli Second),
                    dwell_max: ival!(999, Second) + ival!(900, Milli Second),
                    offset_min: ival!(0, Ohm),
                    offset_max: ival!(100, Milli Ohm),
                    resistance_min: ival!(0, Ohm),
                    resistance_max: ival!(510, Milli Ohm),
                }
            }
        ]
    }
}

pub use sci_4520::{ Device as Sci4520, TestEditor as Sci4520TestEditor };
pub use sci_448::{ Device as Sci448, TestEditor as Sci448TestEditor };
pub use ar_hm_7704::{ Device as ArHypotMax7704, TestEditor as ArHypotMax7704TestEditor };
