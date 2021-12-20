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
        pub fn ac_hipot(mut self, hipot_params: AcHipotTestSpec) -> Self
        {
            self.editor.test_spec(&mut self.steps, TestParams::AcHipot(hipot_params));
            self
        }
    };
    {gnd_bond_test} => {
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
        pub fn edit_sequence<'h>(&'h mut self) -> TestEditor<'h, T>
        {
            TestEditor::new(self, 1)
        }
    };
    {edit_sequence $brand:ident Yes} => {
        /// Edit a test sequence on the device
        ///
        /// This will create a test builder object which can be used to specify the tests and then submit them to the
        /// device.
        pub fn edit_sequence<'h>(&'h mut self, sequence_num: u32) -> TestEditor<'h, T>
        {
            TestEditor::new(self, sequence_num)
        }
    };
    {load_sequence No} => {
        // intentionally left blank
    };
    {load_sequence Yes} => {
        pub async fn load_sequence(&mut self, sequence_num: u32) -> Result<(), std::io::Error>
        {
            self.io_handle.exec_cmd(CmdSet::LoadSequence(sequence_num)).await?;
            Ok(())
        }
    };
    {sequences No} => {
        pub fn sequences(&self) -> u32
        {
            1
        }
    };
    {sequences Yes($steps:literal)} => {
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
        model: $dev:ident,
        brand: $brand:ident,
        multi_sequence: $qualifier:ident $(($sequences:literal))?,
        steps_per_sequence: $steps:literal,
        supported_tests: [$($test_type:ident {limits: $limit_type:ident $lims:tt}),+]
    } => {
        mod $dev {
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

            pub struct Device<T>
            {
                io_handle: impl_device!(executor_type $brand $qualifier),
            }

            impl <T> Device<T>
            {
                impl_device!{sequences $qualifier$(($sequences))*}
                // pub fn sequences(&self) -> u32
                // {
                //     $files
                // }

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
}

define_device!{
    model: sci_4520,
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

define_device!{
    model: sci_448,
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

define_device!{
    model: ar_7704,
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

pub use sci_4520::{ Device as Sci4520, TestEditor as Sci4520TestEditor };
pub use sci_448::{ Device as Sci448, TestEditor as Sci448TestEditor };
pub use ar_7704::{ Device as Ar7704, TestEditor as Ar7704TestEditor };
