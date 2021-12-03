//! **A**synchronous-Rust **R**emote **C**ontrol for **S**CI **Hipot** testers

use std::{
    time::Duration,
    ops::{ Add, Sub },
    cmp::{ PartialEq, PartialOrd, Eq, Ord, Ordering },
};

struct DecimalNano
{
    whole: u32,
    nanos: u32,
}

impl DecimalNano
{
    fn pack_into_u64(&self) -> u64
    {
        ((self.whole as u64) << 32) | (self.nanos as u64)
    }

    pub fn from_parts(whole: u32, nanos: u32) -> Self
    {
        Self {
            whole: whole + nanos / 1_000_000_000,
            nanos: nanos % 1_000_000_000
        }
    }

    pub fn from_micros(micros: u32) -> Self
    {
        Self::from_parts(micros / 1_000_000, (micros % 1_000_000) * 1000)
    }

    pub fn from_millis(millis: u32) -> Self
    {
        Self::from_parts(millis / 1000, (millis % 1000) * 1_000_000)
    }

    pub fn from_whole(whole: u32) -> Self
    {
        Self {
            whole: whole,
            nanos: 0,
        }
    }

    pub fn as_f64(&self) -> f64
    {
        (self.whole as f64) + (self.nanos as f64 / 1_000_000_000f64)
    }
}

impl Add for DecimalNano
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self
    {
        Self::from_parts(
            self.whole + rhs.whole + (self.nanos + rhs.nanos) / 1_000_000_000,
            (self.nanos + rhs.nanos) % 1_000_000_000,
        )
    }
}

impl Sub for DecimalNano
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self
    {
        let fraction = self.nanos as i32 - rhs.nanos as i32;

        if fraction < 0 {
            Self::from_parts(self.whole - rhs.whole - 1, (fraction + 1_000_000_000) as u32)
        }
        else {
            Self::from_parts(self.whole - rhs.whole, fraction as u32)
        }
    }
}

impl PartialEq for DecimalNano
{
    fn eq(&self, rhs: &Self) -> bool
    {
        self.whole == rhs.whole && self.nanos == rhs.nanos
    }
}

impl PartialOrd for DecimalNano
{
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering>
    {
        self.pack_into_u64().partial_cmp(&rhs.pack_into_u64())
    }
}

impl Eq for DecimalNano {}
impl Ord for DecimalNano
{
    fn cmp(&self, rhs: &Self) -> Ordering
    {
        self.partial_cmp(rhs).unwrap()
    }
}

macro_rules! unit
{
    ($u:ident) => {
        pub struct $u
        {
            value: DecimalNano,
        }

        impl $u
        {
            pub fn from_parts(whole: u32, nanos: u32) -> Self
            {
                Self { value: DecimalNano::from_parts(whole, nanos) }
            }

            pub fn from_micros(micros: u32) -> Self
            {
                Self { value: DecimalNano::from_micros(micros) }
            }

            pub fn from_millis(millis: u32) -> Self
            {
                Self { value: DecimalNano::from_millis(millis) }
            }

            pub fn from_whole(whole: u32) -> Self
            {
                Self { value: DecimalNano::from_whole(whole) }
            }

            pub fn as_f64(&self) -> f64
            {
                self.value.as_f64()
            }
        }

        impl Add for $u
        {
            type Output = $u;

            fn add(self, rhs: Self) -> Self
            {
                Self { value: self.value + rhs.value }
            }
        }
        
        impl Sub for $u
        {
            type Output = $u;
        
            fn sub(self, rhs: Self) -> Self
            {
                Self { value: self.value - rhs.value }
            }
        }

        impl PartialEq for $u
        {
            fn eq(&self, rhs: &Self) -> bool
            {
                self.value == rhs.value
            }
        }

        impl PartialOrd for $u
        {
            fn partial_cmp(&self, rhs: &Self) -> Option<Ordering>
            {
                self.value.partial_cmp(&rhs.value)
            }
        }

        impl Eq for $u {}
        impl Ord for $u
        {
            fn cmp(&self, rhs: &Self) -> Ordering
            {
                self.partial_cmp(rhs).unwrap()
            }
        }
    }
}

unit!(Amp);
unit!(Volt);
unit!(Ohm);

pub enum AcFrequency
{
    Hz50,
    Hz60,
}

enum TestLimit
{
    Microamp(u32),
    Megaohm(u16),
    Milliohm(u16),
}

enum CmdSet
{
    /// Ready a test file stored on the device for execution and editing
    ///
    /// Command: `FL <file_number>`
    LoadFile(u32),
    /// Select a step in the currently loaded file
    ///
    /// Command: `SS <step_number>`
    SelectStep(u32),
    /// Change currently selected step to an AC hipot test with default parameters
    ///
    /// Command: `SAA`
    SetAcHipot,
    /// Change currently selected step to a ground bond test with default parameters
    ///
    /// Command: `SAG`
    SetGndBond,
    /// When running the test, changes whether tester halts upon the completion of the current step
    /// or continues to the next.
    ///
    /// Command: `ECC <1|0>`
    ContinueToNext(bool),
    /// On the currently selected step, set the electrical current used to check the quality of the
    /// ground connection in amperes. Applicable only if the current step is a ground
    /// bond test.
    ///
    /// Command: `EC <amps>`
    SetGndCheckCurrent(Amp),
    /// On the currently selected step, set the dwell time of the primary test current in seconds.
    /// Applicable only if the current step is an AC or DC hipot or ground bond.
    ///
    /// Command: `EDW <seconds>`
    SetDwell(Duration),
    /// On the currently selected step, set the AC frequency to be used i.e. 50 or 60 Hz. Applicable
    /// only if the current step is an AC hipot or ground bond.
    ///
    /// Command: `EF <1|0>`
    SetAcFrequency(AcFrequency),
    /// On the currently selected step, set the maximum allowble measured value. The precise
    /// value and semantics depend on the test being run.
    ///
    /// Command: `EH <limit>`
    SetCurrentHiLimit(Amp),
    /// On the currently selected step, set the maximum allowble measured value. The precise
    /// value and semantics depend on the test being run.
    ///
    /// Command: `EL <limit>`
    SetCurrentLoLimit(Amp),
    SetResistanceHiLimit(Ohm),
    SetResistanceLoLimit(Ohm),
    /// On the currently selected step, set the ground connection resistance offset in milliohms.
    /// Applicable only to ground bond tests.
    ///
    /// Command: `EO <milliohm>`
    SetGndOffset(Ohm),
    /// On the currently selected step, set the time it takes to ramp up the voltage to the
    /// full test load. Applicable only to AC and DC hipot tests.
    ///
    /// Command: `ERU <seconds>`
    SetRampTime(Duration),
    /// On the currently selected step, set the high-potential voltage used. Only applicable to AC
    /// and DC hipot tests.
    ///
    /// Command: `EV <kilovolt>`
    SetHipotVoltage(Volt),
}

pub struct AcHipotBuilder
{
    voltage: Option<Volt>,
    dwell: Option<Duration>,
    leak_current_min: Option<Amp>,
    leak_current_max: Option<Amp>,
    frequency: Option<AcFrequency>,
    ramp: Option<Duration>,
}

impl AcHipotBuilder
{
    pub fn new() -> Self
    {
        Self {
            voltage: None,
            dwell: None,
            leak_current_min: None,
            leak_current_max: None,
            frequency: None,
            ramp: None,
        }
    }

    pub fn voltage(mut self, volts: Volt) -> Self
    {
        self.voltage = Some(volts);
        self
    }

    pub fn dwell_time(mut self, seconds: Duration) -> Self
    {
        self.dwell = Some(seconds);
        self
    }

    pub fn leak_current_min(mut self, amps: Amp) -> Self
    {
        self.leak_current_min = Some(amps);
        self
    }

    pub fn leak_current_max(mut self, amps: Amp) -> Self
    {
        self.leak_current_max = Some(amps);
        self
    }

    pub fn ac_frequency(mut self, frequency: AcFrequency) -> Self
    {
        self.frequency = Some(frequency);
        self
    }

    pub fn ramp_time(mut self, seconds: Duration) -> Self
    {
        self.ramp = Some(seconds);
        self
    }
}

pub struct GndBondBuilder
{
    check_current: Option<Amp>,
    dwell: Option<Duration>,
    resistance_max: Option<Ohm>,
    resistance_min: Option<Ohm>,
    frequency: Option<AcFrequency>,
    offset: Option<Ohm>,
}

impl GndBondBuilder
{
    pub fn new() -> Self
    {
        Self {
            check_current: None,
            dwell: None,
            resistance_max: None,
            resistance_min: None,
            frequency: None,
            offset: None,
        }
    }

    pub fn check_current(mut self, amps: Amp) -> Self
    {
        self.check_current = Some(amps);
        self
    }

    pub fn dwell_time(mut self, seconds: Duration) -> Self
    {
        self.dwell = Some(seconds);
        self
    }

    pub fn max_resistance(mut self, ohms: Ohm) -> Self
    {
        self.resistance_max = Some(ohms);
        self
    }

    pub fn min_resistance(mut self, ohms: Ohm) -> Self
    {
        self.resistance_min = Some(ohms);
        self
    }

    pub fn ac_frequency(mut self, frequency: AcFrequency) -> Self
    {
        self.frequency = Some(frequency);
        self
    }

    pub fn resistance_offset(mut self, ohms: Ohm) -> Self
    {
        self.offset = Some(ohms);
        self
    }
}

enum TestParams
{
    AcHipot(AcHipotBuilder),
    GndBond(GndBondBuilder)
}

struct StepInfo
{
    step_num: u32,
    params: Option<TestParams>,
    continue_to_next: Option<bool>,
}

struct TestEditor
{
    file_num: u32,
    steps: Vec<StepInfo>,
    selected_step: u32,
}

impl TestEditor
{
    fn edit_file(file_num: u32) -> Self
    {
        Self {
            file_num: file_num,
            steps: Vec::new(),
            selected_step: 1,
        }
    }

    fn step(&mut self, step_num: u32)
    {
        self.selected_step = step_num;
    }

    fn gnd_bond(&mut self, params: GndBondBuilder)
    {
        let selected_step = self.selected_step; // need to copy the value so we don't put `self` into the closure

        if let Some(step_info) = self.steps.iter_mut().filter(|info| info.step_num == selected_step).next() {
            step_info.params = Some(TestParams::GndBond(params));
        }
        else {
            self.steps.push(StepInfo {
                step_num: self.selected_step,
                params: Some(TestParams::GndBond(params)),
                continue_to_next: None,
            })
        }
    }

    fn ac_hipot(&mut self, params: AcHipotBuilder)
    {
        let selected_step = self.selected_step; // need to copy the value so we don't put `self` into the closure

        if let Some(step_info) = self.steps.iter_mut().filter(|info| info.step_num == selected_step).next() {
            step_info.params = Some(TestParams::AcHipot(params));
        }
        else {
            self.steps.push(StepInfo {
                step_num: self.selected_step,
                params: Some(TestParams::AcHipot(params)),
                continue_to_next: None,
            })
        }
    }

    fn continue_to_next(&mut self, cont: bool)
    {
        let selected_step = self.selected_step; // need to copy the value so we don't put `self` into the closure

        if let Some(step_info) = self.steps.iter_mut().filter(|info| info.step_num == selected_step).next() {
            step_info.continue_to_next = Some(cont);
        }
        else {
            self.steps.push(StepInfo {
                step_num: self.selected_step,
                params: None,
                continue_to_next: Some(cont),
            })
        }
    }

    fn compile(
        self,
        ac_hipot_limits: AcHipotLimits,
        gnd_bond_limits: GndBondLimits,
        max_file_num: u32,
        max_step_num: u32,
    )
        -> Option<Vec<CmdSet>>
    {
        if self.file_num > max_file_num {
            return None;
        }

        let mut cmds = vec![CmdSet::LoadFile(self.file_num)];

        for step in self.steps {
            if step.step_num > max_step_num {
                return None;
            }

            // Steps are only committed by the builder when an override of either the continue_to_next
            // or test type/parameters occurs, so there is no need to delay adding a step selection
            // command
            cmds.push(CmdSet::SelectStep(step.step_num));

            if let Some(overrides) = step.params {
                match overrides {
                    TestParams::AcHipot(params) => {
                        if let Some(voltage) = params.voltage {
                            if voltage > ac_hipot_limits.voltage_max || voltage < ac_hipot_limits.voltage_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetHipotVoltage(voltage));
                        }

                        if let Some(dwell) = params.dwell {
                            if dwell > ac_hipot_limits.dwell_max || dwell < ac_hipot_limits.dwell_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetDwell(dwell));
                        }

                        if let Some(leak_current_min) = params.leak_current_min {
                            if leak_current_min > ac_hipot_limits.leak_current_max
                                || leak_current_min < ac_hipot_limits.leak_current_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetCurrentLoLimit(leak_current_min));
                        }

                        if let Some(leak_current_max) = params.leak_current_max {
                            if leak_current_max > ac_hipot_limits.leak_current_max
                                || leak_current_max < ac_hipot_limits.leak_current_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetCurrentHiLimit(leak_current_max));
                        }

                        if let Some(ramp) = params.ramp {
                            if ramp > ac_hipot_limits.ramp_max || ramp < ac_hipot_limits.ramp_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetRampTime(ramp));
                        }

                        if let Some(frequency) = params.frequency {
                            cmds.push(CmdSet::SetAcFrequency(frequency));
                        }
                    },
                    TestParams::GndBond(params) => {
                        if let Some(check_current) = params.check_current {
                            if check_current > gnd_bond_limits.check_current_max
                                || check_current < gnd_bond_limits.check_current_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetGndCheckCurrent(check_current));
                        }

                        if let Some(dwell) = params.dwell {
                            if dwell > gnd_bond_limits.dwell_max || dwell < gnd_bond_limits.dwell_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetDwell(dwell));
                        }

                        if let Some(resistance_min) = params.resistance_min {
                            if resistance_min > gnd_bond_limits.resistance_max
                                || resistance_min < gnd_bond_limits.resistance_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetResistanceLoLimit(resistance_min));
                        }

                        if let Some(resistance_max) = params.resistance_max {
                            if resistance_max > gnd_bond_limits.resistance_max
                                || resistance_max < gnd_bond_limits.resistance_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetResistanceHiLimit(resistance_max));
                        }

                        if let Some(offset) = params.offset {
                            if offset > gnd_bond_limits.offset_max || offset < gnd_bond_limits.offset_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetGndOffset(offset));
                        }

                        if let Some(frequency) = params.frequency {
                            cmds.push(CmdSet::SetAcFrequency(frequency));
                        }
                    }
                }
            }

            if let Some(cont) = step.continue_to_next {
                cmds.push(CmdSet::ContinueToNext(cont));
            }
        }

        Some(cmds)
    }
}

pub struct AcHipotLimits
{
    pub voltage_min: Volt,
    pub voltage_max: Volt,
    pub dwell_max: Duration,
    pub dwell_min: Duration,
    pub leak_current_max: Amp,
    pub leak_current_min: Amp,
    pub ramp_max: Duration,
    pub ramp_min: Duration,
}

pub struct GndBondLimits
{
    pub check_current_min: Amp,
    pub check_current_max: Amp,
    pub dwell_min: Duration,
    pub dwell_max: Duration,
    pub offset_min: Ohm,
    pub offset_max: Ohm,
    pub resistance_min: Ohm,
    pub resistance_max: Ohm,
}

trait TestSupport
{
    fn ac_hipot(&self) -> Option<AcHipotLimits>
    {
        None
    }

    fn gnd_bond(&self) -> Option<GndBondLimits>
    {
        None
    }
}

// pub struct Sci4520StepBuilder

// pub struct Sci4520<T>
// {
//     interface: T,
// }

macro_rules! impl_test_editor
{
    {ac_hipot} => {
        pub fn ac_hipot(mut self, hipot_params: AcHipotBuilder) -> Self
        {
            self.editor.ac_hipot(hipot_params);
            self
        }
    };
    {gnd_bond} => {
        pub fn gnd_bond(mut self, gnd_bond_params: GndBondBuilder) -> Self
        {
            self.editor.gnd_bond(gnd_bond_params);
            self
        }
    };
}

/// Defines an SCI device by model number, file & step count limits, and supported test types
///
/// The model will be used as a module name to create a namespace for associated data types and thus
/// should be a module-friendly name e.g. `sci_4520`.
macro_rules! define_device
{
    { model: $dev:ident, files: $files:literal, steps_per_file: $steps:literal, supported_tests: [$($test_type:ident {limits: $limit_type:ident $lims:tt}),+] } => {
        mod $dev {
            use super::{Amp, Volt, Ohm, TestSupport, AcHipotLimits, GndBondLimits, AcHipotBuilder, GndBondBuilder};
            use std::time::Duration;
            use tokio::io::{ AsyncReadExt, AsyncWriteExt };

            pub struct Device<T>
            {
                io_handle: T,
            }

            impl <T> Device<T>
            {
                const FILES_NUM: u32 = $files;
                const STEPS_PER_FILE: u32 = $steps;
            }

            impl <T> Device<T>
                where T: AsyncReadExt + AsyncWriteExt + Send
            {
                pub fn edit_test<'h>(&'h mut self, file_num: u32) -> TestEditor<'h, T>
                {
                    TestEditor {
                        dev: self,
                        editor: super::TestEditor::edit_file(file_num),
                    }
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
                dev: &'h Device<T>,
                editor: super::TestEditor,
            }

            impl <'h, T> TestEditor<'h, T>
            {
                fn step(mut self, step_num: u32) -> Self
                {
                    self.editor.step(step_num);
                    self
                }

                fn continue_to_next(mut self, cont: bool) -> Self
                {
                    self.editor.continue_to_next(cont);
                    self
                }

                $(impl_test_editor!{$test_type})+
            }
        }
    }
}

define_device!{
    model: sci_4520,
    files: 6,
    steps_per_file: 6,
    supported_tests: [
        ac_hipot {
            limits: AcHipotLimits {
                voltage_min: Volt::from_whole(1000),
                voltage_max: Volt::from_whole(5000),
                dwell_min: Duration::from_millis(500),
                dwell_max: Duration::from_millis(999_900),
                leak_current_min: Amp::from_whole(0),
                leak_current_max: Amp::from_micros(99_990),
                ramp_min: Duration::from_millis(100),
                ramp_max: Duration::from_millis(999_900),
            }
        },
        gnd_bond {
            limits: GndBondLimits {
                check_current_min: Amp::from_whole(3),
                check_current_max: Amp::from_whole(30),
                dwell_min: Duration::from_millis(500),
                dwell_max: Duration::from_millis(999_900),
                offset_min: Ohm::from_whole(0),
                offset_max: Ohm::from_millis(100),
                resistance_min: Ohm::from_millis(0),
                resistance_max: Ohm::from_millis(510),
            }
        }
    ]
}
/*
device
    .edit_test(1)
    .step(1)
    .gnd_bond(
        GndBondTest::new()
            .set_check_current(Amps::from_whole(25))
            .set_dwell(Duration::from_secs(3))
    )
    .continue_to_next(true)
    .step(2)
    .ac_hipot(
        AcHipotTest::new()
            .set_voltage(Volt::from_whole(1250))
            .set_ramp(Duration::from_millis(333))
            .set_dwell(Duration::from_secs(2))
    )
    .continue_to_next(false)
    .exec()
    .await?;

device
    .load_test(1)
    .step(1)
    .run_test()
    .exec()
    .await?;

device
    .get_result()
    .exec()
    .await?;
*/

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
