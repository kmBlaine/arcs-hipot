//! **A**synchronous-Rust **R**emote **C**ontrol for **S**CI **Hipot** testers
use tokio::io::{ AsyncWriteExt, AsyncReadExt };
use std::{
    time::Duration,
    ops::{ Add, Sub },
    cmp::{ PartialEq, PartialOrd, Eq, Ord, Ordering },
    fmt,
    io,
};

/// Decimal number with fractional billionths (10e-9)
///
/// **Justification:**
///
/// While there are tons of scientific, rational number, arbitrary precision, fixed precision, and
/// other numerics libraries for Rust, most of them are inappropriate or complete overkill for what
/// I'm trying to do here. Basically, I need fine-grained control over parsing and formatting,
/// type safety, lossless numbers with micro (10e-6) precision, and strictly real numbers.
/// Uom is about the closest any library comes to this but it does all of it's arithmetic in base
/// units, meaning if you select integers as the underlying storage, you don't get even close to
/// the level of precision I need in many cases. And if you select floats, then you're not lossless
/// nor strictly real anymore.
#[derive(Debug, Clone, Copy)]
struct DecimalNano
{
    whole: u32,
    nanos: u32,
}

impl DecimalNano
{
    fn is_zero(&self) -> bool
    {
        self.whole == 0 && self.nanos == 0
    }

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

    /// Displays the value with a floating decimal point and certain level of precision
    ///
    /// The position of the decimal point is specified with a power of ten and the precision is
    /// given by a number places after the decimal point to display. Truncates unused digits
    fn display_scalar(&self, magnitude: i32, decimal_pts: u32) -> DecimalNanoDisplay
    {
        DecimalNanoDisplay {
            whole: self.whole,
            nanos: self.nanos,
            power_10: magnitude,
            decimal_pts: decimal_pts,
        }
    }

    fn display(&self, decimal_pts: u32) -> DecimalNanoDisplay
    {
        self.display_scalar(0, decimal_pts)
    }

    fn display_milli(&self, decimal_pts: u32) -> DecimalNanoDisplay
    {
        self.display_scalar(-3, decimal_pts)
    }

    fn display_kilo(&self, decimal_pts: u32) -> DecimalNanoDisplay
    {
        self.display_scalar(3, decimal_pts)
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

pub struct DecimalNanoDisplay
{
    whole: u32,
    nanos: u32,
    decimal_pts: u32,
    power_10: i32,
}

impl fmt::Display for DecimalNanoDisplay
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        let decimal_index = (10 - self.power_10) as usize;
        let mut divisor = 1_000_000_000;
        // convert to binary coded decimal
        // this can then be converted directly to a UTF8 string without checks
        // avoids double allocation of a vector by building and then cloning
        let mut bcd = [0u8; 20]; // 10 bytes for the whole, 9 for the nanos, 1 decimal point
        let mut dividend = self.whole;

        for index in 0..10 {
            let value = (dividend / divisor) as u8 | 0x30;

            if index >= decimal_index {
                if index == decimal_index {
                    bcd[index] = 0x2E;
                }
                bcd[index + 1] = value;
            }
            else {
                bcd[index] = value;
            }
            
            dividend = dividend % divisor;
            divisor /= 10;
        }

        dividend = self.nanos;
        divisor = 100_000_000;

        for index in 10..19 {
            let value = (dividend / divisor) as u8 | 0x30;

            if index >= decimal_index {
                if index == decimal_index {
                    bcd[index] = 0x2E;
                }
                bcd[index + 1] = value;
            }
            else {
                bcd[index] = value;
            }

            dividend = dividend % divisor;
            divisor /= 10;
        }

        // default the start index to on before the decimal place
        // we will find if there are any others in there later
        let mut start_index = decimal_index - 1;
        let final_index = if self.decimal_pts == 0 {
            decimal_index
        }
        else {
            decimal_index + 1 + self.decimal_pts as usize
        };

        // find the first nonzero item
        for index in 0..decimal_index {
            if bcd[index] > 0x30 {
                start_index = index;
                break;
            }
        }

        // This should be safe since any number less than ten plus 0x30 is a valid UTF8/ASCII arabic numeral
        // If the indices are negative or beyond the end of the array, then a panic will be triggered from attempting
        // to slice out of bounds. No undefined behavior should be possible.
        let value_as_string = unsafe { String::from_utf8_unchecked(bcd[start_index..final_index].to_vec()) };

        write!(f, "{}", value_as_string)
    }
}

macro_rules! unit
{
    ($u:ident) => {
        #[derive(Debug, Clone, Copy)]
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

            pub fn display_milli(&self, decimal_pts: u32) -> DecimalNanoDisplay
            {
                self.value.display_milli(decimal_pts)
            }

            pub fn display_kilo(&self, decimal_pts: u32) -> DecimalNanoDisplay
            {
                self.value.display_kilo(decimal_pts)
            }

            pub fn display(&self, decimal_pts: u32) -> DecimalNanoDisplay
            {
                self.value.display(decimal_pts)
            }

            pub fn whole(&self) -> u32
            {
                self.value.whole
            }

            pub fn fraction(&self) -> u32
            {
                self.value.nanos
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum AcFrequency
{
    Hz50,
    Hz60,
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
    /// On the currently selected step, set the maximum allowble leakage current when running an AC
    /// or DC hipot test. Tenth-milliamp precision.
    ///
    /// Command: `EH <milliamp>`
    SetLeakCurrentHiLimit(Amp),
    /// On the currently selected step, set the minimum allowable leakage current when running an AC
    /// or DC hipot test. Tenth-milliamp precison.
    ///
    /// Command: `EL <milliamp>`
    SetLeakCurrentLoLimit(Amp),
    /// On the currently selected step, set the maximum allowable ground line resistance when
    /// running a ground bond test.
    ///
    /// Command: `EH <milliohm>`
    SetResistanceHiLimit(Ohm),
    /// On the currently selected step, set the minimum allowable ground line resistance when
    /// running a ground bond test.
    ///
    /// Command: `EH <milliohm>`
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

impl fmt::Display for CmdSet
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self {
            Self::LoadFile(file_num) => write!(f, "FL {}", file_num),
            Self::SelectStep(step_num) => write!(f, "SS {}", step_num),
            Self::SetAcHipot => write!(f, "SAA"),
            Self::SetGndBond => write!(f, "SAG"),
            Self::ContinueToNext(cont) => write!(f, "ECC {}", if *cont { '1' } else { '0' }),
            Self::SetGndCheckCurrent(check_current) => write!(f, "EC {}", check_current.display(1)),
            Self::SetDwell(seconds) => write!(f, "EDW {}.{}", seconds.as_secs(), seconds.subsec_millis() / 100),
            Self::SetAcFrequency(frequency) => {
                write!(
                    f,
                    "EF {}",
                    match frequency {
                        AcFrequency::Hz50 => '0',
                        AcFrequency::Hz60 => '1',
                    }
                )
            },
            Self::SetLeakCurrentHiLimit(leak_current) => write!(f, "EH {}", leak_current.value.display_milli(1)),
            Self::SetLeakCurrentLoLimit(leak_current) => write!(f, "EL {}", leak_current.value.display_milli(1)),
            Self::SetResistanceHiLimit(resistance) => write!(f, "EH {}", resistance.value.display_milli(0)),
            Self::SetResistanceLoLimit(resistance) => write!(f, "EL {}", resistance.value.display_milli(0)),
            Self::SetGndOffset(offset) => write!(f, "EO {}", offset.value.display_milli(0)),
            Self::SetRampTime(seconds) => write!(f, "ERU {}.{}", seconds.as_secs(), seconds.subsec_millis() / 100),
            Self::SetHipotVoltage(voltage) => write!(f, "EV {}", voltage.value.display_kilo(2)),
        }
    }
}

/// Executes the given command, sending to the device and checking the response
///
/// When the device responds with a positive ACK, `Ok` is returned. When an error occurrs on
/// reading/writing to the stream, `Err` is returned. When the command is successfully written and
/// the device responds with a negative ACK (NAK), an error is returned with an `ErrorKind`
/// depending on the nature if the command. If the command asks the device set up a test it does not
/// support -- e.g. a ground bond when it is a hipot-only tester -- then an I/O error with the kind
/// `Unsupported` is returned. In all other NAK cases, `InvalidData` is returned.
async fn exec_cmd<T>(io_handle: &mut T, cmd: &CmdSet) -> Result<(), io::Error>
    where T: AsyncReadExt + AsyncWriteExt + Unpin + Send
{
    let serialized = format!("{}\n", cmd);
    io_handle.write_all(serialized.as_bytes()).await?;
    let response = io_handle.read_u16().await?;

    // TODO this is a quick hack to work around unknown endianness behavior and lack of buffering
    // This really should be replaced with a read of exactly 2 bytes and a buffer comparison
    if response != 0x060A && response != 0x0A06 {
        Err(io::Error::from(io::ErrorKind::InvalidInput))
    }
    else {
        Ok(())
    }
}

async fn exec_all<T>(io_handle: &mut T, cmds: &[CmdSet]) -> Result<(), io::Error>
    where T: AsyncReadExt + AsyncWriteExt + Unpin + Send
{
    for cmd in cmds.iter() {
        exec_cmd(io_handle, cmd).await?;
    }

    Ok(())
}

#[derive(Clone)]
pub struct AcHipotTestSpec
{
    voltage: Option<Volt>,
    dwell: Option<Duration>,
    leak_current_min: Option<Amp>,
    leak_current_max: Option<Amp>,
    frequency: Option<AcFrequency>,
    ramp: Option<Duration>,
}

impl AcHipotTestSpec
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

#[derive(Clone)]
pub struct GndBondTestSpec
{
    check_current: Option<Amp>,
    dwell: Option<Duration>,
    resistance_max: Option<Ohm>,
    resistance_min: Option<Ohm>,
    frequency: Option<AcFrequency>,
    offset: Option<Ohm>,
}

impl GndBondTestSpec
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

    pub fn resistance_max(mut self, ohms: Ohm) -> Self
    {
        self.resistance_max = Some(ohms);
        self
    }

    pub fn resistance_min(mut self, ohms: Ohm) -> Self
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

#[derive(Clone)]
enum TestParams
{
    AcHipot(AcHipotTestSpec),
    GndBond(GndBondTestSpec)
}

#[derive(Clone)]
struct StepInfo
{
    step_num: u32,
    params: Option<TestParams>,
    continue_to_next: Option<bool>,
}

struct TestEditor
{
    sequence_num: u32,
    selected_step: u32,
}

impl TestEditor
{
    fn edit_sequence(sequence_num: u32) -> Self
    {
        Self {
            sequence_num: sequence_num,
            selected_step: 1,
        }
    }

    fn step(&mut self, step_num: u32)
    {
        self.selected_step = step_num;
    }

    fn test_spec(&self, step_buf: &mut [Option<StepInfo>], params: TestParams)
    {
        let index = self.selected_step as usize - 1;

        if index >= step_buf.len() {
            return;
        }
        
        if let Some(step_info) = &mut step_buf[index] {
            step_info.params = Some(params);
        }
        else {
            step_buf[index] = Some(StepInfo {
                step_num: self.selected_step,
                params: Some(params),
                continue_to_next: None,
            })
        }
    }

    fn continue_to_next(&self, step_buf: &mut [Option<StepInfo>], cont: bool)
    {
        let index = self.selected_step as usize - 1;

        if index >= step_buf.len() {
            return;
        }
        
        if let Some(step_info) = &mut step_buf[index] {
            step_info.continue_to_next = Some(cont);
        }
        else {
            step_buf[index] = Some(StepInfo {
                step_num: self.selected_step,
                params: None,
                continue_to_next: Some(cont),
            })
        }
    }

    fn compile(
        self,
        step_buf: &[Option<StepInfo>],
        ac_hipot_limits: Option<AcHipotDeviceLimits>,
        gnd_bond_limits: Option<GndBondDeviceLimits>,
        max_file_num: u32,
        max_step_num: u32,
    )
        -> Option<Vec<CmdSet>>
    {
        if self.sequence_num > max_file_num {
            return None;
        }

        let mut cmds = vec![CmdSet::LoadFile(self.sequence_num)];

        for step in step_buf {
            if step.is_none() {
                continue;
            }
            let step = step.as_ref().unwrap();

            if step.step_num > max_step_num {
                return None;
            }

            // Steps are only committed by the builder when an override of either the continue_to_next
            // or test type/parameters occurs, so there is no need to delay adding a step selection
            // command
            cmds.push(CmdSet::SelectStep(step.step_num));

            if let Some(overrides) = &step.params {
                match overrides {
                    TestParams::AcHipot(params) => {
                        // devices which don't support AC hipot won't have an AC hipot builder exposed
                        // thus we can't ever get an AC hipot step
                        let ac_hipot_limits = ac_hipot_limits.as_ref().unwrap();
                        cmds.push(CmdSet::SetAcHipot);

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
                            cmds.push(CmdSet::SetLeakCurrentLoLimit(leak_current_min));
                        }

                        if let Some(leak_current_max) = params.leak_current_max {
                            if leak_current_max > ac_hipot_limits.leak_current_max
                                || leak_current_max < ac_hipot_limits.leak_current_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetLeakCurrentHiLimit(leak_current_max));
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
                        let gnd_bond_limits = gnd_bond_limits.as_ref().unwrap();
                        cmds.push(CmdSet::SetGndBond);

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

pub struct AcHipotDeviceLimits
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

pub struct GndBondDeviceLimits
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
    {spawn_multisequence_editor: No} => {
        pub fn edit_sequence<'h>(&'h mut self) -> TestEditor<'h, T>
        {
            TestEditor::new(self, 1)
        }
    };
    {spawn_multisequence_editor: Yes} => {
        pub fn edit_sequence<'h>(&'h mut self, sequence_num: u32) -> TestEditor<'h, T>
        {
            TestEditor::new(self, sequence_num)
        }
    };
    {sequence_count: No} => {
        pub fn sequences(&self) -> u32
        {
            1
        }
    };
    {sequence_count: Yes($steps:literal)} => {
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
    { model: $dev:ident, multi_sequence: $qualifier:ident $($sequences:tt)?, steps_per_sequence: $steps:literal, supported_tests: [$($test_type:ident {limits: $limit_type:ident $lims:tt}),+] } => {
        mod $dev {
            use super::{Amp, Volt, Ohm, TestSupport, AcHipotDeviceLimits, GndBondDeviceLimits, AcHipotTestSpec, GndBondTestSpec,
                exec_all, StepInfo, TestParams
            };
            use std::time::Duration;
            use tokio::io::{ AsyncReadExt, AsyncWriteExt };

            pub struct Device<T>
            {
                io_handle: T,
            }

            impl <T> Device<T>
            {
                impl_device!{sequence_count: $qualifier$($sequences)*}
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
                pub fn with(io_handle: T) -> Self
                {
                    Device {
                        io_handle: io_handle,
                    }
                }

                impl_device!{spawn_multisequence_editor: $qualifier}
                // pub fn edit_test<'h>(&'h mut self, file_num: u32) -> TestEditor<'h, T>
                // {
                //     TestEditor {
                //         dev: self,
                //         editor: super::TestEditor::edit_file(file_num),
                //     }
                // }

                // pub fn print_screen(&mut self) -> Result<String, std::io::Error>
                // {
                //     self.io_handle.write_all("TD?".as_bytes()).await?;
                //     let mut buf = 
                // }
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
                editor: super::TestEditor,
                steps: [Option<StepInfo>; $steps],
            }

            impl <'h, T> TestEditor<'h, T>
                where T: AsyncReadExt + AsyncWriteExt + Unpin + Send,
            {
                fn new(dev: &'h mut Device<T>, sequence_num: u32) -> Self
                {
                    Self {
                        dev: dev,
                        editor: super::TestEditor::edit_sequence(sequence_num),
                        // TODO instatiate this array differently
                        // This only works for a limited number of values. I think up to 32 due limitations in the Rust compiler
                        // And I don't want to implement Copy for the StepInfo type to be able to use [expr; size] notation
                        // It's quite a sizeable type
                        steps: Default::default(),
                    }
                }

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
                    
                    exec_all(&mut self.dev.io_handle, &cmds).await
                }

                $(impl_test_editor!{$test_type})+
            }
        }
    }
}

define_device!{
    model: sci_4520,
    multi_sequence: Yes(6),
    steps_per_sequence: 6,
    supported_tests: [
        ac_hipot_test {
            limits: AcHipotDeviceLimits {
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
        gnd_bond_test {
            limits: GndBondDeviceLimits {
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

pub use sci_4520::{ Device as Sci4520, TestEditor as Sci4520TestEditor };
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
    use super::{ Amp, Ohm, Volt, CmdSet, AcFrequency};
    use std::time::Duration;

    #[test]
    fn display_amp_subscalar_normal()
    {
        assert_eq!(&format!("{}", Amp::from_micros(45_600).display_milli(1)), "45.6");
    }

    #[test]
    fn display_amp_subscalar_subnormal()
    {
        assert_eq!(&format!("{}", Amp::from_parts(0, 000_012_300).display_milli(4)), "0.0123");
    }

    #[test]
    fn display_amp_subscalar_zero_with_pt()
    {
        assert_eq!(&format!("{}", Amp::from_micros(0).display_milli(2)), "0.00");
    }

    #[test]
    fn display_amp_subscalar_zero_no_pt()
    {
        assert_eq!(&format!("{}", Amp::from_micros(0).display_milli(0)), "0");
    }

    #[test]
    fn display_amp_normal()
    {
        assert_eq!(&format!("{}", Amp::from_millis(45_600).display(1)), "45.6");
    }

    #[test]
    fn display_amp_subnormal()
    {
        assert_eq!(&format!("{}", Amp::from_parts(0, 012_300_000).display(3)), "0.012");
    }

    #[test]
    fn display_amp_zero_with_pt()
    {
        assert_eq!(&format!("{}", Amp::from_micros(0).display(2)), "0.00");
    }

    #[test]
    fn display_amp_zero_no_pt()
    {
        assert_eq!(&format!("{}", Amp::from_micros(0).display(0)), "0");
    }

    #[test]
    fn display_amp_superscalar_normal()
    {
        assert_eq!(&format!("{}", Amp::from_whole(7890).display_kilo(2)), "7.89");
    }

    #[test]
    fn display_amp_superscalar_subnormal()
    {
        assert_eq!(&format!("{}", Amp::from_whole(123).display_kilo(3)), "0.123");
    }

    #[test]
    fn display_amp_superscalar_zero_with_pt()
    {
        assert_eq!(&format!("{}", Amp::from_whole(0).display_kilo(2)), "0.00");
    }

    #[test]
    fn display_amp_superscalar_zero_no_pt()
    {
        assert_eq!(&format!("{}", Amp::from_whole(0).display_kilo(0)), "0");
    }

    #[test]
    fn decimal_normalizes()
    {
        assert_eq!(Amp::from_parts(0, 1_100_000_000), Amp::from_parts(1, 100_000_000));
    }

    #[test]
    fn serialize_cmd_set()
    {
        assert_eq!(&format!("{}", CmdSet::LoadFile(3)), "FL 3");
        assert_eq!(&format!("{}", CmdSet::SelectStep(1)), "SS 1");
        assert_eq!(&format!("{}", CmdSet::SetAcHipot,), "SAA");
        assert_eq!(&format!("{}", CmdSet::SetGndBond,), "SAG");
        assert_eq!(&format!("{}", CmdSet::ContinueToNext(true)), "ECC 1");
        assert_eq!(&format!("{}", CmdSet::ContinueToNext(false)), "ECC 0");
        assert_eq!(&format!("{}", CmdSet::SetGndCheckCurrent(Amp::from_whole(25))), "EC 25.0");
        assert_eq!(&format!("{}", CmdSet::SetDwell(Duration::from_millis(2_345))), "EDW 2.3");
        assert_eq!(&format!("{}", CmdSet::SetAcFrequency(AcFrequency::Hz50)), "EF 0");
        assert_eq!(&format!("{}", CmdSet::SetAcFrequency(AcFrequency::Hz60)), "EF 1");
        assert_eq!(&format!("{}", CmdSet::SetLeakCurrentHiLimit(Amp::from_micros(67_800))), "EH 67.8");
        assert_eq!(&format!("{}", CmdSet::SetLeakCurrentLoLimit(Amp::from_micros(0))), "EL 0.0");
        assert_eq!(&format!("{}", CmdSet::SetResistanceHiLimit(Ohm::from_millis(128))), "EH 128");
        assert_eq!(&format!("{}", CmdSet::SetResistanceLoLimit(Ohm::from_millis(0))), "EL 0");
        assert_eq!(&format!("{}", CmdSet::SetGndOffset(Ohm::from_millis(56))), "EO 56");
        assert_eq!(&format!("{}", CmdSet::SetRampTime(Duration::from_millis(4_321))), "ERU 4.3");
        assert_eq!(&format!("{}", CmdSet::SetHipotVoltage(Volt::from_whole(1250))), "EV 1.25");
    }

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
