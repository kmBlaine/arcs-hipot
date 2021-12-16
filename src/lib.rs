//! **A**synchronous-Rust **R**emote **C**ontrol for **S**CI **Hipot** testers
use tokio::io::{ AsyncWriteExt, AsyncReadExt };
use std::{
    time::Duration,
    ops::{ Add, Sub },
    cmp::{ PartialEq, PartialOrd, Eq, Ord, Ordering },
    fmt,
    io,
};

pub mod test_data;
#[macro_use]
pub mod units;

use test_data::{ TestData, SciTestData, ParseTestDataErr };
use units::{ Ampere, Volt, Ohm, Second, Scalar, Milli, Micro, Base, Kilo };

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum AcFrequency
{
    Hz50,
    Hz60,
}

#[derive(Clone)]
enum GndBondParam
{
    Frequency(AcFrequency),
    DwellTime(Second),
    CheckCurrent(Ampere),
    ResistanceMax(Ohm),
    ResistanceMin(Ohm),
    Offset(Ohm),
}

#[derive(Clone)]
enum AcHipotParam
{
    Frequency(AcFrequency),
    DwellTime(Second),
    RampTime(Second),
    Voltage(Volt),
    LeakageMax(Ampere),
    LeakageMin(Ampere),
}

#[derive(Clone)]
enum CmdSet
{
    /// Ready a test file stored on the device for execution and editing
    ///
    /// Command: `FL <file_number>`
    LoadSequence(u32),
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
    SetAcHipotParam(AcHipotParam),
    SetGndBondParam(GndBondParam),
    /// Run the currently loaded test starting at the currently selected step
    RunTest,
    /// Reset the device to a ready-to-run state, cancelling any active test but preserving tests in
    /// memory.
    SoftReset,
    /// Gets the test data for the given step
    GetTestData(u32),
}

fn display_sci_core(cmd: &CmdSet, f: &mut fmt::Formatter<'_>) -> fmt::Result
{
    match cmd {
        CmdSet::SetAcHipot => write!(f, "SAA"),
        CmdSet::SetGndBond => write!(f, "SAG"),
        CmdSet::ContinueToNext(cont) => write!(f, "ECC {}", if *cont { '1' } else { '0' }),
        CmdSet::SetAcHipotParam(param) => match param {
            AcHipotParam::Frequency(frequency) => write!(
                f,
                "EF {}",
                match frequency {
                    AcFrequency::Hz50 => '0',
                    AcFrequency::Hz60 => '1',
                }
            ),
            AcHipotParam::DwellTime(seconds) => write!(f, "EDW {:.1}", view_anon!(seconds)),
            AcHipotParam::RampTime(seconds) => write!(f, "ERU {:.1}", view_anon!(seconds)),
            AcHipotParam::Voltage(voltage) => write!(f, "EV {}", view_anon!(voltage, Kilo)),
            AcHipotParam::LeakageMax(leak_current) => write!(f, "EH {:.1}", view_anon!(leak_current, Milli)),
            AcHipotParam::LeakageMin(leak_current) => write!(f, "EL {:.1}", view_anon!(leak_current, Milli)),
        },
        CmdSet::SetGndBondParam(param) => match param {
            GndBondParam::Frequency(frequency) => write!(
                f,
                "EF {}",
                match frequency {
                    AcFrequency::Hz50 => '0',
                    AcFrequency::Hz60 => '1',
                }
            ),
            GndBondParam::DwellTime(seconds) => write!(f, "EDW {:.1}", view_anon!(seconds)),
            GndBondParam::CheckCurrent(check_current) => write!(f, "EC {}", view_anon!(check_current)),
            GndBondParam::ResistanceMax(resistance) => write!(f, "EH {:.00}", view_anon!(resistance, Milli)),
            GndBondParam::ResistanceMin(resistance) => write!(f, "EL {:.00}", view_anon!(resistance, Milli)),
            GndBondParam::Offset(offset) => write!(f, "EO {:.00}", view_anon!(offset, Milli)),
        },
        CmdSet::RunTest => write!(f, "TEST"),
        CmdSet::SoftReset => write!(f, "RESET"),
        CmdSet::GetTestData(step_num) => write!(f, "RD {}?", step_num),
        CmdSet::LoadSequence(_) => panic!("`LoadSequence` command is not a stable command across devices"),
        CmdSet::SelectStep(_) => panic!("`SelectStep` command is not a stable command across devices"),
    }
}

struct SciMultiSeqDisplay
{
    cmd: CmdSet,
}

impl fmt::Display for SciMultiSeqDisplay
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self.cmd {
            CmdSet::LoadSequence(file_num) => write!(f, "FL {}", file_num),
            CmdSet::SelectStep(step_num) => write!(f, "SS {}", step_num),
            _ => display_sci_core(&self.cmd, f),
        }
    }
}

impl From<CmdSet> for SciMultiSeqDisplay
{
    fn from(cmd: CmdSet) -> Self
    {
        Self {
            cmd: cmd
        }
    }
}

struct SciSingleSeqDisplay
{
    cmd: CmdSet,
}

impl fmt::Display for SciSingleSeqDisplay
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self.cmd {
            CmdSet::LoadSequence(_) => panic!("`LoadSequence` is not supported on SCI single-sequence devices"),
            CmdSet::SelectStep(step_num) => write!(f, "FL {}", step_num),
            _ => display_sci_core(&self.cmd, f),
        }
    }
}

impl From<CmdSet> for SciSingleSeqDisplay
{
    fn from(cmd: CmdSet) -> Self
    {
        Self {
            cmd: cmd
        }
    }
}

struct AssociatedResearchDisplay
{
    cmd: CmdSet,
}

impl fmt::Display for AssociatedResearchDisplay
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self.cmd {
            CmdSet::LoadSequence(sequence_num) => write!(f, "S5 {}", sequence_num),
            CmdSet::SelectStep(step_num) => write!(f, "S6 {}", step_num),
            CmdSet::SetAcHipot => write!(f, "FC"),
            CmdSet::SetGndBond => write!(f, "FF"),
            CmdSet::ContinueToNext(cont) => if cont { write!(f, "FQ") } else { write!(f, "FR") },
            CmdSet::SetAcHipotParam(ref param) => match param {
                AcHipotParam::Frequency(frequency) => match frequency {
                    AcFrequency::Hz50 => write!(f, "FJ"),
                    AcFrequency::Hz60 => write!(f, "FI"),
                },
                AcHipotParam::DwellTime(seconds) => write!(f, "SE {:.1}", view_anon!(seconds)),// seconds.display_anon()),
                AcHipotParam::RampTime(seconds) => write!(f, "SD {:.1}", view_anon!(seconds)),
                AcHipotParam::Voltage(voltage) => write!(f, "SA {}", view_anon!(voltage, Kilo)),
                AcHipotParam::LeakageMax(leak_current) => write!(f, "SB {:.1}", view_anon!(leak_current, Milli)),
                AcHipotParam::LeakageMin(leak_current) => write!(f, "SC {:.1}", view_anon!(leak_current, Milli)),
            },
            CmdSet::SetGndBondParam(ref param) => match param {
                GndBondParam::Frequency(frequency) => match frequency {
                    AcFrequency::Hz50 => write!(f, "FP"),
                    AcFrequency::Hz60 => write!(f, "FO"),
                },
                GndBondParam::DwellTime(seconds) => write!(f, "S2 {:.1}", view_anon!(seconds)),
                GndBondParam::CheckCurrent(check_current) => write!(f, "SY {:.1}", view_anon!(check_current, Milli)),
                GndBondParam::ResistanceMax(resistance) => write!(f, "S0 {:.00}", view_anon!(resistance, Milli)),
                GndBondParam::ResistanceMin(resistance) => write!(f, "S1 {:.00}", view_anon!(resistance, Milli)),
                GndBondParam::Offset(offset) => write!(f, "S4 {:.01}", view_anon!(offset, Milli)),
            },
            CmdSet::RunTest => write!(f, "FA"),
            CmdSet::SoftReset => write!(f, "FB"),
            CmdSet::GetTestData(step_num) => write!(f, "?{}", step_num),
        }
    }
}

impl From<CmdSet> for AssociatedResearchDisplay
{
    fn from(cmd: CmdSet) -> Self
    {
        Self {
            cmd: cmd
        }
    }
}

trait CmdDisplayFactory: fmt::Display
{
    fn display_cmd(cmd: CmdSet) -> Self;
}

impl CmdDisplayFactory for SciSingleSeqDisplay
{
    fn display_cmd(cmd: CmdSet) -> Self
    {
        Self { cmd: cmd }
    }
}

impl CmdDisplayFactory for SciMultiSeqDisplay
{
    fn display_cmd(cmd: CmdSet) -> Self
    {
        Self { cmd: cmd }
    }
}

impl CmdDisplayFactory for AssociatedResearchDisplay
{
    fn display_cmd(cmd: CmdSet) -> Self
    {
        Self { cmd: cmd }
    }
}

struct Executor<T, D>
{
    line_ending: &'static str,
    io_handle: T,
    read_buf: Vec<u8>,
    // This marker exists so we can attach basically just a function using compiler generics which
    // creates the display delegate for the commands. There is no actual data or state to store
    _marker: std::marker::PhantomData<D>,
}

impl <T, D> Executor<T, D>
    where T: AsyncReadExt + AsyncWriteExt + Unpin + Send,
          D: CmdDisplayFactory
{
    fn with(line_ending: &'static str, io_handle: T) -> Self
    {
        Self {
            line_ending: line_ending,
            io_handle: io_handle,
            read_buf: Vec::with_capacity(128),
            _marker: std::marker::PhantomData,
        }
    }

    /// Drops the first `n` bytes from the read buffer
    ///
    /// Drops all bytes if `n >= self.read_buf.len()`
    fn drop_first(&mut self, n: usize)
    {
        if n >= self.read_buf.len() {
            self.read_buf.clear();
        }
        else {
            // relocate any bytes after the Nth byte to index 0
            self.read_buf.rotate_left(n);
            // chop off the bytes we just consumed
            self.read_buf.truncate(self.read_buf.len() - n);
            // shrink the buffer's allocation to keep memory usage down
            self.read_buf.shrink_to(128);
        }
    }

    /// Returns the index of the first linefeed in the read buffer if any attempting to start looking
    /// at the suggested index.
    ///
    /// If the suggested index is out of bounds, then `None` is returned.
    fn find_line_ending(&self, start_hint: usize) -> Option<usize>
    {
        for index in start_hint..self.read_buf.len() {
            if self.read_buf[index] == 0x0A {
                return Some(index);
            }
        }

        None
    }

    /// Reads a line (series of bytes terminated by `LF` / 0x0A) into the read buffer and returns
    /// how many bytes are in the line
    ///
    /// On Error all bytes buffered are destroyed
    ///
    /// # Cancel Safety
    /// This function is cancel safe e.g. when used inside of a `tokio::select!`. This is because
    /// this will never destroy contents of the read buffer -- only append.
    async fn read_line(&mut self) -> Result<usize, std::io::Error>
    {
        let mut total_bytes_read = 0;
        // try to find the ending in already-buffered data first
        let mut end_index = self.find_line_ending(0);

        while end_index.is_none() {
            // TODO this could avoid a double copy with some unsafe trickery
            // fully initialize the vector and then use slicing to write directly into it
            // however this requires manually setting the length field which is tricky to get right
            let mut temp_buf = [0u8; 64];

            match self.io_handle.read(&mut temp_buf[..]).await {
                Ok(bytes_read) => {
                    let prior_end = total_bytes_read;
                    total_bytes_read += bytes_read;
                    self.read_buf.extend_from_slice(&temp_buf[..bytes_read]);
                    end_index = self.find_line_ending(prior_end);
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }

        return Ok(end_index.unwrap() + 1);
    }

    /// Executes the given command, sending to the device and checking the response
    ///
    /// When the device responds with a positive ACK, `Ok` is returned. When an error occurrs on
    /// reading/writing to the stream, `Err` is returned. When the command is successfully written and
    /// the device responds with a negative ACK (NAK), an error is returned with an `ErrorKind`
    /// depending on the nature if the command. If the command asks the device set up a test it does not
    /// support -- e.g. a ground bond when it is a hipot-only tester -- then an I/O error with the kind
    /// `Unsupported` is returned. In all other NAK cases, `InvalidData` is returned.
    async fn exec_cmd(&mut self, cmd: CmdSet) -> Result<usize, io::Error>
    {
        let serialized = format!("{}{}", D::display_cmd(cmd), self.line_ending);
        self.io_handle.write_all(serialized.as_bytes()).await?;
        let response_len = self.read_line().await?;

        if response_len < 2 || self.read_buf[0] == 0x15 {
            self.drop_first(response_len);
            Err(io::Error::from(io::ErrorKind::InvalidInput))
        }
        else {
            Ok(response_len)
        }
    }

    async fn exec_all(&mut self, cmds: &[CmdSet]) -> Result<(), io::Error>
    {
        for cmd in cmds.iter() {
            let response_len = self.exec_cmd(cmd.clone()).await?;
            self.drop_first(response_len);
        }

        Ok(())
    }

    fn get_string(&mut self, size: usize) -> Result<String, std::string::FromUtf8Error>
    {
        let mut response = Vec::with_capacity(size);
        // These devices will reply with extended ASCII, specifically an omega 'Ω' for resistance values
        // thus we need to do some simple substitutions to turn into proper UTF8
        for byte in &self.read_buf[0..size] {
            if *byte == 0xEAu8 {
                let start_loc = response.len();
                let omega = 'Ω';
                response.resize(start_loc + omega.len_utf8(), 0);
                omega.encode_utf8(&mut response[start_loc..]);
            }
            else {
                response.push(*byte);
            }
        }
        self.drop_first(size);

        String::from_utf8(response)
    }

    async fn get_test_data<P>(
        &mut self,
        step_num: u32,
    )
        -> Result<P, ParseTestDataErr>

        where P: std::str::FromStr<Err = ParseTestDataErr>,
    {
        let response_len = self.exec_cmd(CmdSet::GetTestData(step_num)).await?;
        let response = self.get_string(response_len)?;
        let data = response.parse::<P>()?;
        
        Ok(data)
    }
}

struct SciSingleExecutor<T>
{
    delegate: Executor<T, SciSingleSeqDisplay>,
}

impl <T> SciSingleExecutor<T>
    where T: AsyncReadExt + AsyncWriteExt + Unpin + Send
{
    fn with(io_handle: T) -> Self
    {
        Self {
            delegate: Executor::with("\n", io_handle),
        }
    }

    async fn exec_cmd(&mut self, cmd: CmdSet) -> Result<usize, std::io::Error>
    {
        self.delegate.exec_cmd(cmd).await
    }

    async fn exec_all(&mut self, cmds: &[CmdSet]) -> Result<(), std::io::Error>
    {
        self.delegate.exec_all(cmds).await
    }

    async fn get_test_data(&mut self, step_num: u32) -> Result<TestData, ParseTestDataErr>
    {
        let data = self.delegate.get_test_data::<SciTestData>(step_num).await?;
        Ok(data.into())
    }
}

struct SciMultiExecutor<T>
{
    delegate: Executor<T, SciMultiSeqDisplay>,
}

impl <T> SciMultiExecutor<T>
    where T: AsyncReadExt + AsyncWriteExt + Unpin + Send
{
    fn with(io_handle: T) -> Self
    {
        Self {
            delegate: Executor::with("\n", io_handle),
        }
    }

    async fn exec_cmd(&mut self, cmd: CmdSet) -> Result<usize, std::io::Error>
    {
        self.delegate.exec_cmd(cmd).await
    }

    async fn exec_all(&mut self, cmds: &[CmdSet]) -> Result<(), std::io::Error>
    {
        self.delegate.exec_all(cmds).await
    }

    async fn get_test_data(&mut self, step_num: u32) -> Result<TestData, ParseTestDataErr>
    {
        let data = self.delegate.get_test_data::<SciTestData>(step_num).await?;
        Ok(data.into())
    }
}

struct ArExecutor<T>
{
    delegate: Executor<T, AssociatedResearchDisplay>,
}

impl <T> ArExecutor<T>
    where T: AsyncReadExt + AsyncWriteExt + Unpin + Send
{
    fn with(io_handle: T) -> Self
    {
        Self {
            delegate: Executor::with("\r\n", io_handle),
        }
    }

    async fn exec_cmd(&mut self, cmd: CmdSet) -> Result<usize, std::io::Error>
    {
        self.delegate.exec_cmd(cmd).await
    }

    async fn exec_all(&mut self, cmds: &[CmdSet]) -> Result<(), std::io::Error>
    {
        self.delegate.exec_all(cmds).await
    }

    async fn get_test_data(&mut self, _step_num: u32) -> Result<TestData, ParseTestDataErr>
    {
        unimplemented!("Parsing of Associated Research device memory is not complete")
    }
}

#[derive(Clone)]
pub struct AcHipotTestSpec
{
    voltage: Option<Volt>,
    dwell: Option<Second>,
    leak_current_min: Option<Ampere>,
    leak_current_max: Option<Ampere>,
    frequency: Option<AcFrequency>,
    ramp: Option<Second>,
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

    pub fn dwell_time(mut self, seconds: Second) -> Self
    {
        self.dwell = Some(seconds);
        self
    }

    pub fn leak_current_min(mut self, amps: Ampere) -> Self
    {
        self.leak_current_min = Some(amps);
        self
    }

    pub fn leak_current_max(mut self, amps: Ampere) -> Self
    {
        self.leak_current_max = Some(amps);
        self
    }

    pub fn ac_frequency(mut self, frequency: AcFrequency) -> Self
    {
        self.frequency = Some(frequency);
        self
    }

    pub fn ramp_time(mut self, seconds: Second) -> Self
    {
        self.ramp = Some(seconds);
        self
    }
}

#[derive(Clone)]
pub struct GndBondTestSpec
{
    check_current: Option<Ampere>,
    dwell: Option<Second>,
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

    pub fn check_current(mut self, amps: Ampere) -> Self
    {
        self.check_current = Some(amps);
        self
    }

    pub fn dwell_time(mut self, seconds: Second) -> Self
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

        let mut cmds = vec![CmdSet::LoadSequence(self.sequence_num)];

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
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::Voltage(voltage)));
                        }

                        if let Some(dwell) = params.dwell {
                            if dwell > ac_hipot_limits.dwell_max || dwell < ac_hipot_limits.dwell_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::DwellTime(dwell)));
                        }

                        if let Some(leak_current_min) = params.leak_current_min {
                            if leak_current_min > ac_hipot_limits.leak_current_max
                                || leak_current_min < ac_hipot_limits.leak_current_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::LeakageMin(leak_current_min)));
                        }

                        if let Some(leak_current_max) = params.leak_current_max {
                            if leak_current_max > ac_hipot_limits.leak_current_max
                                || leak_current_max < ac_hipot_limits.leak_current_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::LeakageMax(leak_current_max)));
                        }

                        if let Some(ramp) = params.ramp {
                            if ramp > ac_hipot_limits.ramp_max || ramp < ac_hipot_limits.ramp_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::RampTime(ramp)));
                        }

                        if let Some(frequency) = params.frequency {
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::Frequency(frequency)));
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
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::CheckCurrent(check_current)));
                        }

                        if let Some(dwell) = params.dwell {
                            if dwell > gnd_bond_limits.dwell_max || dwell < gnd_bond_limits.dwell_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::DwellTime(dwell)));
                        }

                        if let Some(resistance_min) = params.resistance_min {
                            if resistance_min > gnd_bond_limits.resistance_max
                                || resistance_min < gnd_bond_limits.resistance_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::ResistanceMin(resistance_min)));
                        }

                        if let Some(resistance_max) = params.resistance_max {
                            if resistance_max > gnd_bond_limits.resistance_max
                                || resistance_max < gnd_bond_limits.resistance_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::ResistanceMax(resistance_max)));
                        }

                        if let Some(offset) = params.offset {
                            if offset > gnd_bond_limits.offset_max || offset < gnd_bond_limits.offset_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::Offset(offset)));
                        }

                        if let Some(frequency) = params.frequency {
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::Frequency(frequency)));
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
    pub dwell_max: Second,
    pub dwell_min: Second,
    pub leak_current_max: Ampere,
    pub leak_current_min: Ampere,
    pub ramp_max: Second,
    pub ramp_min: Second,
}

pub struct GndBondDeviceLimits
{
    pub check_current_min: Ampere,
    pub check_current_max: Ampere,
    pub dwell_min: Second,
    pub dwell_max: Second,
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
            use super::{Ampere, Volt, Ohm, TestSupport, AcHipotDeviceLimits, GndBondDeviceLimits, AcHipotTestSpec, GndBondTestSpec,
                StepInfo, TestParams, CmdSet, Second, Milli, Kilo, Micro
            };
            use crate::test_data::{ TestData, ParseTestDataErr };
            use std::time::Duration;
            use tokio::io::{ AsyncReadExt, AsyncWriteExt };

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

#[cfg(test)]
mod tests {
    use super::{ Ampere, Ohm, Volt, CmdSet, AcFrequency, GndBondParam, AcHipotParam, SciSingleSeqDisplay, SciMultiSeqDisplay };
    use std::time::Duration;

    #[test]
    fn serialize_sci_core()
    {
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetAcHipot)), "SAA");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetGndBond)), "SAG");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::ContinueToNext(true))), "ECC 1");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::ContinueToNext(false))), "ECC 0");

        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::CheckCurrent(Amp::from_whole(25))))), "EC 25.0");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::DwellTime(Duration::from_millis(2_345))))), "EDW 2.3");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::Frequency(AcFrequency::Hz50)))), "EF 0");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::Frequency(AcFrequency::Hz60)))), "EF 1");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::ResistanceMax(Ohm::from_millis(128))))), "EH 128");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::ResistanceMin(Ohm::from_millis(0))))), "EL 0");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::Offset(Ohm::from_millis(56))))), "EO 56");

        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::Frequency(AcFrequency::Hz50)))), "EF 0");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::Frequency(AcFrequency::Hz60)))), "EF 1");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::LeakageMax(Amp::from_micros(67_800))))), "EH 67.8");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::LeakageMin(Amp::from_micros(0))))), "EL 0.0");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::RampTime(Duration::from_millis(4_321))))), "ERU 4.3");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::Voltage(Volt::from_whole(1250))))), "EV 1.25");
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::DwellTime(Duration::from_millis(2_345))))), "EDW 2.3");
    }

    #[test]
    fn serialize_sci_multi_seq()
    {
        assert_eq!(&format!("{}", SciMultiSeqDisplay::from(CmdSet::LoadSequence(3))), "FL 3");
        assert_eq!(&format!("{}", SciMultiSeqDisplay::from(CmdSet::SelectStep(1))), "SS 1");
    }

    #[test]
    fn serialize_sci_single_seq()
    {
        assert_eq!(&format!("{}", SciSingleSeqDisplay::from(CmdSet::SelectStep(1))), "FL 1");
    }

    #[test]
    #[should_panic]
    fn sci_single_load_seq_panics()
    {
        format!("{}", SciSingleSeqDisplay::from(CmdSet::LoadSequence(1)));
    }

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
