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

enum GndBondParam
{
    Frequency(AcFrequency),
    DwellTime(Duration),
    CheckCurrent(Amp),
    ResistanceMax(Ohm),
    ResistanceMin(Ohm),
    Offset(Ohm),
}

enum AcHipotParam
{
    Frequency(AcFrequency),
    DwellTime(Duration),
    RampTime(Duration),
    Voltage(Volt),
    LeakageMax(Amp),
    LeakageMin(Amp),
}

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

impl CmdSet
{
    fn display_sci_multi<'a>(&'a self) -> SciMultiSeqDisplay<'a>
    {
        SciMultiSeqDisplay::new(self)
    }

    fn display_sci_single<'a>(&'a self) -> SciSingleSeqDisplay<'a>
    {
        SciSingleSeqDisplay::new(self)
    }

    fn display_ar<'a>(&'a self) -> AssociatedResearchDisplay<'a>
    {
        AssociatedResearchDisplay::new(self)
    }
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
            AcHipotParam::DwellTime(seconds) => write!(f, "EDW {}.{}", seconds.as_secs(), seconds.subsec_millis() / 100),
            AcHipotParam::RampTime(seconds) => write!(f, "ERU {}.{}", seconds.as_secs(), seconds.subsec_millis() / 100),
            AcHipotParam::Voltage(voltage) => write!(f, "EV {}", voltage.value.display_kilo(2)),
            AcHipotParam::LeakageMax(leak_current) => write!(f, "EH {}", leak_current.value.display_milli(1)),
            AcHipotParam::LeakageMin(leak_current) => write!(f, "EL {}", leak_current.value.display_milli(1)),
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
            GndBondParam::DwellTime(seconds) => write!(f, "EDW {}.{}", seconds.as_secs(), seconds.subsec_millis() / 100),
            GndBondParam::CheckCurrent(check_current) => write!(f, "EC {}", check_current.display(1)),
            GndBondParam::ResistanceMax(resistance) => write!(f, "EH {}", resistance.value.display_milli(0)),
            GndBondParam::ResistanceMin(resistance) => write!(f, "EL {}", resistance.value.display_milli(0)),
            GndBondParam::Offset(offset) => write!(f, "EO {}", offset.value.display_milli(0)),
        },
        CmdSet::RunTest => write!(f, "TEST"),
        CmdSet::SoftReset => write!(f, "RESET"),
        CmdSet::GetTestData(step_num) => write!(f, "RD {}?", step_num),
        CmdSet::LoadSequence(_) => panic!("`LoadSequence` command is not a stable command across devices"),
        CmdSet::SelectStep(_) => panic!("`SelectStep` command is not a stable command across devices"),
    }
}

struct SciMultiSeqDisplay<'a>
{
    cmd: &'a CmdSet,
}

impl <'a> SciMultiSeqDisplay<'a>
{
    fn new(cmd: &'a CmdSet) -> Self
    {
        Self {
            cmd: cmd
        }
    }
}

impl <'a> fmt::Display for SciMultiSeqDisplay<'a>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self.cmd {
            CmdSet::LoadSequence(file_num) => write!(f, "FL {}", file_num),
            CmdSet::SelectStep(step_num) => write!(f, "SS {}", step_num),
            _ => display_sci_core(self.cmd, f),
        }
    }
}

struct SciSingleSeqDisplay<'a>
{
    cmd: &'a CmdSet,
}

impl <'a> SciSingleSeqDisplay<'a>
{
    fn new(cmd: &'a CmdSet) -> Self
    {
        Self {
            cmd: cmd
        }
    }
}

impl <'a> fmt::Display for SciSingleSeqDisplay<'a>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self.cmd {
            CmdSet::LoadSequence(file_num) => panic!("`LoadSequence` is not supported on SCI single-sequence devices"),
            CmdSet::SelectStep(step_num) => write!(f, "FL {}", step_num),
            _ => display_sci_core(self.cmd, f),
        }
    }
}

struct AssociatedResearchDisplay<'a>
{
    cmd: &'a CmdSet,
}

impl <'a> AssociatedResearchDisplay<'a>
{
    fn new(cmd: &'a CmdSet) -> Self
    {
        Self {
            cmd: cmd
        }
    }
}

impl <'a> fmt::Display for AssociatedResearchDisplay<'a>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self.cmd {
            CmdSet::LoadSequence(sequence_num) => write!(f, "S5 {}", sequence_num),
            CmdSet::SelectStep(step_num) => write!(f, "S6 {}", step_num),
            CmdSet::SetAcHipot => write!(f, "FC"),
            CmdSet::SetGndBond => write!(f, "FF"),
            CmdSet::ContinueToNext(cont) => if *cont { write!(f, "FQ") } else { write!(f, "FR") },
            CmdSet::SetAcHipotParam(param) => match param {
                AcHipotParam::Frequency(frequency) => match frequency {
                    AcFrequency::Hz50 => write!(f, "FJ"),
                    AcFrequency::Hz60 => write!(f, "FI"),
                },
                AcHipotParam::DwellTime(seconds) => write!(f, "SE {}.{}", seconds.as_secs(), seconds.subsec_millis() / 100),
                AcHipotParam::RampTime(seconds) => write!(f, "SD {}.{}", seconds.as_secs(), seconds.subsec_millis() / 100),
                AcHipotParam::Voltage(voltage) => write!(f, "SA {}", voltage.value.display_kilo(2)),
                AcHipotParam::LeakageMax(leak_current) => write!(f, "SB {}", leak_current.value.display_milli(1)),
                AcHipotParam::LeakageMin(leak_current) => write!(f, "SC {}", leak_current.value.display_milli(1)),
            },
            CmdSet::SetGndBondParam(param) => match param {
                GndBondParam::Frequency(frequency) => match frequency {
                    AcFrequency::Hz50 => write!(f, "FP"),
                    AcFrequency::Hz60 => write!(f, "FO"),
                },
                GndBondParam::DwellTime(seconds) => write!(f, "S2 {}.{}", seconds.as_secs(), seconds.subsec_millis() / 100),
                GndBondParam::CheckCurrent(check_current) => write!(f, "SY {}", check_current.display(1)),
                GndBondParam::ResistanceMax(resistance) => write!(f, "S0 {}", resistance.value.display_milli(0)),
                GndBondParam::ResistanceMin(resistance) => write!(f, "S1 {}", resistance.value.display_milli(0)),
                GndBondParam::Offset(offset) => write!(f, "S4 {}", offset.value.display_milli(0)),
            },
            CmdSet::RunTest => write!(f, "FA"),
            CmdSet::SoftReset => write!(f, "FB"),
            CmdSet::GetTestData(step_num) => write!(f, "?{}", step_num),
        }
    }
}

struct Executor<T>
{
    line_ending: &'static str,
    io_handle: T,
    read_buf: Vec<u8>,
}

impl <T> Executor<T>
    where T: AsyncReadExt + AsyncWriteExt + Unpin + Send
{
    fn with(line_ending: &'static str, io_handle: T) -> Self
    {
        Self {
            line_ending: line_ending,
            io_handle: io_handle,
            read_buf: Vec::with_capacity(128),
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
                    total_bytes_read += bytes_read;
                    self.read_buf.extend_from_slice(&temp_buf[..bytes_read]);
                    end_index = self.find_line_ending(total_bytes_read);
                }
                Err(err) => {
                    self.read_buf.clear();
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
    async fn exec_cmd<'a, D>(&mut self, cmd: &'a CmdSet, display_func: fn(&'a CmdSet) -> D) -> Result<usize, io::Error>
        where D: fmt::Display,
    {
        let serialized = format!("{}{}", display_func(cmd), self.line_ending);
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

    async fn exec_all<'a, D>(&mut self, cmds: &'a [CmdSet], display_func: fn(&'a CmdSet) -> D) -> Result<(), io::Error>
        where D: fmt::Display
    {
        for cmd in cmds.iter() {
            let response_len = self.exec_cmd(cmd, display_func).await?;
            self.drop_first(response_len);
        }

        Ok(())
    }
}

struct SciSingleExecutor<T>
{
    delegate: Executor<T>,
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

    async fn exec_cmd(&mut self, cmd: &CmdSet) -> Result<usize, std::io::Error>
    {
        self.delegate.exec_cmd(cmd, CmdSet::display_sci_single).await
    }

    async fn exec_all(&mut self, cmds: &[CmdSet]) -> Result<(), std::io::Error>
    {
        self.delegate.exec_all(cmds, CmdSet::display_sci_single).await
    }
}

struct SciMultiExecutor<T>
{
    delegate: Executor<T>,
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

    async fn exec_cmd(&mut self, cmd: &CmdSet) -> Result<usize, std::io::Error>
    {
        self.delegate.exec_cmd(cmd, CmdSet::display_sci_multi).await
    }

    async fn exec_all(&mut self, cmds: &[CmdSet]) -> Result<(), std::io::Error>
    {
        self.delegate.exec_all(cmds, CmdSet::display_sci_multi).await
    }
}

struct ArExecutor<T>
{
    delegate: Executor<T>,
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

    async fn exec_cmd(&mut self, cmd: &CmdSet) -> Result<usize, std::io::Error>
    {
        self.delegate.exec_cmd(cmd, CmdSet::display_ar).await
    }

    async fn exec_all(&mut self, cmds: &[CmdSet]) -> Result<(), std::io::Error>
    {
        self.delegate.exec_all(cmds, CmdSet::display_ar).await
    }
}


pub enum AcHipotOutcome
{
    /// A short circuit was detected
    ///
    /// Technically, this only means that the leakage current was so excessive it could not be metered
    /// however this is usually due to a short circuit.
    ShortCircuit,
    /// The leakage current exceeded the maximum acceptable limit
    ///
    /// This implies that the current fell within metering range
    LeakExcessive(Amp),
    /// The leakage current fell below the minimum acceptable limit
    LeakSubnormal(Amp),
    /// Operator aborted the test on the instrument UI
    Aborted,
    /// The leakage current was within acceptable limits
    Passed(Amp),
}

impl AcHipotOutcome
{
    fn passed(&self) -> bool
    {
        match self {
            Self::Passed(_) => true,
            _ => false,
        }
    }
}

pub struct AcHipotData
{
    pub sequence_num: u32,
    pub step_num: u32,
    pub outcome: AcHipotOutcome,
}

pub enum GndBondOutcome
{
    ResistanceExcessive(Ohm),
    ResistanceSubnormal(Ohm),
    Aborted,
    Passed(Ohm),
}

pub struct GndBondData
{
    pub sequence_num: u32,
    pub step_num: u32,
    pub outcome: GndBondOutcome
}

#[derive(Debug)]
pub struct ParseTestStatusErr {}

impl fmt::Display for ParseTestStatusErr
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "Unrecognized status. Acceptable test statuses are ['OFL', 'HI-Lmt', 'LO-Lmt', 'Ramp', 'Dwell', 'Abort', 'Pass'] (case sensitive)")
    }
}

impl std::error::Error for ParseTestStatusErr {}

#[derive(Debug, Clone, Copy)]
pub enum SciTestStatus
{
    /// "OFL"
    Overflow,
    /// "HI-Lmt"
    HiLimit,
    /// "LO-Lmt"
    LoLimit,
    /// "Ramp"
    Ramp,
    /// "Dwell"
    Dwell,
    /// "Abort"
    Abort,
    /// "Pass"
    Pass,
}

impl std::str::FromStr for SciTestStatus
{
    type Err = ParseTestStatusErr;

    fn from_str(status_str: &str) -> Result<Self, Self::Err>
    {
        match status_str {
            "OFL" => Ok(Self::Overflow),
            "HI-Lmt" => Ok(Self::HiLimit),
            "LO-Lmt" => Ok(Self::LoLimit),
            "Ramp" => Ok(Self::Ramp),
            "Dwell" => Ok(Self::Dwell),
            "Abort" => Ok(Self::Abort),
            "Pass" => Ok(Self::Pass),
            _ => Err(ParseTestStatusErr{})
        }
    }
}

struct SciGndBondData
{
    data: GndBondData
}

impl From<SciGndBondData> for GndBondData
{
    fn from(this: SciGndBondData) -> Self
    {
        this.data
    }
}

#[derive(Debug)]
pub enum ParseGndBondDataErr
{
    /// The device did not send back a complete response
    ResponseTooShort,
    /// The test type token was not "GND"
    ///
    /// This could mean the token is gibberish or that the result is for a test other than a ground
    /// bond.
    InvalidTestType,
    /// The test status was not one of Pass, Abort, HiLimit, or LoLimit
    ///
    /// For example, if the test is incomplete due to being in the ramp or dwell phases, this error
    /// will be returned.
    UnexpectedStatus(SciTestStatus),
    /// Invalid status token encountered when attempting to parse the test status from the string
    InvalidStatus(ParseTestStatusErr),
    /// Invalid data encountered when attempting to parse the ground memory location or resistance
    /// value
    InvalidData(std::num::ParseIntError),
}

impl From<ParseTestStatusErr> for ParseGndBondDataErr
{
    fn from(parse_status_err: ParseTestStatusErr) -> Self
    {
        Self::InvalidStatus(parse_status_err)
    }
}

impl From<std::num::ParseIntError> for ParseGndBondDataErr
{
    fn from(parse_num_err: std::num::ParseIntError) -> Self
    {
        Self::InvalidData(parse_num_err)
    }
}

impl fmt::Display for ParseGndBondDataErr
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self {
            Self::ResponseTooShort => write!(f, "Response too short. Not all expected data were contained"),
            Self::InvalidTestType => write!(f, "Invalid test type token. Expected 'GND'"),
            Self::UnexpectedStatus(status) => write!(f, "Unexpected status: {:?}. The test may be in progress", status),
            Self::InvalidStatus(status_err) => write!(f, "{}", status_err),
            Self::InvalidData(num_err) => write!(f, "Failed to parse memory location or result value: {}", num_err),
        }
    }
}

impl std::error::Error for ParseGndBondDataErr {}

impl std::str::FromStr for SciGndBondData
{
    type Err = ParseGndBondDataErr;

    fn from_str(data_str: &str) -> Result<Self, Self::Err>
    {
        let mut sequence_num = None;
        let mut step_num = None;
        let mut test_status = None;
        let mut gnd_resistance = None;

        for (index, token) in data_str.split(',').enumerate() {
            if index == 0 {
                sequence_num = Some(token.parse::<u32>()?);
            }
            else if index == 1 {
                step_num = Some(token.parse::<u32>()?);
            }
            else if index == 2 {
                if token != "GND" {
                    return Err(ParseGndBondDataErr::InvalidTestType)
                }
            }
            else if index == 3 {
                let status = token.parse::<SciTestStatus>()?;
                test_status = match status {
                    SciTestStatus::Ramp | SciTestStatus::Dwell | SciTestStatus::Overflow => 
                        return Err(ParseGndBondDataErr::UnexpectedStatus(status)),
                    status @ _ => Some(status),
                };
            }
            // token at index 4 is just the dwell time which we won't parse for now
            else if index == 5 {
                gnd_resistance = Some(Ohm::from_millis(token.parse::<u32>()?));
            }
        }

        if sequence_num.is_none() || step_num.is_none() || test_status.is_none() {
            return Err(ParseGndBondDataErr::ResponseTooShort)
        }

        if gnd_resistance.is_none() {
            match test_status.as_ref().unwrap() {
                SciTestStatus::Abort => (),
                _ => return Err(ParseGndBondDataErr::ResponseTooShort),
            }
        }

        Ok(Self {
            data: GndBondData {
                sequence_num: sequence_num.unwrap(),
                step_num: step_num.unwrap(),
                outcome: match test_status.unwrap() {
                    SciTestStatus::HiLimit => GndBondOutcome::ResistanceExcessive(gnd_resistance.unwrap()),
                    SciTestStatus::LoLimit => GndBondOutcome::ResistanceSubnormal(gnd_resistance.unwrap()),
                    SciTestStatus::Abort => GndBondOutcome::Aborted,
                    SciTestStatus::Pass => GndBondOutcome::Passed(gnd_resistance.unwrap()),
                    status @ _ => return Err(ParseGndBondDataErr::UnexpectedStatus(status)),
                }
            }
        })
    }
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
    (commit AssociatedResearch Yes) => {
        CmdSet::display_ar
    };
    (commit Sci Yes) => {
        CmdSet::display_sci_multi
    };
    (commit Sci No) => {
        CmdSet::display_sci_single
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
            use super::{Amp, Volt, Ohm, TestSupport, AcHipotDeviceLimits, GndBondDeviceLimits, AcHipotTestSpec, GndBondTestSpec,
                StepInfo, TestParams, CmdSet
            };
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

                // pub async fn start_test(&mut self) -> Result<(), std::io::Error>
                // {
                    
                // }
            
                // pub async fn soft_reset(&mut self) -> Result<(), std::io::Error>
                // {
                    
                // }

                // pub async fn get_test_data(&mut self, step_num: u32) -> Result<(), std::io::Error>
                // {
                    
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

define_device!{
    model: sci_448,
    brand: Sci,
    multi_sequence: No,
    steps_per_sequence: 20,
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

define_device!{
    model: ar_7704,
    brand: AssociatedResearch,
    multi_sequence: Yes(3),
    steps_per_sequence: 8,
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
pub use sci_448::{ Device as Sci448, TestEditor as Sci448TestEditor };
pub use ar_7704::{ Device as Ar7704, TestEditor as Ar7704TestEditor };
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
    use super::{ Amp, Ohm, Volt, CmdSet, AcFrequency, GndBondParam, AcHipotParam};
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
    fn serialize_sci_core()
    {
        assert_eq!(&format!("{}", CmdSet::SetAcHipot.display_sci_single()), "SAA");
        assert_eq!(&format!("{}", CmdSet::SetGndBond.display_sci_single()), "SAG");
        assert_eq!(&format!("{}", CmdSet::ContinueToNext(true).display_sci_single()), "ECC 1");
        assert_eq!(&format!("{}", CmdSet::ContinueToNext(false).display_sci_single()), "ECC 0");

        assert_eq!(&format!("{}", CmdSet::SetGndBondParam(GndBondParam::CheckCurrent(Amp::from_whole(25))).display_sci_single()), "EC 25.0");
        assert_eq!(&format!("{}", CmdSet::SetGndBondParam(GndBondParam::DwellTime(Duration::from_millis(2_345))).display_sci_single()), "EDW 2.3");
        assert_eq!(&format!("{}", CmdSet::SetGndBondParam(GndBondParam::Frequency(AcFrequency::Hz50)).display_sci_single()), "EF 0");
        assert_eq!(&format!("{}", CmdSet::SetGndBondParam(GndBondParam::Frequency(AcFrequency::Hz60)).display_sci_single()), "EF 1");
        assert_eq!(&format!("{}", CmdSet::SetGndBondParam(GndBondParam::ResistanceMax(Ohm::from_millis(128))).display_sci_single()), "EH 128");
        assert_eq!(&format!("{}", CmdSet::SetGndBondParam(GndBondParam::ResistanceMin(Ohm::from_millis(0))).display_sci_single()), "EL 0");
        assert_eq!(&format!("{}", CmdSet::SetGndBondParam(GndBondParam::Offset(Ohm::from_millis(56))).display_sci_single()), "EO 56");

        assert_eq!(&format!("{}", CmdSet::SetAcHipotParam(AcHipotParam::Frequency(AcFrequency::Hz50)).display_sci_single()), "EF 0");
        assert_eq!(&format!("{}", CmdSet::SetAcHipotParam(AcHipotParam::Frequency(AcFrequency::Hz60)).display_sci_single()), "EF 1");
        assert_eq!(&format!("{}", CmdSet::SetAcHipotParam(AcHipotParam::LeakageMax(Amp::from_micros(67_800))).display_sci_single()), "EH 67.8");
        assert_eq!(&format!("{}", CmdSet::SetAcHipotParam(AcHipotParam::LeakageMin(Amp::from_micros(0))).display_sci_single()), "EL 0.0");
        assert_eq!(&format!("{}", CmdSet::SetAcHipotParam(AcHipotParam::RampTime(Duration::from_millis(4_321))).display_sci_single()), "ERU 4.3");
        assert_eq!(&format!("{}", CmdSet::SetAcHipotParam(AcHipotParam::Voltage(Volt::from_whole(1250))).display_sci_single()), "EV 1.25");
        assert_eq!(&format!("{}", CmdSet::SetAcHipotParam(AcHipotParam::DwellTime(Duration::from_millis(2_345))).display_sci_single()), "EDW 2.3");
    }

    #[test]
    fn serialize_sci_multi_seq()
    {
        assert_eq!(&format!("{}", CmdSet::LoadSequence(3).display_sci_multi()), "FL 3");
        assert_eq!(&format!("{}", CmdSet::SelectStep(1).display_sci_multi()), "SS 1");
    }

    #[test]
    fn serialize_sci_single_seq()
    {
        assert_eq!(&format!("{}", CmdSet::SelectStep(1).display_sci_single()), "FL 1");
    }

    #[test]
    #[should_panic]
    fn sci_single_load_seq_panics()
    {
        format!("{}", CmdSet::LoadSequence(1).display_sci_single());
    }

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
