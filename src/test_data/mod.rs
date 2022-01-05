//! Test results and data parsing

use crate::units::{ Ampere, Volt, Ohm, scalar::Milli };
use std::{
    fmt,
    error::Error,
};

pub(crate) mod sci;
pub(crate) mod ar;

#[derive(Debug, Clone)]
pub enum AcHipotOutcome
{
    /// The leakage current exceeded the instrumet's metering range
    LeakOverflow,
    /// The leakage current exceeded the maximum acceptable limit
    ///
    /// This implies that the current fell within metering range
    LeakExcessive(Ampere),
    /// The leakage current fell below the minimum acceptable limit
    LeakSubnormal(Ampere),
    /// An arcing condition was detected
    ArcFault,
    /// The high voltage lead has connected directly to earth, skipping the ground return lead
    GndFault,
    /// Operator aborted the test on the instrument UI
    Aborted,
    /// The leakage current was within acceptable limits
    Passed(Ampere),
}

impl AcHipotOutcome
{
    pub fn passed(&self) -> bool
    {
        match self {
            Self::Passed(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AcHipotData
{
    pub sequence_num: u32,
    pub step_num: u32,
    pub outcome: AcHipotOutcome,
}

#[derive(Debug, Clone)]
pub enum GndBondOutcome
{
    ResistanceOverflow,
    ResistanceExcessive(Ohm),
    ResistanceSubnormal(Ohm),
    Aborted,
    Passed(Ohm),
}

impl GndBondOutcome
{
    pub fn passed(&self) -> bool
    {
        match self {
            Self::Passed(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GndBondData
{
    pub sequence_num: u32,
    pub step_num: u32,
    pub outcome: GndBondOutcome
}

#[derive(Debug, Clone)]
pub enum TestData
{
    AcHipot(AcHipotData),
    GndBond(GndBondData),
}

/// A description of the underlying cause of the parsing failure, if any
#[derive(Debug)]
enum FormatErrorCause
{
    /// The string was not of the expected length
    ///
    /// # Implementation Notes
    /// The parsers tokenize the string using spaces and more specifically the `split()` method
    /// on a `str`. This error will most likely occur when an insufficient number of tokens were
    /// created causing the iterator to terminate early. However, there are other reasons this can
    /// be returned.
    Truncated,
    /// Expected an integer value
    InvalidInteger(std::num::ParseIntError),
    /// Expected a decimal value
    InvalidDecimal(std::num::ParseFloatError),
    /// Expected one of a fixed set of values
    ///
    /// The contained string contains a description of which values were expected.
    ///
    /// # Implementation Notes
    /// The parsers will explicitly reject test status variants which indicate that the test is still
    /// running such as "Dwell" or "Ramp". This is by design so that "results" are not accidentally
    /// retrieved for incomplete tests.
    InvalidEnum(&'static str),
}

impl fmt::Display for FormatErrorCause
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self {
            Self::Truncated => f.write_str("No such token. String ends before this could be parsed"),
            Self::InvalidDecimal(float_err) => write!(f, "Caused by: {}", float_err),
            Self::InvalidInteger(int_err) => write!(f, "Caused by: {}", int_err),
            Self::InvalidEnum(enum_err) => write!(f, "Unexpected or invalid variant. {}", enum_err),
        }
    }
}

impl From<std::num::ParseIntError> for FormatErrorCause
{
    fn from(this: std::num::ParseIntError) -> Self
    {
        Self::InvalidInteger(this)
    }
}

impl From<std::num::ParseFloatError> for FormatErrorCause
{
    fn from(this: std::num::ParseFloatError) -> Self
    {
        Self::InvalidDecimal(this)
    }
}

/// A parsing error caused by a test result string of unexpected or invalid format
#[derive(Debug)]
pub struct FormatError
{
    /// The string returned by the device
    ///
    /// Note that for Associated Research devices, it does not include a line break between the two
    /// lines of display text that it returns. The parser will insert one for easier debugging by a
    /// human.
    pub raw_data: String,
    /// The line number of the error
    line: usize,
    /// Which token the error occurred at. Indexed from 1
    ///
    /// Tokens should be regarded as a space separated.
    token: usize,
    /// A message from the parsing routines about what went wrong
    mesg: &'static str,
    /// Some underlying cause, if any
    maybe_cause: Option<FormatErrorCause>,
}

impl fmt::Display for FormatError
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "At line {}, token {}: {}", self.line, self.token, self.mesg)?;

        if let Some(cause) = &self.maybe_cause {
            write!(f, ". {}", cause)
        }
        else {
            Ok(())
        }
    }
}

impl Error for FormatError {}

/// An error describing a failure to parse test data
#[derive(Debug)]
pub enum ParseError
{
    /// The data does not appear to be in the expected format
    InvalidFormat(FormatError),
    /// The data returned by the device could not be interpreted as a valid UTF8 string
    ///
    /// # Implementation Notes
    /// These devices will reply with extended ASCII encoding, which (naturally) by themselves are
    /// virtually never valid UTF8. However, the only two known instances of this are the mu and
    /// omega characters for the micro- prefix and ohm symbol, respectively. The library explicitly
    /// substitutes these for valid UTF8. Thus, an encoding error _should_ only come up in the case
    /// of a serial hotplug or an incorrect baud rate setting.
    InvalidUtf8(std::string::FromUtf8Error),
    /// An I/O error occurred while trying to query the device for test data
    Io(std::io::Error),
}

impl fmt::Display for ParseError
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self {
            Self::InvalidFormat(fmt_err) => write!(f, "Unable to interpret data. {}", fmt_err),
            Self::InvalidUtf8(decode_err) => write!(f, "Unable to decode data. {}", decode_err),
            Self::Io(io_err) => write!(f, "Failed to query device. {}", io_err),
        }
    }
}

impl Error for ParseError {}

impl From<std::io::Error> for ParseError
{
    fn from(this: std::io::Error) -> Self
    {
        ParseError::Io(this)
    }
}

impl From<std::string::FromUtf8Error> for ParseError
{
    fn from(this: std::string::FromUtf8Error) -> Self
    {
        ParseError::InvalidUtf8(this)
    }
}

impl From<FormatError> for ParseError
{
    fn from(this: FormatError) -> Self
    {
        ParseError::InvalidFormat(this)
    }
}
