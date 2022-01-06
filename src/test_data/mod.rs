//! Test results and data parsing

use crate::units::{ Ampere, Ohm, };
use std::{
    fmt,
    error::Error,
};

pub(crate) mod sci;
pub(crate) mod ar;

/// The final result of an AC hipot test
///
/// This enum is intended to encompass a basic, device-agnostic representation of outcomes. Therefore
/// by design, it does NOT have a 1:1 correspondence with the data returned by the device.
#[derive(Debug, Clone)]
pub enum AcHipotOutcome
{
    /// The leakage current exceeded the metering range of the instrument
    LeakOverflow,
    /// The leakage current exceeded the maximum acceptable limit
    ///
    /// This implies that the current fell within metering range. The measured leakage current is
    /// contained
    LeakExcessive(Ampere),
    /// The leakage current fell below the minimum acceptable limit. The measuered leakage current is
    /// contained
    LeakSubnormal(Ampere),
    /// An arcing condition was detected
    ArcFault,
    /// The instrument detected that the high voltage lead has connected directly to earth, skipping
    /// the ground return lead
    GndFault,
    /// The test was stopped before it could be completed
    Aborted,
    /// The test passed. The measured leakage current is given
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

/// Result of the AC hipot test run at a given sequence and step number
#[derive(Debug, Clone)]
pub struct AcHipotData
{
    /// Sequence number
    pub sequence_num: u32,
    /// Step number in the given sequence
    pub step_num: u32,
    /// Result of the test
    pub outcome: AcHipotOutcome,
}

/// The final result of a ground bond test
///
/// This enum is intended to encompass a basic, device-agnostic representation of outcomes. Therefore
/// by design, it does NOT have a 1:1 correspondence with the data returned by the device.
#[derive(Debug, Clone)]
pub enum GndBondOutcome
{
    /// The ground resistance exceeded the metering range of the instrument
    ResistanceOverflow,
    /// The ground resistance is beyond the maximum resistance allowed. The measured resistance is
    /// contained
    ResistanceExcessive(Ohm),
    /// The ground resistance is below the minimum resistance allowed. The measured resistance is
    /// contained
    ResistanceSubnormal(Ohm),
    /// The test was stopped before it could be finished
    Aborted,
    /// The test passed. The measured ground resistance is given
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

/// Result of the ground bond test at the given sequence and step number
#[derive(Debug, Clone)]
pub struct GndBondData
{
    /// Sequence number of this test
    pub sequence_num: u32,
    /// Step number of this test
    pub step_num: u32,
    /// Result of the test
    pub outcome: GndBondOutcome
}

/// Unified type for reporting test results
///
/// The test results as stored by the device are not strongly but rather _stringly_ typed where the
/// type of data being retrieved is not apparent until it has been parsed at least partway. This is
/// because the same command retrieves all of them and the buffers are not unique to each type.
/// Consequently, the parsing routines -- in order to reduce boilerplate and more complicated error
/// return types -- parse to a unified type which can then be interpreted by the user as matching
/// what they expected or not.
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
///
/// This error type is mostly opaque so that the low level parsing mechanics are not exposed to the
/// the user and because the most likely cause of format errors is bugs in the parsing code itself.
/// If a format error is triggered when attempting to retrieve test results, there's probably nothing
/// that can be done.
///
/// For easing of identification of bugs, a copy of the string causing the problem is publicly visible.
#[derive(Debug)]
pub struct FormatError
{
    /// The string returned by the device
    ///
    /// Note that the response from Associated Research devices does not include a line break between
    /// the two lines of display text. If the line is longer than the line length of 20 characters,
    /// one will be inserted automatically.
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
///
/// In order for test results to be parsed successfully, the test must be complete. For example, if
/// the test status of an AC hipot or ground bond test is in the "Ramp" or "Dwell" phases, an invalid
/// format error will be triggered.
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
