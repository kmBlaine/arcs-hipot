use crate::units::{ Ampere, Volt, Ohm, Milli };
use std::fmt;

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
        write!(f, "Unrecognized status. Acceptable test statuses are ['OFL', 'HI-Limit', 'LO-Limit', 'Ramp', 'Dwell', 'Abort', 'Pass'] (case sensitive)")
    }
}

impl std::error::Error for ParseTestStatusErr {}

#[derive(Debug)]
pub struct ParseTestTypeErr {}

impl fmt::Display for ParseTestTypeErr
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "Unrecognized test type. Acceptable test types are ['ACW', 'GND'] (case sensitive)")
    }
}

impl std::error::Error for ParseTestTypeErr {}

#[derive(Debug, Clone, Copy)]
pub enum SciTestStatus
{
    /// "OFL"
    Overflow,
    /// "HI-Limit"
    HiLimit,
    /// "LO-Limit"
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
            "HI-Limit" => Ok(Self::HiLimit),
            "LO-Limit" => Ok(Self::LoLimit),
            "Ramp" => Ok(Self::Ramp),
            "Dwell" => Ok(Self::Dwell),
            "Abort" => Ok(Self::Abort),
            "Pass" => Ok(Self::Pass),
            _ => Err(ParseTestStatusErr{})
        }
    }
}

pub(super) enum SciTestType
{
    GndBond,
    AcHipot,
}

impl std::str::FromStr for SciTestType
{
    type Err = ParseTestTypeErr;

    fn from_str(status_str: &str) -> Result<Self, Self::Err>
    {
        match status_str {
            "ACW" => Ok(Self::AcHipot),
            "GND" => Ok(Self::GndBond),
            _ => Err(ParseTestTypeErr{})
        }
    }
}

pub enum TestData
{
    AcHipot(AcHipotData),
    GndBond(GndBondData),
}

pub(super) struct SciTestData
{
    data: TestData,
}

impl From<SciTestData> for TestData
{
    fn from(this: SciTestData) -> Self
    {
        this.data
    }
}

#[derive(Debug)]
pub enum ParseTestDataErr
{
    /// The device did not send back a complete response
    ResponseTooShort,
    /// The test type token was not one of [`GND`, `ACW`]
    InvalidTestType(ParseTestTypeErr),
    /// The test status indicated the test was incomplete or was unexpected for the type of test.
    ///
    /// Ground bond tests expect one of Pass, Abort, HiLimit, or LoLimit. AC hipot tests expect one
    /// of Pass, Abort, HiLimit, LoLimit, or Overflow. Dwell or Ramp indicate that the test has not
    /// yet finished.
    UnexpectedStatus(SciTestStatus),
    /// Invalid status token encountered when attempting to parse the test status from the string
    InvalidStatus(ParseTestStatusErr),
    /// Invalid data encountered when attempting to parse the memory location or ground resistance
    /// value
    InvalidInteger(std::num::ParseIntError),
    /// Invalid data encountered when attempting to parse the hipot leakage current
    InvalidDecimal(std::num::ParseFloatError),
    /// The string returned by the device could not be interpreted as valid UTF8
    ///
    /// # Implementation Notes
    /// These devices will reply with extended ASCII encoding. The library explicitly substitutes
    /// extended ASCII bytes when reading the byte data from the device for an appropriate UTF8
    /// character. At the time this library was written, the only known instance of the use of
    /// extended ASCII however was the omega character for resistance values. Others may be lurking
    /// unaccounted for.
    InvalidUtf8(std::string::FromUtf8Error),
    /// An I/O error occurred while attempting retrieve the test data
    Io(std::io::Error),
}

impl From<ParseTestStatusErr> for ParseTestDataErr
{
    fn from(parse_status_err: ParseTestStatusErr) -> Self
    {
        Self::InvalidStatus(parse_status_err)
    }
}

impl From<std::num::ParseIntError> for ParseTestDataErr
{
    fn from(parse_num_err: std::num::ParseIntError) -> Self
    {
        Self::InvalidInteger(parse_num_err)
    }
}

impl From<std::num::ParseFloatError> for ParseTestDataErr
{
    fn from(parse_num_err: std::num::ParseFloatError) -> Self
    {
        Self::InvalidDecimal(parse_num_err)
    }
}

impl From<ParseTestTypeErr> for ParseTestDataErr
{
    fn from(parse_type_err: ParseTestTypeErr) -> Self
    {
        Self::InvalidTestType(parse_type_err)
    }
}

impl From<std::string::FromUtf8Error> for ParseTestDataErr
{
    fn from(parse_utf8_err: std::string::FromUtf8Error) -> Self
    {
        Self::InvalidUtf8(parse_utf8_err)
    }
}

impl From<std::io::Error> for ParseTestDataErr
{
    fn from(io_err: std::io::Error) -> Self
    {
        Self::Io(io_err)
    }
}

impl fmt::Display for ParseTestDataErr
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self {
            Self::ResponseTooShort => write!(f, "Response too short. Not all expected data were contained"),
            Self::InvalidTestType(test_type_err) => write!(f, "Invalid test type token. {}", test_type_err),
            Self::UnexpectedStatus(status) => write!(f, "Unexpected status: {:?}. The test may be in progress", status),
            Self::InvalidStatus(status_err) => write!(f, "{}", status_err),
            Self::InvalidInteger(num_err) => write!(f, "Failed to parse memory location or ground bond result: {}", num_err),
            Self::InvalidDecimal(num_err) => write!(f, "Failed to parse hipot leakage current: {}", num_err),
            Self::InvalidUtf8(utf8_err) => write!(f, "The device replied with an unexpected character. Caused by {}", utf8_err),
            Self::Io(io_err) => write!(f, "{}", io_err),
        }
    }
}

impl std::error::Error for ParseTestDataErr {}

impl std::str::FromStr for SciTestData
{
    type Err = ParseTestDataErr;

    fn from_str(data_str: &str) -> Result<Self, Self::Err>
    {
        let mut sequence_num = None;
        let mut step_num = None;
        let mut test_status = None;
        let mut test_type = None;
        let mut hipot_leak = None;
        let mut gnd_resistance = None;

        for (index, token) in data_str.split(',').enumerate() {
            if index == 0 {
                sequence_num = Some(token.parse::<u32>()?);
            }
            else if index == 1 {
                step_num = Some(token.parse::<u32>()?);
            }
            else if index == 2 {
                test_type = Some(token.parse::<SciTestType>()?);
            }
            else if index == 3 {
                let status = token.parse::<SciTestStatus>()?;
                test_status = match status {
                    SciTestStatus::Ramp | SciTestStatus::Dwell => 
                        return Err(ParseTestDataErr::UnexpectedStatus(status)),
                    status @ _ => Some(status),
                };
            }
            // token at index 4 is just the ground dwell time and hipot voltage which we won't parse for now
            else if index == 5 {
                match test_type.as_ref().unwrap() {
                    SciTestType::GndBond => match test_status.as_ref().unwrap() {
                        SciTestStatus::Abort => (),
                        _ => {
                            gnd_resistance = Some(Ohm::from::<Milli>(token.parse::<u64>()?))
                        },
                    },
                    SciTestType::AcHipot => match test_status.as_ref().unwrap() {
                        SciTestStatus::Abort | SciTestStatus::Overflow => (),
                        _ => {
                            let milliamps = token.parse::<f64>()?;
                            hipot_leak = Some(Ampere::from_f64::<Milli>(milliamps));
                        }
                    }
                }
                
            }
        }

        if sequence_num.is_none() || step_num.is_none() || test_status.is_none() || test_type.is_none() {
            return Err(ParseTestDataErr::ResponseTooShort);
        }

        let test_data = match test_type.unwrap() {
            SciTestType::GndBond => {
                if gnd_resistance.is_none() {
                    match test_status.as_ref().unwrap() {
                        SciTestStatus::Abort => (),
                        _ => return Err(ParseTestDataErr::ResponseTooShort),
                    }
                }

                TestData::GndBond(GndBondData {
                    sequence_num: sequence_num.unwrap(),
                    step_num: step_num.unwrap(),
                    outcome: match test_status.unwrap() {
                        SciTestStatus::HiLimit => GndBondOutcome::ResistanceExcessive(gnd_resistance.unwrap()),
                        SciTestStatus::LoLimit => GndBondOutcome::ResistanceSubnormal(gnd_resistance.unwrap()),
                        SciTestStatus::Abort => GndBondOutcome::Aborted,
                        SciTestStatus::Pass => GndBondOutcome::Passed(gnd_resistance.unwrap()),
                        status @ _ => return Err(ParseTestDataErr::UnexpectedStatus(status)),
                    }
                })
            },
            SciTestType::AcHipot => {
                if hipot_leak.is_none() {
                    match test_status.as_ref().unwrap() {
                        SciTestStatus::Abort | SciTestStatus::Overflow => (),
                        _ => return Err(ParseTestDataErr::ResponseTooShort),
                    }
                }

                TestData::AcHipot(AcHipotData {
                    sequence_num: sequence_num.unwrap(),
                    step_num: step_num.unwrap(),
                    outcome: match test_status.unwrap() {
                        SciTestStatus::Overflow => AcHipotOutcome::LeakOverflow,
                        SciTestStatus::HiLimit => AcHipotOutcome::LeakExcessive(hipot_leak.unwrap()),
                        SciTestStatus::LoLimit => AcHipotOutcome::LeakSubnormal(hipot_leak.unwrap()),
                        SciTestStatus::Abort => AcHipotOutcome::Aborted,
                        SciTestStatus::Pass => AcHipotOutcome::Passed(hipot_leak.unwrap()),
                        status @ _ => return Err(ParseTestDataErr::UnexpectedStatus(status)),
                    }
                })
            },
        };

        Ok(Self {
            data: test_data
        })
    }
}
