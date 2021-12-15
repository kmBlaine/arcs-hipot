use super::{ Amp, Volt, Ohm };
use std::fmt;

pub enum AcHipotOutcome
{
    /// A short circuit was detected
    ///
    /// This outcome occurs when the leakage current exceeded the metering range of the instrument.
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

pub struct SciAcHipotData
{
    data: AcHipotData
}

impl From<SciAcHipotData> for AcHipotData
{
    fn from(this: SciAcHipotData) -> Self
    {
        this.data
    }
}
