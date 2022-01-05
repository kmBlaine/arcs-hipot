//! Associated Research test results and data parsing

use std::{
    fmt,
    error::Error,
    convert::From
};

use crate::{
    test_data::{ AcHipotOutcome, GndBondOutcome, FormatError, FormatErrorCause, TestData, AcHipotData, GndBondData },
    units::{ Ampere, Ohm, scalar::Milli }
};

macro_rules! parse_token
{
    ( $tok:expr, $tok_type:ty, $ln:expr, $idx:expr, $fail_mesg:expr, $raw_str:expr ) => {
        if let Some(token) = $tok {
            token.parse::<$tok_type>()
                .map_err(|err| {
                    FormatError {
                        raw_data: String::from($raw_str),
                        line: $ln,
                        token: $idx,
                        mesg: $fail_mesg,
                        maybe_cause: Some(FormatErrorCause::from(err))
                    }
                })
        }
        else {
            Err(FormatError {
                raw_data: String::from($raw_str),
                line: $ln,
                token: $idx,
                mesg: $fail_mesg,
                maybe_cause: Some(FormatErrorCause::Truncated),
            })
        }
    }
}

macro_rules! impl_parse_enum_err
{
    { $name:ident, $err_str:literal } => {
        impl $name
        {
            fn valid_str_variants() -> &'static str
            {
                $err_str
            }
        }

        impl fmt::Display for $name
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
            {
                f.write_str(Self::valid_str_variants())
            }
        }

        impl std::error::Error for $name {}

        impl From<$name> for super::FormatErrorCause
        {
            fn from(_this: $name) -> FormatErrorCause
            {
                FormatErrorCause::InvalidEnum($name::valid_str_variants())
            }
        }
    }
}

#[derive(Debug)]
struct ParseTestTypeErr {}
impl_parse_enum_err!{ ParseTestTypeErr, "Expected one of ['GND', 'ACW'] (case sensitive)" }

#[derive(Debug)]
struct ParseGndBondStatusErr {}
impl_parse_enum_err!{ ParseGndBondStatusErr, "Expected one of ['HI-Limit', 'LO-Limit', 'Abort', 'Pass'] (case sensitive)" }

#[derive(Debug)]
struct ParseAcHipotStatusErr {}
impl_parse_enum_err!{ ParseAcHipotStatusErr, "Expected one of ['Short', 'Breakdown', 'Arc-Fail', 'GndFault', 'HI-Limit', 'LO-Limit', 'Abort', 'Pass'] (case sensitive)" }

enum TestType
{
    GndBond,
    AcHipot,
}

impl std::str::FromStr for TestType
{
    type Err = ParseTestTypeErr;

    fn from_str(data_str: &str) -> Result<Self, Self::Err>
    {
        match data_str {
            "GND" => Ok(Self::GndBond),
            "ACW" => Ok(Self::AcHipot),
            _ => Err(ParseTestTypeErr {})
        }
    }
}

enum GndBondStatus
{
    HiLimit,
    LoLimit,
    Abort,
    Pass,
}

impl std::str::FromStr for GndBondStatus
{
    type Err = ParseGndBondStatusErr;

    fn from_str(data_str: &str) -> Result<Self, Self::Err>
    {
        match data_str {
            "HI-Limit" => Ok(Self::HiLimit),
            "LO-Limit" => Ok(Self::LoLimit),
            "Abort" => Ok(Self::Abort),
            "Pass" => Ok(Self::Pass),
            _ => Err(ParseGndBondStatusErr {})
        }
    }
}

enum AcHipotStatus
{
    Short,
    Breakdown,
    ArcFail,
    GndFault,
    HiLimit,
    LoLimit,
    Abort,
    Pass,
}

impl std::str::FromStr for AcHipotStatus
{
    type Err = ParseAcHipotStatusErr;

    fn from_str(data_str: &str) -> Result<Self, Self::Err>
    {
        match data_str {
            "Short" => Ok(Self::Short),
            "Breakdown" => Ok(Self::Breakdown),
            "Arc-Fail" => Ok(Self::ArcFail),
            "GND-Fault" => Ok(Self::GndFault),
            "HI-Limit" => Ok(Self::HiLimit),
            "LO-Limit" => Ok(Self::LoLimit),
            "Abort" => Ok(Self::Abort),
            "Pass" => Ok(Self::Pass),
            _ => Err(ParseAcHipotStatusErr {})
        }
    }
}

pub struct ArTestData
{
    data: TestData,
}

impl From<ArTestData> for TestData
{
    fn from(this: ArTestData) -> Self
    {
        this.data
    }
}

impl ArTestData
{
    fn parse_ac_hipot<'a, I1, I2>(
        sequence_num: u32,
        step_num: u32,
        mut tokens1: I1,
        mut tokens2: I2,
        data_str: &str,
    )
        -> Result<Self, FormatError>

        where I1: std::iter::Iterator<Item = &'a str>,
              I2: std::iter::Iterator<Item = &'a str>
    {
        let status = parse_token!(tokens1.next(), AcHipotStatus, 1, 2, "failed to parse AC hipot status", data_str)?;

        // This is just a progress value. Not relevant to final output
        tokens2.next();

        let data = TestData::AcHipot(AcHipotData {
            sequence_num: sequence_num,
            step_num: step_num,
            outcome: match status {
                AcHipotStatus::Short | AcHipotStatus::Breakdown => AcHipotOutcome::LeakOverflow,
                AcHipotStatus::ArcFail => AcHipotOutcome::ArcFault,
                AcHipotStatus::GndFault => AcHipotOutcome::GndFault,
                AcHipotStatus::HiLimit => {
                    if let Some(token) = tokens2.next() {
                        if token.starts_with('>') {
                            AcHipotOutcome::LeakOverflow
                        }
                        else {
                            let leak_current = parse_token!(token.split('m').next(), f64, 2, 3, "failed to parse hipot leak current", data_str)?;
                            AcHipotOutcome::LeakExcessive(Ampere::from_f64::<Milli>(leak_current))
                        }
                    }
                    else {
                        return Err(FormatError {
                            raw_data: String::from(data_str),
                            line: 2,
                            token: 3,
                            mesg: "failed to parse leakage current",
                            maybe_cause: Some(FormatErrorCause::Truncated),
                        });
                    }
                },
                AcHipotStatus::LoLimit => {
                    let gnd_resistance = parse_token!(tokens2.next(), f64, 2, 3, "failed to parse hipot leak current", data_str)?;
                    AcHipotOutcome::LeakSubnormal(Ampere::from_f64::<Milli>(gnd_resistance))
                },
                AcHipotStatus::Abort => AcHipotOutcome::Aborted,
                AcHipotStatus::Pass => {
                    let gnd_resistance = parse_token!(tokens2.next(), f64, 2, 3, "failed to parse hipot leak current", data_str)?;
                    AcHipotOutcome::Passed(Ampere::from_f64::<Milli>(gnd_resistance))
                },
            }
        });

        Ok(Self {
            data: data
        })
    }

    fn parse_gnd_bond<'a, I1, I2>(
        sequence_num: u32,
        step_num: u32,
        mut tokens1: I1,
        mut tokens2: I2,
        data_str: &str,
    )
        -> Result<Self, FormatError>

        where I1: std::iter::Iterator<Item = &'a str>,
              I2: std::iter::Iterator<Item = &'a str>
    {
        let status = parse_token!(tokens1.next(), GndBondStatus, 1, 2, "failed to parse ground bond status", data_str)?;

        // This is just a progress value. Not relevant to final output
        tokens2.next();

        let data = TestData::GndBond(GndBondData {
            sequence_num: sequence_num,
            step_num: step_num,
            outcome: match status {
                GndBondStatus::HiLimit => {
                    let gnd_resistance = parse_token!(tokens2.next(), u64, 2, 3, "failed to parse ground resistance", data_str)?;
                    GndBondOutcome::ResistanceExcessive(Ohm::from::<Milli>(gnd_resistance))
                },
                GndBondStatus::LoLimit => {
                    let gnd_resistance = parse_token!(tokens2.next(), u64, 2, 3, "failed to parse ground resistance", data_str)?;
                    GndBondOutcome::ResistanceSubnormal(Ohm::from::<Milli>(gnd_resistance))
                },
                GndBondStatus::Abort => GndBondOutcome::Aborted,
                GndBondStatus::Pass => {
                    let gnd_resistance = parse_token!(tokens2.next(), u64, 2, 3, "failed to parse ground resistance", data_str)?;
                    GndBondOutcome::Passed(Ohm::from::<Milli>(gnd_resistance))
                },
            }
        });

        Ok(Self {
            data: data
        })
    }
}

impl std::str::FromStr for ArTestData
{
    type Err = FormatError;

    fn from_str(data_str: &str) -> Result<Self, Self::Err>
    {
        let (line1, line2) = if data_str.is_char_boundary(20) {
            data_str.split_at(20)
        }
        else {
            return Err(FormatError {
                raw_data: String::from(data_str),
                line: 1,
                token: 0,
                mesg: "byte at index 20 is out of bounds or not a UTF8 code point boundary",
                maybe_cause: None,
            });
        };

        let line_broken_data = String::from(line1) + "\n" + line2;
        let mut tokens1 = line1.split(' ').filter(|tok| !tok.is_empty());
        let mut tokens2 = line2.split(' ').filter(|tok| !tok.is_empty());

        let (sequence_num, step_num) = if let Some(seq_tok) = tokens2.next() {
            let mut seq_step_iter = seq_tok.split(&[' ', 'M', '-'][..]).filter(|tok| !tok.is_empty());
            let seq_tok = seq_step_iter.next();
            let step_tok = seq_step_iter.next();

            if seq_tok.is_none() || step_tok.is_none() {
                return Err(FormatError {
                    raw_data: String::from(data_str),
                    line: 2,
                    token: 1,
                    mesg: "expected a sequence indicator in format 'M#-#'",
                    maybe_cause: None,
                });
            }

            let seq = parse_token!(seq_tok, u32, 2, 1, "failed to parse sequence number", &line_broken_data)?;
            let step = parse_token!(step_tok, u32, 2, 1, "failed to parse step number", &line_broken_data)?;

            (seq, step)
        }
        else {
            return Err(FormatError {
                raw_data: line_broken_data,
                line: 2,
                token: 1,
                mesg: "failed to parse sequence and step number",
                maybe_cause: Some(FormatErrorCause::Truncated),
            })
        };

        let test_type = parse_token!(tokens1.next(), TestType, 1, 1, "failed to parse test type", &line_broken_data)?;

        match test_type {
            TestType::GndBond => Self::parse_gnd_bond(sequence_num, step_num, tokens1, tokens2, &line_broken_data),
            TestType::AcHipot => Self::parse_ac_hipot(sequence_num, step_num, tokens1, tokens2, &line_broken_data),
        }
    }
}