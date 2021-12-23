
macro_rules! impl_parse_enum_err
{
    { $name:ty, $err_str:literal } => {
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

struct ParseTestTypeErr {}
impl_parse_enum_err!{ ParseTestTypeErr, "Expected one of ['GND', 'ACW'] (case sensitive)" }

struct ParseGndBondStatusErr {}
impl_parse_enum_err!{ ParseGndBondStatusErr, "Expected one of ['HI-Limit', 'LO-Limit', 'Abort', 'Pass'] (case sensitive)" }

struct ParseAcHipotStatusErr {}
impl_parse_enum_err!{ ParseAcHipotStatusErr, "Expected one of ['OFL', 'HI-Limit', 'LO-Limit', 'Abort', 'Pass'] (case sensitive)" }

enum TestType
{
    GndBond,
    AcHipot,
}

impl std::str:FromStr for TestType
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

impl std::str:FromStr for GndBondStatus
{
    type Err = ParseGndBondStatusErr;

    fn from_str(data_str: &str) -> Result<Self, Self::Err>
    {
        match data_str {
            "HI-Limit" => Ok(Self::GndBond),
            "LO-Limit" => Ok(Self::AcHipot),
            "Abort" => Ok(Self::Abort),
            "Pass" => Ok(Self::Pass),
            _ => Err(ParseGndBondStatusErr {})
        }
    }
}

enum AcHipotStatus
{
    Overflow,
    HiLimit,
    LoLimit,
    Abort,
    Pass,
}

impl std::str:FromStr for AcHipotStatus
{
    type Err = ParseAcHipotStatusErr;

    fn from_str(data_str: &str) -> Result<Self, Self::Err>
    {
        match data_str {
            "OFL" => Ok(Self::Overflow),
            "HI-Limit" => Ok(Self::HiLimit),
            "LO-Limit" => Ok(Self::LoLimit),
            "Abort" => Ok(Self::Abort),
            "Pass" => Ok(Self::Pass),
            _ => Err(ParseAcHipotStatusErr {})
        }
    }
}

macro_rules! parse_token
{
    ( $tok:expr, $tok_type:ty, $ln:expr, $idx:ident, $fail_mesg:expr, $raw_str:expr ) => {
        if let Some((index, token)) = $tok {
            $idx = index;
            token.parse::<$tok_type>()
                .map_err(|err| {
                    FormatError {
                        raw_data: String::from($raw_str),
                        line: $ln,
                        token: index + 1,
                        mesg: $fail_mesg,
                        cause: Some(FormatErrorCause::from(err))
                    }
                })
        }
        else {
            Err(FormatError {
                raw_data: String::from($raw_str),
                line: $ln,
                token: $idx + 2,
                mesg: $fail_mesg,
                cause: Some(FormatErrorCause::Truncated),
            })
        }
    }
}

pub(super) struct SciTestData
{
    data: TestData,
}

impl SciTestData
{
    fn parse_gnd_bond<I>(
        sequence_num: u32,
        step_num: u32,
        mut tokens: I,
        mut current_index: usize,
        data_str: &str,
    )
        -> Result<Self, ParseTestDataErr>

        where I: std::iter::Iterator<(usize, &str)>
    {
        let status = parse_token!(tokens.next(), GndBondStatus, 1, current_index, "failed to parse ground bond status", data_str)?;

        // This is just a progress value. Not relevant to final output
        tokens.next();

        TestData::GndBond(GndBondData {
            sequence_num: sequence_num,
            step_num: step_num,
            outcome: match status {
                GndBondStatus::HiLimit => {
                    let gnd_resistance = parse_token!(tokens.next(), u64, 1, current_index, "failed to parse ground resistance", data_str)?;
                    GndBondOutcome::ResistanceExcessive(Ohm::from::<Milli>(gnd_resistance))
                },
                GndBondStatus::LoLimit => {
                    let gnd_resistance = parse_token!(tokens.next(), u64, 1, current_index, "failed to parse ground resistance", data_str)?;
                    GndBondOutcome::ResistanceSubnormal(Ohm::from::<Milli>(gnd_resistance))
                },
                GndBondStatus::Abort => GndBondOutcome::Aborted,
                GndBondStatus::Pass => {
                    let gnd_resistance = parse_token!(tokens.next(), u64, 1, current_index, "failed to parse ground resistance", data_str)?;
                    GndBondOutcome::Passed(Ohm::from::<Milli>(gnd_resistance))
                },
            }
        })
    }

    fn parse_ac_hipot<I>(
        sequence_num: u32,
        step_num: u32,
        mut tokens: I,
        mut current_index: usize,
        data_str: &str,
    )
        -> Result<Self, ParseTestDataErr>

        where I: std::iter::Iterator<(usize, &str)>
    {
        let status = parse_token!(tokens.next(), AcHipotStatus, 1, current_index, "failed to parse ground bond status", data_str)?;

        // This is just a progress value. Not relevant to final output
        tokens.next();

        let data = TestData::AcHipot(AcHipotData {
            sequence_num: sequence_num,
            step_num: step_num,
            outcome: match status {
                AcHipotStatus::Overflow => AcHipotOutcome::Overflow,
                AcHipotStatus::HiLimit => {
                    let leak_current = parse_token!(tokens.next(), f64, 1, current_index, "failed to parse hipot leak current", data_str)?;
                    GndBondOutcome::ResistanceExcessive(Ampere::from_f64::<Milli>(leak_current))
                },
                AcHipotStatus::LoLimit => {
                    let leak_current = parse_token!(tokens.next(), f64, 1, current_index, "failed to parse hipot leak current", data_str)?;
                    GndBondOutcome::ResistanceSubnormal(Ampere::from_f64::<Milli>(leak_current))
                },
                AcHipotStatus::Abort => GndBondOutcome::Aborted,
                AcHipotStatus::Pass => {
                    let leak_current = parse_token!(tokens.next(), f64, 1, current_index, "failed to parse hipot leak current", data_str)?;
                    GndBondOutcome::Passed(Ampere::from_f64::<Milli>(leak_current))
                },
            }
        });

        Ok(Self {
            data: data
        })
    }
}

impl std::str::FromStr for SciTestData
{
    type Err = ParseTestDataErr;

    fn from_str(data_str: &str) -> Result<Self, Self::Err>
    {
        let mut current_index = 0;
        let mut tokens = data_str.split(',').filter(|tok| !tok.is_empty()).enumerate();
        let sequence_num = parse_token!(tokens.next(), u32, 1, current_index, "failed to parse sequence number", data_str)?;
        let step_num = parse_token!(tokens.next(), u32, 1, current_index, "failed to parse step number", data_str)?;
        let test_type = parse_token!(tokens.next(), TestType, 1, current_index, "failed to parse test type", data_str)?;

        match test_type {
            TestType::GndBond => Self::parse_gnd_bond(sequence_num, step_num, tokens, current_index),
            TestType::AcHipot => Self::parse_ac_hipot(sequence_num, step_num, tokens, current_index),
        }
    }
}
