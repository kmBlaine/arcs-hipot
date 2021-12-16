//! Device command definition, parsing, and serialization

use std::fmt;
use crate::{
    units::{ Ampere, Ohm, Volt, Second, Milli, Kilo }
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum AcFrequency
{
    Hz50,
    Hz60,
}

#[derive(Clone)]
pub enum GndBondParam
{
    Frequency(AcFrequency),
    DwellTime(Second),
    CheckCurrent(Ampere),
    ResistanceMax(Ohm),
    ResistanceMin(Ohm),
    Offset(Ohm),
}

#[derive(Clone)]
pub enum AcHipotParam
{
    Frequency(AcFrequency),
    DwellTime(Second),
    RampTime(Second),
    Voltage(Volt),
    LeakageMax(Ampere),
    LeakageMin(Ampere),
}

#[derive(Clone)]
pub enum CmdSet
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

pub struct SciMultiSeqDisplay
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

pub struct SciSingleSeqDisplay
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

pub struct AssociatedResearchDisplay
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

pub trait CmdDisplayFactory: fmt::Display
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
