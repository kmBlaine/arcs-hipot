//! Device command definition, parsing, and serialization.
//!
//! This module revolves around the `CmdSet` enumeration which is more or less a byte code version of
//! the ASCII command set used by SCI devices. The enum exists for:
//!   1. The ease of developers attempting to reason about what the device will be asked to do.
//!
//!   2. Offer a way for there to be multiple wire representations of the same high-level commmand.
//!
//!   3. Make it possible to create programmatically novel sequences, methods, behaviors, etc without
//!      painstakingly arranging bare text commands which offer no compiler assistance whatsoever.
//!
//! The test editors in this library compile a sequences of these byte-coded commands and the other
//! methods usually use it as a backend to assure consistent behavior.
//!
//! One of the most crucial differences in representation comes from the ambiguity of certain commands between
//! generations of devices. Earlier SCI devices (e.g. the 4520) have a concept of both multiple
//! test sequences and multiple steps within each sequence, while later devices like the 448 only
//! have multiple steps. However, both of these commands support an ASCII command `FL <#>` to change
//! which procedure in memory is active. On the early devices, this command changes the sequence and
//! there is a separate command, `SS <#>`, changes the step in the sequence. On the later devices,
//! there is the same `FL` command and it changes the step, but the `SS` command isn't recognized.
//!
//! Another crucial difference is that the semantics of a command sometimes change based on context.
//! For example, the `EC <#>` command edits current values, but it is only applicable for ground
//! bond and hipot tests; insulation resistance need not apply. Even here are are ambiguities. When
//! issued with a hipot step active, the value issued to the device must be in milliamps (mA) while
//! when it is issued with a ground bond step active the value must be in amps (A). This is because
//! while being the same command, they refer to different things entirely. One is the high current
//! used to ensure ground safety and one is the low current expected due to good ground isolation
//! from the mains.
//!
//! The older Associated Research devices handle this better or worse (depending on how you look at
//! it) by having separate commands for every parameter. A key example is that ground check current
//! is a different command `SY` from AC hipot leak current max `SA` and minimum `SB`. But one thing
//! it definitely means is that the simplest way to manage this from the byte code's perspective is
//! that new variants of `CmdSet` should be as precise as possible in their meaning.
//!
//! # Notes for `fmt::Display` and serializer implementations
//! Implementors should NOT encode a line ending into the command. This is handled separately by the
//! protocol executor.

use std::fmt;
use crate::{
    units::{ Ampere, Ohm, Volt, Second, }
};

/// The AC frequency to be used in a test sequence
///
/// Some devices support both 50 and 60 Hz AC testing and can be configured on the fly to use one
/// or the other.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AcFrequency
{
    /// 50Hz AC
    Hz50,
    /// 60Hz AC
    Hz60,
}

/// Parameters relating to ground bond tests
///
/// These are split off as a substructure to reduce the verbosity of the `CmdSet` variants and make
/// it clear that these commands edit parameters strictly related to ground bond tests. Also
/// although these options sometimes serialize to the same wire command as for hipot and insulation
/// resistance tests, they often have different semantics.
#[derive(Debug, Clone)]
pub enum GndBondParam
{
    /// AC frequency to be used for the check current
    Frequency(AcFrequency),
    /// Duration for which the check current will be active
    DwellTime(Second),
    /// The magnitude of the check current
    CheckCurrent(Ampere),
    /// Maximum allowable resistance of the ground connection
    ///
    /// Resistance values exceeding this shall fail the test
    ResistanceMax(Ohm),
    /// Minimum allowable resistance of the ground connection
    ///
    /// Resistance values falling below this shall fail the test
    ResistanceMin(Ohm),
    /// The baseline resistance of the wiring which interfaces the device to the unit under test
    ///
    /// This value is subtracted from the measured resistance value before evaluating pass-fail status.
    Offset(Ohm),
}

/// Parameters relating to AC hipot tests
///
/// These are split off as a substructure to reduce the verbosity of the `CmdSet` variants and make
/// it clear that these commands edit parameters strictly related to AC hipot tests. Also
/// although these options sometimes serialize to the same wire command as for ground bond and insulation
/// resistance tests, they often have different semantics.
#[derive(Debug, Clone)]
pub enum AcHipotParam
{
    /// AC frequency to be used for the check voltage
    Frequency(AcFrequency),
    /// Duration for which the unit under test will be subjected to high voltage
    DwellTime(Second),
    /// Duration over which the check voltage will be ramped from 0 volts to the full check potential
    RampTime(Second),
    /// The magnitude of the check voltage
    Voltage(Volt),
    /// Maximum allowable leakage current from the mains
    ///
    /// Leak currents exceeding this value shall fail the test
    LeakageMax(Ampere),
    /// Minimum allowable leakage current from the mains
    ///
    /// Leak currents falling below this value shall fail the test
    LeakageMin(Ampere),
}

/// Byte-coded command to be issued to the device
///
/// Refer to the module-level documentation for purpose and semantics of this enum.
#[derive(Debug, Clone)]
pub enum CmdSet
{
    /// Ready a test sequence stored on the device for execution and editing
    ///
    /// Only applicable to devices which support multiple sequences.
    LoadSequence(u32),
    /// Select a step in the currently loaded sequence
    ///
    /// Applicable to all devices though serialization may differ.
    SelectStep(u32),
    /// Change currently selected step to an AC hipot test
    ///
    /// Whether this command recalls stored parameters or resets the step to default parameters is
    /// device-dependent.
    SetAcHipot,
    /// Change currently selected step to a ground bond test
    ///
    /// Whether this command recalls stored parameters or resets the step to default parameters is
    /// device-dependent.
    SetGndBond,
    /// Changes whether tester halts upon the completion of the current step or continues to the next
    ///
    /// `true` for continue on step completion. `false` for halting behavior
    ContinueToNext(bool),
    /// Set parameter for an AC hipot test
    ///
    /// See the `AcHipotParam` enum for more details
    SetAcHipotParam(AcHipotParam),
    /// Set parameter for a ground bond test
    ///
    /// See the `GndBondParam` enum for more details
    SetGndBondParam(GndBondParam),
    /// Run the currently loaded test starting at the currently selected step
    RunTest,
    /// Reset the device to a ready-to-run state, cancelling any active test but preserving tests in
    /// memory.
    SoftReset,
    /// Gets the test data for the given step
    GetTestData(u32),
}

/// Formatter/serializer for the core SCI command set
///
/// The `LoadSequence` and `SelectStep` variants of `CmdSet` are not stable across devices so they
/// need different display implementations.
///
/// # Panics
/// Panics if given a `LoadSequence` or `SelectStep` step
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
            GndBondParam::CheckCurrent(check_current) => write!(f, "EC {:.1}", view_anon!(check_current)),
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

/// Display implementor for an SCI device which supports multiple test sequences
///
/// This wraps a `CmdSet` and provides a way of converting it to an ASCII string for sending over the 
/// wire.
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

/// Display implementor for an SCI device which does not support multiple test sequences
///
/// This wraps a `CmdSet` and provides a way of converting it to an ASCII string for sending over the 
/// wire.
///
/// # Panics
/// Attempting to display a `LoadSequence` command will cause a panic
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

/// Display implementor for an Associated Research device
///
/// This wraps a `CmdSet` and provides a way of converting it to an ASCII string for sending over the 
/// wire.
///
/// # Notes on implementation
/// At the time this library was written, it is unknown if there are any AssociatedResearch devices
/// which do not support multiple test sequences. Hence all are assumed to follow the model of the
/// 7704
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

/// Provides a constructor method on a type for creating a string serializer for a `CmdSet`
///
/// This trait is intended as a zero-cost abstraction. Rather than enumerate the possible serizalizers
/// and match at runtime, this allows the `CmdSet` serialization backends to be defined using a generic.
/// That way the correct serialization function is determined at compile and keeps allocations
/// and decision logic at runtime minimized.
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

#[cfg(test)]
mod tests
{
    use crate::{
        cmd::{ SciSingleSeqDisplay, SciMultiSeqDisplay, CmdSet, AcHipotParam, GndBondParam, AcFrequency },
        units::*,
        units::scalar::{ Milli, Micro, Kilo },
    };

    #[test]
    fn serialize_sci_core()
    {
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetAcHipot).to_string(),
            "SAA"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetGndBond).to_string(),
            "SAG"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::ContinueToNext(true)).to_string(),
            "ECC 1"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::ContinueToNext(false)).to_string(),
            "ECC 0"
        );

        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::CheckCurrent(Ampere::from_base(25)))).to_string(),
            "EC 25.0"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::DwellTime(Second::from::<Milli>(2_345)))).to_string(),
            "EDW 2.3"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::Frequency(AcFrequency::Hz50))).to_string(),
            "EF 0"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::Frequency(AcFrequency::Hz60))).to_string(),
            "EF 1"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::ResistanceMax(Ohm::from::<Milli>(128)))).to_string(),
            "EH 128"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::ResistanceMin(Ohm::from_base(0)))).to_string(),
            "EL 0"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetGndBondParam(GndBondParam::Offset(Ohm::from::<Milli>(56)))).to_string(),
            "EO 56"
        );

        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::Frequency(AcFrequency::Hz50))).to_string(),
            "EF 0"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::Frequency(AcFrequency::Hz60))).to_string(),
            "EF 1"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::LeakageMax(Ampere::from::<Micro>(67_800)))).to_string(),
            "EH 67.8"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::LeakageMin(Ampere::from_base(0)))).to_string(),
            "EL 0.0"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::RampTime(Second::from::<Milli>(4_321)))).to_string(),
            "ERU 4.3"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::Voltage(Volt::from_base(1250)))).to_string(),
            "EV 1.25"
        );
        assert_eq!(
            &SciSingleSeqDisplay::from(CmdSet::SetAcHipotParam(AcHipotParam::DwellTime(Second::from::<Milli>(2_345)))).to_string(),
            "EDW 2.3"
        );
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
}
