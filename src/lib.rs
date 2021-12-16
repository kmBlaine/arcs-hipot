//! **A**synchronous-Rust **R**emote **C**ontrol for **S**CI **Hipot** testers

pub mod test_data;
#[macro_use]
pub mod units;
pub mod devices;

mod cmd;
mod executor;
mod test_edit;

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
