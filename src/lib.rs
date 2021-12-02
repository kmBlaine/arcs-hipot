//! **A**synchronous-Rust **R**emote **C**ontrol for **S**CI **Hipot** testers

use tokio::io::{ AsyncReadExt, AsyncWriteExt };

pub enum AcFrequency
{
    Hz50,
    Hz60,
}

enum TestLimit
{
    Microamp(u32),
    Megaohm(u16),
    Milliohm(u16),
}

enum CmdSet
{
    /// Ready a test file stored on the device for execution and editing
    ///
    /// Command: `FL <file_number>`
    LoadFile(u8),
    /// Select a step in the currently loaded file
    ///
    /// Command: `SS <step_number>`
    SelectStep(u8),
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
    /// On the currently selected step, set the electrical current used to check the quality of the
    /// ground connection in amperes. Applicable only if the current step is a ground
    /// bond test.
    ///
    /// Command: `EC <amps>`
    SetGndCheckCurrent(f64),
    /// On the currently selected step, set the dwell time of the primary test current in seconds.
    /// Applicable only if the current step is an AC or DC hipot or ground bond.
    ///
    /// Command: `EDW <seconds>`
    SetDwell(f64),
    /// On the currently selected step, set the AC frequency to be used i.e. 50 or 60 Hz. Applicable
    /// only if the current step is an AC hipot or ground bond.
    ///
    /// Command: `EF <1|0>`
    SetAcFrequency(AcFrequency),
    /// On the currently selected step, set the maximum allowble measured value. The precise
    /// value and semantics depend on the test being run.
    ///
    /// Command: `EH <limit>`
    SetHiLimit(TestLimit),
    /// On the currently selected step, set the maximum allowble measured value. The precise
    /// value and semantics depend on the test being run.
    ///
    /// Command: `EL <limit>`
    SetLoLimit(TestLimit),
    /// On the currently selected step, set the ground connection resistance offset in milliohms.
    /// Applicable only to ground bond tests.
    ///
    /// Command: `EO <milliohm>`
    SetGndOffset(u8),
    /// On the currently selected step, set the time it takes to ramp up the voltage to the
    /// full test load. Applicable only to AC and DC hipot tests.
    ///
    /// Command: `ERU <seconds>`
    SetRampTime(f64),
    /// On the currently selected step, set the high-potential voltage used. Only applicable to AC
    /// and DC hipot tests.
    ///
    /// Command: `EV <kilovolt>`
    SetHipotVoltage(f64),
}

pub struct StepBuilder
{
    cmds: Vec<CmdSet>,
}

impl StepBuilder
{
    pub fn ac_hipot(mut self) -> AcHipotBuilder
    {
        self.cmds.push(CmdSet::SetAcHipot);

        AcHipotBuilder {
            cmds: self.cmds,
        }
    }

    pub fn gnd_bond(mut self) -> GndBondBuilder
    {
        self.cmds.push(CmdSet::SetGndBond);

        GndBondBuilder {
            cmds: self.cmds,
        }
    }
}

pub struct AcHipotBuilder
{
    cmds: Vec<CmdSet>,
}


pub struct GndBondBuilder
{
    cmds: Vec<CmdSet>,
}

impl GndBondBuilder
{
    pub fn set_check_current(mut self, amps: f64) -> Self
    {
        self.cmds.push(CmdSet::SetGndCheckCurrent(amps));
        self
    }

    pub fn set_dwell(mut self, seconds: f64) -> Self
    {
        self.cmds.push(CmdSet::SetDwell(seconds));
        self
    }

    pub fn set_max_resistance(mut self, milliohms: u16) -> Self
    {
        self.cmds.push(CmdSet::SetHiLimit(TestLimit::Milliohm(milliohms)));
        self
    }

    pub fn set_min_resistance(mut self, milliohms: u16) -> Self
    {
        self.cmds.push(CmdSet::SetLoLimit(TestLimit::Milliohm(milliohms)));
        self
    }

    pub fn set_frequency(mut self, frequency: AcFrequency) -> Self
    {
        self.cmds.push(CmdSet::SetAcFrequency(frequency));
        self
    }

    pub fn set_offset(mut self, milliohms: u8) -> Self
    {
        self.cmds.push(CmdSet::SetGndOffset(milliohms));
        self
    }

    pub fn set_continue_to_next(mut self, cont: bool) -> Self
    {
        self.cmds.push(CmdSet::ContinueToNext(cont));
        self
    }

    pub fn select_step(self, step: u8) -> StepBuilder
    {
        StepBuilder {
            cmds: self.cmds
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
