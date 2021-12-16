//! Test sequence editing utilities

use crate::{
    cmd::{ CmdSet, AcFrequency, AcHipotParam, GndBondParam },
    units::{ Ampere, Volt, Ohm, Second },
};

#[derive(Clone)]
pub struct AcHipotTestSpec
{
    voltage: Option<Volt>,
    dwell: Option<Second>,
    leak_current_min: Option<Ampere>,
    leak_current_max: Option<Ampere>,
    frequency: Option<AcFrequency>,
    ramp: Option<Second>,
}

impl AcHipotTestSpec
{
    pub fn new() -> Self
    {
        Self {
            voltage: None,
            dwell: None,
            leak_current_min: None,
            leak_current_max: None,
            frequency: None,
            ramp: None,
        }
    }

    pub fn voltage(mut self, volts: Volt) -> Self
    {
        self.voltage = Some(volts);
        self
    }

    pub fn dwell_time(mut self, seconds: Second) -> Self
    {
        self.dwell = Some(seconds);
        self
    }

    pub fn leak_current_min(mut self, amps: Ampere) -> Self
    {
        self.leak_current_min = Some(amps);
        self
    }

    pub fn leak_current_max(mut self, amps: Ampere) -> Self
    {
        self.leak_current_max = Some(amps);
        self
    }

    pub fn ac_frequency(mut self, frequency: AcFrequency) -> Self
    {
        self.frequency = Some(frequency);
        self
    }

    pub fn ramp_time(mut self, seconds: Second) -> Self
    {
        self.ramp = Some(seconds);
        self
    }
}

#[derive(Clone)]
pub struct GndBondTestSpec
{
    check_current: Option<Ampere>,
    dwell: Option<Second>,
    resistance_max: Option<Ohm>,
    resistance_min: Option<Ohm>,
    frequency: Option<AcFrequency>,
    offset: Option<Ohm>,
}

impl GndBondTestSpec
{
    pub fn new() -> Self
    {
        Self {
            check_current: None,
            dwell: None,
            resistance_max: None,
            resistance_min: None,
            frequency: None,
            offset: None,
        }
    }

    pub fn check_current(mut self, amps: Ampere) -> Self
    {
        self.check_current = Some(amps);
        self
    }

    pub fn dwell_time(mut self, seconds: Second) -> Self
    {
        self.dwell = Some(seconds);
        self
    }

    pub fn resistance_max(mut self, ohms: Ohm) -> Self
    {
        self.resistance_max = Some(ohms);
        self
    }

    pub fn resistance_min(mut self, ohms: Ohm) -> Self
    {
        self.resistance_min = Some(ohms);
        self
    }

    pub fn ac_frequency(mut self, frequency: AcFrequency) -> Self
    {
        self.frequency = Some(frequency);
        self
    }

    pub fn resistance_offset(mut self, ohms: Ohm) -> Self
    {
        self.offset = Some(ohms);
        self
    }
}

#[derive(Clone)]
pub enum TestParams
{
    AcHipot(AcHipotTestSpec),
    GndBond(GndBondTestSpec)
}

#[derive(Clone)]
pub struct StepInfo
{
    step_num: u32,
    params: Option<TestParams>,
    continue_to_next: Option<bool>,
}

pub struct TestEditor
{
    sequence_num: u32,
    selected_step: u32,
}

impl TestEditor
{
    pub fn edit_sequence(sequence_num: u32) -> Self
    {
        Self {
            sequence_num: sequence_num,
            selected_step: 1,
        }
    }

    pub fn step(&mut self, step_num: u32)
    {
        self.selected_step = step_num;
    }

    pub fn test_spec(&self, step_buf: &mut [Option<StepInfo>], params: TestParams)
    {
        let index = self.selected_step as usize - 1;

        if index >= step_buf.len() {
            return;
        }
        
        if let Some(step_info) = &mut step_buf[index] {
            step_info.params = Some(params);
        }
        else {
            step_buf[index] = Some(StepInfo {
                step_num: self.selected_step,
                params: Some(params),
                continue_to_next: None,
            })
        }
    }

    pub fn continue_to_next(&self, step_buf: &mut [Option<StepInfo>], cont: bool)
    {
        let index = self.selected_step as usize - 1;

        if index >= step_buf.len() {
            return;
        }
        
        if let Some(step_info) = &mut step_buf[index] {
            step_info.continue_to_next = Some(cont);
        }
        else {
            step_buf[index] = Some(StepInfo {
                step_num: self.selected_step,
                params: None,
                continue_to_next: Some(cont),
            })
        }
    }

    pub fn compile(
        self,
        step_buf: &[Option<StepInfo>],
        ac_hipot_limits: Option<AcHipotDeviceLimits>,
        gnd_bond_limits: Option<GndBondDeviceLimits>,
        max_file_num: u32,
        max_step_num: u32,
    )
        -> Option<Vec<CmdSet>>
    {
        if self.sequence_num > max_file_num {
            return None;
        }

        let mut cmds = vec![CmdSet::LoadSequence(self.sequence_num)];

        for step in step_buf {
            if step.is_none() {
                continue;
            }
            let step = step.as_ref().unwrap();

            if step.step_num > max_step_num {
                return None;
            }

            // Steps are only committed by the builder when an override of either the continue_to_next
            // or test type/parameters occurs, so there is no need to delay adding a step selection
            // command
            cmds.push(CmdSet::SelectStep(step.step_num));

            if let Some(overrides) = &step.params {
                match overrides {
                    TestParams::AcHipot(params) => {
                        // devices which don't support AC hipot won't have an AC hipot builder exposed
                        // thus we can't ever get an AC hipot step
                        let ac_hipot_limits = ac_hipot_limits.as_ref().unwrap();
                        cmds.push(CmdSet::SetAcHipot);

                        if let Some(voltage) = params.voltage {
                            if voltage > ac_hipot_limits.voltage_max || voltage < ac_hipot_limits.voltage_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::Voltage(voltage)));
                        }

                        if let Some(dwell) = params.dwell {
                            if dwell > ac_hipot_limits.dwell_max || dwell < ac_hipot_limits.dwell_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::DwellTime(dwell)));
                        }

                        if let Some(leak_current_min) = params.leak_current_min {
                            if leak_current_min > ac_hipot_limits.leak_current_max
                                || leak_current_min < ac_hipot_limits.leak_current_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::LeakageMin(leak_current_min)));
                        }

                        if let Some(leak_current_max) = params.leak_current_max {
                            if leak_current_max > ac_hipot_limits.leak_current_max
                                || leak_current_max < ac_hipot_limits.leak_current_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::LeakageMax(leak_current_max)));
                        }

                        if let Some(ramp) = params.ramp {
                            if ramp > ac_hipot_limits.ramp_max || ramp < ac_hipot_limits.ramp_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::RampTime(ramp)));
                        }

                        if let Some(frequency) = params.frequency {
                            cmds.push(CmdSet::SetAcHipotParam(AcHipotParam::Frequency(frequency)));
                        }
                    },
                    TestParams::GndBond(params) => {
                        let gnd_bond_limits = gnd_bond_limits.as_ref().unwrap();
                        cmds.push(CmdSet::SetGndBond);

                        if let Some(check_current) = params.check_current {
                            if check_current > gnd_bond_limits.check_current_max
                                || check_current < gnd_bond_limits.check_current_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::CheckCurrent(check_current)));
                        }

                        if let Some(dwell) = params.dwell {
                            if dwell > gnd_bond_limits.dwell_max || dwell < gnd_bond_limits.dwell_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::DwellTime(dwell)));
                        }

                        if let Some(resistance_min) = params.resistance_min {
                            if resistance_min > gnd_bond_limits.resistance_max
                                || resistance_min < gnd_bond_limits.resistance_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::ResistanceMin(resistance_min)));
                        }

                        if let Some(resistance_max) = params.resistance_max {
                            if resistance_max > gnd_bond_limits.resistance_max
                                || resistance_max < gnd_bond_limits.resistance_min
                            {
                                return None;
                            }
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::ResistanceMax(resistance_max)));
                        }

                        if let Some(offset) = params.offset {
                            if offset > gnd_bond_limits.offset_max || offset < gnd_bond_limits.offset_min {
                                return None;
                            }
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::Offset(offset)));
                        }

                        if let Some(frequency) = params.frequency {
                            cmds.push(CmdSet::SetGndBondParam(GndBondParam::Frequency(frequency)));
                        }
                    }
                }
            }

            if let Some(cont) = step.continue_to_next {
                cmds.push(CmdSet::ContinueToNext(cont));
            }
        }

        Some(cmds)
    }
}

pub struct AcHipotDeviceLimits
{
    pub voltage_min: Volt,
    pub voltage_max: Volt,
    pub dwell_max: Second,
    pub dwell_min: Second,
    pub leak_current_max: Ampere,
    pub leak_current_min: Ampere,
    pub ramp_max: Second,
    pub ramp_min: Second,
}

pub struct GndBondDeviceLimits
{
    pub check_current_min: Ampere,
    pub check_current_max: Ampere,
    pub dwell_min: Second,
    pub dwell_max: Second,
    pub offset_min: Ohm,
    pub offset_max: Ohm,
    pub resistance_min: Ohm,
    pub resistance_max: Ohm,
}
