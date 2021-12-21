#[cfg(test)]
mod sci_4520
{
use arcs_hipot::prelude::*;
use arcs_hipot::{
    ival, view,
    devices::Sci4520,
};

const DEVICE_NAME: &'static str = "/dev/ttyS0";

static PORT_SETTINGS: tokio_serial::SerialPortSettings = tokio_serial::SerialPortSettings {
    baud_rate: 9600,
    parity: tokio_serial::Parity::None,
    stop_bits: tokio_serial::StopBits::One,
    data_bits: tokio_serial::DataBits::Eight,
    flow_control: tokio_serial::FlowControl::None,
    timeout: std::time::Duration::from_millis(10),
};

#[tokio::test]
async fn create_gnd_bond_w_ac_hipot()
{
    let mut device = Sci4520::with(tokio_serial::Serial::from_path(DEVICE_NAME, &PORT_SETTINGS).unwrap());

    assert!(device
        .edit_sequence(4)
        .step(1)
        .continue_to_next(true)
        .gnd_bond(GndBondTestSpec::new()
            .check_current(Ampere::from::<Base>(25))
            .resistance_max(Ohm::from::<Milli>(150))
            .dwell_time(Second::from_f64::<Base>(2.0))
        )
        .step(2)
        .ac_hipot(AcHipotTestSpec::new()
            .voltage(ival!(1250, Volt))
            .leak_current_min(ival!(800, Micro Ampere))
            .leak_current_max(ival!(15, Milli Ampere))
            .ac_frequency(AcFrequency::Hz60)
            .dwell_time(ival!(3, Second))
            .ramp_time(ival!(900, Milli Second))
        )
        .continue_to_next(false)
        .commit()
        .await
        .is_ok()
    );
}

#[tokio::test]
async fn load_and_run_test()
{
    let mut device = Sci4520::with(tokio_serial::Serial::from_path(DEVICE_NAME, &PORT_SETTINGS).unwrap());

    assert!(device.load_sequence(4).await.is_ok());
    assert!(device.select_step(1).await.is_ok());

    // It was observed during integration testing that this device needs a little time after it loads
    // a sequence before it will successfully run a test
    //
    // It will positively acknowledge that it started the test, but it doesn't actually do it
    tokio::time::delay_for(tokio::time::Duration::from_millis(500)).await;

    assert!(device.start_test().await.is_ok());
}

#[tokio::test]
async fn retrieve_results()
{
    let mut device = Sci4520::with(tokio_serial::Serial::from_path(DEVICE_NAME, &PORT_SETTINGS).unwrap());

    let results = device.get_test_data(1).await;
    assert!(results.is_ok());
    let results = results.unwrap();

    match results {
        TestData::GndBond(data) => {
            println!(
                "Sequence: {}\nStep: {}\nOutcome: {}",
                data.sequence_num,
                data.step_num,
                match data.outcome {
                    GndBondOutcome::ResistanceExcessive(ohms) => format!("Ground resistance exceeded allowable ({})", ohms.display::<Milli>()),
                    GndBondOutcome::ResistanceSubnormal(ohms) => format!("Ground resistance below allowable ({})", ohms.display::<Milli>()),
                    GndBondOutcome::Aborted => format!("Ground bond test aborted"),
                    GndBondOutcome::Passed(ohms) => format!("Ground bond passed ({})", ohms.display::<Milli>()),
                }
            );
        },
        TestData::AcHipot(data) => {
            println!(
                "Sequence: {}\nStep: {}\nOutcome: {}",
                data.sequence_num,
                data.step_num,
                match data.outcome {
                    AcHipotOutcome::LeakOverflow => format!("AC leakage too large to be measured. Is there a short circuit?"),
                    AcHipotOutcome::LeakExcessive(amps) => format!("AC leakage exceeded allowable ({})", view!(amps, Milli)),
                    AcHipotOutcome::LeakSubnormal(amps) => format!("AC leakage below allowable ({})", view!(amps, Milli)),
                    AcHipotOutcome::Aborted => format!("AC hipot test aborted"),
                    AcHipotOutcome::Passed(amps) => format!("AC hipot passed ({})", view!(amps, Milli)),
                }
            );
        }
    }
}
}
