
use arcs_hipot::{ Sci4520, AcHipotTestSpec, GndBondTestSpec, Amp, Ohm, Volt, AcFrequency };
use arcs_hipot::test_data::{ TestData, AcHipotOutcome, GndBondOutcome };

const DEVICE_NAME: &'static str = "/dev/ttyS0";

#[tokio::test]
async fn create_ac_hipot_w_gnd_bond()
{
    let port_settings = tokio_serial::SerialPortSettings {
        baud_rate: 9600,
        parity: tokio_serial::Parity::None,
        stop_bits: tokio_serial::StopBits::One,
        data_bits: tokio_serial::DataBits::Eight,
        flow_control: tokio_serial::FlowControl::None,
        ..Default::default()
    };

    let mut device = Sci4520::with(tokio_serial::Serial::from_path(DEVICE_NAME, &port_settings).unwrap());

    assert!(device
        .edit_sequence(4)
        .step(1)
        .continue_to_next(true)
        .ac_hipot(AcHipotTestSpec::new()
            .voltage(Volt::from_whole(1250))
            .leak_current_min(Amp::from_micros(800))
            .leak_current_max(Amp::from_micros(25_400))
            .ac_frequency(AcFrequency::Hz60)
            .dwell_time(std::time::Duration::from_millis(3000))
            .ramp_time(std::time::Duration::from_millis(900))
        )
        .commit()
        .await
        .is_ok()
    );
    
    assert!(device
        .edit_sequence(4)
        .step(2)
        .gnd_bond(GndBondTestSpec::new()
            .check_current(Amp::from_whole(25))
            .resistance_max(Ohm::from_millis(150))
            .dwell_time(std::time::Duration::from_secs(2))
        )
        .continue_to_next(false)
        .commit()
        .await
        .is_ok()
    );

    assert!(device.load_sequence(4).await.is_ok());
    assert!(device.start_test().await.is_ok());

    tokio::time::delay_for(tokio::time::Duration::from_secs(10)).await;

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
                    GndBondOutcome::ResistanceExcessive(ohms) => format!("Ground resistance exceeded allowable ({})", ohms.display_milli(0)),
                    GndBondOutcome::ResistanceSubnormal(ohms) => format!("Ground resistance below allowable ({})", ohms.display_milli(0)),
                    GndBondOutcome::Aborted => format!("Ground bond test aborted"),
                    GndBondOutcome::Passed(ohms) => format!("Ground bond passed ({})", ohms.display_milli(0)),
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
                    AcHipotOutcome::LeakExcessive(amps) => format!("AC leakage exceeded allowable ({})", amps.display_milli(1)),
                    AcHipotOutcome::LeakSubnormal(amps) => format!("AC leakage below allowable ({})", amps.display_milli(1)),
                    AcHipotOutcome::Aborted => format!("AC hipot test aborted"),
                    AcHipotOutcome::Passed(amps) => format!("AC hipot passed ({})", amps.display_milli(1)),
                }
            );
        }
    }
}
