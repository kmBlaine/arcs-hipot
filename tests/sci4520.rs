
use arcs_hipot::{ Sci4520, AcHipotBuilder, GndBondBuilder, Amp, Ohm, Volt, AcFrequency };

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
        .edit_test(1)
        .step(1)
        .continue_to_next(true)
        .ac_hipot(AcHipotBuilder::new()
            .voltage(Volt::from_whole(1250))
            .leak_current_min(Amp::from_micros(800))
            .leak_current_max(Amp::from_micros(25_400))
            .ac_frequency(AcFrequency::Hz60)
            .dwell_time(std::time::Duration::from_millis(3000))
            .ramp_time(std::time::Duration::from_millis(900))
        )
        .exec()
        .await
        .is_ok()
    );
    
    assert!(device
        .edit_test(1)
        .step(2)
        .gnd_bond(GndBondBuilder::new()
            .check_current(Amp::from_whole(25))
            .resistance_max(Ohm::from_millis(150))
            .dwell_time(std::time::Duration::from_secs(2))
        )
        .continue_to_next(false)
        .exec()
        .await
        .is_ok()
    );
}
