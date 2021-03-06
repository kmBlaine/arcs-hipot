// mod sci_448
// {
// #[macro_use]
// use arcs_hipot::prelude::*;
// use arcs_hipot::devices::Sci4520;

// const DEVICE_NAME: &'static str = "/dev/ttyS0";

// #[tokio::test]
// async fn create_ac_hipot_w_gnd_bond()
// {
//     let port_settings = tokio_serial::SerialPortSettings {
//         baud_rate: 9600,
//         parity: tokio_serial::Parity::None,
//         stop_bits: tokio_serial::StopBits::One,
//         data_bits: tokio_serial::DataBits::Eight,
//         flow_control: tokio_serial::FlowControl::None,
//         ..Default::default()
//     };

//     let mut device = Sci448::with(tokio_serial::Serial::from_path(DEVICE_NAME, &port_settings).unwrap());

//     assert!(device
//         .edit_sequence()
//         .step(1)
//         .continue_to_next(true)
//         .ac_hipot(AcHipotTestSpec::new()
//             .voltage(Volt::from_whole(1250))
//             .leak_current_min(Amp::from_micros(800))
//             .leak_current_max(Amp::from_micros(25_400))
//             .ac_frequency(AcFrequency::Hz60)
//             .dwell_time(std::time::Duration::from_millis(3000))
//             .ramp_time(std::time::Duration::from_millis(900))
//         )
//         .commit()
//         .await
//         .is_ok()
//     );
    
//     assert!(device
//         .edit_sequence()
//         .step(2)
//         .gnd_bond(GndBondTestSpec::new()
//             .check_current(Amp::from_whole(25))
//             .resistance_max(Ohm::from_millis(150))
//             .dwell_time(std::time::Duration::from_secs(2))
//         )
//         .continue_to_next(false)
//         .commit()
//         .await
//         .is_ok()
//     );
// }
// }
