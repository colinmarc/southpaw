use std::{io::Read as _, path::Path};

use southpaw::sys::{ABS_HAT0X, ABS_HAT0Y, BTN_EAST, BTN_SOUTH, BTN_START, EV_ABS, EV_KEY};
use southpaw::{AbsAxis, AbsInfo, Device, DeviceTree, KeyCode};

const CODE: [(&str, u16, i32); 11] = [
    ("UP", ABS_HAT0Y, -1),
    ("UP", ABS_HAT0Y, -1),
    ("DOWN", ABS_HAT0Y, 1),
    ("DOWN", ABS_HAT0Y, 1),
    ("LEFT", ABS_HAT0X, -1),
    ("RIGHT", ABS_HAT0X, 1),
    ("LEFT", ABS_HAT0X, -1),
    ("RIGHT", ABS_HAT0X, 1),
    ("B", BTN_SOUTH, 1),
    ("A", BTN_EAST, 1),
    ("START", BTN_START, 1),
];

fn main() -> std::io::Result<()> {
    env_logger::init();

    let mut args = std::env::args();
    let exe = args.next().unwrap();
    let Some(path) = args.next() else {
        eprintln!("usage: {} PATH", exe);
        std::process::exit(1);
    };

    let path = Path::new(&path);
    let _ = std::fs::create_dir_all(path);

    let mut tree = DeviceTree::new();
    let _mount = tree.mount(path);

    let controller = Device::builder()
        .name("SNES Gamepad")
        .id(0x03, 0x0810, 0xe501, 0x01)
        .supported_key_codes([
            KeyCode::BtnSouth,
            KeyCode::BtnNorth,
            KeyCode::BtnEast,
            KeyCode::BtnWest,
        ])
        .supported_absolute_axis(
            AbsAxis::X,
            AbsInfo {
                value: 0,
                minimum: -128,
                maximum: 128,
                ..Default::default()
            },
        )
        .supported_absolute_axis(
            AbsAxis::Y,
            AbsInfo {
                value: 0,
                minimum: -128,
                maximum: 128,
                ..Default::default()
            },
        )
        .add_to_tree(&mut tree, "event1")?;

    println!("Press enter to begin sending the secret code...");
    let _ = std::io::stdin().read(&mut [0u8]).unwrap();

    for (s, code, value) in CODE {
        println!("{}!", s);
        gen_button_press(&controller, code, value)?;
        std::thread::sleep(std::time::Duration::from_millis(500));
    }

    Ok(())
}

fn gen_button_press(controller: &southpaw::Device, code: u16, value: i32) -> std::io::Result<()> {
    let event_type = match code {
        ABS_HAT0X | ABS_HAT0Y => EV_ABS,
        _ => EV_KEY,
    };

    controller.publish_event(southpaw::InputEvent::new(event_type, code, value))?;
    controller.publish_event(southpaw::InputEvent::new(event_type, code, 0))?;

    Ok(())
}
