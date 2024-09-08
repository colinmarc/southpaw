//! A library for emulating input character devices using FUSE.
#![cfg(target_os = "linux")]
#![warn(
    anonymous_parameters,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    nonstandard_style,
    rust_2018_idioms,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    unused_extern_crates,
    unused_qualifications,
    variant_size_differences
)]

use std::{
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

mod cmd;
mod enums;
mod fs;
mod ring;

pub use enums::*;
use fs::*;
use fuser as fuse;

#[doc = "input constants used to construct [InputEvent]s"]
#[allow(missing_docs)]
pub mod sys;

/// input_event from the kernel source. Guaranteed to be ABI-compatible.
#[repr(C)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct InputEvent {
    time_tv_sec: libc::time_t,
    time_tv_usec: libc::suseconds_t,

    /// The type of the event, such as EV_KEY or EV_LED.
    pub event_type: u16,

    /// The specific key or button, or some other identifier, depending on [event_type].
    pub code: u16,

    /// The state being communicated by the event.
    pub value: i32,
}

impl InputEvent {
    /// Constructs a new InputEvent with zeroed time.
    pub fn new<T>(event_type: T, code: impl Into<u16>, value: impl Into<i32>) -> Self
    where
        T: TryInto<EventType>,
        T::Error: std::fmt::Debug,
    {
        let event_type = event_type.try_into().expect("invalid EventType").into();

        Self {
            time_tv_sec: 0,
            time_tv_usec: 0,
            event_type,
            code: code.into(),
            value: value.into(),
        }
    }
}

/// A file that acts like the character devices you would usually see in
/// /dev/input/event*.
pub struct Device {
    _session: fuse::BackgroundSession,
    notifier: fuse::Notifier,
    state: Arc<Mutex<DeviceState>>,
    path: PathBuf,
    name: String,
}

/// A temporary object for configuring a [Device].
pub struct DeviceBuilder {
    root_attr: fuser::FileAttr,
    dev_attr: DeviceAttr,
    mount_options: Vec<fuser::MountOption>,
}

/// An error returned when the FUSE filesystem underlying a [Device] was
/// externally unmounted. This can result from an internal error or from, for
/// example, running `umount` on the host.
#[derive(Debug, Clone, Copy)]
pub struct DefunctError;

impl std::fmt::Display for DefunctError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "device defunct")
    }
}

impl std::error::Error for DefunctError {}

impl std::fmt::Debug for DeviceBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DeviceBuilder").finish_non_exhaustive()
    }
}

impl Default for DeviceBuilder {
    fn default() -> Self {
        let start = std::time::SystemTime::now();
        let uid = rustix::process::getuid().as_raw();
        let gid = rustix::process::getgid().as_raw();

        let mount_options = vec![fuse::MountOption::FSName("southpaw".to_string())];

        Self {
            root_attr: fuse::FileAttr {
                ino: 1,
                size: 0,
                blocks: 0,
                atime: start,
                mtime: start,
                ctime: start,
                crtime: start,
                kind: fuse::FileType::RegularFile,
                perm: 0o660,
                nlink: 1,
                uid,
                gid,
                rdev: 0,
                blksize: 512,
                flags: 0,
            },
            dev_attr: Default::default(),
            mount_options,
        }
    }
}

impl DeviceBuilder {
    /// Sets the name of the device. The name must not contain null bytes.
    pub fn name(mut self, name: impl AsRef<str>) -> Self {
        self.dev_attr.name = std::ffi::CString::new(name.as_ref()).expect("invalid name");
        self
    }

    /// Sets the ID of the device. Values for 'vendor', 'product', and 'version'
    /// are arbitrary and vendor/device specific.
    pub fn id<B>(mut self, bustype: B, vendor: u16, product: u16, version: u16) -> Self
    where
        B: TryInto<BusType>,
        B::Error: std::fmt::Debug,
    {
        let bustype = bustype.try_into().expect("invalid BusType").into();

        self.dev_attr.id = libc::input_id {
            bustype,
            vendor,
            product,
            version,
        };
        self
    }

    /// Sets an input property for the device.
    pub fn property<T>(mut self, property: T) -> Self
    where
        T: TryInto<InputProperty>,
        T::Error: std::fmt::Debug,
    {
        let property: u16 = property.try_into().expect("invalid InputProperty").into();

        sys::set_bit(&mut self.dev_attr.prop_bits, property);
        self
    }

    /// Advertizes supported event codes.
    pub fn supported_event_codes(mut self, codes: impl IntoIterator<Item = Scancode>) -> Self {
        for code in codes.into_iter() {
            self = self.supported_event_codes_raw(code.event_type(), [u16::from(code)])
        }

        self
    }

    /// For the given event_type, advertizes support for the given codes. No
    /// validation on the code is performed, beyond that it is less than
    /// [sys::KEY_MAX].
    pub fn supported_event_codes_raw<T, U>(
        mut self,
        event_type: T,
        codes: impl IntoIterator<Item = U>,
    ) -> Self
    where
        T: TryInto<EventType>,
        T::Error: std::fmt::Debug,
        U: Into<u16>,
    {
        let event_type: EventType = event_type.try_into().expect("invalid EventType");
        let bits = match event_type {
            EventType::Key => &mut self.dev_attr.support.key_bits,
            EventType::RelativeAxis => &mut self.dev_attr.support.rel_bits,
            EventType::AbsoluteAxis => &mut self.dev_attr.support.abs_bits,
            EventType::Misc => &mut self.dev_attr.support.msc_bits,
            EventType::Led => &mut self.dev_attr.support.led_bits,
            EventType::Sound => &mut self.dev_attr.support.snd_bits,
            EventType::ForceFeedback => &mut self.dev_attr.support.ff_bits,
            EventType::Switch => &mut self.dev_attr.support.sw_bits,
            _ => panic!("unexpected EventType: {:?}", event_type),
        };

        sys::set_bit(&mut self.dev_attr.support.ev_bits, u16::from(event_type));
        for code in codes.into_iter() {
            sys::set_bit(bits, code.into())
        }

        self
    }

    /// Create the device by mounting it at the given path. The path must exist.
    pub fn mount(self, path: impl AsRef<Path>) -> std::io::Result<Device> {
        let state = Arc::new(Mutex::new(Default::default()));
        let fs = DeviceFs {
            root_attr: self.root_attr,
            state: state.clone(),
            dev_attr: self.dev_attr.clone(),
        };

        let path = path.as_ref().to_owned();
        let session = fuser::spawn_mount2(fs, &path, &self.mount_options)?;
        let notifier = session.notifier();

        Ok(Device {
            _session: session,
            notifier,
            state,
            name: self.dev_attr.name.to_str().unwrap().to_owned(),
            path,
        })
    }
}

impl Device {
    /// Mounts a new device at `path` with all default options. The path must
    /// already exist.
    pub fn new(path: impl AsRef<Path>) -> std::io::Result<Self> {
        Self::builder().mount(path)
    }

    /// Creates a new device using the builder pattern.
    pub fn builder() -> DeviceBuilder {
        DeviceBuilder::default()
    }

    /// Publishes a packet with a single event, followed by an `EV_SYN`. The
    /// timestamp of the event will be ignored, and replaced by the current time
    /// when the event is written to clients.
    pub fn publish_event(&self, ev: InputEvent) -> std::io::Result<()> {
        self.publish_packet([ev])
    }

    /// Publishes a series of events, followed by an EV_SYN (which should not
    /// be included in events). The timestamp field of the events will be
    /// ignored, and replaced by the current time when the packet is written to
    /// clients.
    pub fn publish_packet(&self, events: impl AsRef<[InputEvent]>) -> std::io::Result<()> {
        let mut state = self.state.lock().unwrap();
        if state.defunct {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotConnected,
                DefunctError,
            ));
        }

        state.write_packet(events, &self.notifier)
    }
}

impl std::fmt::Debug for Device {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Device")
            .field("path", &self.path)
            .field("name", &self.name)
            .finish_non_exhaustive()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tmp_mount_point() -> tempfile::NamedTempFile {
        tempfile::NamedTempFile::new().expect("failed to create tmpfile")
    }

    #[test]
    fn input_event_abi_compatible() {
        let ts = rustix::time::clock_gettime(rustix::time::ClockId::Monotonic);
        let ev = InputEvent {
            time_tv_sec: ts.tv_sec,
            time_tv_usec: ts.tv_nsec / 1000,
            event_type: sys::EV_SYN,
            code: 123,
            value: 456,
        };

        let ev_transmuted: libc::input_event = unsafe { std::mem::transmute(ev) };

        assert_eq!(ev_transmuted.time.tv_sec, ev.time_tv_sec);
        assert_eq!(ev_transmuted.time.tv_usec, ev.time_tv_usec);
        assert_eq!(ev_transmuted.type_, ev.event_type);
        assert_eq!(ev_transmuted.code, ev.code);
        assert_eq!(ev_transmuted.value, ev.value);
    }

    #[test]
    fn device_basic_attrs() {
        let p = tmp_mount_point();
        let _dev = Device::builder()
            .name("test device")
            .id(BusType::Usb, 1, 2, 3)
            .property(InputProperty::PointingStick)
            .mount(&p)
            .expect("failed to create device");

        let dev = evdev::Device::open(p).expect("failed to open device");
        assert_eq!(dev.name(), Some("test device"));

        let id = dev.input_id();
        assert_eq!(id.bus_type(), evdev::BusType::BUS_USB);
        assert_eq!(id.vendor(), 1);
        assert_eq!(id.product(), 2);
        assert_eq!(id.version(), 3);

        let props: Vec<_> = dev.properties().iter().collect();
        assert_eq!(props, [evdev::PropType::POINTING_STICK]);
    }

    #[test]
    fn device_supported_keys() {
        let p = tmp_mount_point();
        let _dev = Device::builder()
            .supported_event_codes([
                Scancode::Key(KeyCode::BtnSouth),
                Scancode::Key(KeyCode::BtnNorth),
                Scancode::Key(KeyCode::BtnEast),
                Scancode::Key(KeyCode::BtnWest),
                Scancode::AbsoluteAxis(AbsAxis::X),
                Scancode::AbsoluteAxis(AbsAxis::Y),
            ])
            .mount(&p)
            .expect("failed to create device");

        let dev = evdev::Device::open(p).expect("failed to open device");
        let supported_events: Vec<_> = dev.supported_events().iter().collect();
        assert_eq!(
            supported_events,
            [
                evdev::EventType::SYNCHRONIZATION,
                evdev::EventType::KEY,
                evdev::EventType::ABSOLUTE
            ]
        );

        let mut supported_keys: Vec<_> = dev
            .supported_keys()
            .expect("no supported keys")
            .iter()
            .collect();
        supported_keys.sort();
        assert_eq!(
            supported_keys,
            [
                evdev::Key::BTN_SOUTH,
                evdev::Key::BTN_EAST,
                evdev::Key::BTN_NORTH,
                evdev::Key::BTN_WEST
            ]
        );

        let supported_axes: Vec<_> = dev
            .supported_absolute_axes()
            .expect("no supported axes")
            .iter()
            .collect();
        assert_eq!(
            supported_axes,
            [
                evdev::AbsoluteAxisType::ABS_X,
                evdev::AbsoluteAxisType::ABS_Y,
            ]
        );
    }
}
