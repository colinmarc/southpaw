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
    ffi::CString,
    os::fd::OwnedFd,
    path::{Path, PathBuf},
    sync::Arc,
};

mod cmd;
mod device;
mod enums;
mod fs;
mod ring;

use device::{DeviceAttr, DeviceState};
use fs::*;
use fuser as fuse;
use libc::input_absinfo;
use parking_lot::Mutex;

pub use enums::*;
pub use fuse::Mount;

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

/// input_absinfo from the kernel source. Guaranteed to be ABI-compatible.
#[repr(C)]
#[derive(Default, Debug, PartialEq, Eq, Clone, Copy)]
pub struct AbsInfo {
    /// The initial (usually center) value.
    pub value: i32,
    /// The minimum value reported.
    pub minimum: i32,
    /// The maximum value reported.
    pub maximum: i32,
    /// Used to filter noise from the stream.
    pub fuzz: i32,
    /// Values less than this will be treated as 0.
    pub flat: i32,
    /// The resolution of values.
    pub resolution: i32,
}

/// A FUSE filesystem representing a tree of devices. Can be safely cloned and
/// modified.
#[derive(Clone)]
pub struct DeviceTree {
    tree: Arc<fs::DeviceTree>,
}

impl std::fmt::Debug for DeviceTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DeviceTree").finish_non_exhaustive()
    }
}

impl Default for DeviceTree {
    fn default() -> Self {
        Self::new()
    }
}

impl DeviceTree {
    /// Creates a new empty filesystem with no devices. To mount it somewhere,
    /// call [DeviceTree::mount].
    pub fn new() -> Self {
        let tree = Arc::new(fs::DeviceTree::new());
        Self { tree }
    }

    /// Creates a new device with empty attributes. For more control over the
    /// device, use [Device::builder].
    pub fn new_device(&mut self, path: impl AsRef<Path>) -> std::io::Result<Device> {
        self.insert(path, Default::default())
    }

    fn insert(
        &mut self,
        path: impl AsRef<Path>,
        dev_attr: Box<DeviceAttr>,
    ) -> std::io::Result<Device> {
        let path = match normalize_path(path) {
            Ok(p) => p,
            Err(e) => return Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, e)),
        };

        let name = dev_attr
            .name
            .as_ref()
            .map(|s| s.to_str().unwrap().to_owned())
            .unwrap_or_default();

        let state = Arc::new(Mutex::new(Default::default()));
        let inode = self.tree.create_device_node(path, dev_attr, state.clone());

        Ok(Device {
            tree: self.tree.clone(),
            inode,
            state,
            name,
        })
    }

    /// Wraps an existing /dev/fuse FD, and spawns a thread to handle requests
    /// from the kernel. Does not mount the FD anywhere.
    pub fn wrap_fd(&self, fd: OwnedFd) {
        let fs = Fs::new(self.tree.clone());
        let mut session = fuser::Session::from_fd(fd, fs, fuser::SessionACL::Owner);

        std::thread::Builder::new()
            .name("southpaw".to_string())
            .spawn(move || session.run())
            .unwrap();
    }

    /// Mounts the device tree at the given path, spawning a thread to handle
    /// requests from the kernel. The path must exist.
    ///
    /// The returned object can be dropped to unmount the filesystem.
    pub fn mount(&self, path: impl AsRef<Path>) -> std::io::Result<Mount> {
        let fs = Fs::new(self.tree.clone());
        let mut session = fuser::Session::new(fs, fuser::SessionACL::Owner)?;

        let mount = Mount::new(
            &session,
            path,
            &[
                fuse::MountOption::FSName("southpaw".to_string()),
                fuse::MountOption::Dev,
                fuse::MountOption::AllowOther,
            ],
        )?;

        std::thread::Builder::new()
            .name("southpaw".to_string())
            .spawn(move || session.run())?;

        Ok(mount)
    }
}

/// A file that acts like the character devices you would usually see in
/// /dev/input/event*. Situated in a [DeviceTree].
///
/// Dropping the [Device] removes the device from the tree.
pub struct Device {
    tree: Arc<fs::DeviceTree>,
    inode: Inode,
    state: Arc<Mutex<DeviceState>>,
    name: String,
}

impl Drop for Device {
    fn drop(&mut self) {
        self.tree.remove_device_node(self.inode);
    }
}

/// A temporary object for configuring a [Device].
#[derive(Default)]
pub struct DeviceBuilder {
    dev_attr: Box<DeviceAttr>,
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

impl DeviceBuilder {
    /// Sets the name of the device. The name must not contain null bytes.
    pub fn name(mut self, name: impl AsRef<str>) -> Self {
        self.dev_attr.name = Some(CString::new(name.as_ref()).expect("invalid name"));
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

    /// Sets the physical address of the device. The name must not contain null
    /// bytes.
    pub fn physical_address(mut self, addr: impl AsRef<str>) -> Self {
        self.dev_attr.phys = Some(CString::new(addr.as_ref()).expect("invalid addr"));
        self
    }

    /// Sets the "unique ID" of the device. The id must not contain null bytes.
    pub fn unique_id(mut self, uniq: impl AsRef<str>) -> Self {
        self.dev_attr.uniq = Some(CString::new(uniq.as_ref()).expect("invalid addr"));
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

    /// Advertizes supported key or button codes.
    pub fn supported_key_codes(mut self, codes: impl IntoIterator<Item = KeyCode>) -> Self {
        for code in codes.into_iter() {
            self = self.supported_event_codes_raw(EventType::Key, [u16::from(code)])
        }

        self
    }

    /// Advertizes supported switch codes.
    pub fn supported_switch_codes(mut self, codes: impl IntoIterator<Item = SwitchCode>) -> Self {
        for code in codes.into_iter() {
            self = self.supported_event_codes_raw(EventType::Switch, [u16::from(code)])
        }

        self
    }

    /// Advertizes support for a particular absolute axis.
    pub fn supported_absolute_axis(mut self, axis: impl Into<AbsAxis>, info: AbsInfo) -> Self {
        let axis: u16 = axis.into().into();
        self = self.supported_event_codes_raw(EventType::AbsoluteAxis, [axis]);

        let info: input_absinfo = unsafe { std::mem::transmute(info) };
        self.dev_attr.absinfo.insert(axis, info);

        self
    }

    /// For the given event_type, advertizes support for the given button codes. No
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

    /// Adds the device to a [DeviceTree].
    pub fn add_to_tree(
        self,
        tree: &mut DeviceTree,
        path: impl AsRef<Path>,
    ) -> std::io::Result<Device> {
        tree.insert(path, self.dev_attr)
    }
}

impl Device {
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
        let mut state = self.state.lock();
        if state.defunct {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotConnected,
                DefunctError,
            ));
        }

        state.write_packet(events)
    }
}

impl std::fmt::Debug for Device {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Device")
            .field("name", &self.name)
            .finish_non_exhaustive()
    }
}

fn normalize_path(path: impl AsRef<Path>) -> Result<PathBuf, &'static str> {
    use std::path::Component;

    let mut ret = PathBuf::new();
    for component in path.as_ref().components() {
        match component {
            Component::Prefix(..) => return Err("Invalid path"),
            Component::RootDir => return Err("Path must be relative"),
            Component::CurDir => (),
            Component::ParentDir => {
                if !ret.pop() {
                    return Err("Path must be relative");
                }
            }
            Component::Normal(c) => {
                ret.push(c);
            }
        }
    }

    if ret.as_os_str().is_empty() {
        return Err("Path must be non-empty");
    }

    Ok(ret)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tmp_mount_point() -> tempfile::TempDir {
        tempfile::TempDir::new().expect("failed to create tmpfile")
    }

    #[test]
    fn device_paths() {
        assert_eq!(normalize_path(""), Err("Path must be non-empty"));
        assert_eq!(normalize_path("."), Err("Path must be non-empty"));
        assert_eq!(normalize_path(".."), Err("Path must be relative"));
        assert_eq!(
            normalize_path("/root/foo/bar"),
            Err("Path must be relative")
        );
        assert_eq!(
            normalize_path("foo/../../bar"),
            Err("Path must be relative")
        );

        assert_eq!(normalize_path("foo"), Ok(Path::new("foo").to_owned()));
        assert_eq!(normalize_path("./foo"), Ok(Path::new("foo").to_owned()));
        assert_eq!(
            normalize_path("foo/bar/../baz"),
            Ok(Path::new("foo/baz").to_owned())
        );
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
    fn absinfo_abi_compatible() {
        let ev = AbsInfo {
            value: 0,
            minimum: -128,
            maximum: 128,
            fuzz: 123,
            flat: 456,
            resolution: 1,
        };

        let ev_transmuted: input_absinfo = unsafe { std::mem::transmute(ev) };

        assert_eq!(ev_transmuted.value, ev.value);
        assert_eq!(ev_transmuted.minimum, ev.minimum);
        assert_eq!(ev_transmuted.maximum, ev.maximum);
        assert_eq!(ev_transmuted.fuzz, ev.fuzz);
        assert_eq!(ev_transmuted.flat, ev.flat);
        assert_eq!(ev_transmuted.resolution, ev.resolution);
    }

    fn readdir(p: impl AsRef<Path>) -> Vec<String> {
        assert!(
            p.as_ref().is_dir(),
            "{} is not a directory",
            p.as_ref().display()
        );

        std::fs::read_dir(p.as_ref())
            .expect("read_dir")
            .map(|entry| {
                entry
                    .unwrap()
                    .path()
                    .strip_prefix(p.as_ref())
                    .unwrap()
                    .display()
                    .to_string()
            })
            .collect::<Vec<_>>()
    }

    #[test]
    fn tree_operations() {
        let p = tmp_mount_point();
        let p = p.path();
        let mut tree = DeviceTree::new();

        let _mount = tree.mount(p);

        assert_eq!(<Vec<String>>::new(), readdir(p));

        let dev1 = Device::builder()
            .name("test device")
            .id(BusType::Usb, 1, 2, 3)
            .property(InputProperty::PointingStick)
            .add_to_tree(&mut tree, "foo/bar/baz/event1")
            .expect("failed to create device");

        let dev2 = Device::builder()
            .name("test device")
            .id(BusType::Usb, 1, 2, 3)
            .property(InputProperty::PointingStick)
            .add_to_tree(&mut tree, "foo/qux/event2")
            .expect("failed to create device");

        assert_eq!(vec!["foo".to_string()], readdir(p));
        assert_eq!(
            vec!["bar".to_string(), "qux".to_string()],
            readdir(p.join("foo"))
        );
        assert_eq!(vec!["baz".to_string()], readdir(p.join("foo/bar")));
        assert_eq!(vec!["event1".to_string()], readdir(p.join("foo/bar/baz")));
        assert_eq!(vec!["event2".to_string()], readdir(p.join("foo/qux")));

        let metadata = p.join("foo/bar/baz/event1").metadata().expect("metadata");
        assert!(metadata.is_file());

        drop(dev2);
        assert!(!p.join("foo/qux/event2").exists());
        assert!(!p.join("foo/qux").exists());
        assert_eq!(vec!["foo".to_string()], readdir(p));
        assert_eq!(vec!["bar".to_string()], readdir(p.join("foo")));

        drop(dev1);
        assert!(!p.join("foo/bar/baz/event1").exists());
        assert!(!p.join("foo/bar/baz").exists());
        assert!(!p.join("foo/bar").exists());
        assert!(!p.join("foo").exists());

        assert_eq!(<Vec<String>>::new(), readdir(p));
    }

    #[test]
    fn device_basic_attrs() {
        let p = tmp_mount_point();
        let mut tree = DeviceTree::new();
        let _mount = tree.mount(&p);

        let _dev = Device::builder()
            .name("test device")
            .id(BusType::Usb, 1, 2, 3)
            .property(InputProperty::PointingStick)
            .add_to_tree(&mut tree, "test")
            .expect("failed to create device");

        let dev = evdev::Device::open(p.path().join("test")).expect("failed to open device");
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
        let mut tree = DeviceTree::new();
        let _mount = tree.mount(&p);

        let _dev = Device::builder()
            .supported_key_codes([
                KeyCode::BtnSouth,
                KeyCode::BtnNorth,
                KeyCode::BtnEast,
                KeyCode::BtnWest,
            ])
            .supported_absolute_axis(AbsAxis::X, Default::default())
            .supported_absolute_axis(AbsAxis::Y, Default::default())
            .add_to_tree(&mut tree, "test")
            .expect("failed to create device");

        let dev = evdev::Device::open(p.path().join("test")).expect("failed to open device");
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

    #[test]
    fn device_absinfo() {
        let p = tmp_mount_point();
        let mut tree = DeviceTree::new();
        let _mount = tree.mount(&p);

        let weird_absinfo = AbsInfo {
            value: 123,
            minimum: 0,
            maximum: 200,
            ..Default::default()
        };

        let _dev = Device::builder()
            .supported_absolute_axis(AbsAxis::X, Default::default())
            .supported_absolute_axis(AbsAxis::Y, weird_absinfo)
            .add_to_tree(&mut tree, "test")
            .expect("failed to create device");

        let dev = evdev::Device::open(p.path().join("test")).expect("failed to open device");

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

        let absinfo = dev.get_abs_state().expect("failed to get abs state");
        let absinfo_x: AbsInfo = unsafe { std::mem::transmute_copy(&absinfo[0]) };
        let absinfo_y: AbsInfo = unsafe { std::mem::transmute_copy(&absinfo[1]) };
        assert_eq!(AbsInfo::default(), absinfo_x);
        assert_eq!(weird_absinfo, absinfo_y);
    }
}
