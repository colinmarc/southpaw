//! Definitions from input.h in the kernel source.

// #![allow(dead_code)] // TODO

use std::ffi::c_int;

use libc::c_uint;
pub(crate) use libc::{ff_effect, input_absinfo, input_id, input_keymap_entry, input_mask};
use rustix::{
    io::Errno,
    ioctl::{Direction, Opcode},
    time::ClockId,
};
use uapi::{_IOC_SIZE, _IOC_SIZEMASK, _IOC_SIZESHIFT};

pub(crate) const EV_VERSION: c_uint = 0x010001;

#[allow(dead_code)]
pub(crate) enum Cmd {
    GetVersion,
    GetId,
    GetRepeat,
    SetRepeat(c_uint, c_uint),

    GetKeyCode,
    GetKeyCodeV2,
    SetKeyCode(input_keymap_entry),

    GetName(usize),
    GetPhys(usize),
    GetUniq(usize),
    GetProps(usize),

    GetMtSlots { num_slots: c_uint },

    GetKeyState(usize),
    GetLedState(usize),
    GetSoundState(usize),
    GetSwitchState(usize),

    GetEventBits(u16, usize),
    GetAbsInfo(c_uint),
    SetAbsInfo(c_uint, input_absinfo),

    SendForceFeedback(ff_effect),
    RemoveForceFeedback(c_int),
    GetMaxEffects,

    Grab(c_int),
    Revoke(c_int),

    GetMask,
    SetMask(input_mask),

    SetClockId(ClockId),
}

impl std::fmt::Debug for Cmd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Cmd::GetVersion => write!(f, "EVIOCGVERSION"),
            Cmd::GetId => write!(f, "EVIOCGID"),
            Cmd::GetRepeat => write!(f, "EVIOCGREP"),
            Cmd::SetRepeat(x, y) => write!(f, "EVIOCSREP({x}, {y})"),
            Cmd::GetKeyCode => write!(f, "EVIOCGKEYCODE"),
            Cmd::GetKeyCodeV2 => write!(f, "EVIOCGKEYCODE_V2"),
            Cmd::SetKeyCode(_) => write!(f, "EVIOCSKEYCODE(...)"),
            Cmd::GetName(len) => write!(f, "EVIOCGNAME()[..{len}]"),
            Cmd::GetPhys(len) => write!(f, "EVIOCGPHYS()[..{len}]"),
            Cmd::GetUniq(len) => write!(f, "EVIOCGUNIQ()[..{len}]"),
            Cmd::GetProps(len) => write!(f, "EVIOCGPROP()[..{len}]"),
            Cmd::GetMtSlots { num_slots } => write!(f, "EVIOCGMTSLOTS(num_slots={num_slots})"),
            Cmd::GetKeyState(len) => write!(f, "EVIOCGKEY()[..{len}]"),
            Cmd::GetLedState(len) => write!(f, "EVIOCGLED()[..{len}]"),
            Cmd::GetSoundState(len) => write!(f, "EVIOCGSND()[..{len}]"),
            Cmd::GetSwitchState(len) => write!(f, "EVIOCGSW()[..{len}]"),
            Cmd::GetEventBits(idx, len) => write!(f, "EVIOCGPHYS({idx})[..{len}]"),
            Cmd::GetAbsInfo(idx) => write!(f, "EVIOCGABS({idx})"),
            Cmd::SetAbsInfo(idx, _) => write!(f, "EVIOCSABS({idx}, ...)"),
            Cmd::SendForceFeedback(_) => write!(f, "EVIOCSFF(...)"),
            Cmd::RemoveForceFeedback(idx) => write!(f, "EVIOCRMFF({idx})"),
            Cmd::GetMaxEffects => write!(f, "EVIOCGEFFECTS"),
            Cmd::Grab(fd) => write!(f, "EVIOCGRAB({fd})"),
            Cmd::Revoke(fd) => write!(f, "EVIOCREVOKE({fd})"),
            Cmd::GetMask => write!(f, "EVIOCGMASK"),
            Cmd::SetMask(_) => write!(f, "EVIOCSMASK(...)"),
            Cmd::SetClockId(clock_id) => write!(f, "EVIOCSCLOCKID({clock_id:?})"),
        }
    }
}

impl Cmd {
    pub(crate) fn from_raw(op: u32, in_data: &[u8], out_len: usize) -> Result<Self, Errno> {
        let opcode = Opcode::old(op);

        use Cmd::*;
        let cmd = match opcode {
            EVIOCGVERSION => GetVersion,
            EVIOCGID => GetId,
            EVIOCGREP => GetRepeat,
            EVIOCSREP => {
                let [x, y] = read_args(in_data)?;
                SetRepeat(x, y)
            }
            EVIOCGKEYCODE => GetKeyCode,
            EVIOCGKEYCODE_V2 => GetKeyCodeV2,
            EVIOCSKEYCODE => {
                let [x, y]: [c_uint; 2] = read_args(in_data)?;
                let mut scancode = [0_u8; 32];
                scancode.copy_from_slice(&x.to_ne_bytes());

                SetKeyCode(input_keymap_entry {
                    index: 0,
                    flags: 0,
                    len: std::mem::size_of::<c_uint>() as u8,
                    keycode: y,
                    scancode,
                })
            }
            EVIOCSKEYCODE_V2 => todo!(),
            EVIOCGMTSLOTS => GetMtSlots {
                num_slots: (out_len / std::mem::size_of::<i32>()) as c_uint - 1,
            },
            x if opcode_in_range(&EVIOCGBIT, x) => {
                GetEventBits(opcode_offset(&EVIOCGBIT, opcode) as u16, out_len)
            }
            x if opcode_in_range(&EVIOCGABS, x) => {
                GetAbsInfo(opcode_offset(&EVIOCGABS, opcode) as c_uint)
            }
            x if opcode_in_range(&EVIOCSABS, x) => SetAbsInfo(
                opcode_offset(&EVIOCGABS, opcode) as c_uint,
                read_args(in_data)?,
            ),
            EVIOCSFF => SendForceFeedback(read_args(in_data)?),
            EVIOCRMFF => RemoveForceFeedback(read_args(in_data)?),
            EVIOCGEFFECTS => GetMaxEffects,
            EVIOCGRAB => Grab(read_args(in_data)?),
            EVIOCREVOKE => Revoke(read_args(in_data)?),
            EVIOCGMASK => GetMask,
            EVIOCSMASK => SetMask(read_args(in_data)?),
            EVIOCSCLOCKID => {
                let clock_id = match read_args::<c_int>(in_data)? {
                    x if x == ClockId::Realtime as i32 => ClockId::Realtime,
                    x if x == ClockId::Monotonic as i32 => ClockId::Monotonic,
                    x if x == ClockId::ProcessCPUTime as i32 => ClockId::ProcessCPUTime,
                    x if x == ClockId::ThreadCPUTime as i32 => ClockId::ThreadCPUTime,
                    x if x == ClockId::RealtimeCoarse as i32 => ClockId::RealtimeCoarse,
                    x if x == ClockId::MonotonicCoarse as i32 => ClockId::MonotonicCoarse,
                    x if x == ClockId::MonotonicRaw as i32 => ClockId::MonotonicRaw,
                    x if x == ClockId::RealtimeAlarm as i32 => ClockId::RealtimeAlarm,
                    x if x == ClockId::Boottime as i32 => ClockId::Boottime,
                    x if x == ClockId::BoottimeAlarm as i32 => ClockId::BoottimeAlarm,
                    _ => return Err(Errno::INVAL),
                };

                SetClockId(clock_id)
            }

            // Variable-length ioctls
            _ => return Self::from_raw_variable_length(op, in_data, out_len),
        };

        Ok(cmd)
    }

    fn from_raw_variable_length(op: u32, _in_data: &[u8], out_len: usize) -> Result<Self, Errno> {
        let len = _IOC_SIZE(op as u64) as usize;
        assert_eq!(len, out_len);

        let op = op as u64 & !(_IOC_SIZEMASK << _IOC_SIZESHIFT);
        let opcode = Opcode::old(op as u32);

        use Cmd::*;
        let cmd = match opcode {
            EVIOCGNAME => GetName(len),
            EVIOCGPHYS => GetPhys(len),
            EVIOCGUNIQ => GetUniq(len),
            EVIOCGPROP => GetProps(len),
            EVIOCGKEY => GetKeyState(len),
            EVIOCGLED => GetLedState(len),
            EVIOCGSND => GetSoundState(len),
            EVIOCGSW => GetSwitchState(len),
            x if opcode_in_range(&EVIOCGBIT, x) => {
                GetEventBits(opcode_offset(&EVIOCGBIT, opcode) as u16, len)
            }
            _ => return Err(Errno::NOSYS),
        };

        Ok(cmd)
    }
}

const EVIOC_BASE: u8 = b'E';

pub(crate) const EVIOCGVERSION: Opcode = Opcode::read::<c_int>(EVIOC_BASE, 0x01);
pub(crate) const EVIOCGID: Opcode = Opcode::read::<input_id>(EVIOC_BASE, 0x02);
pub(crate) const EVIOCGREP: Opcode = Opcode::read::<[c_uint; 2]>(EVIOC_BASE, 0x03);
pub(crate) const EVIOCSREP: Opcode = Opcode::write::<[c_uint; 2]>(EVIOC_BASE, 0x03);

pub(crate) const EVIOCGKEYCODE: Opcode = Opcode::read::<[c_uint; 2]>(EVIOC_BASE, 0x04);
pub(crate) const EVIOCGKEYCODE_V2: Opcode = Opcode::read::<input_keymap_entry>(EVIOC_BASE, 0x04);
pub(crate) const EVIOCSKEYCODE: Opcode = Opcode::write::<[c_uint; 2]>(EVIOC_BASE, 0x04);
pub(crate) const EVIOCSKEYCODE_V2: Opcode = Opcode::write::<input_keymap_entry>(EVIOC_BASE, 0x04);

pub(crate) const EVIOCGNAME: Opcode = Opcode::from_components(Direction::Read, EVIOC_BASE, 0x06, 0);
pub(crate) const EVIOCGPHYS: Opcode = Opcode::from_components(Direction::Read, EVIOC_BASE, 0x07, 0);
pub(crate) const EVIOCGUNIQ: Opcode = Opcode::from_components(Direction::Read, EVIOC_BASE, 0x08, 0);
pub(crate) const EVIOCGPROP: Opcode = Opcode::from_components(Direction::Read, EVIOC_BASE, 0x09, 0);

pub(crate) const EVIOCGMTSLOTS: Opcode =
    Opcode::from_components(Direction::Read, EVIOC_BASE, 0x0a, 0);

pub(crate) const EVIOCGKEY: Opcode = Opcode::from_components(Direction::Read, EVIOC_BASE, 0x18, 0);
pub(crate) const EVIOCGLED: Opcode = Opcode::from_components(Direction::Read, EVIOC_BASE, 0x19, 0);
pub(crate) const EVIOCGSND: Opcode = Opcode::from_components(Direction::Read, EVIOC_BASE, 0x1a, 0);
pub(crate) const EVIOCGSW: Opcode = Opcode::from_components(Direction::Read, EVIOC_BASE, 0x1b, 0);

seq_macro::seq!(N in 0x20..0x40 {
    pub(crate) const EVIOCGBIT: [Opcode; 32] = [
        #(
            Opcode::from_components(Direction::Read, EVIOC_BASE, N, 0),
        )*
    ];
});

seq_macro::seq!(N in 0x40..0x80 {
    pub(crate) const EVIOCGABS: [Opcode; 64] = [
        #(
            Opcode::read::<input_absinfo>(EVIOC_BASE, N),
        )*
    ];
});

seq_macro::seq!(N in 0xc0..=0xff {
    pub(crate) const EVIOCSABS: [Opcode; 64] = [
        #(
            Opcode::write::<input_absinfo>(EVIOC_BASE, N),
        )*
    ];
});

pub(crate) const EVIOCSFF: Opcode = Opcode::write::<ff_effect>(EVIOC_BASE, 0x80);
pub(crate) const EVIOCRMFF: Opcode = Opcode::write::<c_int>(EVIOC_BASE, 0x81);
pub(crate) const EVIOCGEFFECTS: Opcode = Opcode::read::<c_int>(EVIOC_BASE, 0x84);

pub(crate) const EVIOCGRAB: Opcode = Opcode::write::<c_int>(EVIOC_BASE, 0x90);
pub(crate) const EVIOCREVOKE: Opcode = Opcode::write::<c_int>(EVIOC_BASE, 0x91);

pub(crate) const EVIOCGMASK: Opcode = Opcode::read::<input_mask>(EVIOC_BASE, 0x92);
pub(crate) const EVIOCSMASK: Opcode = Opcode::write::<input_mask>(EVIOC_BASE, 0x93);

pub(crate) const EVIOCSCLOCKID: Opcode = Opcode::write::<c_int>(EVIOC_BASE, 0xa0);

fn opcode_in_range(range: &[Opcode], op: Opcode) -> bool {
    let range = range[0].raw()..=range[range.len() - 1].raw();
    range.contains(&op.raw())
}

fn opcode_offset(range: &[Opcode], op: Opcode) -> usize {
    debug_assert!(opcode_in_range(range, op));
    (op.raw() - range[0].raw()) as usize
}

fn read_args<T: Sized>(buf: &[u8]) -> Result<T, Errno> {
    if buf.len() != std::mem::size_of::<T>() {
        return Err(Errno::INVAL);
    }

    // SAFETY: We only call this function with primitve types and repr(C)
    // structs, and we just did a length check.
    let v = unsafe { std::ptr::read(buf.as_ptr() as *const T) };
    Ok(v)
}
