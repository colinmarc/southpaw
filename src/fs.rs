use std::ffi::{CStr, CString};
use std::ops::DerefMut as _;
use std::sync::{Arc, Mutex};
use std::time;

use bit_vec::BitVec;
use fuser as fuse;
use libc::{input_id, EFAULT, EINVAL, ENOENT, ENOTSUP, EPOLLIN, EWOULDBLOCK, O_NONBLOCK};
use log::debug;
use rustix::{io::Errno, time::ClockId};

use crate::cmd::{Cmd, EV_VERSION};
use crate::ring::Ring;
use crate::sys::*;
use crate::InputEvent;

const MAX_RING_SIZE: usize = 1024 * 1024;

pub(crate) struct DeviceFs {
    pub(crate) root_attr: fuse::FileAttr,
    pub(crate) dev_attr: DeviceAttr,
    pub(crate) state: Arc<Mutex<DeviceState>>,
}

#[derive(Default, Debug, Clone)]
pub(crate) struct DeviceSupport {
    pub(crate) ev_bits: BitVec,
    pub(crate) key_bits: BitVec,
    pub(crate) rel_bits: BitVec,
    pub(crate) abs_bits: BitVec,
    pub(crate) msc_bits: BitVec,
    pub(crate) led_bits: BitVec,
    pub(crate) snd_bits: BitVec,
    pub(crate) ff_bits: BitVec,
    pub(crate) sw_bits: BitVec,
}

#[derive(Clone)]
pub(crate) struct DeviceAttr {
    pub(crate) name: Option<CString>,
    pub(crate) phys: Option<CString>,
    pub(crate) uniq: Option<CString>,
    pub(crate) id: input_id,
    pub(crate) prop_bits: BitVec,
    pub(crate) support: DeviceSupport,
}

impl Default for DeviceAttr {
    fn default() -> Self {
        let mut attr = DeviceAttr {
            name: None,
            phys: None,
            uniq: None,
            id: input_id {
                bustype: 0,
                vendor: 0,
                product: 0,
                version: 0,
            },
            prop_bits: Default::default(),
            support: Default::default(),
        };

        // EV_SYN is always supported.
        set_bit(&mut attr.support.ev_bits, EV_SYN);

        attr
    }
}

#[derive(Default)]
pub(crate) struct DeviceState {
    pub(crate) defunct: bool,
    clients: Vec<DeviceClient>,
    ring: Ring<InputEvent>,
    max_fh: u64,
    grabbed_fh: Option<u64>,
}

struct DeviceClient {
    fh: u64,
    ring_offset: usize,
    clock_id: ClockId,

    /// A currently-blocking read.
    // TODO: I'm unsure of the semantics of multiple reads on the same fd.
    pending_read: Option<PendingRead>,

    /// All currently-blocking polls. The value is the fuse-provided poll handle.
    pending_polls: Vec<u64>,
}

struct PendingRead {
    size: usize,
    reply: fuse::ReplyData,
}

impl DeviceClient {
    fn do_read(
        &mut self,
        ring: &Ring<InputEvent>,
        flags: i32,
        size: usize,
        reply: fuse::ReplyData,
    ) {
        // There's already a blocking read in progress?
        if self.pending_read.is_some() {
            reply.error(EFAULT);
            return;
        }

        let ev_iter = ring.iter_from(self.ring_offset);
        if ev_iter.len() == 0 {
            if (flags & O_NONBLOCK) > 0 {
                reply.error(EWOULDBLOCK);
            } else {
                self.pending_read = Some(PendingRead { size, reply });
            }

            return;
        }

        let ts = rustix::time::clock_gettime(self.clock_id);

        let ev_size = size_of::<InputEvent>();
        let n = (size / ev_size).min(ev_iter.len());

        // let mut out = vec![0_u8; ev_size * n];
        // let mut off = 0;

        let evs: Vec<_> = ev_iter
            .take(n)
            .map({
                |ev| InputEvent {
                    time_tv_sec: ts.tv_sec,
                    time_tv_usec: ts.tv_nsec / 1000,
                    ..*ev
                }
            })
            .collect();

        let out: &[u8] = unsafe { std::slice::from_raw_parts(evs.as_ptr() as _, n * ev_size) };

        self.ring_offset += n;
        reply.data(out);
    }

    fn do_poll(&mut self, ring: &Ring<InputEvent>, ph: u64, reply: fuse::ReplyPoll) {
        if self.ring_offset < ring.current_offset() {
            reply.poll(EPOLLIN.try_into().unwrap());
        } else {
            self.pending_polls.push(ph);
            reply.poll(0);
        }
    }
}

impl DeviceState {
    fn allocate_client(&mut self) -> u64 {
        self.max_fh = self.max_fh.checked_add(1).expect("file handle overflow");

        let fh = self.max_fh;
        self.clients.push(DeviceClient {
            fh,
            ring_offset: self.ring.current_offset(),
            clock_id: ClockId::Realtime,

            pending_read: None,
            pending_polls: Vec::new(),
        });

        fh
    }

    fn grab(&mut self, fh: u64) -> Result<(), Errno> {
        if self.grabbed_fh.is_some() {
            return Err(Errno::BUSY);
        }

        self.grabbed_fh = Some(fh);
        Ok(())
    }

    fn ungrab(&mut self, fh: u64) -> Result<(), Errno> {
        if self.grabbed_fh != Some(fh) {
            return Err(Errno::INVAL);
        }

        self.grabbed_fh = None;
        Ok(())
    }

    pub(crate) fn write_packet(
        &mut self,
        events: impl AsRef<[InputEvent]>,
        notifier: &fuse::Notifier,
    ) -> std::io::Result<()> {
        let evs = events.as_ref();
        if evs.is_empty() {
            return Ok(());
        } else if evs.iter().any(|ev| ev.event_type == EV_SYN) {
            return Err(std::io::Error::from_raw_os_error(EINVAL));
        }

        // Free up space in the ring.
        if let Some(min_offset) = self.clients.iter().map(|c| c.ring_offset).min() {
            let write_offset = self.ring.current_offset() + evs.len();

            // Skip forward for clients that are too far behind.
            if write_offset - min_offset > MAX_RING_SIZE {
                let off = write_offset - MAX_RING_SIZE;

                self.ring.discard_until(off);
                for client in self.clients.iter_mut() {
                    client.ring_offset = client.ring_offset.max(off);
                }
            } else {
                self.ring.discard_until(min_offset);
            }
        }

        // Write events into the ring.
        self.ring.extend(evs.iter().copied());
        self.ring.push_back(InputEvent {
            time_tv_sec: 0,
            time_tv_usec: 0,
            event_type: EV_SYN,
            code: SYN_REPORT,
            value: 0,
        });

        // Trigger blocking polls and reads.
        for client in self.clients.iter_mut() {
            if self.grabbed_fh.is_some_and(|fh| fh != client.fh) {
                continue;
            }

            for ph in client.pending_polls.drain(..) {
                notifier.poll(ph)?;
            }

            if let Some(pending_read) = client.pending_read.take() {
                client.do_read(&self.ring, 0, pending_read.size, pending_read.reply);
            }
        }

        Ok(())
    }
}

impl fuse::Filesystem for DeviceFs {
    fn open(&mut self, _req: &fuse::Request<'_>, ino: u64, _flags: i32, reply: fuse::ReplyOpen) {
        if ino != fuse::FUSE_ROOT_ID {
            reply.error(ENOENT);
            return;
        }

        let fh = self.state.lock().unwrap().allocate_client();
        reply.opened(
            fh,
            fuse::consts::FOPEN_DIRECT_IO | fuse::consts::FOPEN_NONSEEKABLE,
        );
    }

    fn release(
        &mut self,
        _req: &fuse::Request<'_>,
        ino: u64,
        fh: u64,
        _flags: i32,
        _lock_owner: Option<u64>,
        _flush: bool,
        reply: fuse::ReplyEmpty,
    ) {
        if ino != fuse::FUSE_ROOT_ID {
            reply.error(ENOENT);
            return;
        }

        let mut state = self.state.lock().unwrap();
        if state.grabbed_fh == Some(fh) {
            state.grabbed_fh = None;
        }

        state.clients.retain(|c| c.fh != fh);
        reply.ok();
    }

    fn lookup(
        &mut self,
        _req: &fuse::Request<'_>,
        _parent: u64,
        name: &std::ffi::OsStr,
        reply: fuse::ReplyEntry,
    ) {
        if name == "/" {
            reply.entry(&time::Duration::MAX, &self.root_attr, 0);
            return;
        }

        reply.error(ENOENT);
    }

    fn getattr(&mut self, _req: &fuse::Request<'_>, ino: u64, reply: fuse::ReplyAttr) {
        if ino != fuse::FUSE_ROOT_ID {
            reply.error(ENOENT);
            return;
        }

        reply.attr(&time::Duration::MAX, &self.root_attr);
    }

    fn ioctl(
        &mut self,
        _req: &fuse::Request<'_>,
        ino: u64,
        fh: u64,
        flags: u32,
        op: u32,
        in_data: &[u8],
        out_size: u32,
        reply: fuse::ReplyIoctl,
    ) {
        if ino != fuse::FUSE_ROOT_ID {
            reply.error(ENOENT);
            return;
        }

        let Ok(out_size) = usize::try_from(out_size) else {
            reply.error(EINVAL);
            return;
        };

        let cmd = match Cmd::from_raw(op, in_data, out_size) {
            Ok(cmd) => cmd,
            Err(errno) => {
                debug!("unrecognized ioctl: {}", op);
                reply.error(errno.raw_os_error());
                return;
            }
        };

        debug!(
            "cmd: {cmd:?}, fh: {fh}, flags: {flags}, in_data.len(): {}, out_size: {out_size}",
            in_data.len(),
        );

        match cmd {
            Cmd::GetVersion => reply.ioctl(0, &EV_VERSION.to_ne_bytes()),
            Cmd::GetId => {
                let mut out = vec![0_u8; size_of::<input_id>()];
                unsafe {
                    std::ptr::write(out.as_mut_ptr() as *mut _, self.dev_attr.id);
                }

                reply.ioctl(0, &out);
            }
            Cmd::GetName(len) => reply_str(reply, len, self.dev_attr.name.as_deref()),
            Cmd::GetPhys(len) => reply_str(reply, len, self.dev_attr.phys.as_deref()),
            Cmd::GetUniq(len) => reply_str(reply, len, self.dev_attr.uniq.as_deref()),
            Cmd::GetProps(len) => reply_bits(reply, len, &self.dev_attr.prop_bits),
            Cmd::GetEventBits(ty, len) => match ty {
                0 => reply_bits(reply, len, &self.dev_attr.support.ev_bits),
                EV_KEY => reply_bits(reply, len, &self.dev_attr.support.key_bits),
                EV_REL => reply_bits(reply, len, &self.dev_attr.support.rel_bits),
                EV_ABS => reply_bits(reply, len, &self.dev_attr.support.abs_bits),
                EV_MSC => reply_bits(reply, len, &self.dev_attr.support.msc_bits),
                EV_LED => reply_bits(reply, len, &self.dev_attr.support.led_bits),
                EV_SND => reply_bits(reply, len, &self.dev_attr.support.snd_bits),
                EV_FF => reply_bits(reply, len, &self.dev_attr.support.ff_bits),
                EV_SW => reply_bits(reply, len, &self.dev_attr.support.sw_bits),
                _ => reply.error(EINVAL),
            },
            Cmd::Grab(v) if v > 0 => reply_empty(reply, self.state.lock().unwrap().grab(fh)),
            Cmd::Grab(v) if v <= 0 => reply_empty(reply, self.state.lock().unwrap().ungrab(fh)),
            _ => reply.error(ENOTSUP),
        }
    }

    fn read(
        &mut self,
        _req: &fuse::Request<'_>,
        ino: u64,
        fh: u64,
        _offset: i64,
        size: u32,
        flags: i32,
        _lock_owner: Option<u64>,
        reply: fuse::ReplyData,
    ) {
        if ino != fuse::FUSE_ROOT_ID {
            reply.error(ENOENT);
            return;
        }

        debug!("read fh: {fh}, size: {size}, flags: {flags:?}");
        let mut guard = self.state.lock().unwrap();
        let state = guard.deref_mut();

        let Some(client) = state.clients.iter_mut().find(|c| c.fh == fh) else {
            reply.error(EFAULT);
            return;
        };

        client.do_read(&state.ring, flags, size as usize, reply)
    }

    fn poll(
        &mut self,
        _req: &fuse::Request<'_>,
        ino: u64,
        fh: u64,
        ph: u64,
        _events: u32,
        _flags: u32,
        reply: fuse::ReplyPoll,
    ) {
        debug!("poll ino: {ino}, fh: {fh}, kh: {ph}, flags: {_flags}");
        if ino != fuse::FUSE_ROOT_ID {
            reply.error(ENOENT);
            return;
        }

        let mut guard = self.state.lock().unwrap();
        let state = guard.deref_mut();

        let Some(client) = state.clients.iter_mut().find(|c| c.fh == fh) else {
            reply.error(EFAULT);
            return;
        };

        client.do_poll(&state.ring, ph, reply)
    }

    fn destroy(&mut self) {
        self.state.lock().unwrap().defunct = true
    }
}

fn reply_str(reply: fuse::ReplyIoctl, len: usize, str: Option<&CStr>) {
    let out: &[u8] = str.map_or(&[], |s| s.to_bytes_with_nul());
    let len = len.min(out.len());
    reply.ioctl(len as i32, &out[..len])
}

fn reply_bits<B: bit_vec::BitBlock>(reply: fuse::ReplyIoctl, len: usize, bits: &BitVec<B>) {
    let storage = bits.storage();
    let out: &[u8] =
        unsafe { std::slice::from_raw_parts(storage.as_ptr() as _, size_of_val(storage)) };

    let len = len.min(out.len());
    reply.ioctl(len as i32, &out[..len])
}

fn reply_empty(reply: fuse::ReplyIoctl, res: Result<(), Errno>) {
    match res {
        Ok(_) => reply.ioctl(0, &[]),
        Err(errno) => reply.error(errno.raw_os_error()),
    }
}
