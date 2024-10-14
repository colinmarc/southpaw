use std::ffi::CString;

use bit_vec::BitVec;
use fuser as fuse;
use libc::{input_absinfo, input_id, EFAULT, EINVAL, EPOLLIN, EWOULDBLOCK, O_NONBLOCK};
use rustix::{io::Errno, time::ClockId};

use crate::ring::Ring;
use crate::sys::*;
use crate::InputEvent;

const MAX_RING_SIZE: usize = 1024 * 1024;

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
    pub(crate) absinfo: litemap::LiteMap<u16, input_absinfo>,
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
            absinfo: Default::default(),
        };

        // EV_SYN is always supported.
        set_bit(&mut attr.support.ev_bits, EV_SYN);

        attr
    }
}

#[derive(Default)]
pub(crate) struct DeviceState {
    pub(crate) defunct: bool,
    pub(crate) clients: Vec<DeviceClient>,
    pub(crate) ring: Ring<InputEvent>,
    pub(crate) grabbed_fh: Option<u64>,
}

pub(crate) struct DeviceClient {
    pub(crate) fh: u64,
    ring_offset: usize,
    clock_id: ClockId,

    /// A currently-blocking read.
    // TODO: I'm unsure of the semantics of multiple reads on the same fd.
    pending_read: Option<PendingRead>,

    /// All currently-blocking polls. The value is the fuse-provided poll handle.
    pending_polls: Vec<fuse::PollHandle>,
}

struct PendingRead {
    size: usize,
    reply: fuse::ReplyData,
}

impl DeviceClient {
    pub(crate) fn do_read(
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

    pub(crate) fn do_poll(
        &mut self,
        ring: &Ring<InputEvent>,
        ph: fuse::PollHandle,
        reply: fuse::ReplyPoll,
    ) {
        if self.ring_offset < ring.current_offset() {
            reply.poll(EPOLLIN.try_into().unwrap());
        } else {
            self.pending_polls.push(ph);
            reply.poll(0);
        }
    }
}

impl DeviceState {
    pub(crate) fn insert_client(&mut self, fh: u64) {
        self.clients.push(DeviceClient {
            fh,
            ring_offset: self.ring.current_offset(),
            clock_id: ClockId::Realtime,

            pending_read: None,
            pending_polls: Vec::new(),
        });
    }

    pub(crate) fn grab(&mut self, fh: u64) -> Result<(), Errno> {
        if self.grabbed_fh.is_some() {
            return Err(Errno::BUSY);
        }

        self.grabbed_fh = Some(fh);
        Ok(())
    }

    pub(crate) fn ungrab(&mut self, fh: u64) -> Result<(), Errno> {
        if self.grabbed_fh != Some(fh) {
            return Err(Errno::INVAL);
        }

        self.grabbed_fh = None;
        Ok(())
    }

    pub(crate) fn write_packet(&mut self, events: impl AsRef<[InputEvent]>) -> std::io::Result<()> {
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
                ph.notify()?;
            }

            if let Some(pending_read) = client.pending_read.take() {
                client.do_read(&self.ring, 0, pending_read.size, pending_read.reply);
            }
        }

        Ok(())
    }
}
