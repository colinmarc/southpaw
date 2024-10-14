use std::{
    ffi::{CStr, OsStr, OsString},
    path::Path,
    sync::Arc,
    time,
};

use bit_vec::BitVec;
use fuser as fuse;
use libc::{EBADF, EFAULT, EINVAL, ENOENT, ENOTSUP};
use log::debug;
use parking_lot::{MappedMutexGuard, Mutex, MutexGuard};
use rustix::{
    io::Errno,
    process::{RawGid, RawUid},
};
use thunderdome::{Arena, Index};

use crate::{
    cmd::{Cmd, EV_VERSION},
    device::{DeviceAttr, DeviceState},
    sys::{EV_ABS, EV_FF, EV_KEY, EV_LED, EV_MSC, EV_REL, EV_SND, EV_SW},
};

pub(crate) use thunderdome::Index as Inode;

struct Node {
    name: OsString,
    parent: Option<Index>,
    attr: fuse::FileAttr,
    contents: NodeType,
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut st = f.debug_struct("Node");

        st.field("name", &self.name)
            .field("parent", &self.parent)
            .field("filetype", &self.attr.kind);

        if let NodeType::Dir { children } = &self.contents {
            st.field("children", children);
        }

        st.finish()
    }
}

enum NodeType {
    Dir {
        children: Vec<Index>,
    },
    Device {
        dev_attr: Box<DeviceAttr>, // Boxed to avoid the enum being too big.
        state: Arc<Mutex<DeviceState>>,
    },
}

pub(crate) struct DeviceTree {
    uid: RawUid,
    gid: RawGid,
    dir_mode: u16,
    root_index: Index,
    nodes: Mutex<Arena<Node>>,
}

impl DeviceTree {
    pub(crate) fn new() -> Self {
        let start = time::SystemTime::now();
        let uid = rustix::process::getuid().as_raw();
        let gid = rustix::process::getgid().as_raw();
        let perm = 0o755;

        let root_attr = fuse::FileAttr {
            ino: fuse::FUSE_ROOT_ID,
            size: 0,
            blocks: 0,
            atime: start,
            mtime: start,
            ctime: start,
            crtime: start,
            kind: fuse::FileType::Directory,
            perm,
            nlink: 1,
            uid,
            gid,
            rdev: 1,
            blksize: 512,
            flags: 0,
        };

        let mut arena = Arena::new();
        let root_index = arena.insert(Node {
            name: "/".into(),
            parent: None,
            attr: root_attr,
            contents: NodeType::Dir {
                children: Vec::new(),
            },
        });

        Self {
            uid,
            gid,
            dir_mode: perm,
            root_index,
            nodes: Mutex::new(arena),
        }
    }

    fn get_node(&self, ino: u64) -> Option<MappedMutexGuard<'_, Node>> {
        let idx = if ino == fuse::FUSE_ROOT_ID {
            self.root_index
        } else {
            Index::from_bits(ino)?
        };

        match MutexGuard::try_map(self.nodes.lock(), |nodes| nodes.get_mut(idx)) {
            Ok(v) => Some(v),
            Err(_) => None,
        }
    }

    /// Creates a leaf node for a device, along with a directory for each of its
    /// parents.
    pub(crate) fn create_device_node(
        &self,
        path: impl AsRef<Path>,
        dev_attr: Box<DeviceAttr>,
        state: Arc<Mutex<DeviceState>>,
    ) -> Index {
        assert!(path.as_ref().is_relative());

        let mut nodes = self.nodes.lock();

        let t = time::SystemTime::now();
        let parent = mkdir_all(
            &mut nodes,
            self.root_index,
            path.as_ref().parent().unwrap(),
            fuse::FileAttr {
                ino: 0,
                size: 0,
                blocks: 0,
                atime: t,
                mtime: t,
                ctime: t,
                crtime: t,
                kind: fuse::FileType::Directory,
                perm: self.dir_mode,
                nlink: 1,
                uid: self.uid,
                gid: self.gid,
                rdev: 1,
                blksize: 512,
                flags: 0,
            },
        );

        insert_node(
            &mut nodes,
            parent,
            Node {
                name: path.as_ref().file_name().unwrap().to_owned(),
                parent: Some(parent),
                attr: fuse::FileAttr {
                    ino: 0,
                    size: 0,
                    blocks: 0,
                    atime: t,
                    mtime: t,
                    ctime: t,
                    crtime: t,
                    kind: fuse::FileType::RegularFile,
                    perm: 0o1755,
                    nlink: 1,
                    uid: self.uid,
                    gid: self.gid,
                    rdev: 1,
                    blksize: 512,
                    flags: 0,
                },
                contents: NodeType::Device { dev_attr, state },
            },
        )
    }

    /// Removes the leaf node for a device, along with any dangling parents.
    pub(crate) fn remove_device_node(&self, index: Index) {
        let mut nodes = self.nodes.lock();
        let Some(node) = nodes.remove(index) else {
            return;
        };

        let mut index = index;
        let mut parent = node.parent.unwrap(); // Leaf nodes always have a parent.
        loop {
            assert!(nodes.contains(parent));
            let NodeType::Dir { children } = &mut nodes[parent].contents else {
                unreachable!(); // The parent is always a dir.
            };

            // Remove ourselves from the parent node.
            children.retain(|i| i != &index);

            // Continue up the chain if the parent is dangling.
            if children.is_empty() && parent != self.root_index {
                index = parent;
                parent = nodes[index].parent.unwrap(); // We're not at the root.

                nodes.remove(index);
            } else {
                return;
            }
        }
    }
}

fn lookup_child(nodes: &Arena<Node>, parent: Index, name: &OsStr) -> Option<Index> {
    let NodeType::Dir { children } = &nodes.get(parent)?.contents else {
        return None;
    };

    for child in children {
        if nodes[*child].name == name {
            return Some(*child);
        }
    }

    None
}

fn mkdir_all(nodes: &mut Arena<Node>, root: Index, path: &Path, attr: fuse::FileAttr) -> Index {
    let parent = match path.parent() {
        None => return root, // This means that path == Path("").
        Some(p) => mkdir_all(nodes, root, p, attr),
    };

    let name = path.file_name().unwrap().to_owned();
    if let Some(child_idx) = lookup_child(nodes, parent, &name) {
        return child_idx;
    }

    // No matching parent, let's create one.
    insert_node(
        nodes,
        parent,
        Node {
            name: path.file_name().unwrap().to_owned(),
            parent: Some(parent),
            attr,
            contents: NodeType::Dir {
                children: Vec::new(),
            },
        },
    )
}

fn insert_node(nodes: &mut Arena<Node>, parent: Index, node: Node) -> Index {
    let index = nodes.insert(node);
    nodes[index].attr.ino = index.to_bits();

    // Insert into the parent's children.
    let NodeType::Dir { children } = &mut nodes[parent].contents else {
        unreachable!()
    };
    children.push(index);

    index
}

pub(crate) struct Fs {
    tree: Arc<DeviceTree>,
    max_fh: u64,
}

impl Fs {
    pub(crate) fn new(tree: Arc<DeviceTree>) -> Self {
        Self { tree, max_fh: 0 }
    }
}

impl fuse::Filesystem for Fs {
    fn destroy(&mut self) {
        let mut nodes = self.tree.nodes.lock();
        for (_, node) in nodes.iter_mut() {
            if let NodeType::Device { state, .. } = &mut node.contents {
                state.lock().defunct = true
            }
        }
    }

    fn lookup(
        &mut self,
        _req: &fuse::Request<'_>,
        parent: u64,
        name: &OsStr,
        reply: fuse::ReplyEntry,
    ) {
        let parent_idx = if parent == fuse::FUSE_ROOT_ID {
            Some(self.tree.root_index)
        } else {
            Index::from_bits(parent)
        };

        let Some(parent_idx) = parent_idx else {
            return reply.error(ENOENT);
        };

        let nodes = self.tree.nodes.lock();
        if let Some(child) = lookup_child(&nodes, parent_idx, name) {
            debug!("lookup ino: {}", nodes[child].attr.ino);
            return reply.entry(&time::Duration::ZERO, &nodes[child].attr, 0);
        }

        reply.error(ENOENT);
    }

    fn getattr(
        &mut self,
        _req: &fuse::Request<'_>,
        ino: u64,
        _fh: Option<u64>,
        reply: fuse::ReplyAttr,
    ) {
        let Some(node) = self.tree.get_node(ino) else {
            return reply.error(ENOENT);
        };

        reply.attr(&time::Duration::ZERO, &node.attr);
    }

    fn open(&mut self, req: &fuse::Request<'_>, ino: u64, _flags: i32, reply: fuse::ReplyOpen) {
        if ino == fuse::FUSE_ROOT_ID {
            return reply.error(ENOTSUP);
        }

        let Some(node) = self.tree.get_node(ino) else {
            return reply.error(ENOENT);
        };

        let NodeType::Device { state, .. } = &node.contents else {
            return reply.error(ENOTSUP);
        };

        self.max_fh = self.max_fh.checked_add(1).expect("file handle overflow");
        let fh = self.max_fh;

        state.lock().insert_client(fh, req.notifier());
        reply.opened(
            fh,
            fuse::consts::FOPEN_DIRECT_IO | fuse::consts::FOPEN_NONSEEKABLE,
        )
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
        debug!("read fh: {fh}, size: {size}, flags: {flags:?}");

        let Some(node) = self.tree.get_node(ino) else {
            return reply.error(ENOENT);
        };

        let NodeType::Device { state, .. } = &node.contents else {
            return reply.error(ENOTSUP);
        };

        let mut nodes = state.lock();
        let state = std::ops::DerefMut::deref_mut(&mut nodes);

        let Some(client) = state.clients.iter_mut().find(|c| c.fh == fh) else {
            return reply.error(EFAULT);
        };

        client.do_read(&state.ring, flags, size as usize, reply);
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
        let Some(node) = self.tree.get_node(ino) else {
            return reply.error(ENOENT);
        };

        let NodeType::Device { state, .. } = &node.contents else {
            return reply.error(EBADF);
        };

        let mut state = state.lock();
        if state.grabbed_fh == Some(fh) {
            state.grabbed_fh = None;
        }

        state.clients.retain(|c| c.fh != fh);
    }

    fn readdir(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        _fh: u64,
        offset: i64,
        mut reply: fuser::ReplyDirectory,
    ) {
        let idx = if ino == fuse::FUSE_ROOT_ID {
            Some(self.tree.root_index)
        } else {
            Index::from_bits(ino)
        };

        let Some(idx) = idx else {
            return reply.error(ENOENT);
        };

        let nodes = self.tree.nodes.lock();
        let Some(parent_node) = nodes.get(idx) else {
            return reply.error(ENOENT);
        };

        let NodeType::Dir { children } = &parent_node.contents else {
            return reply.error(ENOTSUP);
        };

        for (off, child) in children.iter().enumerate().skip(offset as usize) {
            let Some(node) = nodes.get(*child) else {
                return reply.error(EFAULT);
            };

            let filetype = match node.contents {
                NodeType::Dir { .. } => fuse::FileType::Directory,
                NodeType::Device { .. } => fuse::FileType::RegularFile,
            };

            debug!("child ino: {child:?} / {}", child.to_bits());
            if reply.add(child.to_bits(), (off + 1) as _, filetype, &node.name) {
                return reply.ok();
            }
        }

        reply.ok()
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

        let Some(mut node) = self.tree.get_node(ino) else {
            return reply.error(ENOENT);
        };

        let NodeType::Device { state, dev_attr } = &mut node.contents else {
            return reply.error(EBADF);
        };

        match cmd {
            Cmd::GetVersion => reply.ioctl(0, &EV_VERSION.to_ne_bytes()),
            Cmd::GetId => reply_struct(reply, Some(dev_attr.id)),
            Cmd::GetName(len) => reply_str(reply, len, dev_attr.name.as_deref()),
            Cmd::GetPhys(len) => reply_str(reply, len, dev_attr.phys.as_deref()),
            Cmd::GetUniq(len) => reply_str(reply, len, dev_attr.uniq.as_deref()),
            Cmd::GetProps(len) => reply_bits(reply, len, &dev_attr.prop_bits),
            Cmd::GetEventBits(ty, len) => match ty {
                0 => reply_bits(reply, len, &dev_attr.support.ev_bits),
                EV_KEY => reply_bits(reply, len, &dev_attr.support.key_bits),
                EV_REL => reply_bits(reply, len, &dev_attr.support.rel_bits),
                EV_ABS => reply_bits(reply, len, &dev_attr.support.abs_bits),
                EV_MSC => reply_bits(reply, len, &dev_attr.support.msc_bits),
                EV_LED => reply_bits(reply, len, &dev_attr.support.led_bits),
                EV_SND => reply_bits(reply, len, &dev_attr.support.snd_bits),
                EV_FF => reply_bits(reply, len, &dev_attr.support.ff_bits),
                EV_SW => reply_bits(reply, len, &dev_attr.support.sw_bits),
                _ => reply.error(EINVAL),
            },
            Cmd::SetAbsInfo(axis, info) => {
                dev_attr.absinfo.insert(axis as u16, info);
            }
            Cmd::GetAbsInfo(axis) => {
                reply_struct(reply, dev_attr.absinfo.get(&(axis as u16)).copied())
            }
            Cmd::Grab(v) if v > 0 => reply_empty(reply, state.lock().grab(fh)),
            Cmd::Grab(v) if v <= 0 => reply_empty(reply, state.lock().ungrab(fh)),
            _ => reply.error(ENOTSUP),
        }
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

        let Some(node) = self.tree.get_node(ino) else {
            return reply.error(ENOENT);
        };

        let NodeType::Device { state, .. } = &node.contents else {
            return reply.error(EBADF);
        };

        let mut nodes = state.lock();
        let state = std::ops::DerefMut::deref_mut(&mut nodes);

        let Some(client) = state.clients.iter_mut().find(|c| c.fh == fh) else {
            reply.error(EFAULT);
            return;
        };

        client.do_poll(&state.ring, ph, reply)
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

fn reply_struct<T: Sized>(reply: fuse::ReplyIoctl, v: Option<T>) {
    let mut out = vec![0_u8; size_of::<T>()];
    if let Some(value) = v {
        unsafe {
            std::ptr::write(out.as_mut_ptr() as *mut _, value);
        }
    }

    reply.ioctl(0, &out);
}

fn reply_empty(reply: fuse::ReplyIoctl, res: Result<(), Errno>) {
    match res {
        Ok(_) => reply.ioctl(0, &[]),
        Err(errno) => reply.error(errno.raw_os_error()),
    }
}

#[test]
fn first_inode_is_zero() {
    for _ in 0..10 {
        let mut a = Arena::new();
        assert_eq!(a.insert("foobar").to_bits(), u32::MAX as u64 + 1);
    }
}

#[test]
fn arena_index_is_never_root() {
    let mut a = Arena::new();
    for _ in 0..(1024 * 1024) {
        assert_ne!(a.insert(1).to_bits(), fuse::FUSE_ROOT_ID);
    }
}
