## Southpaw

![tests](https://github.com/colinmarc/southpaw/actions/workflows/tests.yaml/badge.svg) [![docs](https://img.shields.io/docsrs/southpaw)](https://docs.rs/southpaw/latest)

This is a crate that lets you emulate linux [evdev](https://docs.kernel.org/input/input.html) character devices completely in userspace, using [FUSE](https://www.kernel.org/doc/html/latest/filesystems/fuse.html).

### Why would I want to do that?

Linux has good support for emulating input devices, with [uinput](https://www.kernel.org/doc/html/latest/input/uinput.html). However, it suffers from a few drawbacks:

 - It doesn't hew to any sort of namespacing, and the devices it emulates are global to the system. That makes it difficult to use inside containers.
 - Because of the above, it usually requires elevated privileges.

Because FUSE mounts can be used inside mount namespaces, southpaw lets you simulate input devices in rootless containers, without even needing root inside the container.

### Features

The following ioctls are currently implemented:

| Ioctl            | Support |
| ---------------- | ------- |
| EVIOCGVERSION    |   ✅    |
| EVIOCGID         |   ✅    |
| EVIOCGREP        |   ❌    |
| EVIOCSREP        |   ❌    |
| EVIOCGKEYCODE    |   ❌    |
| EVIOCGKEYCODE_V2 |   ❌    |
| EVIOCSKEYCODE    |   ❌    |
| EVIOCSKEYCODE_V2 |   ❌    |
| EVIOCGNAME       |   ✅    |
| EVIOCGPHYS       |   ❌    |
| EVIOCGUNIQ       |   ❌    |
| EVIOCGPROP       |   ✅    |
| EVIOCGMTSLOTS    |   ❌    |
| EVIOCGKEY        |   ❌    |
| EVIOCGLED        |   ❌    |
| EVIOCGSND        |   ❌    |
| EVIOCGSW         |   ❌    |
| EVIOCGBIT        |   ✅    |
| EVIOCGABS        |   ❌    |
| EVIOCSABS        |   ❌    |
| EVIOCSFF         |   ❌    |
| EVIOCGRMFF       |   ❌    |
| EVIOCGEFFECTS    |   ❌    |
| EVIOCGRAB        |   ✅    |
| EVIOCREVOKE      |   ❌    |
| EVIOCGMASK       |   ❌    |
| EVIOCSMASK       |   ❌    |
| EVIOCSCLOCKID    |   ❌    |
| EVIOCGVERSION    |   ❌    |
| EVIOCGVERSION    |   ❌    |

Basic reading and publishing events is supported, both for blocking and non-blocking readers. Force feedback is not supported yet.
