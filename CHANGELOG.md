# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.1](https://github.com/colinmarc/southpaw/compare/v0.1.0...v0.1.1) - 2024-10-14

### Added

- support EVIOCGABS and EVIOCSABS
- instead of single device mounts, offer a tree of device nodes
- support EVIOCGPHYS and EVIOCGUNIQ

### Other

- integrate release-plz
- use unshare to run mount tests
- remove libfuse as a dependency
- README tweaks
