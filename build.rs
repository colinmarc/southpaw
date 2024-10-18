use std::{env, path::PathBuf};

use anyhow::Context as _;

fn main() {
    let bindings = bindgen::Builder::default()
        .header("src/bindings.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .parse_callbacks(Box::new(IntKindCallbacks))
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");

    // Configure the project based on the current linux version.
    println!("cargo::rustc-check-cfg=cfg(linux54,linux510,linux515,linux61,linux66)");
    const VERSION_TO_TAG: &[((u64, u64), &str)] = &[
        ((5, 4), "linux54"),
        ((5, 10), "linux510"),
        ((5, 15), "linux515"),
        ((6, 1), "linux61"),
        ((6, 6), "linux66"),
    ];

    if let Ok(version) = linux_kernel_version() {
        for ((major, minor), tag) in VERSION_TO_TAG {
            let semver = semver::Version::new(*major, *minor, 0);
            if version > semver {
                println!("cargo::rustc-cfg={tag}");
            }
        }
    } else {
        println!("cargo::warning=unable to determine linux kernel version!");
    }
}

#[derive(Debug)]
struct IntKindCallbacks;

impl bindgen::callbacks::ParseCallbacks for IntKindCallbacks {
    fn int_macro(&self, _name: &str, _value: i64) -> Option<bindgen::callbacks::IntKind> {
        Some(bindgen::callbacks::IntKind::U16)
    }
}

fn linux_kernel_version() -> anyhow::Result<semver::Version> {
    let uname = rustix::system::uname();
    let version = uname.release().to_str().context("invalid version")?;
    let version = version
        .split_whitespace()
        .next()
        .context("invalid version")?;

    semver::Version::parse(version).context("invalid semver")
}
