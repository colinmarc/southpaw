use std::{env, path::PathBuf};

use anyhow::bail;
use regex::Regex;

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

    match linux_kernel_version() {
        Ok(v) => {
            for ((major, minor), tag) in VERSION_TO_TAG {
                let semver = semver::Version::new(*major, *minor, 0);
                if v > semver {
                    println!("cargo::rustc-cfg={tag}");
                }
            }
        }
        Err(e) => {
            println!(
                "cargo::warning=unable to determine linux kernel version: {:#}",
                e
            );
        }
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
    let Ok(version) = uname.release().to_str() else {
        bail!("invalid uname release");
    };

    // \A here matches only the start of the string - so we're not actually
    // searching.
    let re = Regex::new(r"\A\d+\.\d+\.\d+").unwrap();
    let Some(version) = re
        .find(version)
        .and_then(|m| semver::Version::parse(m.as_str()).ok())
    else {
        bail!(format!("invalid linux release: {version}"));
    };

    Ok(version)
}
