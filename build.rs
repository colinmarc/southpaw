use std::{env, path::PathBuf};

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
}

#[derive(Debug)]
struct IntKindCallbacks;

impl bindgen::callbacks::ParseCallbacks for IntKindCallbacks {
    fn int_macro(&self, _name: &str, _value: i64) -> Option<bindgen::callbacks::IntKind> {
        Some(bindgen::callbacks::IntKind::U16)
    }
}
