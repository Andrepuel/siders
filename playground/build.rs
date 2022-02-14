use std::{fs::File, process::Command};

fn main() {
    Command::new("cargo")
        .args(["run", "--", "playground/main.idl"])
        .current_dir("../")
        .stdout(File::create("main.h").unwrap())
        .output()
        .unwrap();

    Command::new("bindgen")
        .args(["main.h", "--impl-debug"])
        .stdout(File::create("src/main_sys.rs").unwrap())
        .output()
        .unwrap();

    cc::Build::new().file("main.c").compile("main");
}
