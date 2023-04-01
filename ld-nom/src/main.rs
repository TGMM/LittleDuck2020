#![feature(let_chains)]
#![feature(lazy_cell)]

mod ast;
mod compiler;
pub mod lexer;
mod parse_string;
pub mod parser;
mod token;
pub mod token_span;

use clap::Parser;
use compiler::compile_ld;
use std::{env, fs, path::Path, process::exit};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path of the program to compile
    paths: Vec<String>,
}

#[cfg(windows)]
pub(crate) fn do_msvc_check() -> bool {
    use cc::windows_registry;

    // TODO: The x64 compiler should also be able to compile x86
    let host_triple = current_platform::COMPILED_ON;
    if !host_triple.contains("msvc") {
        return true;
    }

    windows_registry::find_tool(&host_triple, "cl.exe").is_some()
}

fn main() {
    #[cfg(windows)]
    if !do_msvc_check() {
        eprintln!(
            "The Microsoft C++ build tools for Visual Studio 2013 or
        later are required, but they don't seem to be installed."
        );
        exit(-1);
    }

    let args = Args::parse();

    if args.paths.is_empty() {
        eprintln!("Littleduck: error: no input files");
        exit(-1);
    }

    for path in args.paths {
        let path = Path::new(&path);

        if !path.is_file() {
            eprintln!("Please provide a valid file path (Is the path a directory?)");
            exit(-1);
        }

        let current_dir = env::current_dir().unwrap();
        let dir = current_dir.to_str().expect("Couldn't find parent");
        let file_name = path
            .file_name()
            .unwrap()
            .to_str()
            .expect("Invalid file name");
        let output_name = path
            .file_stem()
            .unwrap()
            .to_str()
            .expect("Invalid output name");
        println!("Compiling {}...", file_name);
        let file_content = fs::read_to_string(path).expect("Unable to read file");
        compile_ld(&file_content, dir, output_name).unwrap();
    }
}
