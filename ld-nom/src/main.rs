#![feature(let_chains)]
#![feature(once_cell)]

mod ast;
mod compiler;
pub mod lexer;
mod parse_string;
pub mod parser;
mod token;
pub mod token_span;

use clap::Parser;
use compiler::compile_ld;
use std::{fs, path::Path, process::exit};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path of the program to compile
    paths: Vec<String>,
}

fn main() {
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

        let dir = path.parent().unwrap().to_str().unwrap();
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let output_name = path.file_stem().unwrap().to_str().unwrap();
        println!("Compiling {}...", file_name);
        let file_content = fs::read_to_string(path).expect("Unable to read file");
        compile_ld(&file_content, dir, output_name).unwrap();
    }
}
