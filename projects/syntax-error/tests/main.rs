#![allow(unused, dead_code)]
use std::{iter::zip, ops::Range};
use syntax_error::{Color, ColorGenerator, Config, FileCache, FileID, Fmt, Label, Report, ReportKind, Source};

mod multi_file;
mod multi_line;
mod simple;
mod source;
mod stress_test;

#[test]
fn ready() {
    println!("it works!")
}
