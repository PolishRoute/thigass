#![allow(clippy::explicit_iter_loop)]
#![allow(clippy::match_same_arms)]

use bstr::ByteSlice;
use parser::{parse, parse_curve, ParserError, Part, ReaderError, ScriptParser};

#[global_allocator]
static GLOBAL_MIMALLOC: mimalloc_rust::GlobalMiMalloc = mimalloc_rust::GlobalMiMalloc;

fn main() {
    let file_path = std::env::args().nth(1).expect("input file");
    let s = std::time::Instant::now();
    let script = std::fs::read(file_path).unwrap();
    let mut parser = ScriptParser::new(script.as_bstr());
    let script = parser.parse();

    for (_etype, event) in script.events.iter() {
        let parts = parse(event.text).unwrap();
        for part in parts {
            match part {
                Part::Text(t) => {
                    match parse_curve(t.as_bytes()) {
                        Ok(_) => {}
                        Err(ParserError::InvalidCurveOpcode | ParserError::ReaderError(ReaderError::ExpectedWhitespace { .. })) => {
                            //println!("{:?}", t);
                        }
                        Err(e) => panic!("{}: {:?}", t, e),
                    }
                }
                Part::Overrides(_) => {}
                Part::NewLine { .. } => {}
            }
        }
    }

    println!("File parsed in {:?}", s.elapsed());
}
