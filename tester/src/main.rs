#![allow(clippy::explicit_iter_loop)]
#![allow(clippy::match_same_arms)]

use bstr::ByteSlice;
use parser::{parse, parse_curve, ParserError, Part, ReaderError, ScriptParser};
use std::path::Path;

#[global_allocator]
static GLOBAL_MIMALLOC: mimalloc_rust::GlobalMiMalloc = mimalloc_rust::GlobalMiMalloc;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing::Level::TRACE)
        .init();

    let file_path = std::env::args_os().nth(1).expect("input file");
    let path = Path::new(&file_path);
    if path.is_dir() {
        let s = std::time::Instant::now();
        let mut len = 0;

        for entry in walkdir::WalkDir::new(path).into_iter() {
            let entry = entry.unwrap();
            if entry.path().is_dir() {
                continue;
            }

            // tracing::info!("{}", entry.path().display());
            parse_file(entry.path()).unwrap();
            len += entry.metadata().unwrap().len();
        }

        tracing::info!(
            "{} bytes parsed in {:?}; {}MB/s",
            len,
            s.elapsed(),
            (len as f32 / 1024.0 / 1024.0) / s.elapsed().as_secs_f32()
        );
    } else {
        parse_file(path).unwrap();
    }

    Ok(())
}

fn parse_file(path: impl AsRef<Path>) -> anyhow::Result<()> {
    tracing::info!("Parsing '{}'", path.as_ref().display());

    let s = std::time::Instant::now();
    let content = std::fs::read(path)?;
    let mut parser = ScriptParser::new(content.as_bstr());
    let script = parser.parse();

    // let mut curves = Vec::new();

    for (_etype, event) in script.events.iter() {
        let parts = parse(event.text)?;
        for part in parts {
            match part {
                Part::Text(t) => {
                    match parse_curve(t.as_bytes()) {
                        Ok(_c) => {
                            // curves.push(c);
                        }
                        Err(ParserError::InvalidCurveOpcode | ParserError::ReaderError(ReaderError::ExpectedWhitespace { .. })) => {
                            // println!("{:?}", t);
                        }
                        Err(e) => panic!("{}: {:?}", t, e),
                    }
                }
                Part::Overrides(_) => {}
                Part::NewLine { .. } => {}
                Part::Horizontal => {}
            }
        }
    }

    tracing::info!("File of {} bytes parsed in {:?}; {}MB/s", content.len(), s.elapsed(), (content.len() as f32 / 1024.0 / 1024.0) / s.elapsed().as_secs_f32());

    Ok(())
}
