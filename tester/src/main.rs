use bstr::ByteSlice;
use parser::{parse, ScriptParser};

#[global_allocator]
static GLOBAL_MIMALLOC: mimalloc_rust::GlobalMiMalloc = mimalloc_rust::GlobalMiMalloc;

fn main() {
    let file_path = std::env::args().nth(1).expect("input file");
    let s = std::time::Instant::now();
    let script = std::fs::read(file_path).unwrap();
    let mut parser = ScriptParser::new(script.as_bstr());
    let script = parser.parse();

    for (_etype, event) in script.events.into_iter() {
        let _x = parse(event.text).unwrap();
        // println!("{:?}", x);
    }

    println!("File parsed in {:?}", s.elapsed());
}
