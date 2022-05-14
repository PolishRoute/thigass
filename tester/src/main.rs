use parser::{parse, ScriptParser};

fn main() {
    let file_path = std::env::args().nth(1).expect("input file");
    let s = std::time::Instant::now();
    let script = std::fs::read(file_path).unwrap();
    let utf8 = std::str::from_utf8(&script).unwrap();
    let mut parser = ScriptParser::new(&utf8);
    let script = parser.parse();

    for (etype, e) in script.events.into_iter().skip(1000).take(10) {
        if let Ok(x) = parse(e.text.as_bytes()) {
            // dbg!(x);
        }
    }

    println!("File parsed in {:?}", s.elapsed());
}
