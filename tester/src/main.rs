use parser::ScriptParser;

fn main() {
    let file_path = std::env::args().nth(1).expect("input file");
    let s = std::time::Instant::now();
    let script = std::fs::read(file_path).unwrap();
    let utf8 = std::str::from_utf8(&script).unwrap();
    let mut parser = ScriptParser::new(&utf8);
    parser.parse();
    println!("File parsed in {:?}", s.elapsed());
}
