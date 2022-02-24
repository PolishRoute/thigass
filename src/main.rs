use std::{fmt, fs};
use std::fs::read;
use std::io::{BufRead, BufReader};
use std::num::ParseIntError;
use std::str::FromStr;


struct Timestamp {
    value: u64,
}

impl fmt::Debug for Timestamp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{:02}:{:02}.{:02}",
               self.value / 360000,
               (self.value / 6000) % 60,
               (self.value / 100) % 60,
               self.value % 100,
        )
    }
}

#[derive(Debug)]
enum TimestampError {
    Todo,
}

impl From<ParseIntError> for TimestampError {
    fn from(_: ParseIntError) -> Self {
        todo!()
    }
}

impl FromStr for Timestamp {
    type Err = TimestampError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.splitn(3, ':');
        let hours: u64 = parts.next().ok_or_else(|| TimestampError::Todo)?.parse()?;
        let minutes: u64 = parts.next().ok_or_else(|| TimestampError::Todo)?.parse()?;
        let (seconds, hundredths) = parts.next().ok_or_else(|| TimestampError::Todo)?.split_once('.').ok_or_else(|| TimestampError::Todo)?;
        let seconds: u64 = seconds.parse()?;
        let hundredths: u64 = hundredths.parse()?;
        Ok(Timestamp { value: hours * 360000u64 + minutes * 6000u64 + seconds * 100u64 + hundredths })
    }
}

#[cfg(test)]
mod tests {
    use crate::{Timestamp};

    #[test]
    fn foo() {
        assert_eq!(format!("{:?}", "0:13:57.35".parse::<Timestamp>().unwrap()), "0:13:57.35");
    }
}

#[derive(Debug)]
#[allow(unused)]
struct Event<'s> {
    marked: bool,
    layer: i32,
    start: Timestamp,
    end: Timestamp,
    style: &'s str,
    name: &'s str,
    margin_l: Option<u32>,
    margin_r: Option<u32>,
    margin_v: Option<u32>,
    effect: &'s str,
    text: &'s str,
}

#[derive(Default, Debug)]
struct LineMapping {
    marked: usize,
    layer: usize,
    start: usize,
    end: usize,
    style: usize,
    name: usize,
    margin_l: usize,
    margin_r: usize,
    margin_v: usize,
    effect: usize,
    text: usize,
}

fn make_mapping(config: &str) -> Option<LineMapping> {
    let mut mapping = LineMapping::default();
    for (idx, field) in config.split(", ").enumerate() {
        match field {
            "Layer" => mapping.layer = idx,
            "Marked" => mapping.marked = idx,
            "Start" => mapping.start = idx,
            "End" => mapping.end = idx,
            "Style" => mapping.style = idx,
            "Name" => mapping.name = idx,
            "MarginL" => mapping.margin_l = idx,
            "MarginR" => mapping.margin_r = idx,
            "MarginV" => mapping.margin_v = idx,
            "Effect" => mapping.effect = idx,
            "Text" => mapping.text = idx,
            _ => unimplemented!("{}", field),
        }
    }

    Some(mapping)
}

#[derive(Eq, PartialEq)]
enum Section {
    Events
}

fn main() {
    let reader = BufReader::new(fs::File::open("ReinForce_Kishuku_Gakkou_no_Juliet_07_BDRip_1920x1080_x264_FLAC.ass").unwrap());

    let mut mapping = None;

    let mut current_section = None;

    for line in reader.lines() {
        let line = line.unwrap();

        if let Some(section) = line.strip_prefix("[") {
            current_section = match section.strip_suffix(']').unwrap() {
                "Events" => Some(Section::Events),
                _ => None,
            };
        } else if let Some(format) = line.strip_prefix("Format: ") {
            if current_section != Some(Section::Events) {
                continue;
            }
            mapping = dbg!(make_mapping(format));
        } else if let Some(x) = line.strip_prefix("Dialogue: ") {
            let mapping = mapping.as_ref().unwrap();
            let fields = x.splitn(10, ',').collect::<Vec<_>>();
            let line = Event {
                marked: fields[mapping.marked] == "1",
                layer: fields[mapping.layer].parse().unwrap(),
                start: fields[mapping.start].parse().unwrap(),
                end: fields[mapping.end].parse().unwrap(),
                style: fields[mapping.style],
                name: fields[mapping.name],
                margin_l: fields[mapping.margin_l].parse().ok(),
                margin_r: fields[mapping.margin_r].parse().ok(),
                margin_v: fields[mapping.margin_v].parse().ok(),
                effect: fields[mapping.effect],
                text: fields[mapping.text],
            };

            match parse(line.text.as_bytes()) {
                Ok(res) => {
                    // println!("{:?}", res);
                }
                Err(r) => todo!("{:?}", r),
            }
        }
    }
}

struct Reader<'d> {
    buf: &'d [u8],
    pos: usize,
}

impl<'d> Reader<'d> {
    fn new(s: &'d [u8]) -> Reader<'d> {
        Reader {
            buf: s,
            pos: 0,
        }
    }

    fn consume(&mut self) -> Option<u8> {
        let x = self.buf[self.pos];
        self.pos += 1;
        Some(x)
    }

    fn peek(&self) -> Option<u8> {
        self.buf.get(self.pos).copied()
    }

    fn try_tag(&mut self, tag: &[u8]) -> bool {
        if self.buf[self.pos as usize..].starts_with(tag) {
            self.pos += tag.len();
            true
        } else {
            false
        }
    }

    fn read_while(&mut self, f: impl Fn(u8) -> bool) -> &[u8] {
        let pos = self.pos;
        while let Some(p) = self.peek() {
            if !f(p) {
                break;
            }
            self.consume();
        }
        &self.buf[pos..self.pos]
    }

    #[track_caller]
    fn read_float(&mut self) -> f32 {
        let x = self.read_while(|c| matches!(c, b'0'..=b'9' | b'.' | b'-'));
        let x = std::str::from_utf8(x).unwrap();
        x.parse().unwrap()
    }

    fn read_string(&mut self) -> String {
        let x = self.read_while(|c| c != b'\\');
        let x = std::str::from_utf8(x).unwrap();
        x.to_string()
    }

    fn read_number(&mut self) -> u32 {
        let mut n = 0;
        for b in self.read_while(|c| c.is_ascii_digit()).iter().copied() {
            n = n * 10 + (b - b'0') as u32;
        }
        n
    }

    fn expect(&mut self, b: u8) -> Result<(), ()> {
        if self.try_tag(&[b]) {
            Ok(())
        } else {
            Err(())
        }
    }

    fn dbg(&self) {
        println!("!!! {:?}", std::str::from_utf8(&self.buf[self.pos..]));
    }
}

#[derive(Debug)]
enum Code {
    Align(Alignment),
    Blur(f32),
    Border(f32),
    XBorder(f32),
    YBorder(f32),
    FontName(String),
    FontSize(f32),
    FontScaleX(f32),
    FontScaleY(f32),
    RotateX(f32),
    RotateY(f32),
    RotateZ(f32),
    Color(Vec<u8>),
    Pos(u32, u32),
    DrawScale(f32),
    Clip(Vec<DrawCommand>),
    Bold(bool),
    Shadow(u32),
    XShadow(u32),
    YShadow(u32),
    WrappingStyle(u32),
    Reset,
}

#[derive(Debug)]
struct Alignment(u8);

#[derive(Debug)]
enum DrawCommand {
    CloseAndMove(f32, f32),
    Move(f32, f32),
    Line(f32, f32),
    Bezier([(f32, f32); 3]),
}

fn read_n(reader: &mut Reader) -> (f32, f32) {
    let x = reader.read_float();
    reader.read_while(|c| c.is_ascii_whitespace());
    let y = reader.read_float();
    reader.read_while(|c| c.is_ascii_whitespace());
    (x, y)
}

fn parse_override(reader: &mut Reader) -> Result<Vec<Code>, ()> {
    let mut items = Vec::new();
    while let Some(x) = reader.peek() {
        match x {
            b'\\' => {
                reader.expect(b'\\')?;
                if reader.try_tag(b"an") {
                    items.push(Code::Align(Alignment(reader.read_number() as u8)));
                } else if reader.try_tag(b"blur") {
                    items.push(Code::Blur(reader.read_float()));
                } else if reader.try_tag(b"bord") {
                    items.push(Code::Border(reader.read_float()));
                } else if reader.try_tag(b"xbord") {
                    items.push(Code::XBorder(reader.read_float()));
                } else if reader.try_tag(b"ybord") {
                    items.push(Code::YBorder(reader.read_float()));
                } else if reader.try_tag(b"fn") {
                    items.push(Code::FontName(reader.read_string()));
                } else if reader.try_tag(b"fscx") {
                    items.push(Code::FontScaleX(reader.read_float()));
                } else if reader.try_tag(b"fscy") {
                    items.push(Code::FontScaleY(reader.read_float()));
                } else if reader.try_tag(b"fs") {
                    items.push(Code::FontSize(reader.read_float()));
                } else if reader.try_tag(b"frx") {
                    items.push(Code::RotateX(reader.read_float()));
                } else if reader.try_tag(b"fry") {
                    items.push(Code::RotateY(reader.read_float()));
                } else if reader.try_tag(b"frz") {
                    items.push(Code::RotateZ(reader.read_float()));
                } else if reader.try_tag(b"fax") {
                    items.push(Code::RotateZ(reader.read_float()));
                } else if reader.try_tag(b"clip") {
                    reader.expect(b'(')?;
                    let mut cmds = Vec::new();
                    loop {
                        match reader.consume().unwrap() {
                            b'm' => {
                                reader.read_while(|c| c.is_ascii_whitespace());
                                let (x, y) = read_n(reader);
                                cmds.push(DrawCommand::CloseAndMove(x, y));
                            }
                            b'n' => {
                                reader.read_while(|c| c.is_ascii_whitespace());
                                let (x, y) = read_n(reader);
                                cmds.push(DrawCommand::Move(x, y));
                            }
                            b'l' => {
                                reader.read_while(|c| c.is_ascii_whitespace());

                                while reader.peek().unwrap().is_ascii_digit() {
                                    let (x, y) = read_n(reader);
                                    cmds.push(DrawCommand::Line(x, y));
                                }
                            }
                            b'b' => {
                                reader.read_while(|c| c.is_ascii_whitespace());
                                let p1 = read_n(reader);
                                reader.read_while(|c| c.is_ascii_whitespace());
                                let p2 = read_n(reader);
                                reader.read_while(|c| c.is_ascii_whitespace());
                                let p3 = read_n(reader);

                                cmds.push(DrawCommand::Bezier([p1, p2, p3]));
                            }
                            b')' => break,
                            _ => (),
                        }
                    }

                    items.push(Code::Clip(cmds));
                } else if reader.try_tag(b"c")
                    || reader.try_tag(b"1c")
                    || reader.try_tag(b"2c")
                    || reader.try_tag(b"3c")
                    || reader.try_tag(b"4c")
                    || reader.try_tag(b"1a")
                    || reader.try_tag(b"3a")
                    || reader.try_tag(b"4a")
                    || reader.try_tag(b"alpha") {
                    reader.expect(b'&')?;
                    reader.expect(b'H')?;
                    let hex = reader.read_while(|c| c.is_ascii_hexdigit()).to_vec();
                    reader.expect(b'&')?;
                    items.push(Code::Color(hex));
                } else if reader.try_tag(b"pos") {
                    reader.consume();
                    let x = reader.read_number();
                    reader.consume();
                    let y = reader.read_number();
                    reader.consume();

                    items.push(Code::Pos(x, y));
                } else if reader.try_tag(b"p") {
                    items.push(Code::DrawScale(reader.read_float()));
                } else if reader.try_tag(b"b") {
                    items.push(Code::Bold(reader.consume().unwrap() == b'1'));
                } else if reader.try_tag(b"shad") {
                    items.push(Code::Shadow(reader.read_number()));
                } else if reader.try_tag(b"t") || reader.try_tag(b"fad") || reader.try_tag(b"move") {
                    reader.expect(b'(')?;
                    let x = reader.read_while(|b| b != b')');
                    reader.expect(b')')?;
                } else if reader.try_tag(b"xshad") {
                    items.push(Code::XShadow(reader.read_number()));
                } else if reader.try_tag(b"yshad") {
                    items.push(Code::YShadow(reader.read_number()));
                } else if reader.try_tag(b"q") {
                    items.push(Code::WrappingStyle(reader.read_number()));
                } else if reader.try_tag(b"r") {
                    items.push(Code::Reset);
                } else {
                    reader.dbg();
                    return Err(());
                }
            }
            b'}' => {
                break;
            }
            _ => {
                reader.consume();
            }
        }
    }

    Ok(items)
}

#[derive(Debug)]
enum Part {
    Text(String),
    Overrides(Vec<Code>),
}

fn parse(s: &[u8]) -> Result<Vec<Part>, ()> {
    let mut parts = Vec::new();

    let mut buff = Vec::new();
    let mut reader = Reader::new(s);
    while let Some(x) = reader.peek() {
        match x {
            b'{' => {
                if !buff.is_empty() {
                    parts.push(Part::Text(String::from_utf8_lossy(&buff).into_owned()));
                }

                reader.expect(b'{')?;
                let codes = parse_override(&mut reader)?;
                reader.expect(b'}')?;

                parts.push(Part::Overrides(codes));
            }
            b'\\' => {
                reader.consume();
                if reader.consume() == Some(b'n') {
                    buff.push(b'\n');
                }
            }
            _ => {
                buff.push(reader.consume().unwrap());
            }
        }
    }

    if !buff.is_empty() {
        parts.push(Part::Text(String::from_utf8_lossy(&buff).into_owned()));
    }

    Ok(parts)
}

#[cfg(test)]
mod tests2 {
    use crate::parse;

    #[test]
    fn simple_parse() {
        dbg!(parse(br"{\fs16}This is small text. {\fs28}This is\n large text").unwrap());
        dbg!(parse(br"{\an7\blur4\fscx50\frz14.5\fax0.27\c&H303587&\pos(118.73,1232.33)\p1\fscy50\clip(m 549 1080 l 422 876 759 791 980 735 b 1028 724 1054 712 1059 696 l 1281 1080)\t(1740,2140,1,\blur1)}m 1859.05 -127.72 b 1859.79 -128.1 1860.54 -128.48 1861.29 -128.86 1861.64 -128.83 1862 -128.79 1862.36 -128.74 1862.69 -128.38 1863.01 -128.02 1863.34 -127.66 1862.92 -127.2 1862.51 -126.74 1862.09 -126.28 1861.08 -126.76 1860.06 -127.24 1859.05 -127.72"));
    }
}