#![feature(adt_const_params)]

use std::{fmt, fs};
use std::fmt::{Formatter, write};
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

    for (idx, line) in reader.lines().enumerate() {
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
            let mut fields = x.splitn(10, ',').collect::<Vec<_>>();
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
                effect: fields[mapping.text],
            };
            println!("{:?}", line);
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
        Some(self.buf[self.pos])
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

    fn read_number(&mut self) -> u32 {
        let mut n = 0;
        for b in self.read_while(|c| c.is_ascii_digit()).iter().copied() {
            n = n * 10 + (b - b'0') as u32;
        }
        if self.peek() == Some(b'.') {
            self.consume();
        }
        for b in self.read_while(|c| c.is_ascii_digit()).iter().copied() {
            n = n * 10 + (b - b'0') as u32;
        }

        n
    }

    fn dbg(&self) {
        println!("{:?}", std::str::from_utf8(&self.buf[self.pos..]));
    }
}

#[derive(Debug)]
enum Code {
    Align(u32),
    Blur(u32),
    FontScaleX(u32),
    FontScaleY(u32),
    RotateX(u32),
    RotateY(u32),
    RotateZ(u32),
    Color(Vec<u8>),
    Pos(u32, u32),
    DrawScale(u32),
    Clip(Vec<DrawCommand>),
}

#[derive(Debug)]
enum DrawCommand {
    CloseAndMove(u32, u32),
    Move(u32, u32),
    Line(u32, u32),
    Bezier([(u32, u32); 3]),
}

fn read_n(reader: &mut Reader) -> (u32, u32) {
    let x = reader.read_number();
    reader.read_while(|c| c.is_ascii_whitespace());
    let y = reader.read_number();
    reader.read_while(|c| c.is_ascii_whitespace());
    (x, y)
}

fn parse_override(s: &[u8]) -> Result<(), ()> {
    let mut reader = Reader::new(s);
    let mut items = Vec::new();
    while let Some(x) = reader.peek() {
        match x {
            b'\\' => {
                reader.consume();
                if reader.try_tag(b"an") {
                    items.push(Code::Align(reader.read_number()));
                } else if reader.try_tag(b"blur") {
                    items.push(Code::Blur(reader.read_number()));
                } else if reader.try_tag(b"fscx") {
                    items.push(Code::FontScaleX(reader.read_number()));
                } else if reader.try_tag(b"fscy") {
                    items.push(Code::FontScaleY(reader.read_number()));
                } else if reader.try_tag(b"frz") {
                    items.push(Code::RotateZ(reader.read_number()));
                } else if reader.try_tag(b"fax") {
                    items.push(Code::RotateZ(reader.read_number()));
                } else if reader.try_tag(b"clip") {
                    reader.consume();
                    let mut cmds = Vec::new();
                    loop {
                        match reader.consume().unwrap() {
                            b'm' => {
                                reader.read_while(|c| c.is_ascii_whitespace());
                                let (x, y) = read_n(&mut reader);
                                cmds.push(DrawCommand::CloseAndMove(x, y));
                            }
                            b'm' => {
                                reader.read_while(|c| c.is_ascii_whitespace());
                                let (x, y) = read_n(&mut reader);
                                cmds.push(DrawCommand::Move(x, y));
                            }
                            b'l' => {
                                reader.read_while(|c| c.is_ascii_whitespace());

                                while reader.peek().unwrap().is_ascii_digit() {
                                    let (x, y) = read_n(&mut reader);
                                    cmds.push(DrawCommand::Line(x, y));
                                }
                            }
                            b'b' => {
                                reader.read_while(|c| c.is_ascii_whitespace());
                                let p1 = read_n(&mut reader);
                                reader.read_while(|c| c.is_ascii_whitespace());
                                let p2 = read_n(&mut reader);
                                reader.read_while(|c| c.is_ascii_whitespace());
                                let p3 = read_n(&mut reader);

                                cmds.push(DrawCommand::Bezier([p1, p2, p3]));
                            }
                            b')' => break,
                            _ => (),
                        }
                    }


                    items.push(Code::Clip(cmds));
                } else if reader.try_tag(b"c") {
                    reader.consume();
                    let color = reader.read_while(|c| c != b'&').to_vec();
                    reader.consume();
                    items.push(Code::Color(color));
                } else if reader.try_tag(b"pos") {
                    reader.consume();
                    let x = reader.read_number();
                    reader.consume();
                    let y = reader.read_number();
                    reader.consume();

                    items.push(Code::Pos(x, y));
                } else if reader.try_tag(b"p") {
                    items.push(Code::DrawScale(reader.read_number()));
                }
            }
            _ => {
                reader.dbg();
                break;
            }
        }
    }
    println!("{:?}", items);
    Ok(())
}

#[cfg(test)]
mod tests2 {
    use crate::parse_override;

    #[test]
    fn foo() {
        parse_override(b"\\an7\\blur4\\fscx50\\frz14.5\\fax0.27\\c&H3F343A&\\pos(117.73,1231.33)\\p1\\fscy50\\clip(m 549 1080 l 422 876 759 791 980 735 b 1028 724 1054 712 1059 696 l 1281 1080)\\t(1740,2140,1,\\blur1)");

        //m 1974.9 165.64 b 1974.56 165.28 1974.21 164.92 1973.87 164.56 1973.87 164.22 1973.92 163.9 1974.01 163.58 1974.36 163.94 1974.71 164.31 1975.07 164.68 1975.05 165 1974.99 165.32 1974.9 165.64
    }
}