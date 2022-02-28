#![feature(let_else)]

use std::{fmt, fs};
use std::io::{BufRead, BufReader};
use std::num::ParseIntError;
use std::path::Path;
use std::str::FromStr;

use fxhash::FxBuildHasher;
use indexmap::IndexMap;

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
        let (seconds, hundredths) = parts.next()
            .ok_or_else(|| TimestampError::Todo)?
            .split_once('.')
            .ok_or_else(|| TimestampError::Todo)?;
        let seconds: u64 = seconds.parse()?;
        let hundredths: u64 = hundredths.parse()?;
        Ok(Timestamp { value: hours * 360000u64 + minutes * 6000u64 + seconds * 100u64 + hundredths })
    }
}

#[cfg(test)]
mod tests {
    use crate::Timestamp;

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

#[derive(Hash, Eq, PartialEq)]
enum EventField {
    Marked,
    Layer,
    Start,
    End,
    Style,
    Name,
    MarginL,
    MarginR,
    MarginV,
    Effect,
    Text,
}

fn parse_events_mapping(config: &str) -> Option<IndexMap<EventField, usize, FxBuildHasher>> {
    let mut mapping = IndexMap::with_hasher(FxBuildHasher::default());
    for (idx, field) in config.split(", ").enumerate() {
        let key = match field {
            "Layer" => EventField::Layer,
            "Marked" => EventField::Marked,
            "Start" => EventField::Start,
            "End" => EventField::End,
            "Style" => EventField::Style,
            "Name" => EventField::Name,
            "MarginL" => EventField::MarginL,
            "MarginR" => EventField::MarginR,
            "MarginV" => EventField::MarginV,
            "Effect" => EventField::Effect,
            "Text" => EventField::Text,
            _ => unimplemented!("{}", field),
        };
        mapping.insert(key, idx);
    }

    Some(mapping)
}

#[derive(Eq, PartialEq, Debug)]
enum Section {
    Events,
    ScriptInto,
    V4Styles,
}

fn main() {
    let a = std::time::Instant::now();
    parse_file("ReinForce_Kishuku_Gakkou_no_Juliet_07_BDRip_1920x1080_x264_FLAC.ass");
    dbg!(a.elapsed());
}

#[allow(unused)]
struct ScriptInfo {
    wrap_style: u32,
    scaled_border_and_shadow: bool,
    title: String,
    play_res_x: u32,
    play_res_y: u32,
    script_type: String,
    ycb_cr_matrix: String,
    original_translation: String,
    last_style_storage: String,
    audio_file: String,
    video_file: String,
    video_ar_value: f32,
    video_zoom_percent: f32,
    active_line: u32,
    video_position: u32,
}

#[derive(Hash, Eq, PartialEq)]
enum StyleField {
    Name,
    FontName,
    FontSize,
    PrimaryColour,
    SecondaryColour,
    OutlineColour,
    BackColour,
    Bold,
    Italic,
    Underline,
    StrikeOut,
    ScaleX,
    ScaleY,
    Spacing,
    Angle,
    BorderStyle,
    Outline,
    Shadow,
    Alignment,
    MarginL,
    MarginR,
    MarginV,
    Encoding,
}

#[derive(Debug)]
#[allow(unused)]
struct Color {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

impl FromStr for Color {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.strip_prefix("&H").unwrap_or(s);
        Ok(match s.len() {
            8 => Color {
                r: u8::from_str_radix(&s[0..2], 16).unwrap(),
                g: u8::from_str_radix(&s[2..4], 16).unwrap(),
                b: u8::from_str_radix(&s[4..6], 16).unwrap(),
                a: u8::from_str_radix(&s[6..8], 16).unwrap(),
            },
            6 => Color {
                r: u8::from_str_radix(&s[0..2], 16).unwrap(),
                g: u8::from_str_radix(&s[2..4], 16).unwrap(),
                b: u8::from_str_radix(&s[4..6], 16).unwrap(),
                a: 255,
            },
            2 => Color {
                r: 255,
                g: 255,
                b: 255,
                a: u8::from_str_radix(&s[0..2], 16).unwrap(),
            },
            _ => todo!("{}", s),
        })
    }
}

#[derive(Debug)]
#[allow(unused)]
struct Style {
    name: String,
    font_name: String,
    font_size: f32,
    primary_colour: Color,
    secondary_colour: Color,
    outline_colour: Color,
    back_colour: Color,
    bold: bool,
    italic: bool,
    underline: bool,
    strike_out: bool,
    scale_x: f32,
    scale_y: f32,
    spacing: f32,
    angle: f32,
    border_style: (),
    outline: f32,
    shadow: f32,
    alignment: Alignment,
    margin_l: f32,
    margin_r: f32,
    margin_v: f32,
    encoding: (),
}

fn parse_styles_mapping(s: &str) -> Option<IndexMap<StyleField, usize, FxBuildHasher>> {
    let mut mapping = IndexMap::with_hasher(FxBuildHasher::default());
    for (idx, s) in s.split(", ").enumerate() {
        let key = match s {
            "Name" => StyleField::Name,
            "Fontname" => StyleField::FontName,
            "Fontsize" => StyleField::FontSize,
            "PrimaryColour" => StyleField::PrimaryColour,
            "SecondaryColour" => StyleField::SecondaryColour,
            "OutlineColour" => StyleField::OutlineColour,
            "BackColour" => StyleField::BackColour,
            "Bold" => StyleField::Bold,
            "Italic" => StyleField::Italic,
            "Underline" => StyleField::Underline,
            "StrikeOut" => StyleField::StrikeOut,
            "ScaleX" => StyleField::ScaleX,
            "ScaleY" => StyleField::ScaleY,
            "Spacing" => StyleField::Spacing,
            "Angle" => StyleField::Angle,
            "BorderStyle" => StyleField::BorderStyle,
            "Outline" => StyleField::Outline,
            "Shadow" => StyleField::Shadow,
            "Alignment" => StyleField::Alignment,
            "MarginL" => StyleField::MarginL,
            "MarginR" => StyleField::MarginR,
            "MarginV" => StyleField::MarginV,
            "Encoding" => StyleField::Encoding,
            _ => unimplemented!(),
        };
        mapping.insert(key, idx);
    }

    Some(mapping)
}

#[allow(unused)]
fn parse_file(path: impl AsRef<Path>) {
    let mut reader = BufReader::new(fs::File::open(path).unwrap());
    let mut events_mapping = None;
    let mut styles_mapping = None;
    let mut current_section = None;
    let mut buffer = String::new();
    let mut styles = Vec::new();

    let parse_event = |data: &str, mapping: &IndexMap<EventField, usize, FxBuildHasher>, event_type: EventType, line_number: usize| {
        let fields = data.splitn(10, ',').collect::<Vec<_>>();
        let event = Event {
            marked: mapping.get(&EventField::Marked).and_then(|idx| fields.get(*idx)) == Some(&"1"),
            layer: fields[mapping[&EventField::Layer]].parse().unwrap(),
            start: fields[mapping[&EventField::Start]].parse().unwrap(),
            end: fields[mapping[&EventField::End]].parse().unwrap(),
            style: fields[mapping[&EventField::Style]],
            name: fields[mapping[&EventField::Name]],
            margin_l: Some(fields[mapping[&EventField::MarginL]].parse().unwrap()),
            margin_r: Some(fields[mapping[&EventField::MarginR]].parse().unwrap()),
            margin_v: Some(fields[mapping[&EventField::MarginV]].parse().unwrap()),
            effect: fields[mapping[&EventField::Effect]],
            text: fields[mapping[&EventField::Text]],
        };

        match parse(event.text.as_bytes()) {
            Ok(ref res) => {
                let mut x = false;
                for part in res {
                    if let Part::Text(text) = part {
                        let mut reader = Reader::new(text.as_bytes());
                        if let Err(_e) = parse_curve(&mut reader) {
                            // dbg!(line_number, text, e);
                            x = true;
                        }
                    }
                }

                if x {
                    // dbg!(&line.text);
                }
            }
            Err(r) => todo!("{:?}", r),
        }
    };

    enum EventType {
        Dialogue,
        Comment,
    }

    let mut line_number = 0;

    loop {
        buffer.clear();
        let line = match reader.read_line(&mut buffer) {
            Ok(0) => break,
            Ok(_) => buffer.trim_end(),
            Err(e) => panic!("{:?}", e),
        };
        line_number += 1;

        if let Some(section) = line.strip_prefix("[") {
            current_section = match section.strip_suffix(']').unwrap() {
                "Events" => Some(Section::Events),
                "Script Info" => Some(Section::ScriptInto),
                "V4+ Styles" => Some(Section::V4Styles),
                _ => None,
            };
        } else if let Some(format) = line.strip_prefix("Format: ") {
            let Some(section) = current_section.as_ref() else {
                continue;
            };

            match section {
                Section::Events => events_mapping = parse_events_mapping(format),
                Section::V4Styles => styles_mapping = parse_styles_mapping(format),
                _ => todo!(),
            }
        } else if let Some(x) = line.strip_prefix("Dialogue: ") {
            parse_event(x, events_mapping.as_ref().unwrap(), EventType::Dialogue, line_number);
        } else if let Some(x) = line.strip_prefix("Comment: ") {
            parse_event(x, events_mapping.as_ref().unwrap(), EventType::Comment, line_number);
        } else if let Some(s) = line.strip_prefix("Style: ") {
            let mapping = styles_mapping.as_ref().unwrap();
            let fields = s.split(',').collect::<Vec<_>>();
            let style = Style {
                name: fields[mapping[&StyleField::Name]].to_string(),
                font_name: fields[mapping[&StyleField::FontName]].parse().unwrap(),
                font_size: fields[mapping[&StyleField::FontSize]].parse().unwrap(),
                primary_colour: fields[mapping[&StyleField::PrimaryColour]].parse().unwrap(),
                secondary_colour: fields[mapping[&StyleField::SecondaryColour]].parse().unwrap(),
                outline_colour: fields[mapping[&StyleField::OutlineColour]].parse().unwrap(),
                back_colour: fields[mapping[&StyleField::BackColour]].parse().unwrap(),
                bold: fields[mapping[&StyleField::Bold]] == "1",
                italic: fields[mapping[&StyleField::Italic]] == "1",
                underline: fields[mapping[&StyleField::Underline]] == "1",
                strike_out: fields[mapping[&StyleField::StrikeOut]] == "1",
                scale_x: fields[mapping[&StyleField::ScaleX]].parse().unwrap(),
                scale_y: fields[mapping[&StyleField::ScaleY]].parse().unwrap(),
                spacing: fields[mapping[&StyleField::Spacing]].parse().unwrap(),
                angle: fields[mapping[&StyleField::Angle]].parse().unwrap(),
                border_style: (),
                outline: fields[mapping[&StyleField::Outline]].parse().unwrap(),
                shadow: fields[mapping[&StyleField::Shadow]].parse().unwrap(),
                alignment: Alignment(fields[mapping[&StyleField::Alignment]].parse().unwrap()),
                margin_l: fields[mapping[&StyleField::MarginL]].parse().unwrap(),
                margin_r: fields[mapping[&StyleField::MarginR]].parse().unwrap(),
                margin_v: fields[mapping[&StyleField::MarginV]].parse().unwrap(),
                encoding: (),
            };
            styles.push(style);
        } else if let Some((name, value)) = line.split_once(": ") {
            // println!("{} = {}", name, value);
        } else if line.starts_with(";") {
            continue;
        } else {
            println!(">> {}", line);
        }

        buffer.clear();
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

    fn try_consume(&mut self, prefix: &[u8]) -> bool {
        if self.buf[self.pos..].starts_with(prefix) {
            self.pos += prefix.len();
            true
        } else {
            false
        }
    }

    fn take_while(&mut self, f: impl Fn(u8) -> bool) -> &'d [u8] {
        let pos = self.pos;
        while let Some(p) = self.peek() {
            if !f(p) {
                break;
            }
            self.consume().unwrap();
        }
        &self.buf[pos..self.pos]
    }

    fn read_float(&mut self) -> Result<f32, ReaderError> {
        let x = self.take_while(|c| matches!(c, b'0'..=b'9' | b'.' | b'-'));
        let x = std::str::from_utf8(x).unwrap();
        x.parse().map_err(|_| ReaderError::InvalidFloat)
    }

    fn read_string(&mut self) -> String {
        let x = self.take_while(|c| c != b'\\');
        let x = std::str::from_utf8(x).unwrap();
        x.to_string()
    }

    fn read_number(&mut self) -> u32 {
        let mut n = 0;
        for b in self.take_while(|c| c.is_ascii_digit()).iter().copied() {
            n = n * 10 + (b - b'0') as u32;
        }
        n
    }

    fn expect(&mut self, b: u8) -> Result<(), ReaderError> {
        if self.try_consume(&[b]) {
            Ok(())
        } else {
            Err(ReaderError::InvalidChar)
        }
    }

    fn dbg(&self) {
        println!("!!! {:?}", std::str::from_utf8(&self.buf[self.pos..]));
    }
}

#[derive(Debug)]
enum ReaderError {
    InvalidFloat,
    InvalidChar,
}

#[derive(Debug)]
#[allow(unused)]
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
    Color(Option<u32>, Color),
    Alpha(Option<u32>, u8),
    Pos(f32, f32),
    DrawScale(f32),
    Clip(Vec<DrawCommand>),
    ClipRect(f32, f32, f32, f32),
    Bold(bool),
    Shadow(f32),
    XShadow(f32),
    YShadow(f32),
    WrappingStyle(u32),
    Reset,
    Transition { t1: Option<f32>, t2: Option<f32>, accel: Option<f32>, style: Vec<Code> },
    Fade { t1: f32, t2: f32 },
    Move { x1: f32, y1: f32, x2: f32, y2: f32, t1: f32, t2: f32 },
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

#[track_caller]
fn read_n(reader: &mut Reader) -> Result<(f32, f32), ReaderError> {
    let x = reader.read_float()?;
    reader.take_while(|c| c.is_ascii_whitespace());
    let y = reader.read_float()?;
    reader.take_while(|c| c.is_ascii_whitespace());
    Ok((x, y))
}

fn parse_args<'a>(reader: &mut Reader<'a>) -> Result<Vec<&'a str>, ReaderError> {
    reader.expect(b'(')?;
    let args: Vec<&'a str> = reader
        .take_while(|b| b != b')')
        .split(|b| *b == b',')
        .map(|it| std::str::from_utf8(it).unwrap())
        .collect();
    reader.expect(b')')?;
    Ok(args)
}

fn parse_curve(reader: &mut Reader) -> Result<Vec<DrawCommand>, ReaderError> {
    let mut cmds = Vec::new();
    while let Some(c) = reader.peek() {
        match c {
            b'm' => {
                reader.expect(b'm')?;
                reader.take_while(|c| c.is_ascii_whitespace());
                let (x, y) = read_n(reader)?;
                cmds.push(DrawCommand::CloseAndMove(x, y));
            }
            b'n' => {
                reader.expect(b'n')?;
                reader.take_while(|c| c.is_ascii_whitespace());
                let (x, y) = read_n(reader)?;
                cmds.push(DrawCommand::Move(x, y));
            }
            b'l' => {
                reader.expect(b'l')?;
                reader.take_while(|c| c.is_ascii_whitespace());

                while reader.peek().map_or(false, |c| c.is_ascii_digit()) {
                    let (x, y) = read_n(reader)?;
                    cmds.push(DrawCommand::Line(x, y));
                }
            }
            b'b' => {
                reader.expect(b'b')?;
                reader.take_while(|c| c.is_ascii_whitespace());
                let p1 = read_n(reader)?;
                reader.take_while(|c| c.is_ascii_whitespace());
                let p2 = read_n(reader)?;
                reader.take_while(|c| c.is_ascii_whitespace());
                let p3 = read_n(reader)?;

                cmds.push(DrawCommand::Bezier([p1, p2, p3]));
            }
            _ => break,
        }
    }
    Ok(cmds)
}

fn parse_override(reader: &mut Reader) -> Result<Code, ReaderError> {
    reader.expect(b'\\')?;
    Ok(if reader.try_consume(b"an") {
        Code::Align(Alignment(reader.read_number() as u8))
    } else if reader.try_consume(b"blur") {
        Code::Blur(reader.read_float()?)
    } else if reader.try_consume(b"bord") {
        Code::Border(reader.read_float()?)
    } else if reader.try_consume(b"xbord") {
        Code::XBorder(reader.read_float()?)
    } else if reader.try_consume(b"ybord") {
        Code::YBorder(reader.read_float()?)
    } else if reader.try_consume(b"fn") {
        Code::FontName(reader.read_string())
    } else if reader.try_consume(b"fscx") {
        Code::FontScaleX(reader.read_float()?)
    } else if reader.try_consume(b"fscy") {
        Code::FontScaleY(reader.read_float()?)
    } else if reader.try_consume(b"fs") {
        Code::FontSize(reader.read_float()?)
    } else if reader.try_consume(b"frx") {
        Code::RotateX(reader.read_float()?)
    } else if reader.try_consume(b"fry") {
        Code::RotateY(reader.read_float()?)
    } else if reader.try_consume(b"frz") {
        Code::RotateZ(reader.read_float()?)
    } else if reader.try_consume(b"fax") {
        Code::RotateZ(reader.read_float()?)
    } else if reader.try_consume(b"clip") {
        let args = parse_args(reader)?;
        match args.len() {
            1 => {
                let cmds = parse_curve(reader)?;
                Code::Clip(cmds)
            }
            4 => {
                let x1 = args[0].parse().unwrap();
                let y1 = args[1].parse().unwrap();
                let x2 = args[2].parse().unwrap();
                let y2 = args[3].parse().unwrap();
                Code::ClipRect(x1, y1, x2, y2)
            }
            _ => todo!("{:?}", args)
        }
    } else if reader.try_consume(b"pos") {
        reader.consume();
        let x = reader.read_float()?;
        reader.consume();
        let y = reader.read_float()?;
        reader.consume();

        Code::Pos(x, y)
    } else if reader.try_consume(b"p") {
        Code::DrawScale(reader.read_float()?)
    } else if reader.try_consume(b"b") {
        Code::Bold(reader.consume().unwrap() == b'1')
    } else if reader.try_consume(b"shad") {
        Code::Shadow(reader.read_float()?)
    } else if reader.try_consume(b"t") {
        let args = parse_args(reader)?;
        match args.len() {
            4 => {
                let t1: f32 = args[0].parse().unwrap();
                let t2: f32 = args[1].parse().unwrap();
                let accel: f32 = args[2].parse().unwrap();
                let style = read_style(args[3].as_bytes())?;
                Code::Transition { t1: Some(t1), t2: Some(t2), accel: Some(accel), style }
            }
            3 => {
                let t1: f32 = args[0].parse().unwrap();
                let t2: f32 = args[1].parse().unwrap();
                let style = read_style(args[2].as_bytes())?;
                Code::Transition { t1: Some(t1), t2: Some(t2), accel: None, style }
            }
            1 => {
                let style = read_style(args[0].as_bytes())?;
                Code::Transition { t1: None, t2: None, accel: None, style }
            }
            _ => unimplemented!("t: {:?}", args),
        }
    } else if reader.try_consume(b"fad") {
        let args = parse_args(reader)?;
        match args.len() {
            2 => {
                let t1: f32 = args[0].parse().unwrap();
                let t2: f32 = args[0].parse().unwrap();
                Code::Fade { t1, t2 }
            }
            _ => todo!("{:?}", args),
        }
    } else if reader.try_consume(b"move") {
        let args = parse_args(reader)?;
        match args.len() {
            6 => {
                let x1 = args[0].parse().unwrap();
                let y1 = args[1].parse().unwrap();
                let x2 = args[2].parse().unwrap();
                let y2 = args[3].parse().unwrap();
                let t1 = args[4].parse().unwrap();
                let t2 = args[4].parse().unwrap();

                Code::Move { x1, y1, x2, y2, t1, t2 }
            }
            _ => todo!("{:?}", args)
        }
    } else if reader.try_consume(b"xshad") {
        Code::XShadow(reader.read_float()?)
    } else if reader.try_consume(b"yshad") {
        Code::YShadow(reader.read_float()?)
    } else if reader.try_consume(b"q") {
        Code::WrappingStyle(reader.read_number())
    } else if reader.try_consume(b"r") {
        Code::Reset
    } else if reader.try_consume(b"c") {
        Code::Color(None, parse_color(reader)?)
    } else if reader.try_consume(b"alpha") {
        Code::Alpha(None, parse_alpha(reader)?)
    } else if let Some(b'0'..=b'9') = reader.peek() {
        let n = reader.read_number();
        match reader.consume() {
            Some(b'c') => Code::Color(Some(n), parse_color(reader)?),
            Some(b'a') => Code::Alpha(Some(n), parse_alpha(reader)?),
            oth => todo!("{:?}", oth),
        }
    } else {
        reader.dbg();
        return Err(ReaderError::InvalidChar);
    })
}

fn parse_color(reader: &mut Reader) -> Result<Color, ReaderError> {
    reader.expect(b'&')?;
    reader.expect(b'H')?;
    let hex = reader.take_while(|c| c.is_ascii_hexdigit());
    reader.expect(b'&')?;
    Ok(std::str::from_utf8(hex).unwrap().parse().unwrap())
}

fn parse_alpha(reader: &mut Reader) -> Result<u8, ReaderError> {
    reader.expect(b'&')?;
    reader.expect(b'H')?;
    let hex = reader.take_while(|c| c.is_ascii_hexdigit());
    reader.expect(b'&')?;
    Ok(u8::from_str_radix(std::str::from_utf8(hex).unwrap(), 16).unwrap())
}

fn read_style(s: &[u8]) -> Result<Vec<Code>, ReaderError> {
    let mut reader = Reader::new(s);
    parse_overrides(&mut reader)
}

fn parse_overrides(reader: &mut Reader) -> Result<Vec<Code>, ReaderError> {
    let mut items = Vec::new();
    while let Some(x) = reader.peek() {
        match x {
            b'\\' => {
                items.push(parse_override(reader)?);
            }
            b'}' => {
                break;
            }
            _ => {
                let _comment = reader.take_while(|c| !matches!(c, b'\\' | b'}'));
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

fn parse(s: &[u8]) -> Result<Vec<Part>, ReaderError> {
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
                let codes = parse_overrides(&mut reader)?;
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