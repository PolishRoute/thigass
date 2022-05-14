#![feature(derive_default_enum)]
#![feature(let_else)]

use std::fmt;
use std::num::{ParseFloatError, ParseIntError};
use std::str::FromStr;

use fxhash::FxBuildHasher;
use indexmap::IndexMap;

pub struct Timestamp {
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
pub enum TimestampParseError {
    MissingHours,
    MissingMinutes,
    MissingSeconds,
    MissingHundredths,
    InvalidInt(ParseIntError),
}

impl From<ParseIntError> for TimestampParseError {
    fn from(e: ParseIntError) -> Self {
        TimestampParseError::InvalidInt(e)
    }
}

impl FromStr for Timestamp {
    type Err = TimestampParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.splitn(3, ':');
        let hours: u64 = parts.next().ok_or_else(|| TimestampParseError::MissingHours)?.parse()?;
        let minutes: u64 = parts.next().ok_or_else(|| TimestampParseError::MissingMinutes)?.parse()?;
        let (seconds, hundredths) = parts.next()
            .ok_or_else(|| TimestampParseError::MissingSeconds)?
            .split_once('.')
            .ok_or_else(|| TimestampParseError::MissingHundredths)?;
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
pub struct Event<'s> {
    pub marked: bool,
    pub layer: i32,
    pub start: Timestamp,
    pub end: Timestamp,
    pub style: &'s str,
    pub name: &'s str,
    pub margin_l: Option<u32>,
    pub margin_r: Option<u32>,
    pub margin_v: Option<u32>,
    pub effect: &'s str,
    pub text: &'s str,
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
    ScriptInfo,
    V4Styles,
}

#[derive(Debug, Default)]
pub struct ScriptInfo {
    pub wrap_style: WrapStyle,
    pub scaled_border_and_shadow: bool,
    pub title: String,
    pub play_res_x: u32,
    pub play_res_y: u32,
    pub script_type: String,
    pub ycb_cr_matrix: String,
    pub original_translation: String,
    pub last_style_storage: String,
    pub audio_file: String,
    pub video_file: String,
    pub video_ar_value: f32,
    pub video_zoom_percent: f32,
    pub active_line: u32,
    pub video_position: u32,
}

#[derive(Debug, Default)]
pub enum WrapStyle {
    #[default]
    Smart,
    EndOfLine,
    NoWordWrapping,
    SmartWithWiderLowerLine,
}

impl FromStr for WrapStyle {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" => Ok(Self::Smart),
            "1" => Ok(Self::EndOfLine),
            "2" => Ok(Self::NoWordWrapping),
            "3" => Ok(Self::SmartWithWiderLowerLine),
            _ => Err(())
        }
    }
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
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

#[derive(Debug)]
pub enum ColorParseError {
    InvalidInt(ParseIntError),
    UnsupportedLength,
}

impl FromStr for Color {
    type Err = ColorParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.strip_prefix("&H").unwrap_or(s);

        fn parse_hex(s: &str) -> Result<u8, ColorParseError> {
            u8::from_str_radix(&s, 16)
                .map_err(ColorParseError::InvalidInt)
        }

        Ok(match s.len() {
            8 => Color {
                b: parse_hex(&s[0..2])?,
                g: parse_hex(&s[2..4])?,
                r: parse_hex(&s[4..6])?,
                a: parse_hex(&s[6..8])?,
            },
            6 => Color {
                b: parse_hex(&s[0..2])?,
                g: parse_hex(&s[2..4])?,
                r: parse_hex(&s[4..6])?,
                a: 0,
            },
            4 => Color {
                b: parse_hex(&s[0..2])?,
                g: parse_hex(&s[2..4])?,
                r: 0,
                a: 0,
            },
            2 => Color {
                b: parse_hex(&s[0..2])?,
                g: 0,
                r: 0,
                a: 0,
            },
            _ => return Err(ColorParseError::UnsupportedLength),
        })
    }
}

#[derive(Debug)]
pub struct Style {
    pub name: String,
    pub font_name: String,
    pub font_size: f32,
    pub primary_colour: Color,
    pub secondary_colour: Color,
    pub outline_colour: Color,
    pub back_colour: Color,
    pub bold: bool,
    pub italic: bool,
    pub underline: bool,
    pub strike_out: bool,
    pub scale_x: f32,
    pub scale_y: f32,
    pub spacing: f32,
    pub angle: f32,
    pub border_style: (),
    pub outline: f32,
    pub shadow: f32,
    pub alignment: Alignment,
    pub margin_l: f32,
    pub margin_r: f32,
    pub margin_v: f32,
    pub encoding: (),
}

fn parse_styles_mapping(s: &str) -> Option<IndexMap<StyleField, usize, FxBuildHasher>> {
    let mut mapping = IndexMap::with_hasher(FxBuildHasher::default());
    for (idx, s) in s.split(", ").enumerate() {
        let key = match s.trim() {
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
            _ => unimplemented!("{}", s),
        };
        mapping.insert(key, idx);
    }

    Some(mapping)
}

pub struct ScriptParser<'s> {
    script: &'s str,
    pos: usize,
    events_mapping: Option<IndexMap<EventField, usize, FxBuildHasher>>,
    styles_mapping: Option<IndexMap<StyleField, usize, FxBuildHasher>>,
    line_number: usize,
}

#[derive(Debug)]
pub struct Script<'s> {
    pub info: ScriptInfo,
    pub styles: Vec<Style>,
    pub events: Vec<(EventType, Event<'s>)>,
}

impl<'s> ScriptParser<'s> {
    pub fn new(script: &'s str) -> Self {
        // TODO: BOM mark -- handle exotic encodings
        let script = script.strip_prefix('\u{feff}')
            .unwrap_or(script);

        ScriptParser {
            script,
            pos: 0,
            events_mapping: None,
            styles_mapping: None,
            line_number: 1,
        }
    }

    fn next_line(&mut self) -> Option<&'s str> {
        if self.pos >= self.script.len() {
            return None;
        }

        let mut chars = self.script[self.pos..].chars().peekable();

        let mut line_len = 0;
        let mut ending_len = 0;
        loop {
            match (chars.next(), chars.peek()) {
                (Some('\r'), Some('\n')) => {
                    // DOS
                    chars.next(); // Skip LF
                    ending_len = 2;
                    break;
                }
                (Some('\n'), _) => {
                    // Unix
                    ending_len = 1;
                    break;
                }
                (Some(ch), _) => {
                    line_len += ch.len_utf8();
                }
                (None, _) => break,
            }
        }

        let line = &self.script[self.pos..][..line_len];
        self.pos += line_len + ending_len;
        self.line_number += 1;
        Some(line)
    }

    pub fn parse(&mut self) -> Script<'s> {
        let mut script = Script {
            styles: Vec::new(),
            events: Vec::new(),
            info: ScriptInfo::default(),
        };

        let mut current_section = None;

        while let Some(line) = self.next_line() {
            if let Some(section) = line.strip_prefix("[") {
                // Found a section eg. `[Events]`
                current_section = match section.strip_suffix(']').unwrap() {
                    "Events" => Some(Section::Events),
                    "Script Info" => Some(Section::ScriptInfo),
                    "V4+ Styles" => Some(Section::V4Styles),
                    _ => None,
                };
            } else if let Some(format) = line.strip_prefix("Format: ") {
                // Found list of columns provided in lines below.

                let Some(section) = current_section.as_ref() else {
                    // Ignore when not in section
                    continue;
                };

                match section {
                    Section::Events => self.events_mapping = parse_events_mapping(format),
                    Section::V4Styles => self.styles_mapping = parse_styles_mapping(format),
                    _ => todo!(),
                }
            } else if let Some(x) = line.strip_prefix("Dialogue: ") {
                script.events.push((EventType::Dialogue, self.parse_event(x)));
            } else if let Some(x) = line.strip_prefix("Comment: ") {
                script.events.push((EventType::Comment, self.parse_event(x)));
            } else if let Some(s) = line.strip_prefix("Style: ") {
                let style = self.parse_style(s);
                script.styles.push(style);
            } else if let Some((name, value)) = line.split_once(": ") {
                macro_rules! parse_or_skip {
                    ($s:expr) => {
                        match $s.parse() {
                            Ok(value) => value,
                            Err(err) => {
                                println!("Invalid value {:?}", err);
                                continue;
                            }
                        }
                    };
                }

                if current_section != Some(Section::ScriptInfo) {
                    // Not supported section
                    continue;
                }

                match name {
                    "WrapStyle" => script.info.wrap_style = parse_or_skip!(value),
                    "ScaledBorderAndShadow" => script.info.scaled_border_and_shadow = value == "yes",
                    "Title" => script.info.title = value.to_string(),
                    "PlayResX" => script.info.play_res_x = parse_or_skip!(value),
                    "PlayResY" => script.info.play_res_y = parse_or_skip!(value),
                    "ScriptType" => script.info.script_type = parse_or_skip!(value),
                    "YCbCr Matrix" => script.info.ycb_cr_matrix = parse_or_skip!(value),
                    "Original Translation" => script.info.original_translation = parse_or_skip!(value),
                    "Last Style Storage" => script.info.last_style_storage = parse_or_skip!(value),
                    "Audio File" => script.info.audio_file = parse_or_skip!(value),
                    "Video File" => script.info.video_file = parse_or_skip!(value),
                    "Video AR Value" => script.info.video_ar_value = parse_or_skip!(value),
                    "Video Zoom Percent" => script.info.video_zoom_percent = parse_or_skip!(value),
                    "Active Line" => script.info.active_line = parse_or_skip!(value),
                    "Video Position" => script.info.video_position = parse_or_skip!(value),
                    _ => {
                        println!("{}: {}", name, value);
                    }
                }
            } else if line.is_empty() || line.starts_with(";") {
                continue;
            } else {
                println!(">> {:?}", line);
            }
        }

        script
    }

    fn parse_style(&mut self, s: &str) -> Style {
        let mapping = self.styles_mapping.as_ref().unwrap();
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
            border_style: {
                println!("BorderStyle: {}", &fields[mapping[&StyleField::BorderStyle]]);
            },
            outline: fields[mapping[&StyleField::Outline]].parse().unwrap(),
            shadow: fields[mapping[&StyleField::Shadow]].parse().unwrap(),
            alignment: fields[mapping[&StyleField::Alignment]].parse().unwrap(),
            margin_l: fields[mapping[&StyleField::MarginL]].parse().unwrap(),
            margin_r: fields[mapping[&StyleField::MarginR]].parse().unwrap(),
            margin_v: fields[mapping[&StyleField::MarginV]].parse().unwrap(),
            encoding: {
                println!("Encoding: {}", &fields[mapping[&StyleField::Encoding]]);
            },
        };

        style
    }

    fn parse_event(&mut self, data: &'s str) -> Event<'s> {
        let mapping = self.events_mapping.as_ref().unwrap();
        let fields = data.splitn(mapping.len(), ',').collect::<Vec<_>>();
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
        event
    }
}


#[derive(Debug)]
pub enum EventType {
    Dialogue,
    Comment,
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

    #[must_use]
    fn consume(&mut self) -> Option<u8> {
        let x = self.buf.get(self.pos).copied()?;
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

    fn ignore_whitespace(&mut self) {
        self.take_while(|b| b.is_ascii_whitespace());
    }

    fn read_float(&mut self) -> Result<f32, ReaderError> {
        let x = self.take_while(|c| matches!(c, b'0'..=b'9' | b'.' | b'-'));
        let x = std::str::from_utf8(x).unwrap();
        Ok(x.parse()?)
    }

    fn read_string(&mut self) -> String {
        let x = self.take_while(|c| c != b'\\');
        let x = std::str::from_utf8(x).unwrap();
        x.to_string()
    }

    fn read_integer(&mut self) -> u32 {
        let mut n = 0;
        for b in self.take_while(|c| c.is_ascii_digit()).iter().copied() {
            n = n * 10 + (b - b'0') as u32;
        }
        n
    }

    fn read_bool(&mut self) -> Result<bool, ReaderError> {
        match self.consume() {
            Some(b'0') => Ok(false),
            Some(b'1') => Ok(true),
            _ => Err(ReaderError::InvalidBool),
        }
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
pub enum ReaderError {
    InvalidFloat,
    InvalidInt,
    InvalidChar,
    InvalidBool,
}

#[derive(Debug)]
pub enum Effect {
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
    Color { index: Option<u32>, color: Color },
    Alpha { index: Option<u32>, value: u8 },
    Pos(f32, f32),
    DrawScale(f32),
    Clip { mask: Vec<DrawCommand> },
    ClipRect(f32, f32, f32, f32),
    Bold(bool),
    Italic(bool),
    Shadow(f32),
    XShadow(f32),
    YShadow(f32),
    WrappingStyle(u32),
    Reset,
    Transition { t1: Option<f32>, t2: Option<f32>, accel: Option<f32>, style: Vec<Effect> },
    Fade { t1: f32, t2: f32 },
    Move { x1: f32, y1: f32, x2: f32, y2: f32, t1: f32, t2: f32 },
    NewLine { smart_wrapping: bool },
}

#[derive(Debug)]
pub struct Alignment(u8);

impl FromStr for Alignment {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value = s.parse().map_err(|_| ())?;
        Ok(Alignment(value))
    }
}

#[derive(Debug)]
pub enum DrawCommand {
    CloseAndMove(f32, f32),
    Move(f32, f32),
    Line(f32, f32),
    Bezier([(f32, f32); 3]),
}

fn read_point(reader: &mut Reader) -> Result<(f32, f32), ReaderError> {
    let x = reader.read_float()?;
    reader.ignore_whitespace();
    let y = reader.read_float()?;
    reader.ignore_whitespace();
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
                reader.ignore_whitespace();
                let (x, y) = read_point(reader)?;
                cmds.push(DrawCommand::CloseAndMove(x, y));
            }
            b'n' => {
                reader.expect(b'n')?;
                reader.ignore_whitespace();
                let (x, y) = read_point(reader)?;
                cmds.push(DrawCommand::Move(x, y));
            }
            b'l' => {
                reader.expect(b'l')?;
                reader.ignore_whitespace();

                while reader.peek().map_or(false, |c| c.is_ascii_digit()) {
                    let (x, y) = read_point(reader)?;
                    cmds.push(DrawCommand::Line(x, y));
                }
            }
            b'b' => {
                reader.expect(b'b')?;
                reader.ignore_whitespace();
                let p1 = read_point(reader)?;
                reader.ignore_whitespace();
                let p2 = read_point(reader)?;
                reader.ignore_whitespace();
                let p3 = read_point(reader)?;

                cmds.push(DrawCommand::Bezier([p1, p2, p3]));
            }
            _ => break,
        }
    }
    Ok(cmds)
}

impl From<ParseIntError> for ReaderError {
    fn from(_: ParseIntError) -> Self {
        ReaderError::InvalidInt
    }
}


impl From<ParseFloatError> for ReaderError {
    fn from(_: ParseFloatError) -> Self {
        ReaderError::InvalidFloat
    }
}

fn parse_effect(reader: &mut Reader) -> Result<Effect, ReaderError> {
    reader.expect(b'\\')?;
    Ok(if reader.try_consume(b"n") {
        Effect::NewLine { smart_wrapping: false }
    } else if reader.try_consume(b"N'") {
        Effect::NewLine { smart_wrapping: true }
    } else if reader.try_consume(b"an") {
        Effect::Align(Alignment(reader.read_integer() as u8))
    } else if reader.try_consume(b"blur") {
        Effect::Blur(reader.read_float()?)
    } else if reader.try_consume(b"bord") {
        Effect::Border(reader.read_float()?)
    } else if reader.try_consume(b"xbord") {
        Effect::XBorder(reader.read_float()?)
    } else if reader.try_consume(b"ybord") {
        Effect::YBorder(reader.read_float()?)
    } else if reader.try_consume(b"fn") {
        Effect::FontName(reader.read_string())
    } else if reader.try_consume(b"fscx") {
        Effect::FontScaleX(reader.read_float()?)
    } else if reader.try_consume(b"fscy") {
        Effect::FontScaleY(reader.read_float()?)
    } else if reader.try_consume(b"fs") {
        Effect::FontSize(reader.read_float()?)
    } else if reader.try_consume(b"frx") {
        Effect::RotateX(reader.read_float()?)
    } else if reader.try_consume(b"fry") {
        Effect::RotateY(reader.read_float()?)
    } else if reader.try_consume(b"frz") {
        Effect::RotateZ(reader.read_float()?)
    } else if reader.try_consume(b"fax") {
        Effect::RotateZ(reader.read_float()?)
    } else if reader.try_consume(b"clip") {
        let args = parse_args(reader)?;
        match args[..] {
            [curve] => {
                let mut reader = Reader::new(curve.as_bytes());
                let cmds = parse_curve(&mut reader)?;
                Effect::Clip { mask: cmds }
            }
            [x1, y1, x2, y2] => Effect::ClipRect(
                x1.parse()?,
                y1.parse()?,
                x2.parse()?,
                y2.parse()?,
            ),
            _ => todo!("{:?}", args)
        }
    } else if reader.try_consume(b"pos") {
        let args = parse_args(reader)?;
        match args[..] {
            [x, y] => Effect::Pos(
                x.parse()?,
                y.parse()?,
            ),
            _ => todo!("{:?}", args)
        }
    } else if reader.try_consume(b"i") {
        Effect::Italic(reader.read_bool()?)
    } else if reader.try_consume(b"p") {
        Effect::DrawScale(reader.read_float()?)
    } else if reader.try_consume(b"b") {
        Effect::Bold(reader.read_bool()?)
    } else if reader.try_consume(b"shad") {
        Effect::Shadow(reader.read_float()?)
    } else if reader.try_consume(b"t") {
        let args = parse_args(reader)?;
        match args[..] {
            [t1, t2, accel, style] => Effect::Transition {
                t1: Some(t1.parse()?),
                t2: Some(t2.parse()?),
                accel: Some(accel.parse()?),
                style: read_style(style.as_bytes())?,
            },
            [t1, t2, style] => Effect::Transition {
                t1: Some(t1.parse()?),
                t2: Some(t2.parse()?),
                accel: None,
                style: read_style(style.as_bytes())?,
            },
            [style] => Effect::Transition {
                t1: None,
                t2: None,
                accel: None,
                style: read_style(style.as_bytes())?,
            },
            _ => unimplemented!("t: {:?}", args),
        }
    } else if reader.try_consume(b"fad") {
        let args = parse_args(reader)?;
        match args[..] {
            [t1, t2] => Effect::Fade {
                t1: t1.parse()?,
                t2: t2.parse()?,
            },
            _ => todo!("{:?}", args),
        }
    } else if reader.try_consume(b"move") {
        let args = parse_args(reader)?;
        match args[..] {
            [x1, y1, x2, y2, t1, t2] => Effect::Move {
                x1: x1.parse()?,
                y1: y1.parse()?,
                x2: x2.parse()?,
                y2: y2.parse()?,
                t1: t1.parse()?,
                t2: t2.parse()?,
            },
            _ => todo!("{:?}", args)
        }
    } else if reader.try_consume(b"xshad") {
        Effect::XShadow(reader.read_float()?)
    } else if reader.try_consume(b"yshad") {
        Effect::YShadow(reader.read_float()?)
    } else if reader.try_consume(b"q") {
        Effect::WrappingStyle(reader.read_integer())
    } else if reader.try_consume(b"r") {
        Effect::Reset
    } else if reader.try_consume(b"c") {
        Effect::Color {
            index: None,
            color: parse_color(reader)?
        }
    } else if reader.try_consume(b"alpha") {
        Effect::Alpha {
            index: None,
            value: parse_alpha(reader)?
        }
    } else if let Some(b'0'..=b'9') = reader.peek() {
        let n = reader.read_integer();
        match reader.consume() {
            Some(b'c') => Effect::Color {
                index: Some(n),
                color: parse_color(reader)?
            },
            Some(b'a') => Effect::Alpha {
                index: Some(n),
                value: parse_alpha(reader)?
            },
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

fn read_style(s: &[u8]) -> Result<Vec<Effect>, ReaderError> {
    let mut reader = Reader::new(s);
    parse_overrides(&mut reader)
}

fn parse_overrides(reader: &mut Reader) -> Result<Vec<Effect>, ReaderError> {
    let mut items = Vec::new();
    while let Some(x) = reader.peek() {
        match x {
            b'\\' => {
                items.push(parse_effect(reader)?);
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
pub enum Part {
    Text(String),
    Overrides(Vec<Effect>),
    NewLine { smart_wrapping: bool },
}

struct PartsBuilder {
    parts: Vec<Part>,
    buf: Vec<u8>,
}

impl PartsBuilder {
    fn new() -> Self {
        Self {
            parts: Vec::new(),
            buf: Vec::new(),
        }
    }

    fn finalize_buf(&mut self) {
        if !self.buf.is_empty() {
            let text = String::from_utf8_lossy(&self.buf).into_owned();
            self.parts.push(Part::Text(text));
            self.buf.clear();
        }
    }

    fn push_part(&mut self, part: Part) {
        self.finalize_buf();
        self.parts.push(part);
    }

    fn push_byte(&mut self, byte: u8) {
        self.buf.push(byte);
    }

    fn into_parts(mut self) -> Vec<Part> {
        self.finalize_buf();
        self.parts
    }
}

pub fn parse(s: &[u8]) -> Result<Vec<Part>, ReaderError> {
    let mut builder = PartsBuilder::new();
    let mut reader = Reader::new(s);
    while let Some(x) = reader.peek() {
        match x {
            b'{' => {
                reader.expect(b'{')?;
                let effects = parse_overrides(&mut reader)?;
                reader.expect(b'}')?;
                if !effects.is_empty() {
                    builder.push_part(Part::Overrides(effects));
                }
            }
            b'\\' => {
                reader.expect(b'\\')?;
                match reader.consume() {
                    Some(b'n') => builder.push_part(Part::NewLine { smart_wrapping: false }),
                    Some(b'N') => builder.push_part(Part::NewLine { smart_wrapping: true }),
                    _ => {
                        // Ignore
                    }
                }
            }
            _ => {
                builder.push_byte(reader.consume().unwrap());
            }
        }
    }
    Ok(builder.into_parts())
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