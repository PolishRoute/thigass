#![feature(let_else)]
#![feature(never_type)]
#![feature(slice_as_chunks)]
#![feature(specialization)]
#![deny(unsafe_code)]
#![allow(incomplete_features)]
#![allow(clippy::unnecessary_lazy_evaluations)]
#![allow(clippy::len_zero)]
#![allow(clippy::struct_excessive_bools)]
#![allow(clippy::trivially_copy_pass_by_ref)]
#![allow(clippy::redundant_closure_for_method_calls)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::too_many_lines)]

use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use bstr::{BStr, ByteSlice};
use enum_map::{Enum, enum_map, EnumArray, EnumMap};
use tinyvec::ArrayVec;

pub struct Timestamp {
    value: u64,
}

impl fmt::Debug for Timestamp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{:02}:{:02}.{:02}",
               self.value / 360_000,
               (self.value / 6_000) % 60,
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
        Ok(Timestamp { value: hours * 360_000u64 + minutes * 6_000u64 + seconds * 100u64 + hundredths })
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
    pub style: &'s BStr,
    pub name: &'s BStr,
    pub margin_l: Option<u32>,
    pub margin_r: Option<u32>,
    pub margin_v: Option<u32>,
    pub effect: &'s BStr,
    pub text: &'s BStr,
}

#[derive(Hash, Eq, PartialEq, Enum)]
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

fn parse_events_mapping(config: &BStr) -> FieldMapping<EventField> {
    let mut mapping = FieldMapping::empty();
    for (idx, field) in config.split_str(b", ").enumerate() {
        let key = match field {
            b"Layer" => EventField::Layer,
            b"Marked" => EventField::Marked,
            b"Start" => EventField::Start,
            b"End" => EventField::End,
            b"Style" => EventField::Style,
            b"Name" => EventField::Name,
            b"MarginL" => EventField::MarginL,
            b"MarginR" => EventField::MarginR,
            b"MarginV" => EventField::MarginV,
            b"Effect" => EventField::Effect,
            b"Text" => EventField::Text,
            _ => {
                println!("{}", field.as_bstr());
                continue;
            }
        };
        mapping.set(key, idx);
    }
    mapping
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

#[derive(Hash, Eq, PartialEq, Enum)]
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
    InvalidDigit,
    UnsupportedLength,
}

const fn parse_hex_digit(c: u8) -> Result<u8, ColorParseError> {
    match c {
        b'0'..=b'9' => Ok(c - b'0'),
        b'A'..=b'F' => Ok(c - b'A' + 10),
        b'a'..=b'f' => Ok(c - b'a' + 10),
        _ => Err(ColorParseError::InvalidDigit),
    }
}

fn parse_hex(s: &[u8; 2]) -> Result<u8, ColorParseError> {
    let a = parse_hex_digit(s[0])?;
    let b = parse_hex_digit(s[1])?;
    Ok(a * 16 + b)
}

#[inline(never)]
fn parse_hex_color(bytes: &[u8]) -> Result<Color, ColorParseError> {
    let bytes = bytes.strip_prefix(b"&H").unwrap_or(bytes);
    let (channels, rest) = bytes.as_chunks::<2>();
    if rest.len() != 0 {
        return Err(ColorParseError::UnsupportedLength);
    }

    Ok(match channels {
        [b, g, r, a] => Color {
            b: parse_hex(b)?,
            g: parse_hex(g)?,
            r: parse_hex(r)?,
            a: parse_hex(a)?,
        },
        [b, g, r] => Color {
            b: parse_hex(b)?,
            g: parse_hex(g)?,
            r: parse_hex(r)?,
            a: 0,
        },
        [b, g] => Color {
            b: parse_hex(b)?,
            g: parse_hex(g)?,
            r: 0,
            a: 0,
        },
        [b] => Color {
            b: parse_hex(b)?,
            g: 0,
            r: 0,
            a: 0,
        },
        _ => return Err(ColorParseError::UnsupportedLength),
    })
}

impl FromStr for Color {
    type Err = ColorParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_hex_color(s.as_bytes())
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
    pub border_style: u32,
    pub outline: f32,
    pub shadow: f32,
    pub alignment: Alignment,
    pub margin_l: f32,
    pub margin_r: f32,
    pub margin_v: f32,
    pub encoding: u32,
}

fn parse_styles_mapping(s: &BStr) -> FieldMapping<StyleField> {
    let mut mapping = FieldMapping::empty();
    for (idx, s) in s.split(|b| *b == b',').map(|it| it.trim()).enumerate() {
        let key = match s.trim() {
            b"Name" => StyleField::Name,
            b"Fontname" => StyleField::FontName,
            b"Fontsize" => StyleField::FontSize,
            b"PrimaryColour" => StyleField::PrimaryColour,
            b"SecondaryColour" => StyleField::SecondaryColour,
            b"OutlineColour" => StyleField::OutlineColour,
            b"BackColour" => StyleField::BackColour,
            b"Bold" => StyleField::Bold,
            b"Italic" => StyleField::Italic,
            b"Underline" => StyleField::Underline,
            b"StrikeOut" => StyleField::StrikeOut,
            b"ScaleX" => StyleField::ScaleX,
            b"ScaleY" => StyleField::ScaleY,
            b"Spacing" => StyleField::Spacing,
            b"Angle" => StyleField::Angle,
            b"BorderStyle" => StyleField::BorderStyle,
            b"Outline" => StyleField::Outline,
            b"Shadow" => StyleField::Shadow,
            b"Alignment" => StyleField::Alignment,
            b"MarginL" => StyleField::MarginL,
            b"MarginR" => StyleField::MarginR,
            b"MarginV" => StyleField::MarginV,
            b"Encoding" => StyleField::Encoding,
            _ => {
                println!("{}", s.as_bstr());
                continue;
            }
        };
        mapping.set(key, idx);
    }
    mapping
}

pub struct ScriptParser<'s> {
    script: &'s BStr,
    pos: usize,
    events_mapping: Option<FieldMapping<EventField>>,
    styles_mapping: Option<FieldMapping<StyleField>>,
    line_number: usize,
}

struct FieldMapping<T: EnumArray<usize>> {
    inner: EnumMap<T, usize>,
    inserted: usize,
}

impl<T: EnumArray<usize>> FieldMapping<T> {
    fn empty() -> Self {
        Self {
            inner: enum_map! { _ => usize::MAX },
            inserted: 0,
        }
    }

    fn len(&self) -> usize {
        self.inserted
    }

    fn set(&mut self, field: T, idx: usize) {
        self.inner[field] = idx;
        self.inserted += 1;
    }

    fn value_of<'s>(&self, field: T, values: &[&'s [u8]]) -> &'s [u8] {
        match self.inner[field] {
            usize::MAX => b"",
            other => values[other],
        }
    }

    fn value<U: FromBytes>(&self, field: T, values: &[&[u8]]) -> U {
        U::from_bytes(self.value_of(field, values)).map_err(drop).unwrap()
    }

    fn bool(&self, field: T, values: &[&[u8]]) -> bool {
        self.value_of(field, values) == b"1"
    }
}

trait FromBytes: Sized {
    type Err: fmt::Debug;

    fn from_bytes(bytes: &[u8]) -> Result<Self, Self::Err>;
}

impl<T: FromStr> FromBytes for T where <T as FromStr>::Err: fmt::Debug{
    default type Err = <T as FromStr>::Err;

    default fn from_bytes(bytes: &[u8]) -> Result<Self, <Self as FromBytes>::Err> {
        Ok(T::from_str(bytes.to_str().unwrap()).map_err(drop).unwrap())
    }
}

impl FromBytes for f32 {
    type Err = fast_float::Error;

    fn from_bytes(bytes: &[u8]) -> Result<Self, Self::Err> {
        fast_float::parse(bytes)
    }
}

impl FromBytes for Color {
    type Err = ColorParseError;

    fn from_bytes(bytes: &[u8]) -> Result<Self, Self::Err> {
        parse_hex_color(bytes)
    }
}

impl FromBytes for String {
    type Err = ();

    fn from_bytes(bytes: &[u8]) -> Result<Self, Self::Err> {
        Ok(bytes.to_str_lossy().into_owned())
    }
}

#[derive(Debug)]
pub struct Script<'s> {
    pub info: ScriptInfo,
    pub styles: Vec<Style>,
    pub events: Vec<(EventType, Event<'s>)>,
}

impl<'s> ScriptParser<'s> {
    #[must_use]
    pub fn new(script: &'s BStr) -> Self {
        // TODO: BOM mark -- handle exotic encodings
        let mut it = script.chars();
        let script = match it.next() {
            Some('\u{FEFF}') => it.as_bytes().as_bstr(),
            _ => script,
        };

        ScriptParser {
            script,
            pos: 0,
            events_mapping: None,
            styles_mapping: None,
            line_number: 1,
        }
    }

    #[inline(never)]
    fn next_line(&mut self) -> Option<&'s BStr> {
        if self.pos >= self.script.len() {
            return None;
        }

        let pos = memchr::memchr(b'\n', &self.script[self.pos..]);
        let line = match pos {
            Some(p) => &self.script[self.pos..][..p],
            None => &self.script[self.pos..],
        };
        let (line, ending_len) = match line.strip_suffix(b"\r") {
            Some(line) => (line.as_bstr(), 2),
            None => (line, 1),
        };
        self.pos += line.len() + ending_len;
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
            if let Some(section) = line.strip_prefix(b"[") {
                // Found a section eg. `[Events]`
                current_section = match section.strip_suffix(b"]") {
                    Some(b"Events") => Some(Section::Events),
                    Some(b"Script Info") => Some(Section::ScriptInfo),
                    Some(b"V4+ Styles") => Some(Section::V4Styles),
                    _ => {
                        println!("Unknown section: {}", section.as_bstr());
                        None
                    },
                };
            } else if let Some(format) = line.strip_prefix(b"Format: ") {
                // Found list of columns provided in lines below.

                let Some(section) = current_section.as_ref() else {
                    // Ignore when not in section
                    continue;
                };

                match section {
                    Section::Events => self.events_mapping = Some(parse_events_mapping(format.as_bstr())),
                    Section::V4Styles => self.styles_mapping = Some(parse_styles_mapping(format.as_bstr())),
                    Section::ScriptInfo => {
                        // Ignore when in a not supported section
                        continue;
                    },
                }
            } else if let Some(x) = line.strip_prefix(b"Dialogue: ") {
                script.events.push((EventType::Dialogue, self.parse_event(x.as_bstr())));
            } else if let Some(x) = line.strip_prefix(b"Comment: ") {
                script.events.push((EventType::Comment, self.parse_event(x.as_bstr())));
            } else if let Some(s) = line.strip_prefix(b"Style: ") {
                let style = self.parse_style(s.as_bstr());
                script.styles.push(style);
            } else if let Some(pos) = memchr::memchr(b':', line) {
                macro_rules! parse_or_skip {
                    ($s:expr) => {
                        match FromBytes::from_bytes($s) {
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

                let name = &line[..pos];
                let value = line[pos + 1..].trim_start();

                match name.as_bytes() {
                    b"WrapStyle" => script.info.wrap_style = parse_or_skip!(value),
                    b"ScaledBorderAndShadow" => script.info.scaled_border_and_shadow = value == b"yes",
                    b"Title" => script.info.title = value.to_str_lossy().into_owned(),
                    b"PlayResX" => script.info.play_res_x = parse_or_skip!(value),
                    b"PlayResY" => script.info.play_res_y = parse_or_skip!(value),
                    b"ScriptType" => script.info.script_type = parse_or_skip!(value),
                    b"YCbCr Matrix" => script.info.ycb_cr_matrix = parse_or_skip!(value),
                    b"Original Translation" => script.info.original_translation = parse_or_skip!(value),
                    b"Last Style Storage" => script.info.last_style_storage = parse_or_skip!(value),
                    b"Audio File" => script.info.audio_file = parse_or_skip!(value),
                    b"Video File" => script.info.video_file = parse_or_skip!(value),
                    b"Video AR Value" => script.info.video_ar_value = parse_or_skip!(value),
                    b"Video Zoom Percent" => script.info.video_zoom_percent = parse_or_skip!(value),
                    b"Active Line" => script.info.active_line = parse_or_skip!(value),
                    b"Video Position" => script.info.video_position = parse_or_skip!(value),
                    _ => {
                        println!("{}: {}", name, value.as_bstr());
                    }
                }
            } else if line.trim().is_empty() || line.starts_with(b";") {
                continue;
            } else {
                println!(">> {:?}", line);
            }
        }

        script
    }

    #[inline(never)]
    fn parse_style(&mut self, s: &'s BStr) -> Style {
        let mapping = self.styles_mapping.as_ref().unwrap();
        let fields: ArrayVec<[_; StyleField::LENGTH]> = s.splitn(mapping.len(), |b| *b == b',').collect();
        Style {
            name: mapping.value(StyleField::Name, &fields),
            font_name: mapping.value(StyleField::FontName, &fields),
            font_size: mapping.value(StyleField::FontSize, &fields),
            primary_colour: mapping.value(StyleField::PrimaryColour, &fields),
            secondary_colour: mapping.value(StyleField::SecondaryColour, &fields),
            outline_colour: mapping.value(StyleField::OutlineColour, &fields),
            back_colour: mapping.value(StyleField::BackColour, &fields),
            bold: mapping.bool(StyleField::Bold, &fields),
            italic: mapping.bool(StyleField::Italic, &fields),
            underline: mapping.bool(StyleField::Underline, &fields),
            strike_out: mapping.bool(StyleField::StrikeOut, &fields),
            scale_x: mapping.value(StyleField::ScaleX, &fields),
            scale_y: mapping.value(StyleField::ScaleY, &fields),
            spacing: mapping.value(StyleField::Spacing, &fields),
            angle: mapping.value(StyleField::Angle, &fields),
            border_style: mapping.value(StyleField::BorderStyle, &fields),
            outline: mapping.value(StyleField::Outline, &fields),
            shadow: mapping.value(StyleField::Shadow, &fields),
            alignment: mapping.value(StyleField::Alignment, &fields),
            margin_l: mapping.value(StyleField::MarginL, &fields),
            margin_r: mapping.value(StyleField::MarginR, &fields),
            margin_v: mapping.value(StyleField::MarginV, &fields),
            encoding: mapping.value(StyleField::Encoding, &fields),
        }
    }

    #[inline(never)]
    fn parse_event(&mut self, data: &'s BStr) -> Event<'s> {
        let mapping = self.events_mapping.as_ref().unwrap();
        let fields: ArrayVec<[_; EventField::LENGTH]> = data.splitn(mapping.len(), |b| *b == b',').collect();
        let event = Event {
            marked: mapping.bool(EventField::Marked, &fields),
            layer: mapping.value(EventField::Layer, &fields),
            start: mapping.value(EventField::Start, &fields),
            end: mapping.value(EventField::End, &fields),
            style: mapping.value_of(EventField::Style, &fields).as_bstr(),
            name: mapping.value_of(EventField::Name, &fields).as_bstr(),
            margin_l: Some(mapping.value(EventField::MarginL, &fields)),
            margin_r: Some(mapping.value(EventField::MarginR, &fields)),
            margin_v: Some(mapping.value(EventField::MarginV, &fields)),
            effect: mapping.value_of(EventField::Effect, &fields).as_bstr(),
            text: mapping.value_of(EventField::Text, &fields).as_bstr(),
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

    fn take_until(&mut self, term1: u8) -> &'d [u8] {
        let len = memchr::memchr(term1, &self.buf[self.pos..])
            .unwrap_or_else(|| self.buf.len() - self.pos);
        let slice = &self.buf[self.pos..][..len];
        self.pos += len;
        slice
    }

    fn take_until2(&mut self, term1: u8, term2: u8) -> &'d [u8] {
        let len = memchr::memchr2(term1, term2, &self.buf[self.pos..])
            .unwrap_or_else(|| self.buf.len() - self.pos);
        let slice = &self.buf[self.pos..][..len];
        self.pos += len;
        slice
    }

    fn expect_whitespace_or_end(&mut self) -> Result<(), ReaderError> {
        if self.peek().is_none() {
            return Ok(());
        }
        self.expect_whitespace()
    }

    fn expect_whitespace(&mut self) -> Result<(), ReaderError> {
        let ignored = self.take_while(|b| b.is_ascii_whitespace()).len();
        if ignored > 0 {
            Ok(())
        } else {
            Err(ReaderError::ExpectedWhitespace { pos: self.pos })
        }
    }

    fn read_float(&mut self) -> Result<f32, ReaderError> {
        let (value, len) = fast_float::parse_partial(&self.buf[self.pos..])
            .map_err(|_| ReaderError::InvalidFloat { pos: self.pos })?;
        self.pos += len;
        Ok(value)
    }

    fn read_str(&mut self) -> Result<&str, ReaderError> {
        let pos = self.pos;
        let s = self.take_until(b'\\');
        let s = std::str::from_utf8(s).map_err(|_| ReaderError::InvalidStr { pos })?;
        Ok(s)
    }

    fn read_integer(&mut self) -> Result<u32, ReaderError> {
        let pos = self.pos;
        let digits = self.take_while(|c| c.is_ascii_digit());
        if digits.len() == 0 {
            return Err(ReaderError::InvalidInt { pos });
        }

        let mut n = 0;
        for b in digits.iter().copied() {
            n = n * 10 + u32::from(b - b'0');
        }
        Ok(n)
    }

    fn read_bool(&mut self) -> Result<bool, ReaderError> {
        match self.consume() {
            Some(b'0') => Ok(false),
            Some(b'1') => Ok(true),
            _ => Err(ReaderError::InvalidBool { pos: self.pos - 1 }),
        }
    }

    fn expect(&mut self, b: u8) -> Result<(), ReaderError> {
        if self.try_consume(&[b]) {
            Ok(())
        } else {
            Err(ReaderError::InvalidChar { pos: self.pos, expected: b })
        }
    }

    #[allow(unused)]
    #[track_caller]
    fn dbg(&self) {
        println!("!!! {} {:?}", std::panic::Location::caller(), std::str::from_utf8(&self.buf[self.pos..]));
    }
}

#[derive(Debug)]
pub enum ReaderError {
    InvalidInt { pos: usize },
    InvalidFloat { pos: usize },
    InvalidChar { pos: usize, expected: u8 },
    InvalidBool { pos: usize },
    InvalidStr { pos: usize },
    ExpectedWhitespace { pos: usize },
}

#[derive(Debug)]
pub enum ParserError {
    ReaderError(ReaderError),
    UnsupportedEffect(String),
    InvalidCurveOpcode,
    MissingEffectName,
    UnsupportedOverload { args: usize },
}

impl From<ReaderError> for ParserError {
    fn from(e: ReaderError) -> Self {
        Self::ReaderError(e)
    }
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
    ShearingX(f32),
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
    reader.expect_whitespace_or_end()?;
    let y = reader.read_float()?;
    reader.expect_whitespace_or_end()?;
    Ok((x, y))
}

fn parse_args<'a>(reader: &mut Reader<'a>) -> Result<ArrayVec<[&'a BStr; 8]>, ReaderError> {
    reader.expect(b'(')?;
    let args = reader
        .take_until(b')')
        .split(|b| *b == b',')
        .map(|it| it.as_bstr())
        .collect();
    reader.expect(b')')?;
    Ok(args)
}

fn parse_draw_commands(reader: &mut Reader) -> Result<Vec<DrawCommand>, ParserError> {
    let mut commands = Vec::new();
    let mut last_opcode = None;
    while let Some(c) = reader.peek() {
        let opcode = match c {
            b'm' | b'n' | b'l' | b'b' => {
                reader.expect(c)?;
                reader.expect_whitespace()?;
                c
            }
            _ => {
                // if next opcode is the same as the last one it could be omitted
                last_opcode.ok_or(ParserError::InvalidCurveOpcode)?
            }
        };
        last_opcode = Some(opcode);

        match opcode {
            b'm' => {
                let (x, y) = read_point(reader)?;
                commands.push(DrawCommand::CloseAndMove(x, y));
            }
            b'n' => {
                let (x, y) = read_point(reader)?;
                commands.push(DrawCommand::Move(x, y));
            }
            b'l' => {
                while reader.peek().map_or(false, |c| c.is_ascii_digit() || c == b'-') {
                    let (x, y) = read_point(reader)?;
                    commands.push(DrawCommand::Line(x, y));
                }
            }
            b'b' => {
                let p1 = read_point(reader)?;
                let p2 = read_point(reader)?;
                let p3 = read_point(reader)?;

                commands.push(DrawCommand::Bezier([p1, p2, p3]));
            }
            _ => unreachable!(),
        }
    }
    Ok(commands)
}

pub fn parse_curve(s: &[u8]) -> Result<Vec<DrawCommand>, ParserError> {
    let mut reader = Reader::new(s);
    parse_draw_commands(&mut reader)
}

fn parse_float(s: &[u8]) -> Result<f32, ReaderError> {
    Reader::new(s).read_float()
}

fn unsupported_overload(args: &[&BStr]) -> Result<!, ParserError> {
    println!("Unsupported overload: {:?}", args);
    Err(ParserError::UnsupportedOverload { args: args.len() })
}

fn parse_effect(reader: &mut Reader) -> Result<Effect, ParserError> {
    reader.expect(b'\\')?;
    Ok(if reader.try_consume(b"n") {
        Effect::NewLine { smart_wrapping: false }
    } else if reader.try_consume(b"N") {
        Effect::NewLine { smart_wrapping: true }
    } else if reader.try_consume(b"an") {
        Effect::Align(Alignment(reader.read_integer()?.try_into().unwrap()))
    } else if reader.try_consume(b"blur") {
        Effect::Blur(reader.read_float()?)
    } else if reader.try_consume(b"bord") {
        Effect::Border(reader.read_float()?)
    } else if reader.try_consume(b"xbord") {
        Effect::XBorder(reader.read_float()?)
    } else if reader.try_consume(b"ybord") {
        Effect::YBorder(reader.read_float()?)
    } else if reader.try_consume(b"fn") {
        Effect::FontName(reader.read_str()?.into())
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
        Effect::ShearingX(reader.read_float()?)
    } else if reader.try_consume(b"clip") {
        let args = parse_args(reader)?;
        match args[..] {
            [curve] => {
                let mut reader = Reader::new(curve.as_bytes());
                let cmds = parse_draw_commands(&mut reader)?;
                Effect::Clip { mask: cmds }
            }
            [x1, y1, x2, y2] => Effect::ClipRect(
                parse_float(x1)?,
                parse_float(y1)?,
                parse_float(x2)?,
                parse_float(y2)?,
            ),
            _ => unsupported_overload(&args)?,
        }
    } else if reader.try_consume(b"pos") {
        let args = parse_args(reader)?;
        match args[..] {
            [x, y] => Effect::Pos(
                parse_float(x)?,
                parse_float(y)?,
            ),
            _ => unsupported_overload(&args)?,
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
                t1: Some(parse_float(t1)?),
                t2: Some(parse_float(t2)?),
                accel: Some(parse_float(accel)?),
                style: parse_style(style.as_bytes())?,
            },
            [t1, t2, style] => Effect::Transition {
                t1: Some(parse_float(t1)?),
                t2: Some(parse_float(t2)?),
                accel: None,
                style: parse_style(style.as_bytes())?,
            },
            [style] => Effect::Transition {
                t1: None,
                t2: None,
                accel: None,
                style: parse_style(style.as_bytes())?,
            },
            _ => unsupported_overload(&args)?,
        }
    } else if reader.try_consume(b"fad") {
        let args = parse_args(reader)?;
        match args[..] {
            [t1, t2] => Effect::Fade {
                t1: parse_float(t1)?,
                t2: parse_float(t2)?,
            },
            _ => unsupported_overload(&args)?,
        }
    } else if reader.try_consume(b"move") {
        let args = parse_args(reader)?;
        match args[..] {
            [x1, y1, x2, y2, t1, t2] => Effect::Move {
                x1: parse_float(x1)?,
                y1: parse_float(y1)?,
                x2: parse_float(x2)?,
                y2: parse_float(y2)?,
                t1: parse_float(t1)?,
                t2: parse_float(t2)?,
            },
            _ => unsupported_overload(&args)?,
        }
    } else if reader.try_consume(b"xshad") {
        Effect::XShadow(reader.read_float()?)
    } else if reader.try_consume(b"yshad") {
        Effect::YShadow(reader.read_float()?)
    } else if reader.try_consume(b"q") {
        Effect::WrappingStyle(reader.read_integer()?)
    } else if reader.try_consume(b"r") {
        Effect::Reset
    } else if reader.try_consume(b"c") {
        Effect::Color {
            index: None,
            color: parse_color(reader)?,
        }
    } else if reader.try_consume(b"alpha") {
        Effect::Alpha {
            index: None,
            value: parse_alpha(reader)?,
        }
    } else if let Some(b'0'..=b'9') = reader.peek() {
        let n = reader.read_integer()?;
        match reader.consume() {
            Some(b'c') => Effect::Color {
                index: Some(n),
                color: parse_color(reader)?,
            },
            Some(b'a') => Effect::Alpha {
                index: Some(n),
                value: parse_alpha(reader)?,
            },
            oth => todo!("{:?}", oth),
        }
    } else {
        let name = reader.take_while(|b| b.is_ascii_alphabetic());
        return if name.is_empty() {
            Err(ParserError::MissingEffectName)
        } else {
            Err(ParserError::UnsupportedEffect(String::from_utf8_lossy(name).into_owned()))
        };
    })
}

fn parse_color(reader: &mut Reader) -> Result<Color, ReaderError> {
    reader.expect(b'&')?;
    reader.expect(b'H')?;
    let hex = reader.take_while(|c| c.is_ascii_hexdigit());
    reader.expect(b'&')?;
    Ok(parse_hex_color(hex).unwrap())
}

fn parse_alpha(reader: &mut Reader) -> Result<u8, ReaderError> {
    reader.expect(b'&')?;
    reader.expect(b'H')?;
    let hex = reader.take_while(|c| c.is_ascii_hexdigit());
    reader.expect(b'&')?;
    Ok(parse_hex(hex.try_into().unwrap()).unwrap())
}

fn parse_style(s: &[u8]) -> Result<Vec<Effect>, ParserError> {
    let mut reader = Reader::new(s);
    parse_overrides(&mut reader)
}

#[inline(never)]
fn parse_overrides(reader: &mut Reader) -> Result<Vec<Effect>, ParserError> {
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
                let comment = reader.take_until2(b'\\', b'}');
                println!("Comment: {:?}", std::str::from_utf8(comment));
            }
        }
    }

    Ok(items)
}

#[derive(Debug)]
pub enum Part<'s> {
    Text(&'s BStr),
    Overrides(Vec<Effect>),
    NewLine { smart_wrapping: bool },
}

impl Part<'_> {
    #[must_use]
    pub fn as_bstr(&self) -> Option<&BStr> {
        match self {
            Part::Text(s) => Some(s),
            Part::NewLine { .. } => Some(b"\n".as_bstr()),
            Part::Overrides(_) => None,
        }
    }
}

pub fn parse(s: &[u8]) -> Result<Vec<Part>, ParserError> {
    let mut reader = Reader::new(s);
    let mut parts = Vec::new();
    while let Some(c) = reader.peek() {
        match c {
            b'{' => {
                reader.expect(b'{')?;
                let effects = parse_overrides(&mut reader)?;
                reader.expect(b'}')?;
                if !effects.is_empty() {
                    parts.push(Part::Overrides(effects));
                }
            }
            b'\\' => {
                reader.expect(b'\\')?;
                match reader.consume() {
                    Some(b'n') => parts.push(Part::NewLine { smart_wrapping: false }),
                    Some(b'N') => parts.push(Part::NewLine { smart_wrapping: true }),
                    _ => { /* ignore */ }
                }
            }
            _ => {
                let s = reader.take_until2(b'{', b'\\');
                if s.len() > 0 {
                    parts.push(Part::Text(s.as_bstr()));
                }
            }
        }
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