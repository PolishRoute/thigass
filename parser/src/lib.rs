#![feature(never_type)]
#![feature(slice_as_chunks)]
#![feature(specialization)]
#![feature(slice_split_once)]
#![deny(unsafe_code)]
#![allow(incomplete_features)]

mod reader;

use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use bstr::{BStr, ByteSlice};
use enum_map::{Enum, enum_map, EnumArray, EnumMap};
use tinyvec::ArrayVec;
pub use crate::reader::ReaderError;
use crate::reader::Reader;

#[derive(Default)]
pub struct Timestamp {
    value: u64,
}

impl fmt::Display for Timestamp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{:02}:{:02}.{:02}",
               self.value / 360_000,
               (self.value / 6_000) % 60,
               (self.value / 100) % 60,
               self.value % 100,
        )
    }
}

impl fmt::Debug for Timestamp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
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
        let mut parts = s.splitn(3, ':').map(str::trim);
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

#[derive(Debug, Default)]
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

#[derive(Hash, Eq, PartialEq, Enum, Debug, Copy, Clone)]
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
    Actor,
}

fn parse_events_mapping(config: &BStr) -> FieldMapping<EventField> {
    let mut mapping = FieldMapping::empty();
    for (idx, field) in config.split_str(b",").map(|it| it.trim()).enumerate() {
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
            b"Actor" => EventField::Actor,
            _ => {
                mapping.set(idx, None);
                tracing::warn!("Unsupported field for events: {}", field.as_bstr());
                continue;
            }
        };
        mapping.set(idx, Some(key));
    }
    mapping
}

#[derive(Eq, PartialEq, Debug)]
enum Section {
    Events,
    ScriptInfo,
    V4Styles,
    AegisubExtraData,
    Fonts,
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
    pub video_aspect_ratio: AspectRatio,
    pub video_zoom: f32,
    pub scroll_position: f32,
    pub collisions: Collisions,
    pub timer: f32,
    pub export_encoding: String,
    pub audio_uri: String,
    pub keyframes_file: String,
    pub play_depth: u32,
    pub original_script: String,
    pub original_editing: String,
    pub original_timing: String,
    pub timing: String,
    pub script_updated_by: String,
    pub video_colorspace: Colorspace,
    pub synch_point: String,
    pub update_details: String,
}

#[derive(Debug, Default)]
struct Colorspace(String);

impl FromStr for Colorspace {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
       Ok(Colorspace(s.to_string()))
    }
}

#[derive(Debug, Default)]
pub enum Collisions {
    #[default]
    Normal,
    Reverse,
}

impl FromStr for Collisions {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Normal" => Ok(Self::Normal),
            "Reverse" => Ok(Self::Reverse),
            _ => Err(())
        }
    }
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

#[derive(Hash, Eq, PartialEq, Enum, Debug, Copy, Clone)]
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Default for Color {
    fn default() -> Self {
        Self { r: 0, g: 0, b: 0, a: 255 }
    }
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
            b"StrikeOut" | b"Strikeout" => StyleField::StrikeOut,
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
            other => {
                mapping.set(idx, None);
                tracing::warn!("Unsupported field for styles: {}", other.as_bstr());
                continue;
            }
        };
        mapping.set(idx, Some(key));
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
    ignored: usize,
}

impl<T: EnumArray<usize>> FieldMapping<T> {
    fn empty() -> Self {
        Self {
            inner: enum_map! { _ => usize::MAX },
            inserted: 0,
            ignored: 0,
        }
    }

    fn columns(&self) -> usize {
        self.inserted + self.ignored
    }

    fn set(&mut self, idx: usize, field: Option<T>) {
        if let Some(field) = field {
            self.inner[field] = idx;
            self.inserted += 1;
        } else {
            tracing::warn!("unknown field at #{idx}");
            self.ignored += 1;
        }
    }

    fn value_of<'s>(&self, field: T, values: &[&'s [u8]]) -> &'s [u8] {
        match self.inner[field] {
            usize::MAX => b"",
            other => values[other],
        }
    }

    #[track_caller]
    fn value<U: FromBytes + Default + fmt::Debug>(&self, field: T, values: &[&[u8]]) -> U
        where T: Copy + fmt::Debug
    {
        let value = self.value_of(field, values);
        let value = match value.split_once(|sep| *sep == b'=') {
            Some((first, second)) => {
                // TODO: avoid alloc
                if first == format!("{:?}", field).as_bytes() {
                    second
                } else {
                    value
                }
            }
            None => value,
        };

        match U::from_bytes(value) {
            Ok(value) => value,
            Err(err) => {
                if !value.is_empty() {
                    tracing::warn!("Failed to parse value '{}' into field '{:?}' with error '{:?}'; using default {:?}",
                        value.as_bstr(), field, err, U::default());
                    tracing::debug!("{:?}", std::panic::Location::caller());
                }
                Default::default()
            }
        }
    }
}

trait FromBytes: Sized {
    type Err: fmt::Debug;

    fn from_bytes(bytes: &[u8]) -> Result<Self, Self::Err>;
}

impl<T: FromStr + Default + fmt::Debug> FromBytes for T
    where <T as FromStr>::Err: fmt::Debug
{
    default type Err = <T as FromStr>::Err;

    default fn from_bytes(bytes: &[u8]) -> Result<Self, <Self as FromBytes>::Err> {
        let s = match bytes.to_str() {
            Ok(s) => s,
            Err(e) => bytes[..e.valid_up_to()].to_str().unwrap(),
        };

        if s.is_empty() {
            tracing::warn!("no value for {}. using default: {:?}",
                    std::any::type_name::<T>(), &T::default());
            return Ok(T::default());
        }

        match T::from_str(s) {
            Ok(val) => Ok(val),
            Err(e) => {
                tracing::warn!("cannot parse '{}' as {} ({:?}). using default: {:?}",
                    s, std::any::type_name::<T>(), e, &T::default());
                Ok(T::default())
            }
        }
    }
}

impl FromBytes for f32 {
    type Err = fast_float::Error;

    fn from_bytes(bytes: &[u8]) -> Result<Self, Self::Err> {
        fast_float::parse(bytes)
    }
}

impl FromBytes for bool {
    type Err = ();

    fn from_bytes(bytes: &[u8]) -> Result<Self, Self::Err> {
        match bytes {
            b"0" | b"-1" => Ok(true),
            b"1" => Ok(true),
            _ => Err(())
        }
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

#[derive(Debug, Default)]
struct AspectRatio(f32);

impl FromBytes for AspectRatio {
    type Err = fast_float::Error;

    fn from_bytes(bytes: &[u8]) -> Result<Self, Self::Err> {
        if let Some(bytes) = bytes.strip_prefix(b"c") {
            Ok(AspectRatio(fast_float::parse(bytes)?))
        } else {
            Ok(AspectRatio(fast_float::parse(bytes)?))
        }
    }
}

#[derive(Debug)]
pub struct Script<'s> {
    pub info: ScriptInfo,
    pub styles: Vec<Style>,
    pub events: Vec<(EventType, Event<'s>)>,
    pub aegisub: AegisubData,
}

#[derive(Debug, Default)]
struct AegisubData {
    scroll_position: u32,
    active_line: u32,
    video_zoom_percent: f32,
    video_aspect_ratio: AspectRatio,
    video_position: u32,
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
            aegisub: AegisubData::default(),
        };

        let mut current_section = None;

        while let Some(line) = self.next_line() {
            if let Some(section) = line.strip_prefix(b"[") {
                // Found a section eg. `[Events]`
                current_section = match section.strip_suffix(b"]") {
                    Some(b"Events") => Some(Section::Events),
                    Some(b"Script Info") => Some(Section::ScriptInfo),
                    Some(b"V4+ Styles") => Some(Section::V4Styles),
                    Some(b"Aegisub Extradata") => Some(Section::AegisubExtraData),
                    Some(b"Fonts") => Some(Section::Fonts),
                    Some(b"Aegisub Project Garbage") => None,
                    Some(other) => {
                        tracing::info!("Ignored section: {}", other.as_bstr());
                        None
                    }
                    None => {
                        tracing::warn!("Empty section {}", section.as_bstr());
                        None
                    }
                };
                continue;
            }

            if current_section.is_none() {
                continue;
            }

            if let Some(format) = line.strip_prefix(b"Format: ") {
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
                    }
                    Section::AegisubExtraData => {
                        // TODO
                    },
                    Section::Fonts => unimplemented!(),
                }
            } else if let Some(x) = line.strip_prefix(b"Dialogue: ") {
                script.events.push((EventType::Dialogue, self.parse_event(x.as_bstr()).unwrap_or_default()));
            } else if let Some(x) = line.strip_prefix(b"Comment: ") {
                script.events.push((EventType::Comment, self.parse_event(x.as_bstr()).unwrap_or_default()));
            } else if let Some(s) = line.strip_prefix(b"Style: ") {
                let style = self.parse_style(s.as_bstr());
                if let Ok(style) = style {
                    script.styles.push(style);
                }
            } else if line.trim().is_empty() || line.starts_with(b";") {
                continue;
            } else if let Some(pos) = memchr::memchr(b':', line) {
                macro_rules! parse_or_skip {
                    ($s:expr) => {
                        match FromBytes::from_bytes($s) {
                            Ok(value) => value,
                            Err(err) => {
                                tracing::warn!("Error while parsing value '{}': {:?}", $s.as_bstr(), err);
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
                    b"Video Aspect Ratio" => script.info.video_aspect_ratio = parse_or_skip!(value),
                    b"Video Zoom" => script.info.video_zoom = parse_or_skip!(value),
                    b"Scroll Position" => script.info.scroll_position = parse_or_skip!(value),
                    b"Collisions" => script.info.collisions = parse_or_skip!(value),
                    b"Timer" => script.info.timer = parse_or_skip!(value),
                    b"Export Encoding" => script.info.export_encoding = parse_or_skip!(value),
                    b"Audio URI" => script.info.audio_uri = parse_or_skip!(value),
                    b"Aegisub Scroll Position" => script.aegisub.scroll_position = parse_or_skip!(value),
                    b"Aegisub Active Line" => script.aegisub.active_line = parse_or_skip!(value),
                    b"Aegisub Video Zoom Percent" => script.aegisub.video_zoom_percent = parse_or_skip!(value),
                    b"Aegisub Video Aspect Ratio" => script.aegisub.video_aspect_ratio = parse_or_skip!(value),
                    b"Aegisub Video Position" => script.aegisub.video_position = parse_or_skip!(value),
                    b"Keyframes File" => script.info.keyframes_file = parse_or_skip!(value),
                    b"PlayDepth" => script.info.play_depth = parse_or_skip!(value),
                    b"Original Script" => script.info.original_script = parse_or_skip!(value),
                    b"Original Editing" => script.info.original_editing = parse_or_skip!(value),
                    b"Original Timing" => script.info.original_timing = parse_or_skip!(value),
                    b"Timing" => script.info.timing = parse_or_skip!(value),
                    b"Script Updated By" => script.info.script_updated_by = parse_or_skip!(value),
                    b"Video Colorspace" => script.info.video_colorspace = parse_or_skip!(value),
                    b"Synch Point" => script.info.synch_point = parse_or_skip!(value),
                    b"Update Details" => script.info.update_details = parse_or_skip!(value),
                    _ => {
                        tracing::warn!("Unsupported key for script info: '{}' with value '{}'", name, value.as_bstr())
                    }
                }
            } else {
                tracing::trace!("Unsupported line: {} in section {:?}", line.as_bstr(), current_section);
            }
        }

        script
    }

    #[inline(never)]
    fn parse_style(&mut self, s: &'s BStr) -> Result<Style, ()> {
        let Some(mapping) = self.styles_mapping.as_ref() else {
            // Ignore when not in section
            tracing::warn!("Style without field mapping: {}", s.as_bstr());
            return Err(());
        };
        let fields: ArrayVec<[_; StyleField::LENGTH]> = s.splitn(mapping.columns(), |b| *b == b',')
            .map(|f| f.trim())
            .collect();
        let style = Style {
            name: mapping.value(StyleField::Name, &fields),
            font_name: mapping.value(StyleField::FontName, &fields),
            font_size: mapping.value(StyleField::FontSize, &fields),
            primary_colour: mapping.value(StyleField::PrimaryColour, &fields),
            secondary_colour: mapping.value(StyleField::SecondaryColour, &fields),
            outline_colour: mapping.value(StyleField::OutlineColour, &fields),
            back_colour: mapping.value(StyleField::BackColour, &fields),
            bold: mapping.value(StyleField::Bold, &fields),
            italic: mapping.value(StyleField::Italic, &fields),
            underline: mapping.value(StyleField::Underline, &fields),
            strike_out: mapping.value(StyleField::StrikeOut, &fields),
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
        };
        Ok(style)
    }

    #[inline(never)]
    fn parse_event(&mut self, data: &'s BStr) -> Result<Event<'s>, ()> {
        let Some(mapping) = self.events_mapping.as_ref() else {
            tracing::warn!("missing mapping for events");
            return Err(());
        };

        let fields: ArrayVec<[_; EventField::LENGTH]> = data
            .splitn(mapping.columns(), |b| *b == b',')
            .map(|f| f.trim())
            .collect();

        let event = Event {
            marked: mapping.value(EventField::Marked, &fields),
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
        Ok(event)
    }
}

#[derive(Debug)]
pub enum EventType {
    Dialogue,
    Comment,
}

#[derive(Debug)]
pub enum ParserError {
    ReaderError(ReaderError),
    ColorError(ColorParseError),
    UnsupportedEffect(String),
    InvalidCurveOpcode,
    MissingEffectName,
    UnsupportedOverload { args: usize },
    UnexpectedChar { actual: u8 },
    UnexpectedEnd,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Debug>::fmt(self, f)
    }
}

impl std::error::Error for ParserError {}

impl From<ReaderError> for ParserError {
    fn from(e: ReaderError) -> Self {
        Self::ReaderError(e)
    }
}

impl From<ColorParseError> for ParserError {
    fn from(e: ColorParseError) -> Self {
        Self::ColorError(e)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Alpha {
    Percent(u8),
    Byte(u8),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Effect {
    Align(Alignment),
    Blur(f32),
    BlurEdges(f32),
    Border(f32),
    XBorder(f32),
    YBorder(f32),
    FontEncoding,
    FontName(String),
    FontSize(f32),
    FontScale(f32),
    FontScaleX(f32),
    FontScaleY(f32),
    FontSpacing(f32),
    RotateX(f32),
    RotateY(f32),
    RotateZ(f32),
    ShearingX(f32),
    ShearingY(f32),
    Fx(f32),
    Fy(f32),
    Fz(f32),
    Color { index: Option<u32>, color: Option<Color> },
    Alpha { index: Option<u32>, value: Option<Alpha> },
    Pos(f32, f32),
    DrawScale(f32),
    BaselineOffset(f32),
    Clip { mask: Vec<DrawCommand>, scale: f32 },
    ClipRect(f32, f32, f32, f32),
    Bold(bool),
    Italic(bool),
    Underline(bool),
    StrikeOut(bool),
    Shadow(f32),
    XShadow(f32),
    YShadow(f32),
    WrappingStyle(u32),
    Reset(Option<String>),
    Transition { t1: Option<f32>, t2: Option<f32>, accel: Option<f32>, style: Vec<Effect> },
    FadeAlpha { t1: f32, t2: f32 },
    Fade { a1: f32, a2: f32, a3: f32, t1: f32, t2: f32, t3: f32, t4: f32 },
    Move { x1: f32, y1: f32, x2: f32, y2: f32, t1: Option<f32>, t2: Option<f32> },
    NewLine { smart_wrapping: bool },
    Horizontal,
    Org(u32, u32),
    Highlight(u32),
    FillUpHighlight(u32),
    OutlineHighlight(u32),
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Alignment(u8);

impl FromStr for Alignment {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Alignment(s.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Point(f32, f32);

#[derive(Debug, Clone, PartialEq)]
pub enum DrawCommand {
    Close,
    Move(Point),
    Line(Point),
    Bezier([Point; 3]),
}

fn read_point(reader: &mut Reader) -> Result<Point, ReaderError> {
    let x = reader.read_float_or_default()?;
    reader.expect_whitespace()?;
    let y = reader.read_float_or_default()?;
    reader.expect_whitespace_or_end()?;
    Ok(Point(x, y))
}

fn parse_args_simple<'a>(reader: &mut Reader<'a>) -> Result<ArrayVec<[&'a BStr; 8]>, ReaderError> {
    reader.expect(b'(')?;
    let mut args = ArrayVec::new();
    loop {
        let arg = match reader.peek() {
            Some(b',') => {
                reader.consume().unwrap();
                continue;
            },
            Some(b')') => {
                reader.consume().unwrap();
                break;
            },
            Some(b'}') => {
                // missing ')' but we got '}'
                tracing::warn!("missing ')' for argument list; using '}}' instead as a terminator");
                break;
            }
            Some(_) => {
                let Some(raw_arg) = reader.take_until_any(|b| matches!(b, b',' | b')' | b'}')) else {
                    return Err(ReaderError::UnexpectedEnd);
                };
                raw_arg.as_bstr()
            }
            None => break,
        };

        if let Some(arg) = args.try_push(arg) {
            tracing::warn!("ignoring argument: {:?}", arg);
        }
    }
    Ok(args)
}

#[derive(Debug)]
enum Arg<'a> {
    Raw(&'a BStr),
    Effects(Vec<Effect>),
    Expr(&'a BStr),
}

impl Default for Arg<'_> {
    fn default() -> Self {
        Self::Raw(b"".into())
    }
}

fn parse_args_complex<'a>(reader: &mut Reader<'a>) -> Result<ArrayVec<[Arg<'a>; 8]>, ParserError> {
    reader.expect(b'(')?;
    let mut args = ArrayVec::new();
    let mut is_end = false;
    while !is_end {
        let arg = match reader.peek() {
            Some(b',') => {
                reader.consume().unwrap();
                continue;
            },
            Some(b')') => {
                reader.consume().unwrap();
                is_end = true;
                continue;
            },
            Some(b'}') => {
                // missing ')' but we got '}'
                tracing::warn!("missing ')' for argument list; using '}}' instead as a terminator");
                break;
            }
            Some(b'\\') => {
                let mut effects = Vec::new();
                while let Some(byte) = reader.peek() {
                    match byte {
                        b'\\' => {
                            effects.push(parse_effect(reader)?);
                        }
                        b')' => {
                            is_end = true;
                            break;
                        }
                        _ => break,
                    }
                }
                Arg::Effects(effects)
            },
            Some(b'!') => {
                reader.consume().unwrap();
                let expr = reader.take_until(b'!').unwrap().as_bstr();
                Arg::Expr(expr)
            }
            Some(b'$') => {
                let expr = reader.take_while(|c| c == b'$' || c.is_ascii_alphanumeric()).as_bstr();
                Arg::Expr(expr)
            }
            Some(_) => {
                let Some(raw_arg) = reader.take_until_any(|b| matches!(b, b',' | b')' | b'\\' | b'!' | b'}')) else {
                    return Err(ParserError::ReaderError(ReaderError::UnexpectedEnd));
                };
                Arg::Raw(raw_arg.as_bstr())
            }
            None => break,
        };

        if let Some(arg) = args.try_push(arg) {
            tracing::warn!("ignoring argument: {:?}", arg);
        }
    }
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
                let p = read_point(reader)?;
                commands.push(DrawCommand::Close);
                commands.push(DrawCommand::Move(p));
            }
            b'n' => {
                let p = read_point(reader)?;
                commands.push(DrawCommand::Move(p));
            }
            b'l' => {
                let p = read_point(reader)?;
                commands.push(DrawCommand::Line(p));
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
    let cmds = parse_draw_commands(&mut reader)?;
    if !reader.is_end() {
        tracing::debug!("not all parsed as a curve; remaining '{}'", reader.take_remaining().as_bstr());
    }
    Ok(cmds)
}

fn parse_float(s: &[u8]) -> Result<f32, ReaderError> {
    let mut reader = Reader::new(s);
    let value = reader.read_float_or_default()?;
    Ok(value)
}

fn parse_integer(s: &[u8]) -> Result<u32, ReaderError> {
    let mut reader = Reader::new(s);
    let value = reader.read_integer_or_default()?;
    Ok(value)
}

fn unsupported_overload(name: impl AsRef<[u8]>, args: &[&BStr]) -> Result<!, ParserError> {
    tracing::warn!("Unsupported overload for '{}': {:?}", name.as_ref().as_bstr(), args);
    Err(ParserError::UnsupportedOverload { args: args.len() })
}

fn unsupported_overload_complex(name: impl AsRef<[u8]>, args: &[Arg<'_>]) -> Result<!, ParserError> {
    tracing::warn!("Unsupported overload for '{}': {:?}", name.as_ref().as_bstr(), args);
    Err(ParserError::UnsupportedOverload { args: args.len() })
}

fn parse_effect(reader: &mut Reader) -> Result<Effect, ParserError> {
    reader.expect(b'\\')?;

    // Handle effects with jointed string argument, eg. \fnFontName
    const WITH_STR_ARGS: [&[u8]; 7] = [b"fn", b"n", b"N", b"alpha", b"r", b"clip", b"c"];

    let mut name = None;
    for prefix in WITH_STR_ARGS {
        if reader.try_consume(prefix) {
            name = Some(prefix);
            break;
        }
    }

    let name = match name {
        Some(name) => name,
        None => reader.take_while(|c| c.is_ascii_alphabetic()),
    };

    let effect = match name {
        b"n" => Effect::NewLine { smart_wrapping: false },
        b"N" => Effect::NewLine { smart_wrapping: true },
        b"h" => Effect::Horizontal,
        b"an" | b"a" => Effect::Align(Alignment(reader.read_integer_or_default()?.try_into().unwrap())), // TODO: handle differences?
        b"be" => Effect::BlurEdges(reader.read_float_or_default()?),
        b"blur" => Effect::Blur(reader.read_float_or_default()?),
        b"bord" => Effect::Border(reader.read_float_or_default()?),
        b"xbord" => Effect::XBorder(reader.read_float_or_default()?),
        b"ybord" => Effect::YBorder(reader.read_float_or_default()?),
        b"fe" => Effect::FontEncoding,
        b"fn" => Effect::FontName(reader.read_str()?.into()),
        b"fsc" => Effect::FontScale(reader.read_float_or_default()?),
        b"fscx" => Effect::FontScaleX(reader.read_float_or_default()?),
        b"fscy" => Effect::FontScaleY(reader.read_float_or_default()?),
        b"fsp" => Effect::FontSpacing(reader.read_float_or_default()?),
        b"fs" => Effect::FontSize(reader.read_float_or_default()?),
        b"frx" => Effect::RotateX(reader.read_float_or_default()?),
        b"fry" => Effect::RotateY(reader.read_float_or_default()?),
        b"frz" | b"fr" => Effect::RotateZ(reader.read_float_or_default()?),
        b"fax" => Effect::ShearingX(reader.read_float_or_default()?),
        b"fay" => Effect::ShearingY(reader.read_float_or_default()?),
        b"fx" => Effect::Fx(reader.read_float_or_default()?),
        b"fy" => Effect::Fy(reader.read_float_or_default()?),
        b"fz" => Effect::Fz(reader.read_float_or_default()?),
        b"clip" | b"iclip" => { // TODO: split
            let args = parse_args_simple(reader)?;
            match args[..] {
                [] => {
                    Effect::Clip { mask: vec![], scale: 1.0 }
                }
                [curve] => {
                    let mut reader = Reader::new(curve.as_bytes());
                    let cmds = parse_draw_commands(&mut reader)?;
                    Effect::Clip { mask: cmds, scale: 1.0 }
                }
                [scale, curve] => {
                    let mut reader = Reader::new(curve.as_bytes());
                    let cmds = parse_draw_commands(&mut reader)?;
                    Effect::Clip { mask: cmds, scale: parse_float(scale)? }
                }
                [x1, y1, x2, y2] => Effect::ClipRect(
                    parse_float(x1)?,
                    parse_float(y1)?,
                    parse_float(x2)?,
                    parse_float(y2)?,
                ),
                _ => unsupported_overload(name, &args)?,
            }
        }
        b"pos" => {
            let args = parse_args_simple(reader)?;
            match args[..] {
                [x, y] => Effect::Pos(
                    parse_float(x)?,
                    parse_float(y)?,
                ),
                _ => unsupported_overload(name, &args)?,
            }
        }
        b"org" => {
            let args = parse_args_simple(reader)?;
            match args[..] {
                [x, y] => Effect::Org(
                    parse_integer(x)?,
                    parse_integer(y)?,
                ),
                _ => unsupported_overload(name, &args)?,
            }
        }
        b"i" => Effect::Italic(reader.read_bool_or_default()?),
        b"p" => Effect::DrawScale(reader.read_float_or_default()?),
        b"pbo" => Effect::BaselineOffset(reader.read_float_or_default()?),
        b"b" => Effect::Bold(reader.read_bool_or_default()?),
        b"shad" => Effect::Shadow(reader.read_float_or_default()?),
        b"shadow" => Effect::Shadow(reader.read_float_or_default()?),
        b"t" => {
            let args = parse_args_complex(reader)?;
            match &args[..] {
                [Arg::Raw(t1), Arg::Raw(t2), Arg::Raw(accel), Arg::Effects(style)] => Effect::Transition {
                    t1: Some(parse_float(t1)?),
                    t2: Some(parse_float(t2)?),
                    accel: Some(parse_float(accel)?),
                    style: style.clone(),
                },
                [Arg::Raw(t1), Arg::Raw(t2), Arg::Effects(style)] => Effect::Transition {
                    t1: Some(parse_float(t1)?),
                    t2: Some(parse_float(t2)?),
                    accel: None,
                    style: style.clone(),
                },
                [Arg::Raw(t1), Arg::Raw(t2)] => Effect::Transition {
                    t1: Some(parse_float(t1)?),
                    t2: Some(parse_float(t2)?),
                    accel: None,
                    style: Vec::new(),
                },
                [Arg::Raw(t2)] => Effect::Transition {
                    t1: None,
                    t2: Some(parse_float(t2)?),
                    accel: None,
                    style: Vec::new(),
                },
                [Arg::Raw(t2), Arg::Effects(style)] => Effect::Transition {
                    t1: None,
                    t2: Some(parse_float(t2)?),
                    accel: None,
                    style: style.clone(),
                },
                [Arg::Effects(style)] => Effect::Transition {
                    t1: None,
                    t2: None,
                    accel: None,
                    style: style.clone(),
                },
                ref rest => unsupported_overload_complex(name, &rest)?,
            }
        }
        b"fade" | b"fad" => {
            let args = parse_args_simple(reader)?;
            match args[..] {
                [a1, a2, a3, t1, t2, t3, t4] => Effect::Fade {
                    a1: parse_float(a1)?,
                    a2: parse_float(a2)?,
                    a3: parse_float(a3)?,
                    t1: parse_float(t1)?,
                    t2: parse_float(t2)?,
                    t3: parse_float(t3)?,
                    t4: parse_float(t4)?,

                },
                [t1, t2] => Effect::FadeAlpha {
                    t1: parse_float(t1)?,
                    t2: parse_float(t2)?,
                },
                _ => unsupported_overload(name, &args)?,
            }
        }
        b"move" => {
            let args = parse_args_complex(reader)?;
            match args[..] {
                [Arg::Raw(x1), Arg::Raw(y1), Arg::Raw(x2), Arg::Raw(y2), Arg::Raw(t1), Arg::Raw(t2)] => Effect::Move {
                    x1: parse_float(x1)?,
                    y1: parse_float(y1)?,
                    x2: parse_float(x2)?,
                    y2: parse_float(y2)?,
                    t1: Some(parse_float(t1)?),
                    t2: Some(parse_float(t2)?),
                },
                [Arg::Raw(x1), Arg::Raw(y1), Arg::Raw(x2), Arg::Raw(y2)] => Effect::Move {
                    x1: parse_float(x1)?,
                    y1: parse_float(y1)?,
                    x2: parse_float(x2)?,
                    y2: parse_float(y2)?,
                    t1: None,
                    t2: None,
                },
                ref rest => unsupported_overload_complex(name, &rest)?,
            }
        }
        b"xshad" => Effect::XShadow(reader.read_float_or_default()?),
        b"yshad" => Effect::YShadow(reader.read_float_or_default()?),
        b"q" => Effect::WrappingStyle(reader.read_integer_or_default()?),
        b"r" => Effect::Reset(None),
        b"c" => {
            let mut colors: ArrayVec<[_; 4]> = ArrayVec::new();
            while let Some(color) = parse_color(reader)? {
                colors.push(color);
            }

            if colors.len() > 1 {
                // TODO
                tracing::debug!("remaining colors: {:?}", &colors[1..]);
            }

            Effect::Color {
                index: None,
                color: colors.get(0).copied(),
            }
        },
        b"alpha" => Effect::Alpha {
            index: None,
            value: parse_alpha(reader)?,
        },
        b"k" => Effect::Highlight(reader.read_integer_or_default()?),
        b"kf" | b"K" => Effect::FillUpHighlight(reader.read_integer_or_default()?),
        b"ko" => Effect::OutlineHighlight(reader.read_integer_or_default()?),
        b"u" => Effect::Underline(reader.read_bool_or_default()?),
        b"s" => Effect::StrikeOut(reader.read_bool_or_default()?),
        name if !name.is_empty() => {
            let args = if reader.peek() == Some(b'(') {
                parse_args_simple(reader)?
            } else {
                Default::default()
            };

            tracing::warn!("unsupported effect {} {:?}", name.as_bstr(), args);
            return Err(ParserError::UnsupportedEffect(name.to_str_lossy().into_owned()));
        }
        _ => {
            if let Some(b'0'..=b'9') = reader.peek() {
                let n = reader.read_integer_or_default()?;
                match reader.peek() {
                    Some(b'c') => {
                        reader.consume().unwrap();
                        Effect::Color {
                            index: Some(n),
                            color: parse_color(reader)?,
                        }
                    }
                    Some(b'a') => {
                        reader.consume().unwrap();
                        Effect::Alpha {
                            index: Some(n),
                            value: parse_alpha(reader)?,
                        }
                    }
                    Some(c) => {
                        // TODO: color or alpha? "\1&HFF&"
                        return Err(ParserError::UnexpectedChar { actual: c })
                    },
                    None => {
                        return Err(ParserError::UnexpectedEnd)
                    },
                }
            } else {
                return Err(ParserError::MissingEffectName);
            }
        }
    };

    Ok(effect)
}

fn parse_color(reader: &mut Reader) -> Result<Option<Color>, ParserError> {
    if reader.try_consume(b"&") {
        _ = reader.try_consume(b"H");
        let hex = reader.take_while(|c| c.is_ascii_hexdigit());
        _ = reader.try_consume(b"&");
        Ok(Some(parse_hex_color(hex)?))
    } else {
        Ok(None)
    }
}

fn parse_alpha(reader: &mut Reader) -> Result<Option<Alpha>, ReaderError> {
    // reader.dbg();
    _ = reader.try_consume(b"&");
    _ = reader.try_consume(b"&");
    _ = reader.try_consume(b"H");
    _ = reader.try_consume(b"H");
    let value = reader.take_while(|c| c.is_ascii_hexdigit());
    if reader.try_consume(b"%") {
        // TODO: is this in decimal?
        return Ok(Some(Alpha::Percent(parse_integer(value)?.try_into().unwrap())));
    }

    _ = reader.try_consume(b"&");
    match value {
        &[b0, b1] => Ok(Some(Alpha::Byte(parse_hex(&[b0, b1]).unwrap()))),
        &[] => Ok(None),
        value => {
            // tracing::warn!("invalid alpha value: {:?}", value.as_bstr());
            Ok(None)
        }
    }
}

#[inline(never)]
fn parse_overrides(reader: &mut Reader) -> Result<Vec<Effect>, ParserError> {
    let mut items = Vec::new();
    while let Some(x) = reader.peek() {
        match x {
            b'\\' => {
                match parse_effect(reader) {
                    Ok(item) => {
                        items.push(item);
                    }
                    Err(ParserError::MissingEffectName) => {}
                    Err(e) => {
                        // tracing::warn!("unsupported effect: {:?}", e);
                    }
                }
            }
            b'}' => {
                break;
            }
            _ => {
                let comment = reader.take_until2(b'\\', b'}')
                    .unwrap_or_else(|| reader.take_remaining());

                if !comment.starts_with(b"Kara Effector 3.5") {
                    tracing::warn!("Ignored: {}", comment.as_bstr());
                }
            }
        }
    }

    Ok(items)
}

#[derive(PartialEq)]
#[derive(Debug)]
pub enum Part<'s> {
    Text(&'s BStr),
    Overrides(Vec<Effect>),
    NewLine { smart_wrapping: bool },
    Horizontal,
}

impl Part<'_> {
    #[must_use]
    pub fn as_bstr(&self) -> Option<&BStr> {
        match self {
            Part::Text(s) => Some(s),
            Part::NewLine { .. } => Some(b"\n".as_bstr()),
            Part::Overrides(_) => None,
            Part::Horizontal => None,
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
                reader.expect_or_end(b'}')?;
                if !effects.is_empty() {
                    parts.push(Part::Overrides(effects));
                }
            }
            b'\\' => {
                reader.expect(b'\\')?;
                match reader.consume() {
                    Some(b'n') => parts.push(Part::NewLine { smart_wrapping: false }),
                    Some(b'N') => parts.push(Part::NewLine { smart_wrapping: true }),
                    Some(b'h') => parts.push(Part::Horizontal),
                    Some(other) => {
                        tracing::warn!("invalid escaped char '{}'", other as char);
                    }
                    None => { /* ignore */ }
                }
            }
            _ => {
                let s = reader.take_until2(b'{', b'\\')
                    .unwrap_or_else(|| reader.take_remaining());
                if s.len() > 0 {
                    parts.push(Part::Text(s.as_bstr()));
                }
            }
        }
    }
    Ok(parts)
}

#[cfg(test)]
mod tests {
    use bstr::ByteSlice;
    use crate::{parse, Alignment, Color, DrawCommand, Effect, Part, Point, Timestamp};

    #[test]
    fn parse_timestamp() {
        assert_eq!(format!("{:?}", "0:13:57.35".parse::<Timestamp>().unwrap()), "0:13:57.35");
    }

    #[test]
    fn simple_parse() {
        let parsed = parse(br"{\fs16}This is small text. {\fs28}This is\n large text").unwrap();
        assert_eq!(&parsed[..], &[
            Part::Overrides(vec![
                Effect::FontSize(16.0),
            ]),
            Part::Text(b"This is small text. ".as_bstr()),
            Part::Overrides(vec![
                Effect::FontSize(28.0),
            ]),
            Part::Text(b"This is".as_bstr()),
            Part::NewLine { smart_wrapping: false },
            Part::Text(b" large text".as_bstr()),
        ]);
    }

    #[test]
    fn more_complex() {
        let parsed = parse(br"{\an7\blur4\fscx50\frz14.5\fax0.27\c&H303587&\pos(118.73,1232.33)\p1\fscy50\clip(m 549 1080 l 422 876 759 791 980 735 b 1028 724 1054 712 1059 696 l 1281 1080)\t(1740,2140,1,\blur1)}m 1859.05 -127.72 b 1859.79 -128.1 1860.54 -128.48 1861.29 -128.86 1861.64 -128.83 1862 -128.79 1862.36 -128.74 1862.69 -128.38 1863.01 -128.02 1863.34 -127.66 1862.92 -127.2 1862.51 -126.74 1862.09 -126.28 1861.08 -126.76 1860.06 -127.24 1859.05 -127.72").unwrap();
        assert_eq!(&parsed[..], &[
            Part::Overrides(vec![
                Effect::Align(Alignment(7)),
                Effect::Blur(4.0),
                Effect::FontScaleX(50.0),
                Effect::RotateZ(14.5),
                Effect::ShearingX(0.27),
                Effect::Color { index: None, color: Some(Color { r: 135, g: 53, b: 48, a: 0 }) },
                Effect::Pos(118.73, 1232.33),
                Effect::DrawScale(1.0),
                Effect::FontScaleY(50.0),
                Effect::Clip { mask: vec![
                    DrawCommand::Close,
                    DrawCommand::Move(Point(549.0, 1080.0)),
                    DrawCommand::Line(Point(422.0, 876.0)),
                    DrawCommand::Line(Point(759.0, 791.0)),
                    DrawCommand::Line(Point(980.0, 735.0)),
                    DrawCommand::Bezier([
                        Point(1028.0, 724.0),
                        Point(1054.0, 712.0),
                        Point(1059.0, 696.0),
                    ]),
                    DrawCommand::Line(Point(1281.0, 1080.0)),
                ], scale: 1.0 },
                Effect::Blur(1.0)
            ]),
            Part::Text(b"m 1859.05 -127.72 b 1859.79 -128.1 1860.54 -128.48 1861.29 -128.86 1861.64 -128.83 1862 -128.79 1862.36 -128.74 1862.69 -128.38 1863.01 -128.02 1863.34 -127.66 1862.92 -127.2 1862.51 -126.74 1862.09 -126.28 1861.08 -126.76 1860.06 -127.24 1859.05 -127.72".as_bstr())
        ]);
    }

    #[test]
    fn transition_with_overrides() {
        let parsed = parse(br"{\t(540,540,\blur0.9)}").unwrap();
        assert_eq!(&parsed[..], &[
            Part::Overrides(vec![
                Effect::Transition {
                    t1: Some(540.0),
                    t2: Some(540.0),
                    accel: None,
                    style: vec![
                        Effect::Blur(0.9),
                    ],
                },
            ]),
        ]);
    }

    #[test]
    fn unterminated_simple_args() {
        let parsed = parse(br"{\fad(200,200}{\pos(355,484)}Terminal Colony Number 8 - Shirahime").unwrap();
        assert_eq!(&parsed[..], &[
            Part::Overrides(vec![
                Effect::FadeAlpha {
                    t1: 200.0,
                    t2: 200.0,
                },
            ]),
            Part::Overrides(vec![
                Effect::Pos(355.0, 484.0),
            ]),
            Part::Text(b"Terminal Colony Number 8 - Shirahime".as_bstr()),
        ]);
    }

    #[test]
    fn unexpected_escape_sequence() {
        let parsed = parse(br"{\fad(250,250\)}Now, count up your sins").unwrap();
        assert_eq!(&parsed[..], &[
            Part::Overrides(vec![
                Effect::FadeAlpha {
                    t1: 250.0,
                    t2: 250.0,
                },
            ]),
            Part::Text(b"Now, count up your sins".as_bstr()),
        ]);
    }
}