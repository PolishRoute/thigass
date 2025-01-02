use bstr::ByteSlice;

pub struct Reader<'d> {
    buf: &'d [u8],
    pos: usize,
}

impl<'buf> Reader<'buf> {
    pub fn new(input: &'buf [u8]) -> Reader<'buf> {
        Self {
            buf: input,
            pos: 0,
        }
    }

    #[must_use]
    pub(crate) fn is_end(&self) -> bool {
        self.pos == self.buf.len()
    }

    #[must_use]
    pub(crate) fn consume(&mut self) -> Option<u8> {
        let byte = self.buf.get(self.pos).copied()?;
        self.pos += 1;
        Some(byte)
    }

    #[must_use]
    pub(crate) fn peek(&self) -> Option<u8> {
        self.buf.get(self.pos).copied()
    }

    #[must_use]
    pub(crate) fn try_consume(&mut self, prefix: &[u8]) -> bool {
        if self.buf[self.pos..].starts_with(prefix) {
            self.pos += prefix.len();
            true
        } else {
            false
        }
    }

    #[must_use]
    pub(crate) fn take_while(&mut self, predicate: impl Fn(u8) -> bool) -> &'buf [u8] {
        let pos = self.pos;
        while let Some(p) = self.peek() {
            if !predicate(p) {
                break;
            }
            self.consume().unwrap();
        }
        &self.buf[pos..self.pos]
    }

    #[must_use]
    #[allow(dead_code)]
    pub(crate) fn take_until(&mut self, term1: u8) -> Option<&'buf [u8]> {
        let len = memchr::memchr(term1, &self.buf[self.pos..])?;
        let slice = &self.buf[self.pos..][..len];
        self.pos += len;
        Some(slice)
    }

    #[must_use]
    pub(crate) fn take_until2(&mut self, term1: u8, term2: u8) -> Option<&'buf [u8]> {
        let len = memchr::memchr2(term1, term2, &self.buf[self.pos..])?;
        let slice = &self.buf[self.pos..][..len];
        self.pos += len;
        Some(slice)
    }

    #[must_use]
    pub(crate) fn take_until_any(&mut self, is_term: impl Fn(u8) -> bool) -> Option<&'buf [u8]> {
        let raw = self.take_while(|b| !is_term(b));
        if raw.is_empty() {
            return None;
        }
        Some(raw)
    }

    #[must_use]
    pub(crate) fn take_remaining(&mut self) -> &'buf [u8] {
        let remaining = &self.buf[self.pos..];
        self.pos += remaining.len();
        remaining
    }

    pub(crate) fn expect_whitespace_or_end(&mut self) -> Result<(), ReaderError> {
        if self.peek().is_none() {
            return Ok(());
        }
        self.expect_whitespace()
    }

    pub(crate) fn expect_whitespace(&mut self) -> Result<(), ReaderError> {
        let ignored = self.take_while(|b| b.is_ascii_whitespace()).len();
        if ignored > 0 {
            Ok(())
        } else {
            Err(ReaderError::ExpectedWhitespace { pos: self.pos })
        }
    }

    fn try_read_float(&mut self) -> Option<f32> {
        match fast_float::parse_partial(&self.buf[self.pos..]) {
            Ok((value, len)) => {
                self.pos += len;
                Some(value)
            }
            Err(_) => None,
        }
    }

    pub(crate) fn read_float_or_default(&mut self) -> Result<f32, ReaderError> {
        Ok(self.try_read_float().unwrap_or(0.0))
    }

    pub(crate) fn read_str(&mut self) -> Result<&str, ReaderError> {
        let pos = self.pos;
        let s = self.take_while(|b| !matches!(b, b'\\' | b'{' | b'}'));
        let s = std::str::from_utf8(s).map_err(|_| ReaderError::InvalidStr { pos })?;
        Ok(s)
    }

    fn try_read_integer(&mut self) -> Option<u32> {
        let digits = self.take_while(|c| c.is_ascii_digit());
        if digits.len() == 0 {
            return None;
        }

        let mut n = 0;
        for b in digits.iter().copied() {
            n = n * 10 + u32::from(b - b'0');
        }
        Some(n)
    }

    pub(crate) fn read_integer_or_default(&mut self) -> Result<u32, ReaderError> {
        Ok(self.try_read_integer().unwrap_or(0))
    }

    #[inline]
    fn consume_if(&mut self, predicate: impl FnOnce(u8) -> bool) -> Option<u8> {
        match self.peek() {
            Some(byte) if predicate(byte) => {
                self.consume().unwrap();
                Some(byte)
            }
            _ => None,
        }
    }

    pub(crate) fn read_bool_or_default(&mut self) -> Result<bool, ReaderError> {
        match self.consume_if(|b| matches!(b, b'0' | b'1')) {
            Some(b'0') => Ok(false),
            Some(b'1') => Ok(true),
            None => Ok(false),
            _ => unreachable!(),
        }
    }

    pub(crate) fn expect_or_end(&mut self, byte: u8) -> Result<bool, ReaderError> {
        match self.peek() {
            Some(actual) if actual == byte => {
                self.consume().unwrap();
                Ok(true)
            }
            Some(actual) => Err(ReaderError::InvalidChar { pos: self.pos, expected: byte, actual }),
            None => Ok(false),
        }
    }

    pub(crate) fn expect(&mut self, byte: u8) -> Result<(), ReaderError> {
        if self.expect_or_end(byte)? {
            Ok(())
        } else {
            Err(ReaderError::UnexpectedEnd)
        }
    }

    #[allow(unused)]
    #[track_caller]
    pub(crate) fn dbg(&self) {
        println!("{} @ {:?}", std::panic::Location::caller(), &self.buf[self.pos..].as_bstr());
    }
}

#[derive(Debug)]
pub enum ReaderError {
    InvalidInt { pos: usize },
    InvalidFloat { pos: usize },
    InvalidChar { pos: usize, expected: u8, actual: u8 },
    InvalidBool { pos: usize },
    InvalidStr { pos: usize },
    ExpectedWhitespace { pos: usize },
    UnexpectedEnd,
}