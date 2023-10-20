#![warn(clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::missing_errors_doc)]
use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug)]
pub enum DecodeError {
    UnexpectedTag,
    ParseInt(std::num::ParseIntError),
    ParseUtf8(std::str::Utf8Error),
    LeadingZeroes,
    InvalidInput,
    EOF,
    InternalError(&'static str),
}

impl From<std::num::ParseIntError> for DecodeError {
    fn from(e: std::num::ParseIntError) -> Self {
        Self::ParseInt(e)
    }
}

impl From<std::str::Utf8Error> for DecodeError {
    fn from(e: std::str::Utf8Error) -> Self {
        Self::ParseUtf8(e)
    }
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedTag => write!(f, "unexpected tag")?,
            Self::ParseInt(e) => write!(f, "{e}")?,
            Self::ParseUtf8(e) => write!(f, "{e}")?,
            Self::LeadingZeroes => write!(f, "leading zeroes are not allowed on integers")?,
            Self::InvalidInput => write!(f, "input bytes cannot be decoded as bencode")?,
            Self::EOF => write!(f, "unexpected end of input")?,
            Self::InternalError(msg) => write!(f, "internal error: {msg}")?,
        }
        Ok(())
    }
}
impl std::error::Error for DecodeError {}

#[derive(PartialEq, Eq, Debug)]
pub enum Value<'a> {
    Integer(i64),
    Bytestring(&'a [u8]),
    List(Vec<Self>),
    Dictionary(HashMap<&'a [u8], Self>),
}

impl std::fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Integer(value) => write!(f, "{value}")?,
            Value::Bytestring(value) => {
                if let Ok(s) = std::str::from_utf8(value) {
                    write!(f, "{s}")?;
                } else {
                    for byte in *value {
                        write!(f, "{byte}")?;
                    }
                }
            }
            Value::List(items) => {
                write!(f, "[")?;
                for item in items {
                    write!(f, "{item}")?;
                }
                write!(f, "]")?;
            }
            Value::Dictionary(items) => {
                write!(f, "{{")?;
                for (k, v) in items {
                    let k = String::from_utf8_lossy(k);
                    write!(f, "{k}: {v}, ")?;
                }
                write!(f, "}}")?;
            }
        }
        Ok(())
    }
}

pub fn decode(input: &[u8]) -> Result<Value, DecodeError> {
    Decoder::new(input).decode()
}

struct Decoder<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Decoder<'a> {
    const fn new(input: &'a [u8]) -> Self {
        Self { input, pos: 0 }
    }

    fn decode(mut self) -> Result<Value<'a>, DecodeError> {
        self.decode_impl()
    }

    fn decode_impl(&mut self) -> Result<Value<'a>, DecodeError> {
        for parser in [
            Self::integer,
            Self::list,
            Self::dictionary,
            Self::bytestring,
        ] {
            if let Some(v) = self.try_parse(parser)? {
                return Ok(v);
            }
        }
        Err(DecodeError::InvalidInput)
    }

    fn try_parse<Parser>(&mut self, parser: Parser) -> Result<Option<Value<'a>>, DecodeError>
    where
        Parser: FnOnce(&mut Self) -> Result<Value<'a>, DecodeError>,
    {
        match parser(self) {
            Ok(v) => Ok(Some(v)),
            Err(DecodeError::UnexpectedTag) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn dictionary(&mut self) -> Result<Value<'a>, DecodeError> {
        self.expect(b"d")?;
        let mut items = HashMap::new();
        while self.input[self.pos] != b'e' {
            let Value::Bytestring(key) = self.bytestring()? else {
                return Err(DecodeError::InternalError(
                    "self.bytestring returned a non-bytestring value",
                ));
            };
            items.insert(key, self.decode_impl()?);
        }
        self.expect(b"e")?;
        Ok(Value::Dictionary(items))
    }

    fn list(&mut self) -> Result<Value<'a>, DecodeError> {
        self.expect(b"l")?;
        let mut values = vec![];
        while self.input[self.pos] != b'e' {
            values.push(self.decode_impl()?);
        }
        self.expect(b"e")?;
        Ok(Value::List(values))
    }

    fn integer(&mut self) -> Result<Value<'a>, DecodeError> {
        self.expect(b"i")?;
        let inner = self.take_until(b"e")?;
        if inner.len() > 1 && inner[0] == b'0' {
            return Err(DecodeError::LeadingZeroes);
        }
        let value = Value::Integer(std::str::from_utf8(inner)?.parse()?);
        self.expect(b"e")?;
        Ok(value)
    }

    fn bytestring(&mut self) -> Result<Value<'a>, DecodeError> {
        let length_bytes = self.take_while(|b| b.is_ascii_digit())?;
        let length: usize = std::str::from_utf8(length_bytes)?.parse()?;
        self.expect(b":")?;
        if length > self.input.len() - self.pos {
            return Err(DecodeError::EOF);
        }
        let value = Value::Bytestring(&self.input[self.pos..self.pos + length]);
        self.pos += length;
        Ok(value)
    }

    fn expect(&mut self, tag: &[u8]) -> Result<(), DecodeError> {
        if self.input[self.pos..].starts_with(tag) {
            self.pos += tag.len();
            Ok(())
        } else {
            Err(DecodeError::UnexpectedTag)
        }
    }

    fn take_until(&mut self, tag: &[u8]) -> Result<&[u8], DecodeError> {
        let mut end = self.pos;
        while !self.input[end..].starts_with(tag) {
            end += 1;
            if end >= self.input.len() {
                return Err(DecodeError::EOF);
            }
        }
        let inner = &self.input[self.pos..end];
        self.pos = end;
        Ok(inner)
    }

    fn take_while<Predicate>(&mut self, pred: Predicate) -> Result<&[u8], DecodeError>
    where
        Predicate: Fn(u8) -> bool,
    {
        let end = self.pos
            + self.input[self.pos..]
                .iter()
                .copied()
                .position(|b| !pred(b))
                .ok_or(DecodeError::EOF)?;
        let inner = &self.input[self.pos..end];
        self.pos = end;
        Ok(inner)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use testresult::TestResult;

    #[test]
    fn positive_integer() -> TestResult {
        assert_eq!(decode(b"i42e")?, Value::Integer(42));
        Ok(())
    }

    #[test]
    fn zero() -> TestResult {
        assert_eq!(decode(b"i0e")?, Value::Integer(0));
        Ok(())
    }

    #[test]
    fn negative_integer() -> TestResult {
        assert_eq!(decode(b"i-42e")?, Value::Integer(-42));
        Ok(())
    }

    #[test]
    fn unterminated_integer() {
        assert_eq!(decode(b"i42"), Err(DecodeError::EOF));
    }

    #[test]
    fn invalid_integer() {
        let e = "abc".parse::<i32>().unwrap_err();
        assert_eq!(decode(b"i4a2e"), Err(DecodeError::ParseInt(e)));
    }

    #[test]
    fn leading_zeroes() {
        assert_eq!(decode(b"i03e"), Err(DecodeError::LeadingZeroes));
    }

    #[test]
    fn bytestring() -> TestResult {
        assert_eq!(decode(b"4:spam")?, Value::Bytestring(b"spam"));
        Ok(())
    }

    #[test]
    fn bytestring_eof() {
        assert_eq!(decode(b"5:foo"), Err(DecodeError::EOF));
    }

    #[test]
    fn list() -> TestResult {
        assert_eq!(
            decode(b"l4:spami42ee")?,
            Value::List(vec![Value::Bytestring(b"spam"), Value::Integer(42)])
        );
        Ok(())
    }

    #[test]
    fn nested_list() -> TestResult {
        let result = decode(b"lllllllllllllllllllleeeeeeeeeeeeeeeeeeee")?;
        assert!(matches!(result, Value::List(_)));
        Ok(())
    }

    #[test]
    fn dictionary() -> TestResult {
        let mut expected_items = HashMap::<&[u8], Value>::new();
        expected_items.insert(b"bar", Value::Bytestring(b"spam"));
        expected_items.insert(b"foo", Value::Integer(42));
        assert_eq!(
            decode(b"d3:bar4:spam3:fooi42ee")?,
            Value::Dictionary(expected_items)
        );
        Ok(())
    }

    #[test]
    fn nested_dict() -> TestResult {
        let result = decode(b"d1:ad1:bd1:cd1:dd1:ed1:fd1:gd1:hd1:id1:jd1:ki1eeeeeeeeeeee")?;
        assert!(matches!(result, Value::Dictionary(_)));
        Ok(())
    }

    #[test]
    fn alice() -> TestResult {
        let result = decode(include_bytes!("../fixtures/alice.torrent"))?;
        assert!(matches!(result, Value::Dictionary(_)));
        Ok(())
    }
}
