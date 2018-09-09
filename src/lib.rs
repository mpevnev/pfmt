#[cfg(test)] #[macro_use] extern crate galvanic_assert;
#[cfg(test)] #[macro_use] extern crate galvanic_test;

use std::collections::HashMap;
use std::borrow::Borrow;

use parse::{parse, Piece, ParseError};

mod parse;

/* ---------- base traits ---------- */

pub trait Fmt {
    fn format(&self, flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFormatError>;
    fn size_hint(&self, flags: &[char], options: &HashMap<String, String>) -> usize;
}

pub trait FormatTable {
    fn get_fmt(&self, name: &str) -> Option<&Fmt>;
    fn has_fmt(&self, name: &str) -> bool;
}

/* ---------- the thing that does the interpolation ---------- */

impl FormatTable {
    pub fn format<'a>(&'a self, input: &'a str) -> Result<String, FormattingError> {
        let pieces = parse(input)?;
        for piece in pieces.iter() {
            if let Piece::Placeholder(name, _, _) = piece {
                if !self.has_fmt(name) {
                    return Err(FormattingError::UnknownFmt(name.clone()))
                }
            }
        }
        let total_len = pieces.iter().fold(0, |total, piece| {
            match piece {
                Piece::Literal(s) => total + s.len(),
                Piece::Placeholder(name, flags, opts) => {
                    let f = self.get_fmt(name).unwrap();
                    total + f.size_hint(flags, opts)
                }
            }
        });
        let mut res = String::with_capacity(total_len);
        for piece in pieces.iter() {
            match piece {
                Piece::Literal(s) => res.push_str(s),
                Piece::Placeholder(name, flags, opts) => {
                    let f = self.get_fmt(name).unwrap();
                    res.push_str(&f.format(flags, opts)?);
                }
            }
        }
        Ok(res)
    }
}

/* ---------- errors ---------- */

/// Errors that happen in individual formattables.
#[derive(Debug)]
pub enum SingleFormatError {
    UnknownFlag(char),
    UnknownOption(String),
    InvalidOptionValue(String, String)
}

/// Any error that can happen during formatting.
#[derive(Debug)]
pub enum FormattingError {
    ParseError(ParseError),
    SingleFormatError(SingleFormatError),
    UnknownFmt(String)
}

impl From<SingleFormatError> for FormattingError {
    fn from(err: SingleFormatError) -> Self {
        FormattingError::SingleFormatError(err)
    }
}

impl From<ParseError> for FormattingError {
    fn from(err: ParseError) -> Self {
        FormattingError::ParseError(err)
    }
}

/* ---------- implementations of FormatTable for standard types ---------- */

impl<B: Borrow<Fmt>> FormatTable for HashMap<String, B> {
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        self.get(name).map(|r| r.borrow())
    }
    fn has_fmt(&self, name: &str) -> bool {
        self.contains_key(name)
    }
}

/* ---------- implementations of Fmt for standard types ---------- */

impl Fmt for i32 {
    fn format(&self, flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFormatError>
        {
            Ok(self.to_string())
        }
    fn size_hint(&self, flags: &[char], options: &HashMap<String, String>) -> usize {
        12
    }
}
