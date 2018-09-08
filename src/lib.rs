use std::collections::HashMap;

use parse::{parse, Piece, ParseError};

mod parse;

/* ---------- base trait ---------- */

pub trait Fmt {
    fn format(&self, flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFormatError>;
    fn size_hint(&self, flags: &[char], options: &HashMap<String, String>) -> usize;
}

/* ---------- the thing that does the interpolation ---------- */

pub struct FormatTable {
    fmtables: HashMap<String, Box<Fmt>>
}

impl FormatTable {
    pub fn format<'a>(&'a self, input: &'a str) -> Result<String, FormattingError> {
        let pieces = parse(input)?;
        for piece in pieces.iter() {
            if let Piece::Placeholder(name, _, _) = piece {
                if !self.fmtables.contains_key(name) {
                    return Err(FormattingError::UnknownFmt(name.clone()))
                }
            }
        }
        let total_len = pieces.iter().fold(0, |total, piece| {
            match piece {
                Piece::Literal(s) => total + s.len(),
                Piece::Placeholder(name, flags, opts) => {
                    let f = self.fmtables.get(name).unwrap();
                    total + f.size_hint(flags, opts)
                }
            }
        });
        let mut res = String::with_capacity(total_len);
        for piece in pieces.iter() {
            match piece {
                Piece::Literal(s) => res.push_str(s),
                Piece::Placeholder(name, flags, opts) => {
                    let f = self.fmtables.get(name).unwrap();
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

/* ---------- implementations of Fmt for standard types ---------- */
