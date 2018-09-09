#[cfg(test)] #[macro_use] extern crate galvanic_assert;
#[cfg(test)] #[macro_use] extern crate galvanic_test;

use std::borrow::Borrow;
use std::collections::HashMap;

use parse::{parse, Piece, ParseError};

mod parse;

pub mod util;

/* ---------- base traits ---------- */

pub trait Fmt {
    fn format(&self, flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>;
    fn size_hint(&self, _flags: &[char], _options: &HashMap<String, String>) -> usize {
        0
    }
}

pub trait FormatTable {
    fn get_fmt(&self, name: &str) -> Option<&Fmt>;
    fn has_fmt(&self, name: &str) -> bool;

    fn format<'a>(&'a self, input: &'a str) -> Result<String, FormattingError> {
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
#[derive(Debug, PartialEq)]
pub enum SingleFmtError {
    UnknownFlag(char),
    UnknownOption(String),
    InvalidOptionValue(String, String)
}

/// Any error that can happen during formatting.
#[derive(Debug, PartialEq)]
pub enum FormattingError {
    // Parsing errors.
    UnbalancedBrackets(),
    NestedFmts(),
    MissingOpeningBracket(),
    MissingFmtName(),
    MissingOptionName(),
    // Errors from single Fmts.
    UnknownFlag(char),
    UnknownOption(String),
    InvalidOptionValue(String, String),
    // General errors.
    UnknownFmt(String)
}

impl From<SingleFmtError> for FormattingError {
    fn from(err: SingleFmtError) -> Self {
        match err {
            SingleFmtError::UnknownFlag(c) => FormattingError::UnknownFlag(c),
            SingleFmtError::UnknownOption(s) => FormattingError::UnknownOption(s),
            SingleFmtError::InvalidOptionValue(opt, val) =>
                FormattingError::InvalidOptionValue(opt, val)
        }
    }
}

impl From<ParseError> for FormattingError {
    fn from(err: ParseError) -> Self {
        match err {
            ParseError::UnbalancedBrackets() => FormattingError::UnbalancedBrackets(),
            ParseError::NestedPlaceholders() => FormattingError::NestedFmts(),
            ParseError::MissingOpeningBracket() => FormattingError::MissingOpeningBracket(),
            ParseError::MissingPlaceholderName() => FormattingError::MissingFmtName(),
            ParseError::MissingOptionName() => FormattingError::MissingOptionName()
        }
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

/// This instance is aware of the following flags:
/// * `y`, which changes the output from true/false to yes/no;
/// * `Y`, which changes the output to Y/N.
/// Common justification options are available.
impl Fmt for bool {
    fn format(&self, flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut res = if *self {
                if flags.contains(&'y') {
                    "yes".to_string()
                } else if flags.contains(&'Y') {
                    "Y".to_string()
                } else {
                    "true".to_string()
                }
            } else {
                if flags.contains(&'y') {
                    "no".to_string()
                } else if flags.contains(&'Y') {
                    "N".to_string()
                } else {
                    "false".to_string()
                }
            };
            util::apply_common_options(&mut res, options)?;
            Ok(res)
        }
    fn size_hint(&self, _flags: &[char], _options: &HashMap<String, String>) -> usize {
        5
    }
}

impl Fmt for i32 {
    fn format(&self, flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {

            Ok(self.to_string())
        }
    fn size_hint(&self, flags: &[char], options: &HashMap<String, String>) -> usize {
        12
    }
}

/* ---------- tests ---------- */

#[cfg(test)]
mod tests {
    test_suite! {
        name main_formatting_tests;

        use std::collections::HashMap;

        use galvanic_assert::matchers::*;

        use {FormatTable, Fmt, FormattingError};

        test unknown_fmt() {
            let table: HashMap<String, &Fmt> = HashMap::new();
            let s = table.format("i = {i}");
            assert_that!(&s, eq(Err(FormattingError::UnknownFmt("i".to_string().clone()))));
        }

        test fmt_error() {
            let i = 32;
            let mut table: HashMap<String, &Fmt> = HashMap::new();
            table.insert("i".to_string(), &i);
            let s = table.format("i = {i::doesnt_exist=foobar}");
            assert_that!(&s, eq(Err(FormattingError::UnknownOption("doesnt_exist"
                                                                   .to_string()
                                                                   .clone()))));
        }

        test integers_simple_1() {
            let i = 1;
            let j = 23;
            let mut table: HashMap<String, &Fmt> = HashMap::new();
            table.insert("i".to_string(), &i);
            table.insert("j".to_string(), &j);
            let s = table.format("i = {i}, j = {j}").expect("Failed to format");
            assert_that!(&s, eq("i = 1, j = 23".to_string().clone()));
        }

    }
}
