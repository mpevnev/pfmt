/*! 
 * # Overview
 *
 * # Examples
 * Let's start with something boring:
 * ```
 * use std::collections::HashMap;
 * use pfmt::{Fmt, FormatTable};
 *
 * let i = 2;
 * let j = 5;
 * let mut table: HashMap<String, &Fmt> = HashMap::new();
 * table.insert("i".to_string(), &i);
 * table.insert("j".to_string(), &j);
 * let s = table.format("i = {i}, j = {j}").unwrap();
 * assert!(s == "i = 2, j = 5");
 * ```
 * I can do that with `format!` too. Let's see:
 * ```
 * use std::collections::HashMap;
 * use pfmt::{Fmt, FormatTable};
 *
 * let input = "a_really_long_string";
 * let mut table: HashMap<String, &Fmt> = HashMap::new();
 * table.insert("s".to_string(), &input);
 * // (note an escaped colon)
 * let s = table.format("fixed width\\: {s::truncate=r5}").unwrap();
 * assert!(s == "fixed width: a_rea");
 * ```
 * Can't decide if you want your booleans as "true"/"false", or "yes"/"no"?
 * Easy:
 * ```
 * use std::collections::HashMap;
 * use pfmt::{Fmt, FormatTable};
 *
 * let a = true;
 * let b = false;
 * let mut table: HashMap<String, &Fmt> = HashMap::new();
 * table.insert("a".to_string(), &a);
 * table.insert("b".to_string(), &b);
 * let s = table.format("{a}, {b:y}, {b:Y}").unwrap();
 * assert!(s == "true, no, N");
 * ```
 * There are more flags and options, either common or type-specific. See
 * documentation on each implementation of `Fmt` and the section "Common
 * options" below.
 *
 * # Errors
 * `format` method on `FormatTables` returns a `Result<String,
 * FormattingError>`. There are three primary types of these: parsing errors
 * which occur when the format string is not well-formed, errors arising from
 * usage of unknown options and flags or options with invalid values, and
 * finally errors due to requesting `Fmt`s that are missing in the table.
 *
 * With hard-coded format strings and rigid format tables, most of these can be
 * safely ignored, so `unwrap()` away.
 *
 * # Common options
 * Most pre-made implementation of `Fmt` honor several common options. Here's
 * a list of them, with detailed info available further in this section:
 * * `truncate`
 * * `width`
 *
 * ## `truncate`: `{'l', 'r'} + non-negative integer`
 * Controls truncation of the field. If begins with `l`, left part of the
 * field that doesn't fit is truncated, if begins with `r` - the right part is 
 * removed instead. Note that `"l0"` is not actually forbidden, just very
 * useless.
 *
 * ## `width`: `{'l', 'c', 'r'} + non-negative integer`
 * Controls the width of the field. Has no effect if the field is already wider
 * than the value supplied. If starts with "`l`", the field will be
 * left-justified. If starts with "`c`", the field will be centered. If starts
 * with "`r`", the field will be right-justified.
 */

#[cfg(test)] #[macro_use] extern crate galvanic_assert;
#[cfg(test)] #[macro_use] extern crate galvanic_test;

extern crate num;

use std::borrow::Borrow;
use std::collections::HashMap;

use parse::{parse, Piece, ParseError};

mod parse;

pub mod util;

/* ---------- base traits ---------- */

pub trait Fmt {
    fn format(&self, 
              args: &[String],
              flags: &[char],
              options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>;
}

pub trait FormatTable {
    fn get_fmt(&self, name: &str) -> Option<&Fmt>;
    fn produce_fmt(&self, name: &str) -> Option<Box<Fmt>>;

    fn format<'a>(&'a self, input: &'a str) -> Result<String, FormattingError> {
        let pieces = parse(input)?;
        let mut res = String::new();
        for piece in pieces.iter() {
            res.push_str(&self.format_one(piece)?);
        }
        Ok(res)
    }

    fn format_one(&self, piece: &Piece) -> Result<String, FormattingError> {
        match piece {
            Piece::Literal(s) => Ok(s.clone()),
            Piece::Placeholder(name, args, flags, opts) => {
                if let Some(fmt) = self.get_fmt(&name) {
                    self.format_unwrapped(fmt, args, flags, opts)
                } else if let Some(fmtbox) = self.produce_fmt(&name) {
                    self.format_unwrapped(fmtbox.borrow(), args, flags, opts)
                } else {
                    Err(FormattingError::UnknownFmt(name.clone()))
                }
            }
        }
    }

    fn format_unwrapped(&self, fmt: &Fmt, args: &[Piece], flags: &[char],
                        opts: &HashMap<String, Piece>)
        -> Result<String, FormattingError>
    {
        let mut string_args = Vec::new();
        for arg in args.iter() {
            string_args.push(self.format_one(arg)?);
        }
        let mut string_opts = HashMap::new();
        for (key, value) in opts.iter() {
            string_opts.insert(key.clone(), self.format_one(value)?);
        }
        Ok(fmt.format(&string_args, &flags, &string_opts)?)
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
    EmptyName(String),
    UnterminatedArgumentList(String),
    UnterminatedPlaceholder(String),
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
            ParseError::EmptyName(s) => FormattingError::EmptyName(s),
            ParseError::UnterminatedArgumentList(s) =>
                FormattingError::UnterminatedArgumentList(s),
            ParseError::UnterminatedPlaceholder(s) =>
                FormattingError::UnterminatedPlaceholder(s)
        }
    }
}

/* ---------- implementations of FormatTable for standard types ---------- */

impl<B: Borrow<Fmt>> FormatTable for HashMap<String, B> {
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        self.get(name).map(|r| r.borrow())
    }
    fn produce_fmt(&self, name: &str) -> Option<Box<Fmt>> {
        None
    }
}

impl<'a, B: Borrow<Fmt>> FormatTable for HashMap<&'a str, B> {
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        self.get(name).map(|r| r.borrow())
    }
    fn produce_fmt(&self, name: &str) -> Option<Box<Fmt>> {
        None
    }
}

impl<B: Borrow<Fmt>> FormatTable for Vec<B> {
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        if let Ok(index) = name.parse::<usize>() {
            if index < self.len() {
                Some(self[index].borrow())
            } else {
                None
            }
        } else {
            None
        }
    }
    fn produce_fmt(&self, name: &str) -> Option<Box<Fmt>> {
        None
    }
}

/* ---------- implementations of Fmt for standard types ---------- */

/// This instance is aware of the following flags:
/// * `y`, which changes the output from true/false to yes/no;
/// * `Y`, which changes the output to Y/N.
/// Common options are recognised.
impl Fmt for bool {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
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
}

/// This instance has no special flags.
/// Common options are recognised.
impl Fmt for char {
    fn format(&self, _args: &[String], _flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = self.to_string();
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which forces display of the sign;
/// * `e`, which changes the output to the scientific, or exponential,
/// notation.
/// Common options are recognised.
/// Common numeric options are also recognised.
/*
impl Fmt for f32 {
    fn format(&self, flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let exp = flags.contains(&'e');
            let mut s = util::float_to_string(*self, exp, options)?;
            util::apply_common_numeric_options(&mut s, flags, options)?;
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}
*/

/// This instance has no special flags.
/// Common options are recognised.
impl<'a> Fmt for &'a str {
    fn format(&self, _args: &[String], _flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = self.to_string();
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance has no special flags.
/// Common options are recognised.
impl Fmt for String {
    fn format(&self, _args: &[String], _flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = self.clone();
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

impl Fmt for i32 {
    fn format(&self, _args: &[String], _flags: &[char], _options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            Ok(self.to_string())
        }
}

/* ---------- tests ---------- */

#[cfg(test)]
mod tests {
    test_suite! {
        name general;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt, FormattingError};

        test unknown_fmt() {
            let table: HashMap<&str, &Fmt> = HashMap::new();
            let s = table.format("i = {i}");
            assert_that!(&s, eq(Err(FormattingError::UnknownFmt("i".to_string()))));
        }

        test integers_simple_1() {
            let i = 1;
            let j = 23;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("i", &i);
            table.insert("j", &j);
            let s = table.format("i = {i}, j = {j}").unwrap();
            assert_that!(&s.as_str(), eq("i = 1, j = 23"));
        }

    }

    test_suite! {
        name boolean;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt, FormattingError};

        test flags() {
            let a = true;
            let b = false;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("a", &a);
            table.insert("b", &b);
            let s = table.format("{a}, {b:y}, {b:Y}").unwrap();
            assert_that!(&s.as_str(), eq("true, no, N"));
        }

    }

    test_suite! {
        name char;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt, FormattingError};

        test boring() {
            let c = 'z';
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("c", &c);
            let s = table.format("{c}, {c::width=l5}!").unwrap();
            assert_that!(&s.as_str(), eq("z, z    !"));
        }

    }

    /*
    test_suite! {
        name floats;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt, FormattingError};

        test exp_boring_1() {
            let f: f32 = 1_000_000.0;
            let mut table: HashMap<String, &Fmt> = HashMap::new();
            table.insert("f".to_string(), &f);
            let s = table.format("{f:e+:prec=-1}").unwrap();
            assert_that!(&s.as_str(), eq("+1e6"));
        }

    }
    */

    test_suite! {
        name common_options;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt, FormattingError};

        test width_left() {
            let string = "foobar";
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("s", &string);
            let s = table.format("{s::width=l10}").unwrap();
            assert_that!(&s.as_str(), eq("foobar    "));
        }

        test width_right() {
            let string = "foobar";
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("s", &string);
            let s = table.format("{s::width=r10}").unwrap();
            assert_that!(&s.as_str(), eq("    foobar"));
        }

        test width_center() {
            let string = "foobar";
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("s", &string);
            let s = table.format("{s::width=c10}").unwrap();
            assert_that!(&s.as_str(), eq("  foobar  "));
        }

        test truncate_left() {
            let string = "1234567890";
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("s", &string);
            let s = table.format("{s::truncate=l5}").unwrap();
            assert_that!(&s.as_str(), eq("67890"));
        }
        
        test truncate_right() {
            let string = "1234567890";
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("s", &string);
            let s = table.format("{s::truncate=r5}").unwrap();
            assert_that!(&s.as_str(), eq("12345"));
        }

    }

}
