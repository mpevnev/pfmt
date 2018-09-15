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
    fn produce_fmt(&self, _name: &str) -> Option<Box<Fmt>> {
        None
    }

    fn format(&self, input: &str) -> Result<String, FormattingError> {
        let pieces = parse(input)?;
        let mut res = String::new();
        for piece in pieces.iter() {
            res.push_str(&format_one(&self, piece)?);
        }
        Ok(res)
    }
}

fn format_one<T: FormatTable>(table: &T, piece: &Piece) -> Result<String, FormattingError> {
    match piece {
        Piece::Literal(s) => Ok(s.clone()),
        Piece::Placeholder(name, args, flags, opts) => {
            if let Some(fmt) = table.get_fmt(&name) {
                format_unwrapped(table, fmt, args, flags, opts)
            } else if let Some(fmtbox) = table.produce_fmt(&name) {
                format_unwrapped(table, fmtbox.borrow(), args, flags, opts)
            } else {
                Err(FormattingError::UnknownFmt(name.clone()))
            }
        }
    }
}

fn format_unwrapped<T: FormatTable>(table: &T, fmt: &Fmt, args: &[Piece], flags: &[char],
                    opts: &HashMap<String, Piece>)
    -> Result<String, FormattingError>
{
    let mut string_args = Vec::new();
    for arg in args.iter() {
        string_args.push(format_one(table, arg)?);
    }
    let mut string_opts = HashMap::new();
    for (key, value) in opts.iter() {
        string_opts.insert(key.clone(), format_one(table, value)?);
    }
    Ok(fmt.format(&string_args, &flags, &string_opts)?)
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

impl<'a, T: FormatTable + ?Sized> FormatTable for &'a T {
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        (*self).get_fmt(name)
    }
    fn produce_fmt(&self, name: &str) -> Option<Box<Fmt>> {
        (*self).produce_fmt(name)
    }
}

impl<B: Borrow<Fmt>> FormatTable for HashMap<String, B> {
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        self.get(name).map(|r| r.borrow())
    }
    fn produce_fmt(&self, _name: &str) -> Option<Box<Fmt>> {
        None
    }
}

impl<'a, B: Borrow<Fmt>> FormatTable for HashMap<&'a str, B> {
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        self.get(name).map(|r| r.borrow())
    }
    fn produce_fmt(&self, _name: &str) -> Option<Box<Fmt>> {
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
    fn produce_fmt(&self, _name: &str) -> Option<Box<Fmt>> {
        None
    }
}

impl<A, B> FormatTable for (A, B) 
    where A: FormatTable,
          B: FormatTable
{
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        self.0.get_fmt(name)
            .or_else(|| self.1.get_fmt(name))
    }
    fn produce_fmt(&self, name: &str) -> Option<Box<Fmt>> {
        self.0.produce_fmt(name)
            .or_else(|| self.1.produce_fmt(name))
    }
}

impl<A, B, C> FormatTable for (A, B, C)
    where A: FormatTable,
          B: FormatTable,
          C: FormatTable
{
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        self.0.get_fmt(name)
            .or_else(|| self.1.get_fmt(name))
            .or_else(|| self.2.get_fmt(name))
    }
    fn produce_fmt(&self, name: &str) -> Option<Box<Fmt>> {
        self.0.produce_fmt(name)
            .or_else(|| self.1.produce_fmt(name))
            .or_else(|| self.2.produce_fmt(name))
    }
}

impl<A, B, C, D> FormatTable for (A, B, C, D)
    where A: FormatTable,
          B: FormatTable,
          C: FormatTable,
          D: FormatTable
{
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        self.0.get_fmt(name)
            .or_else(|| self.1.get_fmt(name))
            .or_else(|| self.2.get_fmt(name))
            .or_else(|| self.3.get_fmt(name))
    }
    fn produce_fmt(&self, name: &str) -> Option<Box<Fmt>> {
        self.0.produce_fmt(name)
            .or_else(|| self.1.produce_fmt(name))
            .or_else(|| self.2.produce_fmt(name))
            .or_else(|| self.3.produce_fmt(name))
    }
}

impl<A, B, C, D, E> FormatTable for (A, B, C, D, E)
    where A: FormatTable,
          B: FormatTable,
          C: FormatTable,
          D: FormatTable,
          E: FormatTable
{
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        self.0.get_fmt(name)
            .or_else(|| self.1.get_fmt(name))
            .or_else(|| self.2.get_fmt(name))
            .or_else(|| self.3.get_fmt(name))
            .or_else(|| self.4.get_fmt(name))
    }
    fn produce_fmt(&self, name: &str) -> Option<Box<Fmt>> {
        self.0.produce_fmt(name)
            .or_else(|| self.1.produce_fmt(name))
            .or_else(|| self.2.produce_fmt(name))
            .or_else(|| self.3.produce_fmt(name))
            .or_else(|| self.4.produce_fmt(name))
    }
}

impl<A, B, C, D, E, F> FormatTable for (A, B, C, D, E, F)
    where A: FormatTable,
          B: FormatTable,
          C: FormatTable,
          D: FormatTable,
          E: FormatTable,
          F: FormatTable
{
    fn get_fmt(&self, name: &str) -> Option<&Fmt> {
        self.0.get_fmt(name)
            .or_else(|| self.1.get_fmt(name))
            .or_else(|| self.2.get_fmt(name))
            .or_else(|| self.3.get_fmt(name))
            .or_else(|| self.4.get_fmt(name))
            .or_else(|| self.5.get_fmt(name))
    }
    fn produce_fmt(&self, name: &str) -> Option<Box<Fmt>> {
        self.0.produce_fmt(name)
            .or_else(|| self.1.produce_fmt(name))
            .or_else(|| self.2.produce_fmt(name))
            .or_else(|| self.3.produce_fmt(name))
            .or_else(|| self.4.produce_fmt(name))
            .or_else(|| self.5.produce_fmt(name))
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
impl Fmt for f32 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut res: String;
            if flags.contains(&'e') {
                res = util::float_to_exp(*self, options)?;
            } else {
                res = util::float_to_normal(*self, options)?;
            }
            util::add_sign(&mut res, *self, flags)?;
            util::apply_common_options(&mut res, options)?;
            Ok(res)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which forces display of the sign;
/// * `e`, which changes the output to scientific format.
/// Common options are recognized.
/// Common numeric options are also recognized.
impl Fmt for f64 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut res: String;
            if flags.contains(&'e') {
                res = util::float_to_exp(*self, options)?;
            } else {
                res = util::float_to_normal(*self, options)?;
            }
            util::add_sign(&mut res, *self, flags)?;
            util::apply_common_options(&mut res, options)?;
            Ok(res)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which forces display of the sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes output hexadecimal;
/// Common and common numeric options are recognized.
impl Fmt for i8 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            util::add_sign(&mut s, *self, flags)?;
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which forces display of the sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes output hexadecimal;
/// Common and common numeric options are recognized.
impl Fmt for i16 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            util::add_sign(&mut s, *self, flags)?;
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which forces display of the sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes output hexadecimal;
/// Common and common numeric options are recognized.
impl Fmt for i32 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            util::add_sign(&mut s, *self, flags)?;
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which forces display of the sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes output hexadecimal;
/// Common and common numeric options are recognized.
impl Fmt for i64 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            util::add_sign(&mut s, *self, flags)?;
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which forces display of the sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes output hexadecimal;
/// Common and common numeric options are recognized.
impl Fmt for i128 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            util::add_sign(&mut s, *self, flags)?;
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which forces display of the sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes output hexadecimal;
/// Common and common numeric options are recognized.
impl Fmt for isize {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            util::add_sign(&mut s, *self, flags)?;
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

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

/// This instance is aware of the following flags:
/// * `+`, which add a leading plus sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes the output hexadecimal.
/// Common and common numeric options are recognised.
impl Fmt for u8 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            if flags.contains(&'+') {
                s.insert(0, '+');
            }
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which add a leading plus sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes the output hexadecimal.
/// Common and common numeric options are recognised.
impl Fmt for u16 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            if flags.contains(&'+') {
                s.insert(0, '+');
            }
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which add a leading plus sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes the output hexadecimal.
/// Common and common numeric options are recognised.
impl Fmt for u32 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            if flags.contains(&'+') {
                s.insert(0, '+');
            }
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which add a leading plus sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes the output hexadecimal.
/// Common and common numeric options are recognised.
impl Fmt for u64 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            if flags.contains(&'+') {
                s.insert(0, '+');
            }
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which add a leading plus sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes the output hexadecimal.
/// Common and common numeric options are recognised.
impl Fmt for u128 {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            if flags.contains(&'+') {
                s.insert(0, '+');
            }
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/// This instance is aware of the following flags:
/// * `+`, which add a leading plus sign;
/// * `b`, which makes the output binary;
/// * `o`, which makes the output octal;
/// * `p`, which in combination with '`b`', '`o`' or '`x`' adds a base prefix
/// to the output.
/// * `x`, which makes the output hexadecimal.
/// Common and common numeric options are recognised.
impl Fmt for usize {
    fn format(&self, _args: &[String], flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = util::int_to_str(*self, flags, options)?;
            if flags.contains(&'+') {
                s.insert(0, '+');
            }
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
}

/* ---------- tests for Fmts ---------- */

#[cfg(test)]
mod fmt_tests {
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
        use {FormatTable, Fmt};

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
        use {FormatTable, Fmt};

        test boring() {
            let c = 'z';
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("c", &c);
            let s = table.format("{c}, {c::width=l5}!").unwrap();
            assert_that!(&s.as_str(), eq("z, z    !"));
        }

    }

    test_suite! {
        name floats;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt};

        test exp_precision_neg() {
            let f: f32 = 1_234_567.891;
            let mut table: HashMap<String, &Fmt> = HashMap::new();
            table.insert("f".to_string(), &f);
            let s = table.format("{f:e+:prec=-1}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("+1.23457e6"));
        }

        test exp_precision_pos() {
            let f: f32 = 1000.123;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f:e:prec=2}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1.00012e3"));
        }

        test exp_negative_power() {
            let f: f32 = 0.0625;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f:e}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("6.25e-2"));
        }

        test norm_rounding_up() {
            let f = 0.2;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f::round=up:prec=0}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1"));
        }

        test norm_rounding_down() {
            let f = 0.8;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f::round=down:prec=0}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("0"));
        }

        test norm_rounding_usual() {
            let f = 0.5;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f::round=nearest:prec=0}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1"));
        }

        test negative() {
            let f = -1.0;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("-1"));
        }

    }

    test_suite! {
        name integers;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt};

        test basic() {
            let i = 10;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("10"));
        }

        test different_bases() {
            let i = 11;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i:b}, {i:o}, {i:x}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1011, 13, b"));
        }

        test base_prefixes() {
            let i = 1;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i:bp}, {i:op}, {i:xp}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("0b1, 0o1, 0x1"));
        }

        test bases_for_negative_numbers() {
            let i = -11;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i:b}, {i:o}, {i:x}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("-1011, -13, -b"));
        }

        test rounding() {
            let i = 1235;
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i::prec=-1}, {i::prec=-2}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1240, 1200"));
        }

    }

    test_suite! {
        name common_options;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt};

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

/* ---------- tests for FormatTables ---------- */

#[cfg(test)]
mod table_tests {
    test_suite! {
        name vec;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt, FormattingError};

        test unknown_fmt_1() {
            let i = 1;
            let j = 2;
            let table: Vec<&Fmt> = vec![&i, &j];
            let err = table.format("{10}").expect_err("Unexpectedly found a fmt");
            assert_that!(&err, eq(FormattingError::UnknownFmt("10".to_string())));
        }

        test unknown_fmt_2() {
            let i = 1;
            let j = 2;
            let table: Vec<&Fmt> = vec![&i, &j];
            let err = table.format("{-3}").expect_err("Unexpectedly found a fmt");
            assert_that!(&err, eq(FormattingError::UnknownFmt("-3".to_string())));
        }

        test boring() {
            let i = 1;
            let j = 2;
            let table: Vec<&Fmt> = vec![&i, &j];
            let s = table.format("{0}, {1}").expect("Failed to format");
            assert_that!(&s, eq("1, 2".to_string()));
        }

    }

    test_suite! {
        name tuples;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt};

        test defaulting() {
            let a: Vec<Box<Fmt>> = vec![Box::new(-1), Box::new(-2)];
            let b: Vec<Box<Fmt>> = (0..10_i32).map(|i| Box::new(i) as Box<Fmt>).collect();
            let s = (a, b).format("{5}").expect("Failed");
            assert_that!(&s, eq("5".to_string()));
        }

        test precedence() {
            let a: Vec<Box<Fmt>> = vec![Box::new(1)];
            let b: Vec<Box<Fmt>> = vec![Box::new(10)];
            let s = (a, b).format("{0}").expect("Failed");
            assert_that!(&s, eq("1".to_string()));
        }

    }

}
