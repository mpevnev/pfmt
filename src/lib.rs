/*! 
 * # Overview
 * This library provides a powerful and simple way to format your data. At the
 * core of it lie two traits: `Fmt`, which describes a formattable field, and
 * `FormatTable`, which is a collection of `Fmt`s. At the moment, `FormatTable`
 * is implemented for `HashMap`s with keys of `String` and values of `Fmt`
 * (which allows to keep any kind of `Fmt` to be kept in it, but required some
 * explicit type annotations to help out the compiler) and for `Vec`s of
 * `Fmt`s.
 *
 * So once you have a format table of your choice, the only thing you need to
 * do is to 
 * `let maybe_string = format_table.format("see Syntax section");`
 * If you feel confident that an error is not possible (say, the format string
 * is hard-coded, as in example above, but see 'Errors' section below), just
 * `.unwrap` the `Result` you've got.
 *
 * Note that unlike with `format!`, the format strings is not required to be
 * static. Non-static strings just carry a bit more risk of producing an error.
 *
 * # Format string syntax
 * Format strings are strings with some placeholders embedded in them. A
 * placeholder is a request for a format table to find a named `Fmt` and use
 * it here, applying flags and options from that placeholder. A placeholder
 * looks like this:
 * `"{name or (for `Vec` tables) index:flags:option1=value:option2=value...}"`
 * A placeholder is surrounded by `{}` (and if you need them in the
 * non-placeholder parts of your string, escape them with `\`), and must have a
 * name. The rest (starting with the first `:`, is optional). Flags are the
 * part between the first and the second colons, and must be specified if you
 * want to use options, which come later and are separated by colons. Each
 * options takes the form `name = value`, where both trailing and leading
 * whitespace will be stripped from the name and the value. An empty name is an
 * error. An empty value is not. Colons are not allowed at the moment in either
 * names or values. There can be as many options as you want.
 *
 * Currently, `{` and `}` are not allowed in placeholder parts, either escaped
 * or not. This will be, eventually, lifted.
 *
 * # Examples
 * Let's start with something boring:
 * ```
 * extern crate pfmt;
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
 * extern crate pfmt;
 * use std::collections::HashMap;
 * use pfmt::{Fmt, FormatTable};
 *
 * let input = "a_really_long_string";
 * let mut table: HashMap<String, &Fmt> = HashMap::new();
 * table.insert("s".to_string(), &input);
 * let s = table.format("fixed width: {s::truncate=r5}").unwrap();
 * assert!(s == "fixed width: a_rea");
 * ```
 * Can't decide if you want your booleans as "true"/"false", or "yes"/"no"?
 * Easy:
 * ```
 * extern crate pfmt;
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
 * # Common options
 * Most pre-made implementation of `Fmt` honor several common options. Here's
 * a list of them, with detailed info available further in this section:
 * * `just`
 * * `truncate`
 * * `width`
 *
 * ## `just`: `{'l', 'c', 'r'}`
 * Controls justification of the field. If `l`, the field is left-justified, if
 * `c`, it is centered, and if `r` - right-justified. Only has effect if
 * `width` is used.
 *
 * ## `truncate`: `{'l', 'r'} + non-negative integer`
 * Controls truncation of the field. If begins with `l`, left part of the
 * field that doesn't fit is truncated, if begins with `r` - the right part is 
 * removed instead. Note that `"l0"` is not actually forbidden, just very
 * useless.
 */

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
    fn has_fmt(&self, name: &str) -> bool {
        if let Ok(index) = name.parse::<usize>() {
            index < self.len()
        } else {
            false
        }
    }
}

/* ---------- implementations of Fmt for standard types ---------- */

/// This instance is aware of the following flags:
/// * `y`, which changes the output from true/false to yes/no;
/// * `Y`, which changes the output to Y/N.
/// Common options are recognised.
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

/// This instance has no special flags.
/// Common options are recognised.
impl<'a> Fmt for &'a str {
    fn format(&self, _flags: &[char], options: &HashMap<String, String>)
        -> Result<String, SingleFmtError>
        {
            let mut s = self.to_string();
            util::apply_common_options(&mut s, options)?;
            Ok(s)
        }
    fn size_hint(&self, _flags: &[char], _options: &HashMap<String, String>) -> usize {
        self.len()
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
        name general;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt, FormattingError};

        test unknown_fmt() {
            let table: HashMap<String, &Fmt> = HashMap::new();
            let s = table.format("i = {i}");
            assert_that!(&s, eq(Err(FormattingError::UnknownFmt("i".to_string().clone()))));
        }

        test integers_simple_1() {
            let i = 1;
            let j = 23;
            let mut table: HashMap<String, &Fmt> = HashMap::new();
            table.insert("i".to_string(), &i);
            table.insert("j".to_string(), &j);
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
            let mut table: HashMap<String, &Fmt> = HashMap::new();
            table.insert("a".to_string(), &a);
            table.insert("b".to_string(), &b);
            let s = table.format("{a}, {b:y}, {b:Y}").unwrap();
            assert_that!(&s.as_str(), eq("true, no, N"));
        }

    }

    test_suite! {
        name common_options;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt, FormattingError};

        test width_left() {
            let string = "foobar";
            let mut table: HashMap<String, &Fmt> = HashMap::new();
            table.insert("s".to_string(), &string);
            let s = table.format("{s::width=10:just=l}").unwrap();
            assert_that!(&s.as_str(), eq("foobar    "));
        }

        test width_right() {
            let string = "foobar";
            let mut table: HashMap<String, &Fmt> = HashMap::new();
            table.insert("s".to_string(), &string);
            let s = table.format("{s::width=10:just=r}").unwrap();
            assert_that!(&s.as_str(), eq("    foobar"));
        }

        test width_center() {
            let string = "foobar";
            let mut table: HashMap<String, &Fmt> = HashMap::new();
            table.insert("s".to_string(), &string);
            let s = table.format("{s::width=10:just=c}").unwrap();
            assert_that!(&s.as_str(), eq("  foobar  "));
        }

        test truncate_left() {
            let string = "1234567890";
            let mut table: HashMap<String, &Fmt> = HashMap::new();
            table.insert("s".to_string(), &string);
            let s = table.format("{s::truncate=l5}").unwrap();
            assert_that!(&s.as_str(), eq("67890"));
        }
        
        test truncate_right() {
            let string = "1234567890";
            let mut table: HashMap<String, &Fmt> = HashMap::new();
            table.insert("s".to_string(), &string);
            let s = table.format("{s::truncate=r5}").unwrap();
            assert_that!(&s.as_str(), eq("12345"));
        }

    }

}
