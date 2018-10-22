/*!
 * # Overview
 * This library provides a flexible and powerful method to format data. At the
 * heart of the library lie two traits: `Fmt`, for things that are formattable,
 * and `FormatTable`, that maps placeholders in format strings to actual `Fmt`s
 * and supply those with information they need to produce output. Unlike with
 * `format!` from the standard library, there is no restriction that format
 * strings need to be static; in fact the whole point of the library is to
 * allow moving as much control over formatting process into the format strings
 * themselves (and ideally those - into user-editable config files).
 *
 * There are several `impl`s of `FormatTable`, most notable for `HashMap`s
 * (with either `str` or `String` keys, and `Borrow<dyn Fmt>` values, which
 * means a bit of type annotations required to use them) and `Vec`s (also with
 * `Borrow<dyn Fmt>` elements). The method on `FormatTable` to format a string
 * is `format(&self, format_string: &str) -> Result<String, FormattingError>`
 *
 * # Format string syntax
 * A format string consists of literals and placeholders. They can be separated
 * by colons, but this is not required. Just keep in mind that colons need to
 * be escaped in literals.
 *
 * A literal is any string not containing unescaped opening brackets "`{`" or
 * colons "`:`". Escaping is done in the usual fashion, with backslashes.
 *
 * A placeholder has a more complex structure. Each placeholder is contained
 * within a set of curly brackets, and starts with a name of the `Fmt` it
 * requests formatting from. Name is a dot-separated list of segments denoting
 * access to a (possibly nested) `Fmt`. There must be at least one segment, and
 * empty segments are not allowed (`FormattingError`s happen in both cases).
 * All of the following are valid name-only placeholders:
 * * `"{name}"`
 * * `"{a.path.to.some.nested.field}"`
 * * `r"{escapes.\{are\}.allowed}"`
 *
 * Note that trailing and leading whitespace is stripped from each segment
 * (which means that whitespace-only segments are not allowed as well as empty
 * segments).
 *
 * The name may be followed by an arguments block. An arguments block is just a
 * valid format string surrounded by a pair of curly brackets. Colons take on a
 * special meaning in argument blocks: they separate individual arguments.
 * While in a top-level format string the following two would behave
 * identically, they are very different if used as arguments:
 * * `"{foo{baz}}"`
 * * `"{foo:{baz}}"`
 *
 * If used as an argument block, the first string would form a single argument,
 * concatenating a literal `"foo"` and the expansion of placeholder `"baz"`.
 * The second would form two arguments instead.
 *
 * Here are some examples of valid placeholders with arguments:
 * * `"{name{simple!}}"`
 * * `"{name{two:arguments}}"`
 * * `"{name{{a}{b}:{c}foobar}}"`
 * * `"{nested{{also.can.have.arguments{arg}}}}"`
 *
 * There's one limitation to the above: there is a rather arbitrary limit of
 * 100 to the allowed nesting of placeholders inside placeholders in general
 * and inside argument lists in particular, to avoid blowing the stack up by
 * mistake of from malice.
 *
 * After the argument block (or after the name if there is no arguments) may be
 * a flags block. If the flags block follows the name, it has to be separated
 * from it by a colon. If the flags block follows an argument list, it may or
 * may not be separated from it by the colon. Flags block may be empty, or
 * contain one or more single-character flags. Flags can be repeated, the exact
 * meaning of the repetition depends on the `Fmt` the flags will be fed to. If 
 * the flags block is followed by options (see below) it has to be terminated
 * with a colon, otherwise the colon is optional.
 *
 * Here are some examples of placeholders with flags:
 * * `"{name:a=}"`
 * * `"{name{arg}asdf}"`
 * * `"{name{arg}:asdf:}"`
 *
 * Finally, a placeholder can contain zero or more options after the flags
 * block. Note that if the options are present, flags block must be present as
 * well (but may be empty). Options are key-value pairs, with keys being simple
 * strings with all leading and trailing whitespace stripped, while values can
 * be any valid format strings (the same caveat about nesting as with arguments
 * applies here as well). A key is separated from a value by an equals sign.
 * Key-value pairs are separated by colons. Empty values are allowed, empty
 * keys are not.
 *
 * Here are some examples of placeholders with options:
 * * `"{name::opt=value}"`
 * * `"{name::a=foo:b=baz}"`
 * * `"{name::opt=foo{can.be.a.placeholder.as.well}}"`
 *
 * Different implementations of `Fmt` support different flags and options, see
 * each entry to find out which. There is also a group of common options,
 * described in a separate section below.
 *
 * # Examples
 * Let's start with something boring:
 * ```
 * use std::collections::HashMap;
 * use pfmt::{Fmt, FormatTable};
 *
 * let i = 2;
 * let j = 5;
 * let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
 * table.insert("i", &i);
 * table.insert("j", &j);
 * let s = table.format("i = {i}, j = {j}").unwrap();
 * assert_eq!(s, "i = 2, j = 5");
 * ```
 * I can do that with `format!` too. This is a bit more fun, and shows both
 * options and flags:
 * ```
 * use std::collections::HashMap;
 * use pfmt::{Fmt, FormatTable};
 *
 * let s = "a_really_long_string";
 * let i = 10;
 * let j = 12;
 * let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
 * table.insert("s", &s);
 * table.insert("i", &i);
 * table.insert("j", &j);
 * // (note escaped colons)
 * let s = table.format("hex\\: {i:px}, octal\\: {j:o}, fixed width\\: {s::truncate=r5}").unwrap();
 * assert_eq!(s, "hex: 0xa, octal: 14, fixed width: a_rea");
 * ```
 * Can't decide if you want your booleans as "true" and "false", or "yes" and
 * "no"? Easy:
 * ```
 * use std::collections::HashMap;
 * use pfmt::{Fmt, FormatTable};
 *
 * let a = true;
 * let b = false;
 * let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
 * table.insert("a", &a);
 * table.insert("b", &b);
 * let s = table.format("{a}, {b:y}, {b:Y}").unwrap();
 * assert_eq!(s, "true, no, N");
 * ```
 * And here are `Vec`s as format tables:
 * ```
 * use pfmt::{Fmt, FormatTable};
 * let i = 1;
 * let j = 2;
 * let table: Vec<&dyn Fmt> = vec![&i, &j];
 * let s = table.format("{0}, {1}, {0}").unwrap();
 * assert_eq!(s, "1, 2, 1");
 * ```
 * All of the above examples used references as the element type of the format
 * tables, but `FormatTable` is implemented (for hashmaps and vectors) for
 * anything that is `Borrow<dyn Fmt>`, which means boxes, and reference
 * counters and more. Tables thus can fully own the data:
 * ```
 * use std::collections::HashMap;
 * use pfmt::{Fmt, FormatTable};
 *
 * let mut table: HashMap<String, Box<Fmt>> = HashMap::new();
 * table.insert("a".to_string(), Box::new(2) as Box<Fmt>);
 * table.insert("b".to_string(), Box::new("foobar".to_string()) as Box<Fmt>);
 * let s = table.format("{a}, {b}").unwrap();
 * assert_eq!(s, "2, foobar");
 * ```
 * This is a bit on the verbose side, though.
 *
 * The library also suppports accessing elements of `Fmt`s through the same
 * syntax Rust uses: dot-notation, provided the implementation of `Fmt` in
 * question allows it:
 * ```
 * use std::collections::HashMap;
 * use pfmt::{Fmt, FormatTable, SingleFmtError, util};
 * 
 * struct Point {
 *     x: i32,
 *     y: i32
 * }
 * 
 * impl Fmt for Point {
 *     fn format(
 *         &self,
 *         full_name: &[String],
 *         name: &[String],
 *         args: &[String],
 *         flags: &[char],
 *         options: &HashMap<String, String>,
 *     ) -> Result<String, SingleFmtError> {
 *         if name.is_empty() {
 *             Err(SingleFmtError::NamespaceOnlyFmt(util::join_name(full_name)))
 *         } else if name[0] == "x" {
 *             self.x.format(full_name, &name[1..], args, flags, options)
 *         } else if name[0] == "y" {
 *             self.y.format(full_name, &name[1..], args, flags, options)
 *         } else {
 *             Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)))
 *         }
 *     }
 * }
 * 
 * let p = Point { x: 1, y: 2 };
 * let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
 * table.insert("p", &p);
 * let s = table.format("{p.x}, {p.y}").unwrap();
 * assert_eq!(s, "1, 2");
 * ```
 * This can be nested to arbitrary depth.
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
 * It is an `InvalidOptionValue` to pass anything not fitting into the template
 * in the header as the value of this option.
 *
 * ## `width`: `{'l', 'c', 'r'} + non-negative integer`
 * Controls the width of the field. Has no effect if the field is already wider
 * than the value supplied. If starts with "`l`", the field will be
 * left-justified. If starts with "`c`", the field will be centered. If starts
 * with "`r`", the field will be right-justified.
 *
 * It is an `InvalidOptionValue` to pass anything not fitting into the template
 * in the header as the value for this option.
 *
 * # Common numeric options
 * Most numeric Fmts honor these. For the detailed description skip to the end
 * of this section.
 * * `prec`
 * * `round`
 *
 * ## `prec`: `integer`
 * Controls precision of the displayed number, with bigger values meaning more
 * significant digits will be displayed. If negative, the number will be
 * rounded, the rounding direction is controlled by the `round` option.
 * Positive values are accepted by integer Fmts, but have no effect.
 *
 * It is an `InvalidOptionValue` to pass a string that doesn't parse as a
 * signed integer as a value to this option.
 *
 * ## `round`: `{"up", "down", "nearest"}`
 * Controls the direction of rounding by the `round` option, and has no effect
 * without it. Defaults to `nearest`.
 *
 * It is an `InvalidOptionValue` to pass a string different from the mentioned
 * three to this option.
 *
 * # More fun
 * Format tables are not required to actually *hold* the `Fmt`s. They can
 * produce those on the fly, if you make them to. You only need to implement
 * the `get_fmt` method a bit differently:
 * ```
 * use pfmt::{Fmt, FormatTable, BoxOrRef};
 *
 * struct Producer { }
 *
 * impl FormatTable for Producer {
 *      fn get_fmt<'a, 'b>(&'a self, name: &'b str)
 *          -> Option<BoxOrRef<'a, dyn Fmt>>
 *      {
 *          if let Ok(i) = name.parse::<i32>() {
 *              Some(BoxOrRef::Boxed(Box::new(i)))
 *          } else {
 *              None
 *          }
 *      }
 * }
 *
 * let table = Producer { };
 * let s = table.format("{1}, {12}").unwrap();
 * assert_eq!(s, "1, 12");
 * ```
 * The above example is not particularly useful, but shows the point.
 *
 * There's also an implementation of `FormatTable` for tuples (up to 6-tuples)
 * that contain format tables. When encountering a placeholder, it first
 * searches for the relevant `Fmt` in the first table, then in the second and
 * so on. This allows to easily override some `Fmt`s or provide defaults
 * without changing the tables themselves.
 * ```
 * use std::collections::HashMap;
 * use pfmt::{Fmt, FormatTable};
 *
 * let i1 = 10;
 * let i2 = 100;
 * let j = 2;
 * let mut table1: HashMap<&str, &dyn Fmt> = HashMap::new();
 * table1.insert("i", &i1);
 * table1.insert("j", &j);
 * let mut table2: HashMap<&str, &dyn Fmt> = HashMap::new();
 * table2.insert("i", &i2);
 * let s = (table2, table1).format("{i}, {j}").unwrap();
 * assert_eq!(s, "100, 2");
 * ```
 */

#[cfg(test)]
#[macro_use]
extern crate galvanic_assert;
#[cfg(test)]
#[macro_use]
extern crate galvanic_test;

extern crate num;

use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::HashMap;

use parse::{parse, ParseError, Piece};

mod parse;

pub mod extras;
pub mod util;

/* ---------- base traits ---------- */

/// A unit of formatting.
pub trait Fmt {
    /// Perform the formatting of a single placeholder. Placeholder's full
    /// name, name segments of child format units, arguments, flags and options
    /// will be passed to this method.
    ///
    /// You should not use this method directly, use `format` on `FormatTable`
    /// instead.
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError>;
}

/// A collection or producer of format units.
pub trait FormatTable<'a> {
    type Item: Fmt;

    /// Produce or retrieve a format unit with the given name stem.
    fn get_fmt(&'a self, name: &str) -> Option<Self::Item>;

    /// Perform formatting of a format string.
    ///
    /// This method is not meant to be overridden. You can, but you would have
    /// to reimplement format strings parser yourself.
    fn format(&'a self, input: &str) -> Result<String, FormattingError> {
        format_one(self, &parse(input, 0, 0)?)
    }
}

fn format_one<'a, 'b, T: FormatTable<'a> + ?Sized>(
    table: &'a T,
    piece: &'b Piece,
) -> Result<String, FormattingError> {
    match piece {
        Piece::Literal(s) => Ok(s.clone()),
        Piece::Placeholder(name, args, flags, opts) => {
            if let Some(root) = table.get_fmt(&name[0]) {
                let mut processed_args = Vec::with_capacity(args.len());
                for arg in args.iter() {
                    processed_args.push(format_one(table, arg)?);
                }
                let mut processed_opts = HashMap::new();
                for (key, piece) in opts.iter() {
                    processed_opts.insert(key.clone(), format_one(table, piece)?);
                }
                Ok(root.format(name, &name[1..], &processed_args, flags, &processed_opts)?)
            } else {
                Err(FormattingError::UnknownFmt(util::join_name(&name)))
            }
        },
        Piece::Multi(pieces) => {
            let mut res = String::new();
            for piece in pieces.iter() {
                res.push_str(&format_one(table, piece)?);
            }
            Ok(res)
        }
    }
}

/* ---------- errors ---------- */

/// Errors that happen in individual `Fmt`s. All of them contain the full path
/// to the `Fmt` in error as the first field.
#[derive(Debug, PartialEq)]
pub enum SingleFmtError {
    /// Returned if a `Fmt` receives a flag it doesn't know how to handle.
    /// It's not actually used by the `impl`s for the standard types, but you
    /// can use it if you wish to be strict. Contains the erroneous flag.
    UnknownFlag(String, char),
    /// Returned if a `Fmt` receives an option it doesn't know how to handle.
    /// Again, standard types do not do this, they are not strict. Contains the
    /// erroneous option.
    UnknownOption(String, String),
    /// Returned when a given option (stored in the first field) contains an
    /// invalid value (stored in the second field). Standard types *do* use
    /// this. Contains a pair of erroneous option's name and value.
    InvalidOptionValue(String, String, String),
    /// Returned when a `Fmt` that is only used as a container to hold/produce
    /// other `Fmt`s via the dot access syntax is used directly. Contains the
    /// full path to the format unit used in such fashion.
    NamespaceOnlyFmt(String),
    /// Returned when a `Fmt`does not contain a requested sub-`Fmt`. Contains
    /// the full path to the child format unit.
    UnknownSubfmt(String),
    /// Returned when a `Fmt` was passed wrong number of arguments. The
    /// `Ordering` member specifies whether more, less or precisely the
    /// expected number (given by the `usize` field) is required.
    WrongNumberOfArguments(String, Ordering, usize),
    /// Returned when a `Fmt` is passed an argument it can't process. Contains
    /// the index and contents of the argument in error.
    InvalidArgument(String, usize, String),
}

/// Any error that can happen during formatting.
#[derive(Debug, PartialEq)]
pub enum FormattingError {
    // Parsing errors.
    /// Returned when the brackets in the format string are not balanced.
    /// Contains the byte address of the offending bracket.
    UnbalancedBrackets(usize),
    /// Returned when a placeholder in the format string has an empty name
    /// segment. Contains the byte offset of the empty segment.
    EmptyNameSegment(usize),
    /// Returned when a placeholder's option has an empty name. Contains the
    /// byte offset of the place where the name should have been.
    EmptyOptionName(usize),
    // Errors from single Fmts.
    /// A `SingleFmtError::UnknownFlag` is propagated as this.
    UnknownFlag(String, char),
    /// A `SingleFmtError::UnknownOption` is propagated as this.
    UnknownOption(String, String),
    /// A `SingleFmtError::InvalidOptionValue` is propagated as this.
    InvalidOptionValue(String, String, String),
    /// A `SingleFmtError::NamespaceOnlyFmt` is propagated as this.
    NamespaceOnlyFmt(String),
    /// A `SingleFmtError::WrongNumberOfArguments` is propagated as this.
    WrongNumberOfArguments(String, Ordering, usize),
    /// A `SingleFmtError::InvalidArgument` is propagated as this.
    InvalidArgument(String, usize, String),
    // General errors.
    /// Returned when a requested `Fmt` does not exist (or cannot be created)
    /// in the format table. A `SingleFmtError::UnknownSubfmt` is also
    /// propagated as this. Contains the full path to the failed format unit.
    UnknownFmt(String),
}

impl From<SingleFmtError> for FormattingError {
    fn from(err: SingleFmtError) -> Self {
        match err {
            SingleFmtError::UnknownFlag(s, c) => FormattingError::UnknownFlag(s, c),
            SingleFmtError::UnknownOption(p, o) => FormattingError::UnknownOption(p, o),
            SingleFmtError::InvalidOptionValue(p, opt, val) =>
                FormattingError::InvalidOptionValue(p, opt, val),
            SingleFmtError::NamespaceOnlyFmt(s) => FormattingError::NamespaceOnlyFmt(s),
            SingleFmtError::UnknownSubfmt(s) => FormattingError::UnknownFmt(s),
            SingleFmtError::WrongNumberOfArguments(s, o, n) =>
                FormattingError::WrongNumberOfArguments(s, o, n),
            SingleFmtError::InvalidArgument(s, i, a) =>
                FormattingError::InvalidArgument(s, i, a),
        }
    }
}

impl From<ParseError> for FormattingError {
    fn from(err: ParseError) -> Self {
        use parse::ParseError::*;
        match err {
            UnbalancedBrackets(i) => FormattingError::UnbalancedBrackets(i),
            EmptyNameSegment(i) => FormattingError::EmptyNameSegment(i),
            EmptyOptionName(i) => FormattingError::EmptyOptionName(i),
        }
    }
}

/* ---------- key implementations ---------- */

impl<'a, T: Fmt + ?Sized> Fmt for &'a T { 
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        (*self).format(full_name, name, args, flags, options)
    }
}

impl<'a, 'b, I: Fmt, T: FormatTable<'a, Item = I>> FormatTable<'a> for &'b T {
    type Item = I;

    fn get_fmt(&'a self, name: &str) -> Option<Self::Item> {
        (*self).get_fmt(name)
    }
}

/* ---------- implementations of FormatTable for standard types ---------- */

/// This implementation recognizes placeholders with string names stored in the
/// hash map.
impl<'a, B: Borrow<dyn Fmt>> FormatTable<'a> for HashMap<String, B> {
    type Item = &'a dyn Fmt;

    fn get_fmt(&'a self, name: &str) -> Option<Self::Item> {
        self.get(name).map(|b| b.borrow())
    }
}

/// This implementation recognizes placeholders with string names (represented by
/// string slices) stored in the hash map.
impl<'a, 'b, B: Borrow<dyn Fmt>> FormatTable<'a> for HashMap<&'b str, B> {
    type Item = &'a dyn Fmt;

    fn get_fmt(&'a self, name: &str) -> Option<Self::Item> {
        self.get(name).map(|b| b.borrow())
    }
}

/// This implementation recognizes placeholders which names are valid integer
/// indices into the vector.
impl<'a, B: Borrow<dyn Fmt>> FormatTable<'a> for Vec<B> {
    type Item = &'a dyn Fmt;

    fn get_fmt(&'a self, name: &str) -> Option<Self::Item> {
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
}

/// This implementation first uses the first table to look up placeholders'
/// names, and then the second.
impl<'a, F, A, B> FormatTable<'a> for (A, B)
where
    F: Fmt,
    A: FormatTable<'a, Item = F>,
    B: FormatTable<'a, Item = F>,
{
    type Item = F;

    fn get_fmt(&'a self, name: &str) -> Option<Self::Item> {
        self.0.get_fmt(name)
            .or_else(|| self.1.get_fmt(name))
    }
}

/// This implementation looks up placeholders' names consecutively in three
/// tables.
impl<'a, F, A, B, C> FormatTable<'a> for (A, B, C)
where
    F: Fmt,
    A: FormatTable<'a, Item = F>,
    B: FormatTable<'a, Item = F>,
    C: FormatTable<'a, Item = F>,
{
    type Item = F;

    fn get_fmt(&'a self, name: &str) -> Option<Self::Item> {
        self.0
            .get_fmt(name)
            .or_else(|| self.1.get_fmt(name))
            .or_else(|| self.2.get_fmt(name))
    }
}

/// This implementation looks up placeholders' names consecutively in four
/// tables.
impl<'a, F, A, B, C, D> FormatTable<'a> for (A, B, C, D)
where
    F: Fmt,
    A: FormatTable<'a, Item = F>,
    B: FormatTable<'a, Item = F>,
    C: FormatTable<'a, Item = F>,
    D: FormatTable<'a, Item = F>,
{
    type Item = F;

    fn get_fmt(&'a self, name: &str) -> Option<Self::Item> {
        self.0
            .get_fmt(name)
            .or_else(|| self.1.get_fmt(name))
            .or_else(|| self.2.get_fmt(name))
            .or_else(|| self.3.get_fmt(name))
    }
}

/// This implementation looks up placeholders' names consecutively in five
/// tables.
impl<'a, T, A, B, C, D, E> FormatTable<'a> for (A, B, C, D, E)
where
    T: Fmt,
    A: FormatTable<'a, Item = T>,
    B: FormatTable<'a, Item = T>,
    C: FormatTable<'a, Item = T>,
    D: FormatTable<'a, Item = T>,
    E: FormatTable<'a, Item = T>,
{
    type Item = T;

    fn get_fmt(&'a self, name: &str) -> Option<Self::Item> {
        self.0
            .get_fmt(name)
            .or_else(|| self.1.get_fmt(name))
            .or_else(|| self.2.get_fmt(name))
            .or_else(|| self.3.get_fmt(name))
            .or_else(|| self.4.get_fmt(name))
    }
}

/// This implementation looks up placeholders' names consecutively in six
/// tables.
impl<'a, T, A, B, C, D, E, F> FormatTable<'a> for (A, B, C, D, E, F)
where
    T: Fmt,
    A: FormatTable<'a, Item = T>,
    B: FormatTable<'a, Item = T>,
    C: FormatTable<'a, Item = T>,
    D: FormatTable<'a, Item = T>,
    E: FormatTable<'a, Item = T>,
    F: FormatTable<'a, Item = T>,
{
    type Item = T;

    fn get_fmt(&'a self, name: &str) -> Option<Self::Item> {
        self.0
            .get_fmt(name)
            .or_else(|| self.1.get_fmt(name))
            .or_else(|| self.2.get_fmt(name))
            .or_else(|| self.3.get_fmt(name))
            .or_else(|| self.4.get_fmt(name))
            .or_else(|| self.5.get_fmt(name))
    }
}

/* ---------- implementations of Fmt for standard types ---------- */

/// This instance is aware of the following flags:
/// * `y`, which changes the output from true/false to yes/no;
/// * `Y`, which changes the output to Y/N.
///
/// Common options are recognized.
impl Fmt for bool {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut res = if *self {
            if flags.contains(&'y') {
                "yes".to_string()
            } else if flags.contains(&'Y') {
                "Y".to_string()
            } else {
                "true".to_string()
            }
        } else if flags.contains(&'y') {
            "no".to_string()
        } else if flags.contains(&'Y') {
            "N".to_string()
        } else {
            "false".to_string()
        };
        util::apply_common_options(full_name, &mut res, options)?;
        Ok(res)
    }
}

/// This instance has no special flags.
///
/// Common options are recognized.
impl Fmt for char {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        _flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = self.to_string();
        util::apply_common_options(full_name, &mut s, options)?;
        Ok(s)
    }
}

/// This instance is aware of the following flags:
/// * `+`, which forces display of the sign;
/// * `e`, which changes the output to the scientific, or exponential,
/// notation.
///
/// Common options are recognized.
/// Common numeric options are also recognized.
impl Fmt for f32 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut res: String;
        if flags.contains(&'e') {
            res = util::float_to_exp(full_name, *self, options)?;
        } else {
            res = util::float_to_normal(full_name, *self, options)?;
        }
        util::add_sign(&mut res, *self, flags)?;
        util::apply_common_options(full_name, &mut res, options)?;
        Ok(res)
    }
}

/// This instance is aware of the following flags:
/// * `+`, which forces display of the sign;
/// * `e`, which changes the output to scientific format.
///
/// Common options are recognized.
/// Common numeric options are also recognized.
impl Fmt for f64 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut res: String;
        if flags.contains(&'e') {
            res = util::float_to_exp(full_name, *self, options)?;
        } else {
            res = util::float_to_normal(full_name, *self, options)?;
        }
        util::add_sign(&mut res, *self, flags)?;
        util::apply_common_options(full_name, &mut res, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for i8 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        util::add_sign(&mut s, *self, flags)?;
        util::apply_common_options(full_name, &mut s, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for i16 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        util::add_sign(&mut s, *self, flags)?;
        util::apply_common_options(full_name, &mut s, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for i32 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        util::add_sign(&mut s, *self, flags)?;
        util::apply_common_options(full_name, &mut s, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for i64 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        util::add_sign(&mut s, *self, flags)?;
        util::apply_common_options(full_name, &mut s, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for i128 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        util::add_sign(&mut s, *self, flags)?;
        util::apply_common_options(full_name, &mut s, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for isize {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        util::add_sign(&mut s, *self, flags)?;
        util::apply_common_options(full_name, &mut s, options)?;
        Ok(s)
    }
}

/// This instance has no special flags.
///
/// Common options are recognized.
impl<'a> Fmt for &'a str {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        _flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = self.to_string();
        util::apply_common_options(full_name, &mut s, options)?;
        Ok(s)
    }
}

/// This instance has no special flags.
///
/// Common options are recognized.
impl Fmt for String {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        _flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = self.clone();
        util::apply_common_options(full_name, &mut s, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for u8 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        if flags.contains(&'+') {
            s.insert(0, '+');
        }
        util::apply_common_options(full_name, &mut s, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for u16 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        if flags.contains(&'+') {
            s.insert(0, '+');
        }
        util::apply_common_options(full_name, &mut s, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for u32 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        if flags.contains(&'+') {
            s.insert(0, '+');
        }
        util::apply_common_options(full_name, &mut s, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for u64 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        if flags.contains(&'+') {
            s.insert(0, '+');
        }
        util::apply_common_options(full_name, &mut s, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for u128 {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        if flags.contains(&'+') {
            s.insert(0, '+');
        }
        util::apply_common_options(full_name, &mut s, options)?;
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
///
/// Common and common numeric options are recognized.
impl Fmt for usize {
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        if !name.is_empty() {
            return Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)));
        }
        let mut s = util::int_to_str(full_name, *self, flags, options)?;
        if flags.contains(&'+') {
            s.insert(0, '+');
        }
        util::apply_common_options(full_name, &mut s, options)?;
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
            let table: HashMap<&str, &dyn Fmt> = HashMap::new();
            let s = table.format("i = {i}");
            assert_that!(&s, eq(Err(FormattingError::UnknownFmt("i".to_string()))));
        }

        test unknown_fmt_nested() {
            let i = 1;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i.a}");
            assert_that!(&s, eq(Err(FormattingError::UnknownFmt("i.a".to_string()))));
        }

        test integers_simple_1() {
            let i = 1;
            let j = 23;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("i", &i);
            table.insert("j", &j);
            let s = table.format("i = {i}, j = {j}").unwrap();
            assert_that!(&s.as_str(), eq("i = 1, j = 23"));
        }

        test separated_by_colons() {
            let table: HashMap<&str, &dyn Fmt> = HashMap::new();
            let s = table.format("a:b").unwrap();
            assert_that!(&s.as_str(), eq("ab"));
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
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
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
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
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
            let mut table: HashMap<String, &dyn Fmt> = HashMap::new();
            table.insert("f".to_string(), &f);
            let s = table.format("{f:e+:prec=-1}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("+1.23457e6"));
        }

        test exp_precision_pos() {
            let f: f32 = 1000.123;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f:e:prec=2}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1.00012e3"));
        }

        test exp_negative_power() {
            let f: f32 = 0.0625;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f:e}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("6.25e-2"));
        }

        test norm_rounding_up() {
            let f = 0.2;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f::round=up:prec=0}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1"));
        }

        test norm_rounding_down() {
            let f = 0.8;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f::round=down:prec=0}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("0"));
        }

        test norm_rounding_usual() {
            let f = 0.5;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f::round=nearest:prec=0}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1"));
        }

        test negative() {
            let f = -1.0;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
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
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("10"));
        }

        test different_bases() {
            let i = 11;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i:b}, {i:o}, {i:x}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1011, 13, b"));
        }

        test base_prefixes() {
            let i = 1;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i:bp}, {i:op}, {i:xp}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("0b1, 0o1, 0x1"));
        }

        test bases_for_negative_numbers() {
            let i = -11;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i:b}, {i:o}, {i:x}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("-1011, -13, -b"));
        }

        test rounding() {
            let i = 1235;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i::prec=-1}, {i::prec=-2}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1240, 1200"));
        }

        test rounding_for_negatives() {
            let i = -1235;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i::prec=-1}, {i::prec=-2}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("-1240, -1200"));
        }

        test rounding_in_different_bases() {
            let o = 0o124;
            let b = 0b1101;
            let x = 0x1a2;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("o", &o);
            table.insert("b", &b);
            table.insert("x", &x);
            let s1 = table.format("{o:op:prec=-1}, {o:op:prec=-2}").expect("Failed to parse 1");
            let s2 = table.format("{b:bp:prec=-1}, {b:bp:prec=-2}").expect("Failed to parse 2");
            let s3 = table.format("{x:xp:prec=-1}, {x:xp:prec=-2}").expect("Failed to parse 3");
            assert_that!(&s1.as_str(), eq("0o130, 0o100"));
            assert_that!(&s2.as_str(), eq("0b1110, 0b1100"));
            assert_that!(&s3.as_str(), eq("0x1a0, 0x200"));
        }

    }

    test_suite! {
        name common_options;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt};

        test width_left() {
            let string = "foobar";
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("s", &string);
            let s = table.format("{s::width=l10}").unwrap();
            assert_that!(&s.as_str(), eq("foobar    "));
        }

        test width_right() {
            let string = "foobar";
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("s", &string);
            let s = table.format("{s::width=r10}").unwrap();
            assert_that!(&s.as_str(), eq("    foobar"));
        }

        test width_center() {
            let string = "foobar";
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("s", &string);
            let s = table.format("{s::width=c10}").unwrap();
            assert_that!(&s.as_str(), eq("  foobar  "));
        }

        test truncate_left() {
            let string = "1234567890";
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("s", &string);
            let s = table.format("{s::truncate=l5}").unwrap();
            assert_that!(&s.as_str(), eq("67890"));
        }

        test truncate_right() {
            let string = "1234567890";
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("s", &string);
            let s = table.format("{s::truncate=r5}").unwrap();
            assert_that!(&s.as_str(), eq("12345"));
        }

    }

    test_suite! {
        name nested_fmts;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt, FormattingError, SingleFmtError, util};

        struct Point {
            x: i32,
            y: i32
        }

        struct Line {
            start: Point,
            end: Point
        }

        impl Fmt for Point {
            fn format(&self,
                      full_name: &[String],
                      name: &[String],
                      args: &[String],
                      flags: &[char],
                      options: &HashMap<String, String>)
                -> Result<String, SingleFmtError>
                {
                    if name.is_empty() {
                        Err(SingleFmtError::NamespaceOnlyFmt(util::join_name(full_name)))
                    } else if name[0] == "x" {
                        self.x.format(full_name, &name[1..], args, flags, options)
                    } else if name[0] == "y" {
                        self.y.format(full_name, &name[1..], args, flags, options)
                    } else {
                        Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)))
                    }
                }
        }

        impl Fmt for Line {
            fn format(&self,
                      full_name: &[String],
                      name: &[String],
                      args: &[String],
                      flags: &[char],
                      options: &HashMap<String, String>)
                -> Result<String, SingleFmtError>
                {
                    if name.is_empty() {
                        Err(SingleFmtError::NamespaceOnlyFmt(util::join_name(full_name)))
                    } else if name[0] == "start" || name[0] == "a" {
                        self.start.format(full_name, &name[1..], args, flags, options)
                    } else if name[0] == "end" || name[0] == "b" {
                        self.end.format(full_name, &name[1..], args, flags, options)
                    } else {
                        Err(SingleFmtError::UnknownSubfmt(util::join_name(full_name)))
                    }
                }
        }

        test single_nested() {
            let a = Point { x: 0, y: 0 };
            let b = Point { x: 2, y: 10 };
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("a", &a);
            table.insert("b", &b);
            let s = table.format("{a.x}, {b.y}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("0, 10"));
        }

        test double_nested() {
            let line = Line {
                start: Point { x: 0, y: 2 },
                end: Point { x: 6, y: 10},
            };
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("line", &line);
            let s = table.format("{line.start.x}, {line.end.y}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("0, 10"));
        }

        test namespace_only() {
            let p = Point { x: 1, y: 2 };
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("p", &p);
            let s = table.format("{p}");
            assert_that!(&s, eq(Err(FormattingError::NamespaceOnlyFmt("p".to_string()))));
        }

    }

    test_suite! {
        name placeholder_substitution;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;
        use {FormatTable, Fmt};

        test placeholder_in_options() {
            let p1 = 3;
            let p2 = 5;
            let f = 0.7654321;
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("p1", &p1);
            table.insert("p2", &p2);
            table.insert("f", &f);
            let s = table.format("{f::prec={p1}}, {f::prec={p2}}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("0.765, 0.76543"));
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
            let table: Vec<&dyn Fmt> = vec![&i, &j];
            let err = table.format("{10}").expect_err("Unexpectedly found a fmt");
            assert_that!(&err, eq(FormattingError::UnknownFmt("10".to_string())));
        }

        test unknown_fmt_2() {
            let i = 1;
            let j = 2;
            let table: Vec<&dyn Fmt> = vec![&i, &j];
            let err = table.format("{-3}").expect_err("Unexpectedly found a fmt");
            assert_that!(&err, eq(FormattingError::UnknownFmt("-3".to_string())));
        }

        test boring() {
            let i = 1;
            let j = 2;
            let table: Vec<&dyn Fmt> = vec![&i, &j];
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

        test lifetimes() {
            let i = 10;
            let j = 12;
            let v1: Vec<&dyn Fmt> = vec![&i, &j];
            {
                let k = 13;
                let v2: Vec<&dyn Fmt> = vec![&k];
                let s1 = (&v1, &v2).format("{0}").expect("Failed to format");
                let s2 = (&v2, &v1).format("{0}").expect("Failed to format");
                assert_that!(&s1.as_str(), eq("10"));
                assert_that!(&s2.as_str(), eq("13"));
            }
        }

    }

}
