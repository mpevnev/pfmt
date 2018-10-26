//! Non-essential niceties
//!
//! # Closures support
//! There are several struct wrappers over closures which allow using closures
//! as `Fmt`s. All of them are defined for `FnMut` closures. The simplest form
//! of these is `Lazy`, which wraps a closure producing a single `Fmt` to
//! actually control the formatting. There are also several `AdHoc*`
//! structures, with arguments of each being a subset of arguments of the
//! `format` method on `Fmt`. The intent is to provide a way to get some
//! quick-and-dirty formatting without defining a new data type.
//!
//! Since all of these are defined not just for `Fn`s, but for `FnMut`s, it's
//! possible to do neat things like self-incrementing lists:
//! ```
//! use std::collections::HashMap;
//! 
//! use pfmt::{Fmt, FormatTable};
//! use pfmt::extras::Lazy;
//! 
//! let mut index = 0;
//! let mut counter = Lazy::new(move || { index += 1; index });
//! let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
//! table.insert("i", &counter);
//! let s = table.format("{i} - spam\n{i} - eggs").unwrap();
//! assert_eq!(&s, "1 - spam\n2 - eggs");
//! ```
//!
//! # Single-entry format tables
//! Sometimes all you need is a quick and dirty formatting, which clashes
//! somewhat with the nature of the format tables in the main module - their
//! construction is quite burdensome. Enter `Mono` and tuples. A `Mono` is a
//! tuple struct with a string key (or anything that derefs to a `str`) and
//! some `Fmt` value. It is also a format table. The following works:
//! ```
//! use pfmt::FormatTable;
//! use pfmt::extras::Mono;
//! 
//! let a = Mono("a", 5);
//! let b = Mono("b", "foobar");
//! let s = (&a, &b).format("{a}, {b}").unwrap();
//! assert_eq!(&s, "5, foobar");
//! ```
//!
//! They can be combined with hash tables and vectors in the same fashion, the
//! main module docs have an example.

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;

use {Fmt, SingleFmtError, FormatTable};

/* ---------- simple closure support ---------- */

/// A wrapper over a closure producing a `Fmt`.
///
/// Note that the closure can be `FnMut`, not just `Fn`.
/// Automatically honors any flags, arguments and options the produced `Fmt`
/// recognizes.
pub struct Lazy<T, F>
where
    T: Fmt,
    F: FnMut() -> T
{
    closure: RefCell<F>,
}

impl<T: Fmt, F: FnMut() -> T> Lazy<T, F> {
    pub fn new(closure: F) -> Self {
        Lazy {
            closure: RefCell::new(closure),
        }
    }
}

impl<T: Fmt, F: FnMut() -> T> Fmt for Lazy<T, F> {
    /// Call the wrapped closure to get a `Fmt`, then call `format` on it.
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        let mut r = self.closure.borrow_mut();
        let fmt = (&mut *r)();
        fmt.format(full_name, name, args, flags, options)
    }
}

/* ---------- ad-hoc closures with arguments ---------- */

/// A wrapper over a closure that accepts `full_name`, `name` and `args`
/// parameters of the `format` method on `Fmt`.
///
/// Note that the closure can be `FnMut`, not just `Fn`. 
/// Also note that while it is possible to call other `Fmt`s recursively from
/// the closure, the flags and options of the original `format` call will be
/// necessarily lost and will have to be replaced somehow by the closure.
pub struct AdHocArgs<F>
where
    F: FnMut(&[String], &[String], &[String]) -> Result<String, SingleFmtError>
{
    closure: RefCell<F>,
}

impl<F> AdHocArgs<F>
where
    F: FnMut(&[String], &[String], &[String]) -> Result<String, SingleFmtError>
{
    pub fn new(closure: F) -> Self {
        AdHocArgs {
            closure: RefCell::new(closure),
        }
    }
}

impl<F> Fmt for AdHocArgs<F>
where
    F: FnMut(&[String], &[String], &[String]) -> Result<String, SingleFmtError>
{
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        args: &[String],
        _flags: &[char],
        _options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        let mut closure = self.closure.borrow_mut();
        (&mut *closure)(full_name, name, args)
    }
}

/* ---------- ad-hoc closures with options ---------- */

/// A wrapper over a closure that accepts `full_name`, `name` and `options`
/// parameters of the `format` method on `Fmt`.
///
/// Note that the closure can be `FnMut`, not just `Fn`.
/// Also note that while it is possible to call other `Fmt`s recursively from
/// the closure, the arguments and flags of the original `format` call will be
/// lost and will have to be somehow replaced by the closure itself.
pub struct AdHocOpts<F>
where
    F: FnMut(&[String], &[String], &HashMap<String, String>) -> Result<String, SingleFmtError>
{
    closure: RefCell<F>,
}

impl<F> AdHocOpts<F>
where 
    F: FnMut(&[String], &[String], &HashMap<String, String>) -> Result<String, SingleFmtError>
{
    pub fn new(closure: F) -> Self {
        AdHocOpts {
            closure: RefCell::new(closure),
        }
    }
}

impl<F> Fmt for AdHocOpts<F>
where
    F: FnMut(&[String], &[String], &HashMap<String, String>) -> Result<String, SingleFmtError>
{
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        _flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        let mut closure = self.closure.borrow_mut();
        (&mut *closure)(full_name, name, options)
    }
}

/* ---------- ad-hoc closures with flags and options ---------- */

/// A wrapper over a closure that accepts `full_name`, `name`, `flags` and
/// `options` parameters of the `format` method on `Fmt`.
///
/// Note that the closure can be `FnMut`, not just `Fn`.
/// Also note that while it is possible to call other `Fmt`s from the closure,
/// the `args` parameter of the original `format` call will be lost and will
/// have to be replaced somehow by the closure.
pub struct AdHocFlagsOpts<F>
where
    F: FnMut(&[String], &[String], &[char], &HashMap<String, String>)
        -> Result<String, SingleFmtError>
{
    closure: RefCell<F>,
}

impl<F> AdHocFlagsOpts<F>
where 
    F: FnMut(&[String], &[String], &[char], &HashMap<String, String>)
        -> Result<String, SingleFmtError>
{
    pub fn new(closure: F) -> Self {
        AdHocFlagsOpts {
            closure: RefCell::new(closure),
        }
    }
}

impl<F> Fmt for AdHocFlagsOpts<F>
where
    F: FnMut(&[String], &[String], &[char], &HashMap<String, String>)
        -> Result<String, SingleFmtError>
{
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        flags: &[char],
        opts: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        let mut closure = self.closure.borrow_mut();
        (&mut *closure)(full_name, name, flags, opts)
    }
}

/* ---------- ad-hoc closures of maximum complexity ---------- */

/// A wrapper over a closure with the same signature as the `format` method on
/// `Fmt`.
///
/// The closure doesn't have to be `Fn`, it can be `FnMut`.
pub struct AdHocFull<F>
where
    F: FnMut(&[String], &[String], &[String], &[char], &HashMap<String, String>)
        -> Result<String, SingleFmtError>,
{
    closure: RefCell<F>,
}

impl<F> AdHocFull<F>
where
    F: FnMut(&[String], &[String], &[String], &[char], &HashMap<String, String>)
        -> Result<String, SingleFmtError>,
{
    pub fn new(closure: F) -> Self {
        AdHocFull {
            closure: RefCell::new(closure),
        }
    }
}

impl<F> Fmt for AdHocFull<F>
where
    F: FnMut(&[String], &[String], &[String], &[char], &HashMap<String, String>)
        -> Result<String, SingleFmtError>,
{
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        let mut closure = self.closure.borrow_mut();
        (&mut *closure)(full_name, name, args, flags, options)
    }
}

/* ---------- single-entry format tables ---------- */

/// A format table with a single entry, accessed by a string key.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Mono<S: Deref<Target = str>, T: Fmt>(pub S, pub T);

impl<'a, S: Deref<Target = str>, T: Fmt + 'a> FormatTable<'a> for Mono<S, T> {
    type Item = &'a dyn Fmt;

    fn get_fmt(&'a self, name: &str) -> Option<Self::Item> {
        if name == self.0.deref() {
            Some(&self.1)
        } else {
            None
        }
    }
}

/* ---------- tests ---------- */

#[cfg(test)]
mod extras_tests {
    test_suite! {
        name closures;
        use std::cmp::Ordering;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;

        use {FormatTable, Fmt, SingleFmtError};
        use extras::*;
        use util;

        test lazy() {
            let i = Lazy::new(|| 10_i32);
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("10"));
        }

        test mutable_lazy() {
            let mut i = 0;
            let c = Lazy::new(move || { i += 1; i });
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("i", &c);
            let s = table.format("{i}, {i}, {i}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1, 2, 3"));
        }

        test ad_hoc_args() {
            // Convert the first argument to upper case. If there is no
            // argument, return '!!!'.
            let f = AdHocArgs::new(|_, _, args| {
                if args.is_empty() {
                    Ok("!!!".to_string())
                } else {
                    Ok(args[0].to_uppercase())
                }
            });
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f{asdf}}, {f}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("ASDF, !!!"));
        }

        test ad_hoc_opts() {
            // Return 'defined' if 'opt' is set, 'undefined' otherwise.
            let f = AdHocOpts::new(|_, _, opts| {
                if opts.contains_key("opt") {
                    Ok("defined".to_string())
                } else {
                    Ok("undefined".to_string())
                }
            });
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f::opt=}, {f}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("defined, undefined"));
        }

        test ad_hoc_full() {
            // This thing converts its first argument to upper case.
            let f = AdHocFull::new(|full_name, _, args: &[String], _, _| {
                if args.is_empty() {
                    Err(SingleFmtError::WrongNumberOfArguments(
                            util::join_name(full_name),
                            Ordering::Equal,
                            1))
                } else {
                    let s = &args[0];
                    Ok(s.to_uppercase())
                }
            });
            let mut table: HashMap<&str, &dyn Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f{asdf}}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("ASDF"));
        }

    }

    test_suite! {
        name mono;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;

        use {FormatTable, Fmt};
        use extras::Mono;

        test several_monos() {
            let a = Mono("a", 5_i32);
            let b = Mono("b", "foobar");
            let s = (&a, &b).format("{a}, {b}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("5, foobar"));
        }

        test monos_and_hashmaps() {
            let a = Mono("foobar", 10);
            let t = {
                let mut res: HashMap<&str, &dyn Fmt> = HashMap::new();
                res.insert("a", &10);
                res
            };
            let s = (&a, &t).format("{foobar}, {a}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("10, 10"));
        }

    }
}
