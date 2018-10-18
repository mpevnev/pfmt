//! # Overview
//! This module provides various non-essential, but nice-to-have things, mostly
//! newtype wrappers with specific implementations of `Fmt`.

use std::cell::RefCell;
use std::collections::HashMap;

use {Fmt, SingleFmtError};

/* ---------- simple closure support ---------- */

pub struct Lazy<F> {
    closure: RefCell<F>,
}

impl<T: Fmt, F: FnMut() -> T> Lazy<F> {
    pub fn new(closure: F) -> Self {
        Lazy {
            closure: RefCell::new(closure),
        }
    }
}

impl<T: Fmt, F: FnMut() -> T> Fmt for Lazy<F> {
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

/* ---------- ad-hoc formatting from just name ---------- */

pub struct AdHocMin<F>
where
    F: FnMut(&[String], &[String]) -> Result<String, SingleFmtError>
{
    closure: RefCell<F>,
}

impl<F> AdHocMin<F>
where
    F: FnMut(&[String], &[String]) -> Result<String, SingleFmtError>
{
    pub fn new(closure: F) -> Self {
        AdHocMin {
            closure: RefCell::new(closure)
        }
    }
}

impl<F> Fmt for AdHocMin<F>
where
    F: FnMut(&[String], &[String]) -> Result<String, SingleFmtError>
{
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        _args: &[String],
        _flags: &[char],
        _options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        let mut closure = self.closure.borrow_mut();
        (&mut *closure)(full_name, name)
    }
}

/* ---------- ad-hoc closures with arguments ---------- */

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

/* ---------- tests ---------- */

#[cfg(test)]
mod extras_tests {
    test_suite! {
        name closures;
        use std::cmp::Ordering;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;

        use {FormatTable, Fmt, SingleFmtError};
        use extras::{Lazy, AdHocFull};
        use util;

        test lazy() {
            let i = Lazy::new(|| 10_i32);
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("10"));
        }

        test ad_hoc() {
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
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("f", &f);
            let s = table.format("{f{asdf}}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("ASDF"));
        }

        test mutable_lazy() {
            let mut i = 0;
            let c = Lazy::new(move || { i += 1; i });
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("i", &c);
            let s = table.format("{i}, {i}, {i}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("1, 2, 3"));
        }

    }

}
