//! # Overview
//! This module provides various non-essential, but nice-to-have things, mostly
//! newtype wrappers with specific implementations of `Fmt`.

use std::cell::RefCell;
use std::collections::HashMap;

use {Fmt, SingleFmtError};

/* ---------- simple closure support ---------- */

pub struct Lazy<F> {
    closure: RefCell<F>
}

impl<T, F: FnMut() -> T> Lazy<F> {
    pub fn new(closure: F) -> Self {
        Lazy {
            closure: RefCell::new(closure)
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

/* ---------- ad-hoc formatting using more complex closures ---------- */

pub struct AdHoc<F>
where 
    F: Fn(&[String], &[String], &[String], &[char], &HashMap<String, String>)
        -> Result<String, SingleFmtError>
{
    closure: Box<F>
}

impl<F> AdHoc<F>
where
    F: Fn(&[String], &[String], &[String], &[char], &HashMap<String, String>)
        -> Result<String, SingleFmtError>
{
    pub fn new(closure: F) -> Self {
        AdHoc {
            closure: Box::new(closure)
        }
    }
}

impl<F> Fmt for AdHoc<F>
where
    F: Fn(&[String], &[String], &[String], &[char], &HashMap<String, String>)
        -> Result<String, SingleFmtError>
{
    fn format(
        &self,
        full_name: &[String],
        name: &[String],
        args: &[String],
        flags: &[char],
        options: &HashMap<String, String>,
    ) -> Result<String, SingleFmtError> {
        (*self.closure)(full_name, name, args, flags, options)
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
        use extras::{Lazy, AdHoc};
        use util;

        test lazy() {
            let i = Lazy::new(|| 10_i32);
            let mut table: HashMap<&str, &Fmt> = HashMap::new();
            table.insert("i", &i);
            let s = table.format("{i}").expect("Failed to format");
            assert_that!(&s.as_str(), eq("10"));
        }

        test ad_hoc() {
            // This thing converts its first argument in upper case.
            let f = AdHoc::new(|full_name, _, args: &[String], _, _| {
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
