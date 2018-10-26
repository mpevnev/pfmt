use std::collections::HashMap;
use std::iter::repeat;
use std::string::ToString;

use num;

use {SingleFmtError, SingleFmtError::*};

/* ---------- general formatting options ---------- */

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Justification {
    Left(),
    Center(),
    Right(),
}

pub fn apply_common_options<H: std::hash::BuildHasher>(
    name: &[String],
    s: &mut String,
    options: &HashMap<String, String, H>,
) -> Result<(), SingleFmtError> {
    apply_truncation(name, s, options)?;
    apply_width(name, s, options)?;
    Ok(())
}

pub fn apply_width<H: std::hash::BuildHasher>(
    name: &[String],
    s: &mut String,
    options: &HashMap<String, String, H>,
) -> Result<(), SingleFmtError> {
    if let Some(width_str) = options.get("width") {
        let justification = match width_str.chars().nth(0) {
            Some('l') => Justification::Left(),
            Some('c') => Justification::Center(),
            Some('r') => Justification::Right(),
            _ => {
                return Err(SingleFmtError::InvalidOptionValue(
                    join_name(name),
                    "width".to_string(),
                    width_str.to_string(),
                ))
            }
        };
        if let Ok(width) = width_str[1..].parse::<usize>() {
            let len = s.chars().count();
            if len > width {
                return Ok(());
            }
            let delta = width - len;
            match justification {
                Justification::Left() => {
                    let padding: String = repeat(' ').take(delta).collect();
                    s.push_str(&padding);
                }
                Justification::Center() => {
                    let left = delta / 2;
                    let right = delta - left;
                    let leftstr: String = repeat(' ').take(left).collect();
                    let rightstr: String = repeat(' ').take(right).collect();
                    *s = leftstr + s + &rightstr;
                }
                Justification::Right() => {
                    let padding: String = repeat(' ').take(delta).collect();
                    *s = padding + s;
                }
            }
        } else {
            return Err(InvalidOptionValue(
                join_name(name),
                "width".to_string(),
                width_str.to_string(),
            ));
        }
    }
    Ok(())
}

pub fn apply_truncation<H: std::hash::BuildHasher>(
    name: &[String],
    s: &mut String,
    options: &HashMap<String, String, H>,
) -> Result<(), SingleFmtError> {
    if let Some(opt_str) = options.get("truncate") {
        if opt_str.is_empty() {
            return Err(InvalidOptionValue(
                join_name(name),
                "truncate".to_string(),
                opt_str.to_string(),
            ));
        }
        let is_left = match opt_str.chars().nth(0) {
            Some('l') => true,
            Some('r') => false,
            _ => {
                return Err(InvalidOptionValue(
                    join_name(name),
                    "truncate".to_string(),
                    opt_str.to_string(),
                ))
            }
        };
        if let Ok(truncate_to_width) = opt_str[1..].parse::<usize>() {
            let len = s.chars().count();
            if len < truncate_to_width {
                return Ok(());
            }
            if is_left {
                *s = s.chars().skip(len - truncate_to_width).collect();
            } else {
                *s = s.chars().take(truncate_to_width).collect();
            }
            Ok(())
        } else {
            Err(InvalidOptionValue(
                join_name(name),
                "truncate".to_string(),
                opt_str.to_string(),
            ))
        }
    } else {
        Ok(())
    }
}

/* ---------- numerical formatting ---------- */

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Rounding {
    Down(),
    Nearest(),
    Up(),
}

pub fn float_to_exp<T, H>(
    name: &[String],
    f: T, options: &HashMap<String, String, H>
) -> Result<String, SingleFmtError>
where
    T: num::Float + num::FromPrimitive + Copy + ToString,
    H: std::hash::BuildHasher,
{
    if f.is_nan() || f.is_infinite() {
        return Ok(f.to_string());
    }
    let mut abs = f.abs();
    let ten = T::from_i32(10).unwrap();
    let mut p = 0;
    while abs < T::one() || abs >= ten {
        if abs < T::one() {
            p -= 1;
            abs = abs * ten;
        } else {
            p += 1;
            abs = abs / ten;
        }
    }
    if let Some(prec) = get_precision(name, options)? {
        let mult = ten.powi(prec + p);
        abs = abs * mult;
        abs = match get_rounding(name, options)? {
            Some(Rounding::Up()) => abs.ceil(),
            Some(Rounding::Down()) => abs.floor(),
            Some(Rounding::Nearest()) | None => abs.round(),
        };
        abs = abs / mult;
    }
    let abs = abs.to_string();
    let power = p.to_string();
    let mut res = String::with_capacity(abs.len() + power.len() + 2);
    res += &abs;
    res += "e";
    res += &power;
    Ok(res)
}

pub fn float_to_normal<T, H>(
    name: &[String],
    f: T, options: &HashMap<String, String, H>
) -> Result<String, SingleFmtError>
where
    T: num::Float + num::FromPrimitive + Copy + ToString,
    H: std::hash::BuildHasher,
{
    if f.is_nan() || f.is_infinite() {
        return Ok(f.to_string());
    }
    let mut f = f.abs();
    if let Some(prec) = get_precision(name, options)? {
        let mult = T::from_i32(10).unwrap().powi(prec);
        f = f * mult;
        f = match get_rounding(name, options)? {
            Some(Rounding::Up()) => f.ceil(),
            Some(Rounding::Down()) => f.floor(),
            Some(Rounding::Nearest()) | None => f.round(),
        };
        f = f / mult;
    }
    Ok(f.to_string())
}

pub fn int_to_str<T, H>(
    name: &[String],
    i: T,
    flags: &[char],
    options: &HashMap<String, String, H>,
) -> Result<String, SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::ToPrimitive + ToString + Copy,
    H: std::hash::BuildHasher,
{
    let prefix = flags.contains(&'p');
    if flags.contains(&'b') {
        Ok(present_binary(name, i, prefix, options)?)
    } else if flags.contains(&'o') {
        Ok(present_octal(name, i, prefix, options)?)
    } else if flags.contains(&'x') {
        Ok(present_hexadecimal(name, i, prefix, options)?)
    } else {
        Ok(present_decimal(name, i, options)?)
    }
}

pub fn add_sign<T>(s: &mut String, signed: T, flags: &[char]) -> Result<(), SingleFmtError>
where
    T: num::Signed + Copy,
{
    if signed.is_negative() {
        s.insert(0, '-');
    } else if flags.contains(&'+') {
        s.insert(0, '+');
    }
    Ok(())
}

/* ---------- misc ---------- */

pub fn join_name(name: &[String]) -> String {
    let mut res = String::with_capacity(name.len() + name.iter().fold(0, |acc, s| acc + s.len()));
    let mut iter = name.iter();
    if let Some(s) = iter.next() {
        res.push_str(&s);
    }
    for s in iter {
        res.push('.');
        res.push_str(&s);
    }
    res
}

/* ---------- helpers ---------- */

fn get_precision<H: std::hash::BuildHasher>(
    name: &[String],
    options: &HashMap<String, String, H>
) -> Result<Option<i32>, SingleFmtError> {
    if let Some(s) = options.get("prec") {
        if let Ok(i) = s.parse::<i32>() {
            Ok(Some(i))
        } else {
            Err(InvalidOptionValue(join_name(name), "prec".to_string(), s.to_string()))
        }
    } else {
        Ok(None)
    }
}

fn present_binary<T, H>(
    name: &[String],
    i: T,
    prefix: bool,
    options: &HashMap<String, String, H>,
) -> Result<String, SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::ToPrimitive + Copy,
    H: std::hash::BuildHasher,
{
    let mut i = if i >= T::zero() { i } else { T::zero() - i };
    let two = T::from_i32(2).unwrap();
    apply_integer_rounding(name, &mut i, two, options)?;
    let mut chars = Vec::new();
    while i != T::zero() {
        let ch = ((i % two).to_u8().unwrap() + b'0') as char;
        chars.push(ch);
        i = i / two;
    }
    if prefix {
        chars.push('b');
        chars.push('0');
    }
    Ok(chars.iter().rev().collect())
}

fn present_octal<T, H>(
    name: &[String],
    i: T,
    prefix: bool,
    options: &HashMap<String, String, H>,
) -> Result<String, SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::ToPrimitive + Copy,
    H: std::hash::BuildHasher,
{
    let mut chars = Vec::new();
    let mut i = if i >= T::zero() { i } else { T::zero() - i };
    let eight = T::from_i32(8).unwrap();
    apply_integer_rounding(name, &mut i, eight, options)?;
    while i != T::zero() {
        let ch = ((i % eight).to_u8().unwrap() + b'0') as char;
        chars.push(ch);
        i = i / eight;
    }
    if prefix {
        chars.push('o');
        chars.push('0');
    }
    Ok(chars.iter().rev().collect())
}

fn present_hexadecimal<T, H>(
    name: &[String],
    i: T,
    prefix: bool,
    options: &HashMap<String, String, H>,
) -> Result<String, SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::ToPrimitive + Copy,
    H: std::hash::BuildHasher,
{
    let mut chars = Vec::new();
    let mut i = if i >= T::zero() { i } else { T::zero() - i };
    let hex = T::from_i32(16).unwrap();
    apply_integer_rounding(name, &mut i, hex, options)?;
    while i > T::zero() {
        let index = (i % hex).to_u8().unwrap();
        let ch = if index < 10 {
            (index + b'0') as char
        } else {
            (index - 10 + b'a') as char
        };
        chars.push(ch);
        i = i / hex;
    }
    if prefix {
        chars.push('x');
        chars.push('0');
    }
    Ok(chars.iter().rev().collect())
}

fn present_decimal<T, H>(
    name: &[String],
    i: T, options: &HashMap<String, String, H>
) -> Result<String, SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::Zero + ToString + Copy,
    H: std::hash::BuildHasher,
{
    let mut i = if i >= T::zero() { i } else { T::zero() - i };
    let ten = T::from_i32(10).unwrap();
    apply_integer_rounding(name, &mut i, ten, options)?;
    Ok(i.to_string())
}

fn get_rounding<H: std::hash::BuildHasher>(
    name: &[String],
    options: &HashMap<String, String, H>
) -> Result<Option<Rounding>, SingleFmtError> {
    if let Some(s) = options.get("round") {
        if s == "up" {
            Ok(Some(Rounding::Up()))
        } else if s == "down" {
            Ok(Some(Rounding::Down()))
        } else if s == "nearest" {
            Ok(Some(Rounding::Nearest()))
        } else {
            Err(InvalidOptionValue(join_name(name), "round".to_string(), s.to_string()))
        }
    } else {
        Ok(None)
    }
}

fn ipow<T, P>(i: T, p: P) -> T
where
    T: num::Integer + num::Zero + num::One + Copy,
    P: num::Integer + num::Zero + num::One + Copy,
{
    let mut res = T::one();
    let mut p = p;
    while p > P::zero() {
        res = res * i;
        p = p - P::one();
    }
    res
}

fn apply_integer_rounding<T, H>(
    name: &[String],
    i: &mut T,
    base: T,
    options: &HashMap<String, String, H>,
) -> Result<(), SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::Zero + num::One + Copy,
    H: std::hash::BuildHasher,
{
    if let Some(prec) = get_precision(name, options)? {
        if prec >= 0 {
            return Ok(());
        }
        let two = T::from_i32(2).unwrap();
        let div = ipow(base, -prec);
        let rem = *i % div;
        let quot = *i / div;
        match get_rounding(name, options)? {
            Some(Rounding::Up()) => {
                if rem > T::zero() {
                    *i = (quot + T::one()) * div;
                } else {
                    *i = quot * div;
                }
            }
            Some(Rounding::Down()) => {
                *i = quot * div;
            }
            Some(Rounding::Nearest()) | None => {
                if rem >= div / two {
                    *i = (quot + T::one()) * div;
                } else {
                    *i = quot * div;
                }
            }
        }
    }
    Ok(())
}
