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

pub fn apply_common_options(
    s: &mut String,
    options: &HashMap<String, String>,
) -> Result<(), SingleFmtError> {
    apply_truncation(s, options)?;
    apply_width(s, options)?;
    Ok(())
}

pub fn apply_width(
    s: &mut String,
    options: &HashMap<String, String>,
) -> Result<(), SingleFmtError> {
    if let Some(width_str) = options.get("width") {
        let justification = match width_str.chars().nth(0) {
            Some('l') => Justification::Left(),
            Some('c') => Justification::Center(),
            Some('r') => Justification::Right(),
            _ => {
                return Err(SingleFmtError::InvalidOptionValue(
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
                "width".to_string(),
                width_str.to_string(),
            ));
        }
    }
    Ok(())
}

pub fn apply_truncation(
    s: &mut String,
    options: &HashMap<String, String>,
) -> Result<(), SingleFmtError> {
    if let Some(opt_str) = options.get("truncate") {
        if opt_str.is_empty() {
            return Err(InvalidOptionValue(
                "truncate".to_string(),
                opt_str.to_string(),
            ));
        }
        let is_left = match opt_str.chars().nth(0) {
            Some('l') => true,
            Some('r') => false,
            _ => {
                return Err(InvalidOptionValue(
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

pub fn float_to_exp<T>(f: T, options: &HashMap<String, String>) -> Result<String, SingleFmtError>
where
    T: num::Float + num::FromPrimitive + Copy + ToString,
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
    if let Some(prec) = get_precision(options)? {
        let mult = ten.powi(prec + p);
        abs = abs * mult;
        abs = match get_rounding(options)? {
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

pub fn float_to_normal<T>(f: T, options: &HashMap<String, String>) -> Result<String, SingleFmtError>
where
    T: num::Float + num::FromPrimitive + Copy + ToString,
{
    if f.is_nan() || f.is_infinite() {
        return Ok(f.to_string());
    }
    let mut f = f.abs();
    if let Some(prec) = get_precision(options)? {
        let mult = T::from_i32(10).unwrap().powi(prec);
        f = f * mult;
        f = match get_rounding(options)? {
            Some(Rounding::Up()) => f.ceil(),
            Some(Rounding::Down()) => f.floor(),
            Some(Rounding::Nearest()) | None => f.round(),
        };
        f = f / mult;
    }
    Ok(f.to_string())
}

pub fn int_to_str<T>(
    i: T,
    flags: &[char],
    options: &HashMap<String, String>,
) -> Result<String, SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::ToPrimitive + ToString + Copy,
{
    let prefix = flags.contains(&'p');
    if flags.contains(&'b') {
        Ok(present_binary(i, prefix, options)?)
    } else if flags.contains(&'o') {
        Ok(present_octal(i, prefix, options)?)
    } else if flags.contains(&'x') {
        Ok(present_hexadecimal(i, prefix, options)?)
    } else {
        Ok(present_decimal(i, options)?)
    }
}

pub fn add_sign<T>(s: &mut String, signed: T, flags: &[char]) -> Result<(), SingleFmtError>
where
    T: num::Signed,
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

fn get_precision(options: &HashMap<String, String>) -> Result<Option<i32>, SingleFmtError> {
    if let Some(s) = options.get("prec") {
        if let Ok(i) = s.parse::<i32>() {
            Ok(Some(i))
        } else {
            Err(InvalidOptionValue("prec".to_string(), s.to_string()))
        }
    } else {
        Ok(None)
    }
}

fn present_binary<T>(
    i: T,
    prefix: bool,
    options: &HashMap<String, String>,
) -> Result<String, SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::ToPrimitive + Copy,
{
    let mut i = if i >= T::zero() { i } else { T::zero() - i };
    let two = T::from_i32(2).unwrap();
    apply_integer_rounding(&mut i, two, options)?;
    let mut chars = Vec::new();
    while i != T::zero() {
        let ch = ((i % two).to_u8().unwrap() + '0' as u8) as char;
        chars.push(ch);
        i = i / two;
    }
    if prefix {
        chars.push('b');
        chars.push('0');
    }
    Ok(chars.iter().rev().collect())
}

fn present_octal<T>(
    i: T,
    prefix: bool,
    options: &HashMap<String, String>,
) -> Result<String, SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::ToPrimitive + Copy,
{
    let mut chars = Vec::new();
    let mut i = if i >= T::zero() { i } else { T::zero() - i };
    let eight = T::from_i32(8).unwrap();
    apply_integer_rounding(&mut i, eight, options)?;
    while i != T::zero() {
        let ch = ((i % eight).to_u8().unwrap() + '0' as u8) as char;
        chars.push(ch);
        i = i / eight;
    }
    if prefix {
        chars.push('o');
        chars.push('0');
    }
    Ok(chars.iter().rev().collect())
}

fn present_hexadecimal<T>(
    i: T,
    prefix: bool,
    options: &HashMap<String, String>,
) -> Result<String, SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::ToPrimitive + Copy,
{
    let mut chars = Vec::new();
    let mut i = if i >= T::zero() { i } else { T::zero() - i };
    let hex = T::from_i32(16).unwrap();
    apply_integer_rounding(&mut i, hex, options)?;
    while i > T::zero() {
        let index = (i % hex).to_u8().unwrap();
        let ch = if index < 10 {
            (index + '0' as u8) as char
        } else {
            (index - 10 + 'a' as u8) as char
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

fn present_decimal<T>(i: T, options: &HashMap<String, String>) -> Result<String, SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::Zero + ToString + Copy,
{
    let mut i = if i >= T::zero() { i } else { T::zero() - i };
    let ten = T::from_i32(10).unwrap();
    apply_integer_rounding(&mut i, ten, options)?;
    Ok(i.to_string())
}

fn get_rounding(options: &HashMap<String, String>) -> Result<Option<Rounding>, SingleFmtError> {
    if let Some(s) = options.get("round") {
        if s == "up" {
            Ok(Some(Rounding::Up()))
        } else if s == "down" {
            Ok(Some(Rounding::Down()))
        } else if s == "nearest" {
            Ok(Some(Rounding::Nearest()))
        } else {
            Err(InvalidOptionValue("round".to_string(), s.to_string()))
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

fn apply_integer_rounding<T>(
    i: &mut T,
    base: T,
    options: &HashMap<String, String>,
) -> Result<(), SingleFmtError>
where
    T: num::Integer + num::FromPrimitive + num::Zero + num::One + Copy,
{
    if let Some(prec) = get_precision(options)? {
        if prec >= 0 {
            return Ok(());
        }
        let two = T::from_i32(2).unwrap();
        let div = ipow(base, -prec);
        let rem = *i % div;
        let quot = *i / div;
        match get_rounding(options)? {
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
