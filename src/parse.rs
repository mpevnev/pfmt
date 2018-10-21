use std::collections::HashMap;
use std::iter::Peekable;

const ESCAPE: char = '\\';
const SETOPT: char = '=';
const FIELD_SEPARATOR: char = ':';
const OPENING_BRACKET: char = '{';
const CLOSING_BRACKET: char = '}';
const DOT: char = '.';
const MAX_RECURSION_DEPTH: u8 = 100;

#[derive(Debug, PartialEq)]
/// An element of a format string.
pub enum Piece {
    /// A literal string.
    Literal(String),
    /// A single placeholder.
    ///
    /// Contains name segments, arguments, flags and options.
    Placeholder(Vec<String>, Vec<Piece>, Vec<char>, HashMap<String, Piece>),
    /// Multiple elements.
    Multi(Vec<Piece>),
}

#[derive(Debug, PartialEq, Eq)]
/// A part of a format string that will map to a single literal or placeholder.
struct InputChunk<'a> {
    input: &'a str,
    start: usize,
    kind: InputChunkKind,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
/// A kind of input chunk.
enum InputChunkKind {
    Literal,
    Placeholder,
    MultiplePieces,
}

/// An error that can occur while parsing a format string.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ParseError {
    /// Returned if the brackets in a format string are unbalanced.
    ///
    /// Contains the position (byte-wise) of the unbalanced bracket in the
    /// input.
    UnbalancedBrackets(usize),
    /// Returned if a name segment in one of the placeholders is empty.
    ///
    /// Contains the position (byte-wise) of the empty segment in the input.
    EmptyNameSegment(usize),
    /// Returned when an option has an empty name.
    ///
    /// Contains the position (byte-wise) of the option in the input.
    EmptyOptionName(usize),
}

pub fn parse(input: &str, position: usize, recursion_depth: u8) -> Result<Piece, ParseError> {
    validate_brackets(input)?;
    let mut pieces = Vec::new();
    for chunk in split(input, position) {
        pieces.push(parse_chunk(chunk, recursion_depth)?);
    }
    if pieces.len() == 1 {
        Ok(pieces.remove(0))
    } else {
        Ok(Piece::Multi(pieces))
    }
}

/// Transform an input chunk into a literal or a placeholder.
fn parse_chunk<'a>(chunk: InputChunk<'a>, recursion_depth: u8) -> Result<Piece, ParseError> {
    if recursion_depth >= MAX_RECURSION_DEPTH {
        return Ok(parse_literal(chunk.input));
    }
    match chunk.kind {
        InputChunkKind::Literal => Ok(parse_literal(chunk.input)),
        InputChunkKind::Placeholder => parse_placeholder(chunk.input, chunk.start, recursion_depth),
        InputChunkKind::MultiplePieces => parse(chunk.input, chunk.start, recursion_depth),
    }
}

/// Transform an input string into a literal.
fn parse_literal(input: &str) -> Piece {
    let mut prev = None;
    let mut string = String::with_capacity(input.len());
    for ch in input.chars() {
        if prev == Some(ESCAPE) {
            if ch == ESCAPE {
                string.push(ESCAPE);
                prev = None;
            } else {
                string.push(ch);
                prev = Some(ch);
            }
        } else {
            if ch != ESCAPE {
                string.push(ch);
            }
            prev = Some(ch);
        }
    }
    Piece::Literal(string)
}

/// Transform an input string into a placeholder.
fn parse_placeholder(
    input: &str,
    position: usize,
    recursion_depth: u8,
) -> Result<Piece, ParseError> {
    let mut iter = input.char_indices().peekable();
    let name = extract_name(&mut iter, position)?;
    let args = extract_args(input, &mut iter, position, recursion_depth)?;
    let flags = extract_flags(&mut iter);
    let options = extract_options(input, &mut iter, position, recursion_depth)?;
    Ok(Piece::Placeholder(name, args, flags, options))
}

/// Extract a name from the input string. Colons and opening brackets terminate
/// the name and are not consumed by the function.
fn extract_name<T>(iter: &mut Peekable<T>, position: usize) -> Result<Vec<String>, ParseError>
where
    T: Iterator<Item = (usize, char)>,
{
    let mut name = Vec::new();
    let mut segment = String::new();
    let mut prev = None;
    let mut last_segment_start = 0;
    while let Some(&(i, ch)) = iter.peek() {
        if prev == Some(ESCAPE) {
            if ch == ESCAPE {
                segment.push(ESCAPE);
                prev = None;
            } else {
                if ch != ESCAPE {
                    segment.push(ch);
                }
                prev = Some(ch);
            }
            iter.next();
        } else {
            if ch == DOT {
                name.push(trim_name_segment(segment, position + last_segment_start)?);
                segment = String::new();
                last_segment_start = i + 1;
                iter.next();
            } else if ch == FIELD_SEPARATOR || ch == OPENING_BRACKET {
                name.push(trim_name_segment(segment, position + last_segment_start)?);
                return Ok(name);
            } else {
                if ch != ESCAPE {
                    segment.push(ch);
                }
                iter.next();
            }
            prev = Some(ch);
        }
    }
    name.push(trim_name_segment(segment, position + last_segment_start)?);
    Ok(name)
}

/// Extract placeholder's arguments from the input string, if any. Terminating
/// bracket is consumed.
fn extract_args<T>(
    source: &str,
    iter: &mut Peekable<T>,
    position: usize,
    recursion_depth: u8,
) -> Result<Vec<Piece>, ParseError>
where
    T: Iterator<Item = (usize, char)>,
{
    let mut res = Vec::new();
    if let Some(&(_, ch)) = iter.peek() {
        if ch != OPENING_BRACKET {
            return Ok(res);
        }
        let args_portion = extract_between_brackets(source, iter);
        let chunks = split_on_colons(args_portion, position);
        for chunk in chunks.iter() {
            let piece = parse(chunk.input, chunk.start, recursion_depth + 1)?;
            res.push(piece);
        }
    }
    Ok(res)
}

/// Extract placeholder's flags from the input string, if any. Terminating
/// colon is consumed.
fn extract_flags<T>(iter: &mut Peekable<T>) -> Vec<char>
where
    T: Iterator<Item = (usize, char)>,
{
    if let Some(&(_, FIELD_SEPARATOR)) = iter.peek() {
        iter.next();
    }
    let mut res = Vec::new();
    let mut prev = None;
    while let Some((_, ch)) = iter.next() {
        if prev == Some(ESCAPE) {
            res.push(ch);
            prev = if ch == ESCAPE { None } else { Some(ch) };
        } else {
            if ch == FIELD_SEPARATOR {
                return res;
            }
            if ch != ESCAPE {
                res.push(ch);
            }
            prev = Some(ch);
        }
    }
    res
}

/// Extract placeholder's options from the input string, if any.
fn extract_options<T>(
    source: &str,
    iter: &mut Peekable<T>,
    sourcepos: usize,
    recursion_depth: u8,
) -> Result<HashMap<String, Piece>, ParseError>
where
    T: Iterator<Item = (usize, char)>,
{
    let section_start;
    if let Some((i, _)) = iter.next() {
        section_start = i;
    } else {
        return Ok(HashMap::new());
    }
    let chunks = split_on_colons(&source[section_start..], sourcepos + section_start);
    let mut res = HashMap::new();
    for chunk in chunks {
        let (opt, val) = parse_option(chunk.input, chunk.start, recursion_depth)?;
        res.insert(opt, val);
    }
    Ok(res)
}

/// Split a (portion of) format string into individual chunks.
fn split(source: &str, sourcepos: usize) -> Vec<InputChunk<'_>> {
    let mut iter = source.char_indices().peekable();
    let mut res = Vec::new();
    while let Some(&(i, ch)) = iter.peek() {
        let chunk;
        if ch == OPENING_BRACKET {
            let portion = extract_between_brackets(source, &mut iter);
            chunk = InputChunk {
                input: portion,
                start: sourcepos + i,
                kind: InputChunkKind::Placeholder,
            };
        } else {
            let portion = extract_literal(source, &mut iter);
            chunk = InputChunk {
                input: portion,
                start: sourcepos + i,
                kind: InputChunkKind::Literal,
            };
        }
        res.push(chunk);
    }
    res
}

/// Split a portion of format string on colons that are not nested in
/// placeholders. The function assumes that brackets in the input are balanced.
fn split_on_colons(source: &str, sourcepos: usize) -> Vec<InputChunk<'_>> {
    let mut iter = source.char_indices().peekable();
    let mut res = Vec::new();
    let mut balance: usize = 0;
    let mut prev = None;
    let mut chunk_start = 0;
    if let Some(&(_, FIELD_SEPARATOR)) = iter.peek() {
        iter.next();
    }
    for (i, ch) in iter {
        if prev == Some(ESCAPE) {
            if ch == ESCAPE {
                prev = None;
            } else {
                prev = Some(ch);
            }
        } else {
            if ch == OPENING_BRACKET {
                balance += 1;
            } else if ch == CLOSING_BRACKET {
                balance -= 1;
            } else if ch == FIELD_SEPARATOR && balance == 0 {
                res.push(InputChunk {
                    input: &source[chunk_start..i],
                    start: sourcepos + chunk_start,
                    kind: InputChunkKind::MultiplePieces,
                });
                chunk_start = i + 1;
            }
            prev = Some(ch);
        }
    }
    if chunk_start < source.len() {
        res.push(InputChunk {
            input: &source[chunk_start..],
            start: sourcepos + chunk_start,
            kind: InputChunkKind::MultiplePieces,
        });
    }
    res
}

/// Parse a key-value pair into an option name and a Piece.
fn parse_option(
    input: &str,
    sourcepos: usize,
    recursion_depth: u8,
) -> Result<(String, Piece), ParseError> {
    let mut name = String::new();
    let mut prev = None;
    for (i, ch) in input.char_indices() {
        if prev == Some(ESCAPE) {
            name.push(ch);
            prev = if ch == ESCAPE { None } else { Some(ch) };
        } else {
            if ch == SETOPT {
                let value = parse(&input[i + 1..], sourcepos + i + 1, recursion_depth + 1)?;
                let name = name.trim().to_string();
                if name.is_empty() {
                    return Err(ParseError::EmptyOptionName(sourcepos));
                } else {
                    return Ok((name, value));
                }
            } else if ch != ESCAPE {
                name.push(ch);
            }
            prev = Some(ch);
        }
    }
    Err(ParseError::EmptyOptionName(sourcepos))
}

/// Extract a literal from a format string. Stops either at an opening bracket
/// without consuming it, or at a colon (consuming it).
fn extract_literal<'a, T>(source: &'a str, iter: &mut Peekable<T>) -> &'a str
where
    T: Iterator<Item = (usize, char)>,
{
    let mut prev = None;
    let mut end = source.len();
    let mut start = 0;
    if let Some(&(i, ch)) = iter.peek() {
        start = i;
        if ch == FIELD_SEPARATOR {
            iter.next();
            start += 1;
        }
    }
    while let Some(&(i, ch)) = iter.peek() {
        if prev == Some(ESCAPE) {
            if ch == ESCAPE {
                prev = None;
            } else {
                prev = Some(ch);
            }
            iter.next();
        } else {
            if ch == FIELD_SEPARATOR {
                iter.next();
                end = i;
                break;
            } else if ch == OPENING_BRACKET {
                end = i;
                break;
            } else {
                prev = Some(ch);
                iter.next();
            }
        }
    }
    &source[start..end]
}

/// Extract a portion of input between a balanced pair of brackets. The first
/// character of the input is assumed to be a bracket, and the terminating
/// bracket is consumed. This function also assumes that brackets are balanced
/// in the input.
fn extract_between_brackets<'a, T>(source: &'a str, iter: &mut Peekable<T>) -> &'a str
where
    T: Iterator<Item = (usize, char)>,
{
    let mut prev = None;
    let mut balance: usize = 1;
    let start = iter.next().unwrap().0;
    for (i, ch) in iter {
        if prev == Some(ESCAPE) {
            if ch == ESCAPE {
                prev = None;
            } else {
                prev = Some(ch);
            }
        } else {
            if ch == OPENING_BRACKET {
                balance += 1;
            } else if ch == CLOSING_BRACKET {
                balance -= 1;
                if balance == 0 {
                    return &source[start + 1 .. i];
                }
            }
            prev = Some(ch);
        }
    }
    unreachable!("A string with imbalanced brackets was passed to 'extract_between_brackets'");
}

fn trim_name_segment(segment: String, segment_start: usize) -> Result<String, ParseError> {
    let res = segment.trim().to_string();
    if res.is_empty() {
        Err(ParseError::EmptyNameSegment(segment_start))
    } else {
        Ok(res)
    }
}

fn validate_brackets(source: &str) -> Result<(), ParseError> {
    let mut prev = None;
    let mut opening_brackets = Vec::new();
    for (i, ch) in source.char_indices() {
        if prev == Some(ESCAPE) {
            if ch == ESCAPE {
                prev = None;
            } else {
                prev = Some(ch);
            }
        } else {
            if ch == OPENING_BRACKET {
                opening_brackets.push(i);
            }
            if ch == CLOSING_BRACKET {
                if opening_brackets.pop().is_none() {
                    return Err(ParseError::UnbalancedBrackets(i));
                }
            }
            prev = Some(ch);
        }
    }
    if let Some(i) = opening_brackets.pop() {
        Err(ParseError::UnbalancedBrackets(i))
    } else {
        Ok(())
    }
}

#[cfg(test)]
impl Piece {
    pub fn get_subpieces(&self) -> &[Piece] {
        use util;
        match self {
            Piece::Literal(s) => panic!("Expected a Multi piece, got literal '{}'", s),
            Piece::Placeholder(name, ..) => panic!("Expected a Multi piece, got placeholder '{}'",
                                                   util::join_name(name)),
            Piece::Multi(v) => &v,
        }
    }
}

#[cfg(test)]
mod tests {
    test_suite! {
        name basic_tests;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;

        use parse::*;
        use Piece::*;

        test literal() {
            let s = "asdf 1";
            let piece = parse(s, 0, 0).expect("Failed to parse");
            assert_that!(&piece, eq(Literal(s.to_string())));
        }

        test single_placeholder_1() {
            let s = "a{b}c";
            let multi = parse(s, 0, 0).expect("Failed to parse");
            let pieces = multi.get_subpieces();
            assert_that!(&pieces.len(), eq(3));
            assert_that!(&pieces[0], eq(Literal("a".to_string())));
            assert_that!(&pieces[1], has_structure!(Placeholder [
                eq(vec!["b".to_string()]),
                eq(vec![]),
                eq(vec![]),
                eq(HashMap::new())
            ]));
            assert_that!(&pieces[2], eq(Literal("c".to_string())));
        }

        test single_placeholder_2() {
            let s = "a{b}";
            let res = parse(s, 0, 0).expect("Failed to parse");
            let pieces = res.get_subpieces();
            assert_that!(&pieces.len(), eq(2));
            assert_that!(&pieces[0], eq(Literal("a".to_string())));
            assert_that!(&pieces[1], has_structure!(Placeholder [
                eq(vec!["b".to_string()]),
                eq(vec![]),
                eq(vec![]),
                eq(HashMap::new())
            ]));
        }

        test several_placeholders() {
            let s = "a{b}c{d}";
            let res = parse(s, 0, 0).expect("Failed to get any pieces");
            let pieces = res.get_subpieces();
            assert_that!(&pieces.len(), eq(4));
            assert_that!(&pieces[0], eq(Literal("a".to_string())));
            assert_that!(&pieces[1], has_structure!(Placeholder [
                eq(vec!["b".to_string()]),
                eq(vec![]),
                eq(vec![]),
                eq(HashMap::new())
            ]));
            assert_that!(&pieces[2], eq(Literal("c".to_string())));
            assert_that!(&pieces[3], has_structure!(Placeholder [
                eq(vec!["d".to_string()]),
                eq(vec![]),
                eq(vec![]),
                eq(HashMap::new())
            ]));
        }

        test explicit_separator_before_literal() {
            let s = "{foobar}:asdf";
            let res = parse(s, 0, 0).expect("Failed to parse");
            let pieces = res.get_subpieces();
            assert_that!(&pieces.len(), eq(2));
            assert_that!(&pieces[0], has_structure!(Placeholder [
                eq(vec!["foobar".to_string()]),
                eq(vec![]),
                eq(vec![]),
                eq(HashMap::new())
            ]));
            assert_that!(&pieces[1], eq(Literal("asdf".to_string())));
        }

        test escapes_in_literals() {
            let s = "a\\:b\\{c\\}d\\\\";
            let piece = parse(s, 0, 0).expect("Failed to parse");
            assert_that!(&piece, eq(Literal("a:b{c}d\\".to_string())));
        }

        test escapes_in_placeholder_names() {
            let s = "{fo\\:ob\\\\ar\\{\\}}";
            let piece = parse(s, 0, 0).expect("Failed to parse");
            assert_that!(&piece, has_structure!(Placeholder [
                eq(vec!["fo:ob\\ar{}".to_string()]),
                eq(vec![]),
                eq(vec![]),
                eq(HashMap::new())
            ]));
        }

        test escapes_in_option_names() {
            let s = "{foobar::o\\:p\\{\\}t\\\\ion=1}";
            let piece = parse(s, 0, 0).expect("Failed to parse");
            assert_that!(&piece, has_structure!(Placeholder [
                eq(vec!["foobar".to_string()]),
                eq(vec![]),
                eq(vec![]),
                eq({
                    let mut res = HashMap::new();
                    let lit = Literal("1".to_string());
                    res.insert("o:p{}t\\ion".to_string(), lit);
                    res
                })
            ]));
        }

        test multiple_options() {
            let s = "{foobar::a=a:b=b}";
            let piece = parse(s, 0, 0).expect("Parse failed");
            assert_that!(&piece, has_structure!(Placeholder [
                eq(vec!["foobar".to_string()]),
                eq(vec![]),
                eq(vec![]),
                eq({
                    let mut res = HashMap::new();
                    let a = Literal("a".to_string());
                    let b = Literal("b".to_string());
                    res.insert("a".to_string(), a);
                    res.insert("b".to_string(), b);
                    res
                })
            ]));
        }

        test several_segments_in_name() {
            let s = "{a.b.c}";
            let piece = parse(s, 0, 0).expect("Failed to parse");
            assert_that!(&piece, has_structure!(Placeholder [
                eq(vec!["a".to_string(), "b".to_string(), "c".to_string()]),
                any_value(),
                any_value(),
                any_value()
            ]));
        }

    }

    test_suite! {
        name errors;
        use galvanic_assert::matchers::*;

        use parse::*;
        use parse::ParseError::*;

        test unbalanced_brackets_1() {
            let s = "{";
            let err = parse(s, 0, 0).expect_err("Parse erroneously succeeded");
            assert_that!(&err, eq(UnbalancedBrackets(0)));
        }

        test unbalanced_brackets_2() {
            let s = "{{}";
            let err = parse(s, 0, 0).expect_err("Parse erroneously succeeded");
            assert_that!(&err, eq(UnbalancedBrackets(0)));
        }

        test unbalanced_brackets_3() {
            let s = "{}}";
            let err = parse(s, 0, 0).expect_err("Parse erroneously succeeded");
            assert_that!(&err, eq(UnbalancedBrackets(2)));
        }

        test unbalanced_brackets_4() {
            let s = "0123{";
            let err = parse(s, 0, 0).expect_err("Parse erroneously succeeded");
            assert_that!(&err, eq(UnbalancedBrackets(4)));
        }

        test empty_name_segment_1() {
            let s = "0123{}";
            let err = parse(s, 0, 0).expect_err("Parse erroneously succeeded");
            assert_that!(&err, eq(EmptyNameSegment(4)));
        }

        test empty_name_segment_2() {
            let s = "{.a}";
            let err = parse(s, 0, 0).expect_err("Parse erroneously succeeded");
            assert_that!(&err, eq(EmptyNameSegment(0)));
        }

        test empty_name_segment_3() {
            let s = "{12..b}";
            let err = parse(s, 0, 0).expect_err("Parse erroneously succeeded");
            assert_that!(&err, eq(EmptyNameSegment(3)));
        }

        test empty_name_segment_4() {
            let s = "{a.}";
            let err = parse(s, 0, 0).expect_err("Parse erroneously succeeded");
            assert_that!(&err, eq(EmptyNameSegment(2)));
        }

        test empty_option_name_1() {
            let s = "{f::=4}";
            let err = parse(s, 0, 0).expect_err("Parse erroneously succeeded");
            assert_that!(&err, eq(EmptyOptionName(3)));
        }

        test empty_option_name_2() {
            let s = "{f::a=b::}";
            let err = parse(s, 0, 0).expect_err("Parse erroneously succeeded");
            assert_that!(&err, eq(EmptyOptionName(7)));
        }

    }

    test_suite! {
        name arguments;
        use std::collections::HashMap;
        use galvanic_assert::matchers::*;

        use parse::*;
        use Piece::*;

        test single_argument() {
            let s = "{foobar{asdf}}";
            let piece = parse(s, 0, 0).expect("Failed to parse");
            assert_that!(&piece, has_structure!(Placeholder [
                eq(vec!["foobar".to_string()]),
                eq(vec![Literal("asdf".to_string())]),
                any_value(),
                any_value()
            ]));
        }

        test two_literals() {
            let s = "{foobar{a:b}}";
            let piece = parse(s, 0, 0).expect("Failed to parse");
            assert_that!(&piece, has_structure!(Placeholder [
                eq(vec!["foobar".to_string()]),
                eq(vec![Literal("a".to_string()), Literal("b".to_string())]),
                any_value(),
                any_value()
            ]));
        }

        test empty_arguments() {
            let s = "{foobar{::}}";
            let piece = parse(s, 0, 0).expect("Failed to parse");
            assert_that!(&piece, has_structure!(Placeholder [
                eq(vec!["foobar".to_string()]),
                eq(vec![Literal("".to_string()), Literal("".to_string())]),
                any_value(),
                any_value()
            ]));
        }

        test literal_and_placeholder_as_one_arg() {
            let s = "{foobar{aaa{bbb}}}";
            let piece = parse(s, 0, 0).expect("Failed to parse");
            assert_that!(&piece, has_structure!(Placeholder [
                eq(vec!["foobar".to_string()]),
                eq(vec![Multi(
                    vec![Literal("aaa".to_string()),
                         Placeholder(vec!["bbb".to_string()],
                                     vec![],
                                     vec![],
                                     HashMap::new())
                    ])]),
                any_value(),
                any_value()
            ]));
        }

    }

}
