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
#[derive(Debug, PartialEq)]
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
            string.push(ch);
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
                last_segment_start = i;
                iter.next();
            } else if ch == FIELD_SEPARATOR || ch == OPENING_BRACKET {
                name.push(trim_name_segment(segment, position + last_segment_start)?);
                return Ok(name);
            } else {
                segment.push(ch);
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
            res.push(ch);
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
    for InputChunk {
        input: chunk,
        start,
        ..
    } in chunks
    {
        let (opt, val) = parse_option(chunk, start, recursion_depth)?;
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
            } else {
                name.push(ch);
            }
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
    let mut balance: usize = 0;
    let mut last_bracket_pos = 0;
    for (i, ch) in source.char_indices() {
        if prev == Some(ESCAPE) {
            if ch == ESCAPE {
                prev = None;
            } else {
                prev = Some(ch);
            }
        } else {
            if ch == OPENING_BRACKET {
                balance += 1;
                last_bracket_pos = i;
            }
            if ch == CLOSING_BRACKET {
                if balance == 0 {
                    return Err(ParseError::UnbalancedBrackets(i));
                }
                balance -= 1;
            }
            prev = Some(ch);
        }
    }
    if balance != 0 {
        Err(ParseError::UnbalancedBrackets(last_bracket_pos))
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
            let piece = parse(&s, 0, 0).expect("Failed to parse");
            assert_that!(&piece, eq(Literal(s.to_string())));
        }

        test single_placeholder_1() {
            let s = "a{b}c";
            let multi = parse(&s, 0, 0).expect("Failed to parse");
            let pieces = multi.get_subpieces();
            assert_that!(&pieces[0], eq(Literal("a".to_string())));
            assert_that!(&pieces[1], has_structure!(Placeholder [
                eq(vec!["b".to_string()]),
                eq(vec![]),
                eq(vec![]),
                eq(HashMap::new())
            ]));
            assert_that!(&pieces[2], eq(Literal("c".to_string())));
        }

    }
}
/*

        test single_placeholder_2() {
            let s = "a{b}";
            let pieces = parse(&s, 0, 0).expect("Failed to get any pieces");
            assert_that!(&pieces.len(), eq(2));
            let a = &pieces[0];
            let b = &pieces[1];
            assert_that!(&a, eq(Literal("a".to_string())));
            assert_that!(&b, has_structure!(Placeholder [
                                            eq(vec!["b".to_string()]),
                                            eq(Vec::new()),
                                            eq(Vec::new()),
                                            eq(HashMap::new())
            ]));
        }

        test several_placeholders() {
            let s = "a{b}c{d}";
            let pieces = parse(&s, 0, 0).expect("Failed to get any pieces");
            assert_that!(&pieces.len(), eq(4));
            let a = &pieces[0];
            let b = &pieces[1];
            let c = &pieces[2];
            let d = &pieces[3];
            assert_that!(&a, eq(Literal("a".to_string())));
            assert_that!(&b, has_structure!(Placeholder [
                                            eq(vec!["b".to_string()]),
                                            eq(Vec::new()),
                                            eq(Vec::new()),
                                            eq(HashMap::new())
            ]));
            assert_that!(&c, eq(Literal("c".to_string())));
            assert_that!(&d, has_structure!(Placeholder [
                                            eq(vec!["d".to_string()]),
                                            eq(Vec::new()),
                                            eq(Vec::new()),
                                            eq(HashMap::new())
            ]));
        }

        test explicit_separator_before_literal() {
            let s = "{foobar}:asdf";
            let pieces = parse(&s, 0, 0).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(2));
            let pl = &pieces[0];
            assert_that!(&pl,
                         eq(Placeholder(vec!["foobar".to_string()],
                         Vec::new(),
                         Vec::new(),
                         HashMap::new()
                         )));
            let lit = &pieces[1];
            assert_that!(&lit, eq(Literal("asdf".to_string())));
        }

        test escapes_in_literals() {
            let s = "a\\:b\\{c\\}d\\\\";
            let pieces = parse(&s, 0, 0).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, eq(Literal("a:b{c}d\\".to_string())));
        }

        test escapes_in_placeholder_names() {
            let s = "{fo\\:ob\\\\ar\\{\\}}";
            let pieces = parse(&s, 0, 0).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, eq(Placeholder(vec!["fo:ob\\ar{}".to_string()],
                                                Vec::new(),
                                                Vec::new(),
                                                HashMap::new())));
        }

        test escapes_in_option_names() {
            let s = "{foobar::o\\:p\\{\\}t\\\\ion=1}";
            let pieces = parse(&s, 0, 0).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, eq(Placeholder(vec!["foobar".to_string()],
                                                Vec::new(),
                                                Vec::new(),
                                                {
                                                    let mut m = HashMap::new();
                                                    let s = "o:p{}t\\ion".to_string();
                                                    let lit = Literal("1".to_string());
                                                    m.insert(s, lit);
                                                    m
                                                })));
        }

        test multiple_options() {
            let s = "{foobar::a=a:b=b}";
            let pieces = parse(&s, 0, 0).expect("Parse failed");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(piece, eq(Placeholder(vec!["foobar".to_string()],
                                    Vec::new(),
                                    Vec::new(),
                                    {
                                        let mut m = HashMap::new();
                                        let a = Literal("a".to_string());
                                        let b = Literal("b".to_string());
                                        m.insert("a".to_string(), a);
                                        m.insert("b".to_string(), b);
                                        m
                                    })));
        }

    }

    test_suite! {
        name errors;
        use galvanic_assert::matchers::*;

        use parse::*;
        use parse::ParseError::*;

        test unterminated_name() {
            let s = "12{asdf";
            let err = parse(&s, 0, 0).expect_err("Parse succeeded");
            assert_that!(&err, has_structure!(
                    UnterminatedPlaceholder [eq("{asdf".to_string())]
                    ));
        }

        test unterminated_arguments_list() {
            let s = "12{asdf{qq";
            let err = parse(&s, 0, 0).expect_err("Parse succeeded");
            assert_that!(&err, eq(UnterminatedArgumentList("{asdf{qq".to_string())));
        }

        test unterminated_argument() {
            let s = "12{asdf{{a";
            let err = parse(&s, 0, 0).expect_err("Parse succeeded");
            assert_that!(&err, eq(UnterminatedPlaceholder("{a".to_string())));
        }

        test no_closing_bracket_after_arguments() {
            let s = "{foobar{asdf}";
            let err = parse(&s, 0, 0).expect_err("Parse succeeded");
            assert_that!(&err, eq(UnterminatedPlaceholder(s.to_string())));
        }

        test unterminated_flags() {
            let s = "{foobar:asdf";
            let err = parse(&s, 0, 0).expect_err("Parse succeeded");
            assert_that!(&err, eq(UnterminatedPlaceholder(s.to_string())));
        }

        test unterminated_options() {
            let s= "{foobar::";
            let err = parse(&s, 0, 0).expect_err("Parse succeeded");
            assert_that!(&err, eq(UnterminatedPlaceholder(s.to_string())));
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
            let pieces = parse(&s, 0, 0).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, has_structure!(
                    Placeholder [
                        any_value(),
                        eq(vec![Literal("asdf".to_string())]),
                        any_value(),
                        any_value()
                    ]));
        }

        test two_literals() {
            let s = "{foobar{a:b}}";
            let pieces = parse(&s, 0, 0).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, has_structure!(
                    Placeholder [
                        any_value(),
                        eq(vec![Literal("a".to_string()), Literal("b".to_string())]),
                        any_value(),
                        any_value()
                    ]));
        }

        test empty_arguments() {
            let s = "{foobar{::}}";
            let pieces = parse(&s, 0, 0).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, has_structure!(
                    Placeholder [
                        any_value(),
                        eq(vec![Literal("".to_string()),
                                Literal("".to_string()),
                                Literal("".to_string())
                        ]),
                        any_value(),
                        any_value()
                    ]));
        }

        test full_literal() {
            let s = "{foobar{{baz{arg}flags:opt=1}}}";
            let pieces = parse(&s, 0, 0).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            if let Placeholder(_, args, _, _) = piece {
                assert_that!(&args.len(), eq(1));
                assert_that!(&args[0], eq(Placeholder(vec!["baz".to_string()],
                                            vec![Literal("arg".to_string())],
                                            vec!['f', 'l', 'a', 'g', 's'],
                                            {
                                                let mut m = HashMap::new();
                                                let lit = Literal("1".to_string());
                                                m.insert("opt".to_string(), lit);
                                                m
                                            })));
            } else {
                panic!("Not a placeholder: {:?}", piece);
            }
        }

    }

    test_suite! {
        name names;
        use galvanic_assert::matchers::*;

        use parse::*;
        use Piece::*;

        test several_segments() {
            let s = "{a.b.c}";
            let res = parse(&s, 0, 0);
            let pieces = res.expect("Failed to get any pieces");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, has_structure!(
                    Placeholder [
                        eq(vec!["a".to_string(), "b".to_string(), "c".to_string()]),
                        any_value(),
                        any_value(),
                        any_value()
                    ]));
        }

        test empty_segment() {
            let s = "{a..c}";
            let res = parse(&s, 0, 0);
            assert_that!(&res, eq(Err(ParseError::EmptyNameSegment("{a..c}".to_string()))));
        }

        test escapes_in_segments() {
            let s = "{a\\..b}";
            let res = parse(&s, 0, 0);
            let pieces = res.expect("Failed to get any pieces");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, has_structure!(
                    Placeholder [
                        eq(vec!["a.".to_string(), "b".to_string()]),
                        any_value(),
                        any_value(),
                        any_value()
                    ]));
        }

    }

}
*/
