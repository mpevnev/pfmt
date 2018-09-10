use std::collections::HashMap;

const ESCAPE: char = '\\';
const SETOPT: char = '=';
const FIELD_SEPARATOR: char = ':';
const OPENING_BRACKET: char = '{';
const CLOSING_BRACKET: char = '}';
const MAX_RECURSION_DEPTH: u8 = 100;

/// Either a literal string, or a placecholder.
#[derive(Debug, PartialEq)]
pub enum Piece {
    Literal(String),
    Placeholder(String, Vec<Piece>, Vec<char>, HashMap<String, Piece>)
}

/// Errors that occur during parsing a format string.
#[derive(Debug, PartialEq)]
pub enum ParseError {
    EmptyName(String),
    ArgumentListOrFieldSeparatorExpected(String),
    UnterminatedArgumentList(String),
    FieldSeparatorExpectedToStartFlags(String),
    FieldSeparatorExpectedToStartOptions(String),
    UnterminatedPlaceholder(String)
}

pub fn parse(input: &str) -> Result<Vec<Piece>, ParseError> {
    let mut input = input;
    let mut res = Vec::new();
    while !input.is_empty() {
        let (piece, rest) = parse_piece(input, 0)?;
        input = rest;
        res.push(piece);
    }
    Ok(res)
}

fn parse_piece(input: &str, recursion_depth: u8) -> Result<(Piece, &str), ParseError> {
    let mut iter = input.chars();
    match iter.next() {
        Some(OPENING_BRACKET) => parse_placeholder(input, recursion_depth),
        _ => parse_literal(input)
    }
}

fn parse_literal(input: &str) -> Result<(Piece, &str), ParseError> {
    let mut prev = None;
    let mut literal = String::new();
    for (i, ch) in input.char_indices() {
        if ch == FIELD_SEPARATOR && prev != Some(ESCAPE) {
            return Ok((Piece::Literal(literal), &input[i..]));
        }
        if ch == OPENING_BRACKET && prev != Some(ESCAPE) {
            return Ok((Piece::Literal(literal), &input[i..]));
        }
        if ch == CLOSING_BRACKET && prev != Some(ESCAPE) {
            return Ok((Piece::Literal(literal), &input[i..]));
        }
        if ch == FIELD_SEPARATOR || ch == OPENING_BRACKET || ch == CLOSING_BRACKET {
            literal.push(ch);
        } else if ch == ESCAPE && prev == Some(ESCAPE) {
            literal.push(ch);
            prev = None;
        } else {
            literal.push(ch);
            prev = Some(ch);
        }
    }
    Ok((Piece::Literal(literal), ""))
}

fn parse_placeholder(input: &str, recursion_depth: u8) -> Result<(Piece, &str), ParseError> {
    if recursion_depth > MAX_RECURSION_DEPTH {
        return grab_until_terminator(input);
    }
    let first_input = input;
    let input = &input[1..]; // skip the {
    let (name, input) = extract_name(first_input, input)?;
    let (arguments, input) = extract_arguments(first_input, input, recursion_depth)?;
    let (flags, input) = extract_flags(first_input, input)?;
    let (options, input) = extract_options(first_input, input, recursion_depth)?;
    let input = extract_placeholder_terminator(first_input, input)?;
    Ok((Piece::Placeholder(name, arguments, flags, options), input))
}

fn extract_name<'a, 'b>(orig_input: &'a str, input: &'b str) 
    -> Result<(String, &'b str), ParseError>
{
    let mut name = String::new();
    let mut prev = None;
    for (i, ch) in input.char_indices() {
        if ch == FIELD_SEPARATOR || ch == OPENING_BRACKET || ch == CLOSING_BRACKET {
            if prev == Some(ESCAPE) {
                name.push(ch);
                prev = Some(ch);
            } else {
                return Ok((trim_name(orig_input, &name)?, &input[i..]));
            }
        } else if ch == ESCAPE && prev == Some(ESCAPE) {
            name.push(ESCAPE);
            prev = None;
        } else if ch == ESCAPE {
            prev = Some(ch);
        } else {
            name.push(ch);
            prev = Some(ch);
        }
    }
    Err(ParseError::UnterminatedPlaceholder(orig_input.to_string()))
}

fn extract_arguments<'a, 'b>(first_input: &'a str, input: &'b str, recursion_depth: u8) 
    -> Result<(Vec<Piece>, &'b str), ParseError>
{
    let mut iter = input.char_indices();
    match iter.next() {
        Some((_, OPENING_BRACKET)) => (),
        Some((_, FIELD_SEPARATOR)) => return Ok((Vec::new(), input)),
        Some((_, CLOSING_BRACKET)) => return Ok((Vec::new(), input)),
        Some(_) => return Err(ParseError::ArgumentListOrFieldSeparatorExpected(
                first_input.to_string())),
        None => return Err(ParseError::UnterminatedPlaceholder(first_input.to_string()))
    }
    let mut input = input;
    let mut args = Vec::new();
    while input.chars().next() != Some(CLOSING_BRACKET) {
        let (piece, rest) = parse_piece(input, recursion_depth + 1)?;
        args.push(piece);
        input = rest;
        if input.is_empty() {
            return Err(ParseError::UnterminatedArgumentList(first_input.to_string()));
        }
    }
    Ok((args, &input[1..]))
}

fn extract_flags<'a, 'b>(full_input: &'a str, input: &'b str) 
    -> Result<(Vec<char>, &'b str), ParseError>
{
    let mut iter = input.char_indices();
    match iter.next() {
        Some((_, FIELD_SEPARATOR)) => (),
        Some((_, CLOSING_BRACKET)) => return Ok((Vec::new(), input)),
        None => return Ok((Vec::new(), "")),
        _ => return Err(ParseError::FieldSeparatorExpectedToStartFlags(full_input.to_string()))
    }
    let mut flags = Vec::new();
    let mut prev = None;
    for (i, ch) in iter {
        if ch == FIELD_SEPARATOR && prev != Some(ESCAPE) {
            return Ok((flags, &input[i..]));
        }
        if ch == FIELD_SEPARATOR && prev == Some(ESCAPE) {
            flags.push(ch);
        }
        if ch == ESCAPE && prev == Some(ESCAPE) {
            flags.push(ch);
            prev = None;
            continue;
        }
        if ch != ESCAPE {
            flags.push(ch);
        }
        prev = Some(ch);
    }
    if prev == Some(ESCAPE) {
        flags.push(ESCAPE);
    }
    Ok((flags, ""))
}

fn extract_options<'a, 'b>(full_input: &'a str, input: &'b str, recursion_depth: u8) 
    -> Result<(HashMap<String, Piece>, &'b str), ParseError>
{
    let mut iter = input.char_indices();
    match iter.next() {
        Some((_, FIELD_SEPARATOR)) => (),
        Some((_, CLOSING_BRACKET)) => return Ok((HashMap::new(), input)),
        None => return Ok((HashMap::new(), "")),
        _ => return Err(ParseError::FieldSeparatorExpectedToStartOptions(full_input.to_string()))
    }
    let mut res: HashMap<String, Piece> = HashMap::new();
    let mut prev = None;
    let mut name = String::new();
    let mut input = input;
    loop {
        let maybe_next = iter.next();
        if maybe_next.is_none() {
            break;
        }
        let (i, ch) = maybe_next.unwrap();
        if ch == FIELD_SEPARATOR && prev != Some(ESCAPE) {
            name = trim_name(input, &name)?;
            res.insert(name, Piece::Literal("".to_string()));
            name = String::new();
        } else if ch == FIELD_SEPARATOR && prev == Some(ESCAPE) {
            name.push(ch);
            prev = Some(ch);
        } else if ch == SETOPT && prev == Some(ESCAPE) {
            name.push(ch);
            prev = Some(ch);
        } else if ch == ESCAPE && prev == Some(ESCAPE) {
            name.push(ch);
            prev = None;
        } else if ch != SETOPT {
            name.push(ch);
            prev = Some(ch);
        } else { // Is an unescaped SETOPT.
            name = trim_name(full_input, &name)?;
            let (piece, rest) = parse_piece(&input[i + 1 ..], recursion_depth + 1)?;
            res.insert(name, piece);
            input = rest;
            name = String::new();
            prev = None;
            iter = input.char_indices();
        }
    }
    if !name.is_empty() {
        res.insert(name, Piece::Literal("".to_string()));
    }
    Ok((res, input))
}

fn extract_placeholder_terminator<'a, 'b>(full_input: &'a str, input: &'b str) 
    -> Result<&'b str, ParseError>
{
    match input.chars().next() {
        Some(CLOSING_BRACKET) => Ok(&input[1..]),
        _ => Err(ParseError::UnterminatedPlaceholder(full_input.to_string()))
    }
}

fn grab_until_terminator(input: &str) -> Result<(Piece, &str), ParseError> {
    let mut prev = None;
    let mut literal = String::new();
    let mut balance = 1;
    for (i, ch) in input.char_indices() {
        if ch == OPENING_BRACKET && prev != Some(ESCAPE) {
            balance += 1;
            literal.push(ch);
            prev = Some(ch);
        } else if ch == CLOSING_BRACKET && prev != Some(ESCAPE) {
            balance -= 1;
            literal.push(ch);
            prev = Some(ch);
        } else if ch == ESCAPE && prev == Some(ESCAPE) {
            literal.push(ch);
            prev = None;
        } else if ch != ESCAPE {
            literal.push(ch);
            prev = Some(ch);
        }
        if balance == 0 {
            return Ok((Piece::Literal(literal), &input[i + 1 ..]));
        }
    }
    Err(ParseError::UnterminatedPlaceholder(input.to_string()))
}

fn trim_name(global_source: &str, name: &str) -> Result<String, ParseError> {
    let res = name.trim().to_string();
    if res.is_empty() {
        Err(ParseError::EmptyName(global_source.to_string()))
    } else {
        Ok(res)
    }
}
                    
#[cfg(test)]
mod tests {
    test_suite! {
        name parsing_tests;

        use std::collections::HashMap;

        use galvanic_assert::matchers::*;

        use parse::*;
        use Piece::*;

        test literal() {
            let s = "asdf 1";
            let res = parse(&s);
            let pieces = res.expect("Failed to get any pieces");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, eq(Literal(s.to_string())));
        }

        test single_placeholder_1() {
            let s = "a{b}c";
            let res = parse(&s);
            let pieces = res.expect("Failed to get any pieces");
            assert_that!(&pieces.len(), eq(3));
            let a = &pieces[0];
            let b = &pieces[1];
            let c = &pieces[2];
            assert_that!(&a, eq(Literal("a".to_string())));
            assert_that!(&c, eq(Literal("c".to_string())));
            assert_that!(&b, has_structure!(Placeholder [
                                            eq("b".to_string()),
                                            eq(Vec::new()),
                                            eq(Vec::new()),
                                            eq(HashMap::new())
            ]));
        }

    }

}
