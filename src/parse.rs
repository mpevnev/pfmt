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
        res.push(piece);
    }
    Ok(res)
}

fn parse_piece(input: &str, recursion_depth: u8) -> Result<(Piece, &str), ParseError> {
    let mut iter = input.chars();
    match iter.next() {
        Some(OPENING_BRACKET) => parse_placeholder(&input[1..], recursion_depth),
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
    let first_input = input;
    let (name, input) = extract_name(first_input, input)?;
    let (arguments, input) = extract_arguments(first_input, input, recursion_depth)?;
    let (flags, input) = extract_flags(first_input, input)?;
    let (options, input) = extract_options(first_input, input, recursion_depth)?;
    let input = extract_placeholder_terminator(first_input, input)?;
    Ok((Piece::Placeholder(name, arguments, flags, options), input))
}

fn extract_name<'a, 'b>(first_input: &'a str, input: &'b str) 
    -> Result<(String, &'b str), ParseError>
{
    let mut name = String::new();
    let mut prev = None;
    for (i, ch) in input.char_indices() {
        if ch == FIELD_SEPARATOR || ch == OPENING_BRACKET {
            if prev == Some(ESCAPE) {
                name.push(ch);
            } else {
                return Ok((trim_name(input, &name)?, &input[i..]));
            }
        } else if ch == ESCAPE && prev == Some(ESCAPE) {
            name.push(ESCAPE);
            prev = None;
        } else {
            prev = Some(ch);
        }
    }
    Ok((trim_name(input, &name)?, ""))
}

fn extract_arguments<'a, 'b>(first_input: &'a str, input: &'b str, recursion_depth: u8) 
    -> Result<(Vec<Piece>, &'b str), ParseError>
{
    let mut iter = input.char_indices();
    match iter.next() {
        Some((_, OPENING_BRACKET)) => (),
        Some((_, FIELD_SEPARATOR)) => return Ok((Vec::new(), input)),
        None => return Ok((Vec::new(), input)),
        Some(_) => return Err(ParseError::ArgumentListOrFieldSeparatorExpected(
                first_input.to_string()))
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
        None => return Ok((Vec::new(), "")),
        _ => return Err(ParseError::FieldSeparatorExpectedToStartFlags(full_input.to_string()))
    }
    let mut flags = Vec::new();
    let prev = None;
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
            let name = trim_name(input, &name)?;
            res.insert(name, Piece::Literal("".to_string()));
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
            let name = trim_name(full_input, &name)?;
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

        test single_literal() {
            let s = String::from("foobar");
            let res = parse(&s).expect("Parse failed");
            assert_that!(&res.len(), eq(1));
            let lit = &res[0];
            assert_that!(lit, has_structure!(Piece::Literal [eq(s.clone())]));
        }

        test escapes_1() {
            let s = String::from("foobar\\{");
            let escaped = String::from("foobar{");
            let res = parse(&s).expect("Parse failed");
            assert_that!(&res.len(), eq(1));
            let lit = &res[0];
            assert_that!(lit, has_structure!(Piece::Literal [eq(escaped.clone())]));
        }

        test escapes_2() {
            let s = String::from("\\{asdf");
            let escaped = String::from("{asdf");
            let res = parse(&s).expect("Parse failed");
            assert_that!(&res.len(), eq(1));
            let lit = &res[0];
            assert_that!(lit, has_structure!(Piece::Literal [eq(escaped.clone())]));
        }

        test escapes_3() {
            let s = String::from("asdf\\{asdf");
            let escaped = String::from("asdf{asdf");
            let res = parse(&s).expect("Parse failed");
            assert_that!(&res.len(), eq(1));
            let lit = &res[0];
            assert_that!(lit, has_structure!(Piece::Literal [eq(escaped.clone())]));
        }

        test escapes_4() {
            let s = String::from("\\}asdf");
            let escaped = String::from("}asdf");
            let res = parse(&s).expect("Parse failed");
            assert_that!(&res.len(), eq(1));
            let lit = &res[0];
            assert_that!(lit, has_structure!(Piece::Literal [eq(escaped.clone())]));
        }

        test escapes_5() {
            let s = String::from("asdf\\}asdf");
            let escaped = String::from("asdf}asdf");
            let res = parse(&s).expect("Parse failed");
            assert_that!(&res.len(), eq(1));
            let lit = &res[0];
            assert_that!(lit, has_structure!(Piece::Literal [eq(escaped.clone())]));
        }

        test nested_placeholders() {
            let s = String::from("{{}}");
            let res = parse(&s);
            assert_that!(&res, has_structure!(Err [eq(ParseError::NestedPlaceholders())]));
        }

        test missing_opening_bracket() {
            let s = String::from("}{");
            let res = parse(&s);
            assert_that!(&res, has_structure!(Err [eq(ParseError::MissingOpeningBracket())]));
        }

        test single_placeholder_1() {
            let s = String::from("{asdf}");
            let pieces = parse(&s).expect("Parse failed");
            assert_that!(&pieces.len(), eq(1));
            let res = &pieces[0];
            let flags = Vec::new();
            let opts = HashMap::new();
            match res {
                Piece::Literal(lit) => panic!("Expected a placeholder, got literal \"{}\"", lit),
                Piece::Placeholder(name, actual_flags, actual_opts) => {
                    assert_that!(&name, eq("asdf".to_string().clone()));
                    assert_that!(&actual_flags, eq(flags.clone()));
                    assert_that!(&actual_opts, eq(opts.clone()));
                }
            }
        }

        test single_placeholder_2() {
            let s = String::from("{asdf:}");
            let pieces = parse(&s).expect("Parse failed");
            assert_that!(&pieces.len(), eq(1));
            let res = &pieces[0];
            let flags = Vec::new();
            let opts = HashMap::new();
            match res {
                Piece::Literal(lit) => panic!("Expected a placeholder, got literal \"{}\"", lit),
                Piece::Placeholder(name, actual_flags, actual_opts) => {
                    assert_that!(&name, eq("asdf".to_string().clone()));
                    assert_that!(&actual_flags, eq(flags.clone()));
                    assert_that!(&actual_opts, eq(opts.clone()));
                }
            }
        }

        test single_placeholder_3() {
            let s = String::from("{asdf::}");
            let pieces = parse(&s).expect("Parse failed");
            assert_that!(&pieces.len(), eq(1));
            let res = &pieces[0];
            let flags = Vec::new();
            let opts = HashMap::new();
            match res {
                Piece::Literal(lit) => panic!("Expected a placeholder, got literal \"{}\"", lit),
                Piece::Placeholder(name, actual_flags, actual_opts) => {
                    assert_that!(&name, eq("asdf".to_string().clone()));
                    assert_that!(&actual_flags, eq(flags.clone()));
                    assert_that!(&actual_opts, eq(opts.clone()));
                }
            }
        }

        test single_placeholder_4() {
            let s = String::from("{asdf:abc}");
            let pieces = parse(&s).expect("Parse failed");
            assert_that!(&pieces.len(), eq(1));
            let res = &pieces[0];
            let flags = vec!['a', 'b', 'c'];
            let opts = HashMap::new();
            match res {
                Piece::Literal(lit) => panic!("Expected a placeholder, got literal \"{}\"", lit),
                Piece::Placeholder(name, actual_flags, actual_opts) => {
                    assert_that!(&name, eq("asdf".to_string().clone()));
                    assert_that!(&actual_flags, eq(flags.clone()));
                    assert_that!(&actual_opts, eq(opts.clone()));
                }
            }
        }

        test single_placeholder_5() {
            let s = String::from("{asdf:abc:a=b}");
            let pieces = parse(&s).expect("Parse failed");
            assert_that!(&pieces.len(), eq(1));
            let res = &pieces[0];
            let flags = vec!['a', 'b', 'c'];
            let mut opts = HashMap::new();
            opts.insert("a".to_string(), "b".to_string());
            match res {
                Piece::Literal(lit) => panic!("Expected a placeholder, got literal \"{}\"", lit),
                Piece::Placeholder(name, actual_flags, actual_opts) => {
                    assert_that!(&name, eq("asdf".to_string().clone()));
                    assert_that!(&actual_flags, eq(flags.clone()));
                    assert_that!(&actual_opts, eq(opts.clone()));
                }
            }
        }

        test single_placeholder_6() {
            let s = String::from("{asdf:abc:a=b:bc=as}");
            let pieces = parse(&s).expect("Parse failed");
            assert_that!(&pieces.len(), eq(1));
            let res = &pieces[0];
            let flags = vec!['a', 'b', 'c'];
            let mut opts = HashMap::new();
            opts.insert("a".to_string(), "b".to_string());
            opts.insert("bc".to_string(), "as".to_string());
            match res {
                Piece::Literal(lit) => panic!("Expected a placeholder, got literal \"{}\"", lit),
                Piece::Placeholder(name, actual_flags, actual_opts) => {
                    assert_that!(&name, eq("asdf".to_string().clone()));
                    assert_that!(&actual_flags, eq(flags.clone()));
                    assert_that!(&actual_opts, eq(opts.clone()));
                }
            }
        }

        test single_placeholder_7() {
            let s = String::from("{asdf:abc:a=:bc=}");
            let pieces = parse(&s).expect("Parse failed");
            assert_that!(&pieces.len(), eq(1));
            let res = &pieces[0];
            let flags = vec!['a', 'b', 'c'];
            let mut opts = HashMap::new();
            opts.insert("a".to_string(), "".to_string());
            opts.insert("bc".to_string(), "".to_string());
            match res {
                Piece::Literal(lit) => panic!("Expected a placeholder, got literal \"{}\"", lit),
                Piece::Placeholder(name, actual_flags, actual_opts) => {
                    assert_that!(&name, eq("asdf".to_string().clone()));
                    assert_that!(&actual_flags, eq(flags.clone()));
                    assert_that!(&actual_opts, eq(opts.clone()));
                }
            }
        }

        test single_placeholder_no_flags() {
            let s = String::from("{asdf::a=bbc}");
            let pieces = parse(&s).expect("Parse failed");
            assert_that!(&pieces.len(), eq(1));
            let res = &pieces[0];
            let flags = Vec::new();
            let mut opts = HashMap::new();
            opts.insert("a".to_string(), "bbc".to_string());
            match res {
                Piece::Literal(lit) => panic!("Expected a placeholder, got literal \"{}\"", lit),
                Piece::Placeholder(name, actual_flags, actual_opts) => {
                    assert_that!(&name, eq("asdf".to_string().clone()));
                    assert_that!(&actual_flags, eq(flags.clone()));
                    assert_that!(&actual_opts, eq(opts.clone()));
                }
            }
        }

        test multiple_placeholders() {
            let s = String::from("foo{1} asdf {bar}11");
            let pieces = parse(&s).expect("Parse failed");
            assert_that!(&pieces.len(), eq(5));
            assert_that!(&pieces[0], eq(Piece::Literal("foo".to_string().clone())));
            assert_that!(&pieces[2], eq(Piece::Literal(" asdf ".to_string().clone())));
            assert_that!(&pieces[4], eq(Piece::Literal("11".to_string().clone())));
            let flags = Vec::new();
            let opts = HashMap::new();
            assert_that!(&pieces[1], eq(Piece::Placeholder("1".to_string().clone(),
                flags.clone(),
                opts.clone()
                )));
            assert_that!(&pieces[3], eq(Piece::Placeholder("bar".to_string().clone(),
                flags.clone(),
                opts.clone()
                )));
        }

    }
}
