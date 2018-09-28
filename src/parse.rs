use std::collections::HashMap;

const ESCAPE: char = '\\';
const SETOPT: char = '=';
const FIELD_SEPARATOR: char = ':';
const OPENING_BRACKET: char = '{';
const CLOSING_BRACKET: char = '}';
const DOT: char = '.';
const MAX_RECURSION_DEPTH: u8 = 100;

/// Either a literal string, or a placecholder.
#[derive(Debug, PartialEq)]
pub enum Piece {
    Literal(String),
    Placeholder(Vec<String>, Vec<Piece>, Vec<char>, HashMap<String, Piece>)
}

/// Errors that occur during parsing a format string.
#[derive(Debug, PartialEq)]
pub enum ParseError {
    EmptyNameSegment(String),
    UnterminatedArgumentList(String),
    UnterminatedPlaceholder(String)
}

pub fn parse(input: &str) -> Result<Vec<Piece>, ParseError> {
    let mut input = input;
    let mut res = Vec::new();
    while !input.is_empty() {
        let (piece, rest) = parse_piece(input, 0, false)?;
        input = rest;
        res.push(piece);
    }
    Ok(res)
}

fn parse_piece(input: &str, recursion_depth: u8, new_arglist: bool) 
    -> Result<(Piece, &str), ParseError> 
{
    let mut iter = input.chars();
    match iter.next() {
        Some(OPENING_BRACKET) => parse_placeholder(input, recursion_depth),
        _ => parse_literal(input, new_arglist)
    }
}

fn parse_literal(input: &str, new_arglist: bool) -> Result<(Piece, &str), ParseError> {
    let mut prev = None;
    let mut literal = String::new();
    for (i, ch) in input.char_indices() {
        if i == 0 && ch == FIELD_SEPARATOR {
            if new_arglist {
                return Ok((Piece::Literal(literal), input));
            } else {
                continue;
            }
        }
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
            prev = Some(ch);
        } else if ch == ESCAPE && prev == Some(ESCAPE) {
            literal.push(ch);
            prev = None;
        } else if ch != ESCAPE {
            literal.push(ch);
            prev = Some(ch);
        } else {
            prev = Some(ch);
        }
    }
    Ok((Piece::Literal(literal), ""))
}

fn parse_placeholder(input: &str, recursion_depth: u8) 
    -> Result<(Piece, &str), ParseError> 
{
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
    -> Result<(Vec<String>, &'b str), ParseError>
{
    let mut name = Vec::new();
    let mut segment = String::new();
    let mut prev = None;
    for (i, ch) in input.char_indices() {
        if ch == FIELD_SEPARATOR || ch == OPENING_BRACKET || ch == CLOSING_BRACKET {
            if prev == Some(ESCAPE) {
                segment.push(ch);
                prev = Some(ch);
            } else {
                name.push(segment);
                let mut res = Vec::with_capacity(name.len());
                for segm in name.iter() {
                    res.push(trim_name(orig_input, &segm)?);
                }
                return Ok((res, &input[i..]));
            }
        } else if ch == DOT && prev != Some(ESCAPE) {
            name.push(segment);
            segment = String::new();
            prev = Some(ch);
        } else if ch == ESCAPE && prev == Some(ESCAPE) {
            segment.push(ESCAPE);
            prev = None;
        } else if ch == ESCAPE {
            prev = Some(ch);
        } else {
            segment.push(ch);
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
        _ => panic!("Somehow dead code in 'extract_arguments' was reached on input: '{}'", input),
    }
    let mut input = &input[1..];
    let mut args = Vec::new();
    let mut first = true;
    while input.chars().next() != Some(CLOSING_BRACKET) {
        let (piece, rest) = parse_piece(input, recursion_depth + 1, first)?;
        first = false;
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
    let mut iter = input.char_indices().peekable();
    match iter.peek() {
        Some((_, FIELD_SEPARATOR)) => { iter.next(); },
        Some((_, CLOSING_BRACKET)) => return Ok((Vec::new(), input)),
        Some(_) => (),
        None => return Err(ParseError::UnterminatedPlaceholder(full_input.to_string())),
    }
    let mut flags = Vec::new();
    let mut prev = None;
    for (i, ch) in iter {
        if ch == FIELD_SEPARATOR && prev != Some(ESCAPE) {
            return Ok((flags, &input[i..]));
        } else if ch == FIELD_SEPARATOR && prev == Some(ESCAPE) {
            flags.push(ch);
            prev = Some(ch);
        } else if ch == ESCAPE && prev == Some(ESCAPE) {
            flags.push(ch);
            prev = None;
        } else if ch == CLOSING_BRACKET && prev != Some(ESCAPE) {
            return Ok((flags, &input[i..]));
        } else if ch != ESCAPE {
            flags.push(ch);
            prev = Some(ch);
        } else {
            prev = Some(ch);
        }
    }
    if prev != Some(CLOSING_BRACKET) {
        Err(ParseError::UnterminatedPlaceholder(full_input.to_string()))
    } else {
        Ok((flags, ""))
    }
}

fn extract_options<'a, 'b>(full_input: &'a str, input: &'b str, recursion_depth: u8) 
    -> Result<(HashMap<String, Piece>, &'b str), ParseError>
{
    let mut iter = input.char_indices();
    match iter.next() {
        Some((_, FIELD_SEPARATOR)) => (),
        Some((_, CLOSING_BRACKET)) => return Ok((HashMap::new(), input)),
        None => return Err(ParseError::UnterminatedPlaceholder(full_input.to_string())),
        _ => panic!("Reached dead code in 'extract_options' on input '{}'", full_input)
    }
    let mut res: HashMap<String, Piece> = HashMap::new();
    let mut prev = None;
    let mut name = String::new();
    let mut input = input;
    let mut end = 0;
    loop {
        let maybe_next = iter.next();
        if maybe_next.is_none() {
            break;
        }
        let (i, ch) = maybe_next.unwrap();
        end = i;
        if ch == CLOSING_BRACKET && prev != Some(ESCAPE) {
            break;
        } else if ch == FIELD_SEPARATOR && prev != Some(ESCAPE) {
            input = &input[1..];
            iter = input.char_indices();
            name = String::new();
            continue;
        } else if ch == SETOPT && prev != Some(ESCAPE) {
            name = trim_name(full_input, &name)?;
            let (opt, rest) = parse_piece(&input[i + 1 ..], recursion_depth + 1, false)?;
            res.insert(name, opt);
            iter = rest.char_indices();
            input = rest;
            name = String::new();
            continue;
        } else if ch == ESCAPE && prev == Some(ESCAPE) {
            name.push(ESCAPE);
            prev = None;
        } else if ch == ESCAPE && prev != Some(ESCAPE) {
            prev = Some(ch);
        }  else {
            name.push(ch);
            prev = Some(ch);
        }
    }
    if !name.is_empty() {
        res.insert(name, Piece::Literal("".to_string()));
    }
    Ok((res, &input[end..]))
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
        Err(ParseError::EmptyNameSegment(global_source.to_string()))
    } else {
        Ok(res)
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

        test single_placeholder_2() {
            let s = "a{b}";
            let pieces = parse(&s).expect("Failed to get any pieces");
            assert_that!(&pieces.len(), eq(2));
            let a = &pieces[0];
            let b = &pieces[1];
            assert_that!(&a, eq(Literal("a".to_string())));
            assert_that!(&b, has_structure!(Placeholder [
                                            eq("b".to_string()),
                                            eq(Vec::new()),
                                            eq(Vec::new()),
                                            eq(HashMap::new())
            ]));
        }

        test several_placeholders() {
            let s = "a{b}c{d}";
            let pieces = parse(&s).expect("Failed to get any pieces");
            assert_that!(&pieces.len(), eq(4));
            let a = &pieces[0];
            let b = &pieces[1];
            let c = &pieces[2];
            let d = &pieces[3];
            assert_that!(&a, eq(Literal("a".to_string())));
            assert_that!(&b, has_structure!(Placeholder [
                                            eq("b".to_string()),
                                            eq(Vec::new()),
                                            eq(Vec::new()),
                                            eq(HashMap::new())
            ]));
            assert_that!(&c, eq(Literal("c".to_string())));
            assert_that!(&d, has_structure!(Placeholder [
                                            eq("d".to_string()),
                                            eq(Vec::new()),
                                            eq(Vec::new()),
                                            eq(HashMap::new())
            ]));
        }

        test explicit_separator_before_literal() {
            let s = "{foobar}:asdf";
            let pieces = parse(&s).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(2));
            let pl = &pieces[0];
            assert_that!(&pl,
                         eq(Placeholder("foobar".to_string(),
                         Vec::new(),
                         Vec::new(),
                         HashMap::new()
                         )));
            let lit = &pieces[1];
            assert_that!(&lit, eq(Literal("asdf".to_string())));
        }

        test escapes_in_literals() {
            let s = "a\\:b\\{c\\}d\\\\";
            let pieces = parse(&s).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, eq(Literal("a:b{c}d\\".to_string())));
        }

        test escapes_in_placeholder_names() {
            let s = "{fo\\:ob\\\\ar\\{\\}}";
            let pieces = parse(&s).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, eq(Placeholder("fo:ob\\ar{}".to_string(),
                                                Vec::new(),
                                                Vec::new(),
                                                HashMap::new())));
        }

        test escapes_in_option_names() {
            let s = "{foobar::o\\:p\\{\\}t\\\\ion=1}";
            let pieces = parse(&s).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece, eq(Placeholder("foobar".to_string(),
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
            let pieces = parse(&s).expect("Parse failed");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(piece, eq(Placeholder("foobar".to_string(),
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
            let err = parse(&s).expect_err("Parse succeeded");
            assert_that!(&err, has_structure!(
                    UnterminatedPlaceholder [eq("{asdf".to_string())]
                    ));
        }

        test unterminated_arguments_list() {
            let s = "12{asdf{qq";
            let err = parse(&s).expect_err("Parse succeeded");
            assert_that!(&err, eq(UnterminatedArgumentList("{asdf{qq".to_string())));
        }

        test unterminated_argument() {
            let s = "12{asdf{{a";
            let err = parse(&s).expect_err("Parse succeeded");
            assert_that!(&err, eq(UnterminatedPlaceholder("{a".to_string())));
        }

        test no_closing_bracket_after_arguments() {
            let s = "{foobar{asdf}";
            let err = parse(&s).expect_err("Parse succeeded");
            assert_that!(&err, eq(UnterminatedPlaceholder(s.to_string())));
        }

        test unterminated_flags() {
            let s = "{foobar:asdf";
            let err = parse(&s).expect_err("Parse succeeded");
            assert_that!(&err, eq(UnterminatedPlaceholder(s.to_string())));
        }

        test unterminated_options() {
            let s= "{foobar::";
            let err = parse(&s).expect_err("Parse succeeded");
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
            let pieces = parse(&s).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece,
                         eq(Placeholder("foobar".to_string(),
                            vec![Literal("asdf".to_string())],
                            Vec::new(),
                            HashMap::new()
                            )));
        }

        test two_literals() {
            let s = "{foobar{a:b}}";
            let pieces = parse(&s).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece,
                         eq(Placeholder("foobar".to_string(),
                         vec![Literal("a".to_string()), Literal("b".to_string())],
                         Vec::new(),
                         HashMap::new()
                         )));
        }

        test empty_arguments() {
            let s = "{foobar{::}}";
            let pieces = parse(&s).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            assert_that!(&piece,
                         eq(Placeholder("foobar".to_string(),
                            vec![Literal("".to_string()), Literal("".to_string()),
                                Literal("".to_string())],
                            Vec::new(),
                            HashMap::new()
                        )));
        }

        test full_literal() {
            let s = "{foobar{{baz{arg}flags:opt=1}}}";
            let pieces = parse(&s).expect("Failed to parse");
            assert_that!(&pieces.len(), eq(1));
            let piece = &pieces[0];
            if let Placeholder(_, args, _, _) = piece {
                assert_that!(&args.len(), eq(1));
                assert_that!(&args[0], eq(Placeholder("baz".to_string(),
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

}
