use std::collections::HashMap;

/// Errors that occur during parsing a format string.
#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnbalancedBrackets(),
    NestedPlaceholders(),
    MissingOpeningBracket(),
    MissingPlaceholderName(),
    MissingOptionName()
}

const ESCAPE: char = '\\';
const SETOPT: char = '=';
const FIELD_SEPARATOR: char = ':';

/// Either a literal string, or a placecholder.
#[derive(Debug, PartialEq)]
pub enum Piece {
    Literal(String),
    Placeholder(String, Vec<char>, HashMap<String, String>)
}

pub fn parse(input: &str) -> Result<Vec<Piece>, ParseError> {
    let mut pieces = Vec::new();
    validate_brackets(input)?;
    let mut start = 0;
    let mut is_placeholder = false;
    let mut prev = None;
    for (i, ch) in input.char_indices() {
        if !is_placeholder && ch == '{' && prev != Some(ESCAPE) {
            add_literal(&mut pieces, input, start, i);
            is_placeholder = true;
            start = i + 1;
            prev = Some(ch);
            continue;
        }
        if !is_placeholder && ch == '}' && prev != Some(ESCAPE) {
            return Err(ParseError::MissingOpeningBracket());
        }
        if !is_placeholder {
            prev = Some(ch);
            continue;
        }
        if ch == '{' && prev != Some(ESCAPE) {
            return Err(ParseError::NestedPlaceholders());
        }
        if ch == '}' && prev != Some(ESCAPE) {
            add_placeholder(&mut pieces, input, start, i)?;
            is_placeholder = false;
            start = i + 1;
            prev = Some(ch);
            continue;
        }
        prev = Some(ch);
    }
    if is_placeholder && prev == Some('}') {
        add_placeholder(&mut pieces, input, start, input.len())?;
    } else {
        add_literal(&mut pieces, input, start, input.len());
    }
    Ok(pieces)
}

fn add_literal(pieces: &mut Vec<Piece>, input: &str, start: usize, end: usize) {
    if start >= end {
        return;
    }
    let mut s = String::with_capacity(end - start);
    let mut prev = None;
    let sub = &input[start..end];
    for ch in sub.chars() {
        if prev == Some(ESCAPE) {
            if ch == '{' {
                s.push('{');
                prev = Some(ch);
            } else if ch == '}' {
                s.push('}');
                prev = Some(ch);
            } else if ch == ESCAPE {
                s.push(ESCAPE);
                prev = None;
            }
        } else {
            if ch == ESCAPE {
                prev = Some(ch);
                continue;
            }
            s.push(ch);
            prev = Some(ch);
        }
    }
    if prev == Some(ESCAPE) {
        s.push(ESCAPE);
    }
    pieces.push(Piece::Literal(s));
}

fn add_placeholder(pieces: &mut Vec<Piece>, input: &str, start: usize, end: usize)
    -> Result<(), ParseError>
{
    let sub = &input[start..end];
    let (name, sub) = extract_name(sub)?;
    let (flags, sub) = extract_flags(sub)?;
    let options = extract_options(sub)?;
    pieces.push(Piece::Placeholder(name, flags, options));
    Ok(())
}

fn extract_name(input: &str) -> Result<(String, &str), ParseError> {
    if input.len() == 0 {
        return Err(ParseError::MissingPlaceholderName());
    }
    for (i, ch) in input.char_indices() {
        if ch == ':' {
            let name = String::from(&input[0..i]);
            let rest = &input[i + 1 .. input.len()];
            return Ok((name, rest))
        }
    }
    Ok((String::from(input.trim()), &input[input.len()..]))
}

fn extract_flags(input: &str) -> Result<(Vec<char>, &str), ParseError> {
    if input.len() == 0 {
        return Ok((Vec::new(), input))
    }
    let mut flags = Vec::new();
    for (i, ch) in input.char_indices() {
        if ch == ':' {
            let rest = &input[i + 1 .. input.len()];
            return Ok((flags, rest))
        }
        if ch.is_whitespace() {
            continue;
        }
        flags.push(ch);
    }
    Ok((flags, &input[input.len()..]))
}

fn extract_options(input: &str) -> Result<HashMap<String, String>, ParseError> {
    if input.len() == 0 {
        return Ok(HashMap::new());
    }
    let mut options = HashMap::new();
    let mut name_start = Some(0);
    let mut name_end = None;
    let mut value_start = None;
    for (i, ch) in input.char_indices() {
        if name_start.is_none() && ch == SETOPT {
            return Err(ParseError::MissingOptionName());
        }
        if name_start.is_none() {
            name_start = Some(i);
            name_end = None;
            continue;
        }
        if name_start.is_some() && name_end.is_none() {
            if ch == SETOPT {
                name_end = Some(i);
                value_start = Some(i + 1);
                continue;
            }
            continue;
        }
        if value_start.is_none() {
            value_start = Some(i);
            continue;
        }
        if ch == FIELD_SEPARATOR {
            let name = String::from(input[name_start.unwrap() .. name_end.unwrap()].trim());
            if name.is_empty() {
                return Err(ParseError::MissingOptionName());
            }
            let value = String::from(input[value_start.unwrap() .. i].trim());
            options.insert(name, value);
            name_start = None;
            name_end = None;
            value_start = None;
        }
    }
    if name_start.is_some() && value_start.is_some() {
        let name = String::from(input[name_start.unwrap() .. name_end.unwrap()].trim());
        let value = String::from(input[value_start.unwrap() .. input.len()].trim());
        options.insert(name, value);
    }
    Ok(options)
}

fn validate_brackets(input: &str) -> Result<(), ParseError> {
    let mut balance = 0;
    let mut prev = None;
    for c in input.chars() {
        if c == '{' && prev != Some(ESCAPE) {
            balance += 1;
        }
        if c == '}' && prev != Some(ESCAPE) {
            balance -= 1;
        }
        prev = Some(c)
    }
    if balance == 0 {
        Ok(())
    } else {
        Err(ParseError::UnbalancedBrackets())
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

        test unbalanced_brackets() {
            let s = String::from("{{}");
            let res = parse(&s);
            assert_that!(&res, has_structure!(Err [eq(ParseError::UnbalancedBrackets())]));
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
