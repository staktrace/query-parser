#[derive(Debug, PartialEq)]
pub struct Query {
    pub raw_query: String,
    pub terms: Vec<Term>,
}

#[derive(Debug, PartialEq)]
pub enum TermValue {
    Simple(String),
    // in the future, other types like List or Range might be added
}

#[derive(Debug, PartialEq)]
pub struct Term {
    pub negated: bool,
    pub key: Option<String>,
    pub value: TermValue,
}

impl Term {
    fn new<S: Into<String>>(negated: bool, key: Option<S>, value: S) -> Self {
        Term {
            negated,
            key: key.map(S::into),
            value: TermValue::Simple(value.into()),
        }
    }

    fn from_value<S: Into<String>>(value: S) -> Self {
        Term {
            negated: false,
            key: None,
            value: TermValue::Simple(value.into()),
        }
    }
}

#[derive(Debug)]
pub struct ParseOptions {
}

impl ParseOptions {
    pub fn default() -> Self {
        Self {
        }
    }
}

pub fn parse(raw: &str) -> Query {
    parse_with_options(raw, ParseOptions::default())
}

pub fn parse_with_options(raw: &str, opts: &ParseOptions) -> Query {
    Query {
        raw_query: String::from(raw),
        terms: parse_terms(raw),
    }
}

#[derive(Clone, Copy, Debug)]
enum ParseState {
    Initial, // before a new term is processed
    Negated, // term started with a '-' character
    SingleQuote, // term started with a single quote
    DoubleQuote, // term started with a double quote
    RawToken, // term started without quoting
    NegatedSingleQuote, // term started with a '-' followed by a single quote
    NegatedDoubleQuote, // term started with a '-' followed by a double quote
    NegatedRawToken, // term started with a '-' followed by unquoted characters
    Value, // after encountering the ':' to separate key from value
    NegatedValue, // after encountering the ':' to separated negated key from value
    RawValue, // Once the value has been determined to be unquoted
    SingleQuotedValue, // Once the value has been determined to be single-quoted
    DoubleQuotedValue, // Once the value has been determined to be double-quoted
    NegatedRawValue, // Once the value for a negated term has been determined to be unquoted
    NegatedSingleQuotedValue, // Once the value for a negated term has been determined to be single-quoted
    NegatedDoubleQuotedValue, // Once the value for a negated term has been determined to be double-quoted
}

impl ParseState {
    fn is_negated(&self) -> bool {
        match self {
            Self::Negated |
            Self::NegatedSingleQuote |
            Self::NegatedDoubleQuote |
            Self::NegatedRawToken |
            Self::NegatedValue |
            Self::NegatedRawValue |
            Self::NegatedSingleQuotedValue |
            Self::NegatedDoubleQuotedValue => true,
            _ => false,
        }
    }
}

fn parse_terms(raw: &str) -> Vec<Term> {
    let mut result = Vec::new();

    let mut state = ParseState::Initial;
    let mut key = None;
    let mut token = String::new();

    let mut c = raw.chars();
    loop {
        match (state, c.next()) {
            // Initial state handlers
            (ParseState::Initial, None) => {
                break;
            }
            (ParseState::Initial, Some('-')) => {
                state = ParseState::Negated;
            }
            (ParseState::Initial, Some('\'')) => {
                state = ParseState::SingleQuote;
            }
            (ParseState::Initial, Some('"')) => {
                state = ParseState::DoubleQuote;
            }
            (ParseState::Initial, Some(ref ch)) if ch.is_ascii_whitespace() => {
                continue;
            }
            (ParseState::Initial, Some(ref ch)) => {
                state = ParseState::RawToken;
                token.push(*ch);
            }

            // Negated state handlers
            (ParseState::Negated, None) => {
                result.push(Term::from_value("-"));
                break;
            }
            (ParseState::Negated, Some('\'')) => {
                state = ParseState::NegatedSingleQuote;
            }
            (ParseState::Negated, Some('"')) => {
                state = ParseState::NegatedDoubleQuote;
            }
            (ParseState::Negated, Some(ref ch)) if ch.is_ascii_whitespace() => {
                result.push(Term::from_value("-"));
                state = ParseState::Initial;
            }
            (ParseState::Negated, Some(ref ch)) => {
                state = ParseState::NegatedRawToken;
                token.push(*ch);
            }

            // [Negated] Single/Double quoted state handlers
            (ParseState::SingleQuote, None) |
            (ParseState::NegatedSingleQuote, None) => {
                result.push(Term::from_value(format!("{}'{}", if state.is_negated() { "-" } else { "" }, token)));
                break;
            }
            (ParseState::DoubleQuote, None) |
            (ParseState::NegatedDoubleQuote, None) => {
                result.push(Term::from_value(format!("{}\"{}", if state.is_negated() { "-" } else { "" }, token)));
                break;
            }
            (ParseState::SingleQuote, Some('\'')) |
            (ParseState::DoubleQuote, Some('"')) |
            (ParseState::NegatedSingleQuote, Some('\'')) |
            (ParseState::NegatedDoubleQuote, Some('"')) => {
                result.push(Term::new(state.is_negated(), None, token));
                token = String::new();
                state = ParseState::Initial;
            }
            (ParseState::SingleQuote, Some(ref ch)) |
            (ParseState::DoubleQuote, Some(ref ch)) |
            (ParseState::NegatedSingleQuote, Some(ref ch)) |
            (ParseState::NegatedDoubleQuote, Some(ref ch)) => {
                token.push(*ch);
            }

            // Raw token state handlers
            (ParseState::RawToken, None) => {
                result.push(Term::from_value(token));
                break;
            }
            (ParseState::RawToken, Some(':')) => {
                key = Some(token);
                token = String::new();
                state = ParseState::Value;
            }
            (ParseState::RawToken, Some(ref ch)) if ch.is_ascii_whitespace() => {
                result.push(Term::from_value(token));
                token = String::new();
                state = ParseState::Initial;
            }
            (ParseState::RawToken, Some(ref ch)) => {
                token.push(*ch);
            }

            // Negated raw token state handlers
            (ParseState::NegatedRawToken, None) => {
                result.push(Term::new(true, None, token));
                break;
            }
            (ParseState::NegatedRawToken, Some(':')) => {
                key = Some(token);
                token = String::new();
                state = ParseState::NegatedValue;
            }
            (ParseState::NegatedRawToken, Some(ref ch)) if ch.is_ascii_whitespace() => {
                result.push(Term::new(true, None, token));
                token = String::new();
                state = ParseState::Initial;
            }
            (ParseState::NegatedRawToken, Some(ref ch)) => {
                token.push(*ch);
            }

            // Value/raw-value state handlers
            (ParseState::Value, None) |
            (ParseState::RawValue, None) |
            (ParseState::NegatedValue, None) |
            (ParseState::NegatedRawValue, None) => {
                result.push(Term::new(state.is_negated(), key, token));
                break;
            }
            (ParseState::Value, Some('\'')) => {
                state = ParseState::SingleQuotedValue;
            }
            (ParseState::Value, Some('"')) => {
                state = ParseState::DoubleQuotedValue;
            }
            (ParseState::NegatedValue, Some('\'')) => {
                state = ParseState::NegatedSingleQuotedValue;
            }
            (ParseState::NegatedValue, Some('"')) => {
                state = ParseState::NegatedDoubleQuotedValue;
            }
            (ParseState::Value, Some(ref ch)) |
            (ParseState::RawValue, Some(ref ch)) |
            (ParseState::NegatedValue, Some(ref ch)) |
            (ParseState::NegatedRawValue, Some(ref ch))
                if ch.is_ascii_whitespace() =>
            {
                result.push(Term::new(state.is_negated(), key, token));
                key = None;
                token = String::new();
                state = ParseState::Initial;
            }
            (ParseState::Value, Some(ref ch)) |
            (ParseState::RawValue, Some(ref ch)) |
            (ParseState::NegatedValue, Some(ref ch)) |
            (ParseState::NegatedRawValue, Some(ref ch)) => {
                token.push(*ch);
                state = if state.is_negated() { ParseState::NegatedRawValue } else { ParseState::RawValue };
            }

            (ParseState::SingleQuotedValue, None) |
            (ParseState::NegatedSingleQuotedValue, None) => {
                result.push(Term::new(state.is_negated(), key, format!("'{}", token)));
                break;
            }
            (ParseState::DoubleQuotedValue, None) |
            (ParseState::NegatedDoubleQuotedValue, None) => {
                result.push(Term::new(state.is_negated(), key, format!("\"{}", token)));
                break;
            }
            (ParseState::SingleQuotedValue, Some('\'')) |
            (ParseState::DoubleQuotedValue, Some('"')) |
            (ParseState::NegatedSingleQuotedValue, Some('\'')) |
            (ParseState::NegatedDoubleQuotedValue, Some('"')) => {
                result.push(Term::new(state.is_negated(), key, token));
                key = None;
                token = String::new();
                state = ParseState::Initial;
            }
            (ParseState::SingleQuotedValue, Some(ref ch)) |
            (ParseState::DoubleQuotedValue, Some(ref ch)) |
            (ParseState::NegatedSingleQuotedValue, Some(ref ch)) |
            (ParseState::NegatedDoubleQuotedValue, Some(ref ch)) => {
                token.push(*ch);
            }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        assert_eq!(parse("").terms, &[]);
        assert_eq!(parse(" ").terms, &[]);
        assert_eq!(parse("\t \n").terms, &[]);
    }

    #[test]
    fn negations() {
        assert_eq!(parse("-").terms, &[Term::new(false, None, "-")]);
        assert_eq!(parse("- -").terms, &[Term::new(false, None, "-"), Term::new(false, None, "-")]);
        assert_eq!(parse("--").terms, &[Term::new(true, None, "-")]);
        assert_eq!(parse("---").terms, &[Term::new(true, None, "--")]);
        assert_eq!(parse("--- ---").terms, &[Term::new(true, None, "--"), Term::new(true, None, "--")]);
        assert_eq!(parse("---:---").terms, &[Term::new(true, Some("--"), "---")]);
    }

    #[test]
    fn quoted() {
        assert_eq!(parse("'hello' 'world'").terms, &[Term::new(false, None, "hello"), Term::new(false, None, "world")]);
        assert_eq!(parse(" 'hello''world' ").terms, &[Term::new(false, None, "hello"), Term::new(false, None, "world")]);
        assert_eq!(parse("\"hello\" \"world\"").terms, &[Term::new(false, None, "hello"), Term::new(false, None, "world")]);
        assert_eq!(parse(" \"hello\"\"world\" ").terms, &[Term::new(false, None, "hello"), Term::new(false, None, "world")]);

        assert_eq!(parse("-'hello' 'world'").terms, &[Term::new(true, None, "hello"), Term::new(false, None, "world")]);
        assert_eq!(parse(" 'hello'-'world' ").terms, &[Term::new(false, None, "hello"), Term::new(true, None, "world")]);
        assert_eq!(parse("\"hello\" -\"world\"").terms, &[Term::new(false, None, "hello"), Term::new(true, None, "world")]);
        assert_eq!(parse(" -\"hello\"-\"world\" ").terms, &[Term::new(true, None, "hello"), Term::new(true, None, "world")]);
    }

    #[test]
    fn raw_tokens() {
        assert_eq!(parse("hello").terms, &[Term::from_value("hello")]);
        assert_eq!(parse(" hello ").terms, &[Term::from_value("hello")]);
        assert_eq!(parse(" hello world ").terms, &[Term::from_value("hello"), Term::from_value("world")]);
        assert_eq!(parse("\rhello\nworld\t").terms, &[Term::from_value("hello"), Term::from_value("world")]);

        assert_eq!(parse(" -hello ").terms, &[Term::new(true, None, "hello")]);
        assert_eq!(parse(" -hello-world ").terms, &[Term::new(true, None, "hello-world")]);
        assert_eq!(parse(" -hello -world ").terms, &[Term::new(true, None, "hello"), Term::new(true, None, "world")]);
    }

    #[test]
    fn raw_values() {
        assert_eq!(parse("key:value").terms, &[Term::new(false, Some("key"), "value")]);
        assert_eq!(parse("key:value key2:value2").terms, &[Term::new(false, Some("key"), "value"), Term::new(false, Some("key2"), "value2")]);
        assert_eq!(parse("key: anotherValue").terms, &[Term::new(false, Some("key"), ""), Term::new(false, None, "anotherValue")]);
        assert_eq!(parse(" key:value ").terms, &[Term::new(false, Some("key"), "value")]);

        assert_eq!(parse("-key:value").terms, &[Term::new(true, Some("key"), "value")]);
        assert_eq!(parse(" -key:value ").terms, &[Term::new(true, Some("key"), "value")]);
        assert_eq!(parse(" key:-value ").terms, &[Term::new(false, Some("key"), "-value")]);
        assert_eq!(parse(" key:- ").terms, &[Term::new(false, Some("key"), "-")]);
    }

    #[test]
    fn quoted_values() {
        assert_eq!(parse("key:'value'").terms, &[Term::new(false, Some("key"), "value")]);
        assert_eq!(parse("key:\"value with spaces\"").terms, &[Term::new(false, Some("key"), "value with spaces")]);
        assert_eq!(parse("key:\"value\" key2:'another value'").terms, &[Term::new(false, Some("key"), "value"), Term::new(false, Some("key2"), "another value")]);

        assert_eq!(parse("-key:'value'").terms, &[Term::new(true, Some("key"), "value")]);
        assert_eq!(parse("-key:\"value with spaces\"").terms, &[Term::new(true, Some("key"), "value with spaces")]);
        assert_eq!(parse("key:\"value\" -key2:'another value'").terms, &[Term::new(false, Some("key"), "value"), Term::new(true, Some("key2"), "another value")]);
    }

    #[test]
    fn end_unexpectedly() {
        assert_eq!(parse(" -").terms, &[Term::new(false, None, "-")]);
        assert_eq!(parse("'hello").terms, &[Term::new(false, None, "'hello")]);
        assert_eq!(parse("\"hello ").terms, &[Term::new(false, None, "\"hello ")]);
        assert_eq!(parse("-'hello").terms, &[Term::new(false, None, "-'hello")]);
        assert_eq!(parse("-\"hello ").terms, &[Term::new(false, None, "-\"hello ")]);

        assert_eq!(parse("key:\"value ").terms, &[Term::new(false, Some("key"), "\"value ")]);
        assert_eq!(parse("-key:'value ").terms, &[Term::new(true, Some("key"), "'value ")]);
    }
}
