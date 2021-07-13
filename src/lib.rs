pub struct Query {
    pub raw_query: String,
    pub terms: Vec<QueryTerm>,
}

#[derive(Debug, PartialEq)]
pub struct QueryTerm {
    pub negated: bool,
    pub key: Option<String>,
    pub value: String,
}

impl QueryTerm {
    fn new<S: Into<String>>(negated: bool, key: Option<S>, value: S) -> Self {
        QueryTerm {
            negated,
            key: key.map(S::into),
            value: value.into(),
        }
    }

    fn from_value<S: Into<String>>(value: S) -> Self {
        QueryTerm {
            negated: false,
            key: None,
            value: value.into(),
        }
    }
}

pub fn parse(raw: &str) -> Query {
    Query {
        raw_query: String::from(raw),
        terms: parse_tree(raw),
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

fn parse_tree(raw: &str) -> Vec<QueryTerm> {
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
                result.push(QueryTerm::from_value("-"));
                break;
            }
            (ParseState::Negated, Some('\'')) => {
                state = ParseState::NegatedSingleQuote;
            }
            (ParseState::Negated, Some('"')) => {
                state = ParseState::NegatedDoubleQuote;
            }
            (ParseState::Negated, Some(ref ch)) if ch.is_ascii_whitespace() => {
                result.push(QueryTerm::from_value("-"));
                state = ParseState::Initial;
            }
            (ParseState::Negated, Some(ref ch)) => {
                state = ParseState::NegatedRawToken;
                token.push(*ch);
            }

            // [Negated] Single/Double quoted state handlers
            (ParseState::SingleQuote, None) |
            (ParseState::NegatedSingleQuote, None) => {
                result.push(QueryTerm::from_value(format!("{}'{}", if state.is_negated() { "-" } else { "" }, token)));
                break;
            }
            (ParseState::DoubleQuote, None) |
            (ParseState::NegatedDoubleQuote, None) => {
                result.push(QueryTerm::from_value(format!("{}\"{}", if state.is_negated() { "-" } else { "" }, token)));
                break;
            }
            (ParseState::SingleQuote, Some('\'')) |
            (ParseState::DoubleQuote, Some('"')) |
            (ParseState::NegatedSingleQuote, Some('\'')) |
            (ParseState::NegatedDoubleQuote, Some('"')) => {
                result.push(QueryTerm::new(state.is_negated(), None, token));
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
                result.push(QueryTerm::from_value(token));
                break;
            }
            (ParseState::RawToken, Some(':')) => {
                key = Some(token);
                token = String::new();
                state = ParseState::Value;
            }
            (ParseState::RawToken, Some(ref ch)) if ch.is_ascii_whitespace() => {
                result.push(QueryTerm::from_value(token));
                token = String::new();
                state = ParseState::Initial;
            }
            (ParseState::RawToken, Some(ref ch)) => {
                token.push(*ch);
            }

            // Negated raw token state handlers
            (ParseState::NegatedRawToken, None) => {
                result.push(QueryTerm::new(true, None, token));
                break;
            }
            (ParseState::NegatedRawToken, Some(':')) => {
                key = Some(token);
                token = String::new();
                state = ParseState::NegatedValue;
            }
            (ParseState::NegatedRawToken, Some(ref ch)) if ch.is_ascii_whitespace() => {
                result.push(QueryTerm::new(true, None, token));
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
                result.push(QueryTerm::new(state.is_negated(), key, token));
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
                result.push(QueryTerm::new(state.is_negated(), key, token));
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
                result.push(QueryTerm::new(state.is_negated(), key, format!("'{}", token)));
                break;
            }
            (ParseState::DoubleQuotedValue, None) |
            (ParseState::NegatedDoubleQuotedValue, None) => {
                result.push(QueryTerm::new(state.is_negated(), key, format!("\"{}", token)));
                break;
            }
            (ParseState::SingleQuotedValue, Some('\'')) |
            (ParseState::DoubleQuotedValue, Some('"')) |
            (ParseState::NegatedSingleQuotedValue, Some('\'')) |
            (ParseState::NegatedDoubleQuotedValue, Some('"')) => {
                result.push(QueryTerm::new(state.is_negated(), key, token));
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
    fn quoted() {
        assert_eq!(parse("'hello' 'world'").terms, &[QueryTerm::new(false, None, "hello"), QueryTerm::new(false, None, "world")]);
        assert_eq!(parse(" 'hello''world' ").terms, &[QueryTerm::new(false, None, "hello"), QueryTerm::new(false, None, "world")]);
        assert_eq!(parse("\"hello\" \"world\"").terms, &[QueryTerm::new(false, None, "hello"), QueryTerm::new(false, None, "world")]);
        assert_eq!(parse(" \"hello\"\"world\" ").terms, &[QueryTerm::new(false, None, "hello"), QueryTerm::new(false, None, "world")]);

        assert_eq!(parse("-'hello' 'world'").terms, &[QueryTerm::new(true, None, "hello"), QueryTerm::new(false, None, "world")]);
        assert_eq!(parse(" 'hello'-'world' ").terms, &[QueryTerm::new(false, None, "hello"), QueryTerm::new(true, None, "world")]);
        assert_eq!(parse("\"hello\" -\"world\"").terms, &[QueryTerm::new(false, None, "hello"), QueryTerm::new(true, None, "world")]);
        assert_eq!(parse(" -\"hello\"-\"world\" ").terms, &[QueryTerm::new(true, None, "hello"), QueryTerm::new(true, None, "world")]);
    }

    #[test]
    fn raw_tokens() {
        assert_eq!(parse("hello").terms, &[QueryTerm::from_value("hello")]);
        assert_eq!(parse(" hello ").terms, &[QueryTerm::from_value("hello")]);
        assert_eq!(parse(" hello world ").terms, &[QueryTerm::from_value("hello"), QueryTerm::from_value("world")]);
        assert_eq!(parse("\rhello\nworld\t").terms, &[QueryTerm::from_value("hello"), QueryTerm::from_value("world")]);

        assert_eq!(parse(" -hello ").terms, &[QueryTerm::new(true, None, "hello")]);
        assert_eq!(parse(" -hello-world ").terms, &[QueryTerm::new(true, None, "hello-world")]);
        assert_eq!(parse(" -hello -world ").terms, &[QueryTerm::new(true, None, "hello"), QueryTerm::new(true, None, "world")]);
    }

    #[test]
    fn raw_values() {
        assert_eq!(parse("key:value").terms, &[QueryTerm::new(false, Some("key"), "value")]);
        assert_eq!(parse("key:value key2:value2").terms, &[QueryTerm::new(false, Some("key"), "value"), QueryTerm::new(false, Some("key2"), "value2")]);
        assert_eq!(parse("key: anotherValue").terms, &[QueryTerm::new(false, Some("key"), ""), QueryTerm::new(false, None, "anotherValue")]);
        assert_eq!(parse(" key:value ").terms, &[QueryTerm::new(false, Some("key"), "value")]);

        assert_eq!(parse("-key:value").terms, &[QueryTerm::new(true, Some("key"), "value")]);
        assert_eq!(parse(" -key:value ").terms, &[QueryTerm::new(true, Some("key"), "value")]);
        assert_eq!(parse(" key:-value ").terms, &[QueryTerm::new(false, Some("key"), "-value")]);
        assert_eq!(parse(" key:- ").terms, &[QueryTerm::new(false, Some("key"), "-")]);
    }

    #[test]
    fn quoted_values() {
        assert_eq!(parse("key:'value'").terms, &[QueryTerm::new(false, Some("key"), "value")]);
        assert_eq!(parse("key:\"value with spaces\"").terms, &[QueryTerm::new(false, Some("key"), "value with spaces")]);
        assert_eq!(parse("key:\"value\" key2:'another value'").terms, &[QueryTerm::new(false, Some("key"), "value"), QueryTerm::new(false, Some("key2"), "another value")]);

        assert_eq!(parse("-key:'value'").terms, &[QueryTerm::new(true, Some("key"), "value")]);
        assert_eq!(parse("-key:\"value with spaces\"").terms, &[QueryTerm::new(true, Some("key"), "value with spaces")]);
        assert_eq!(parse("key:\"value\" -key2:'another value'").terms, &[QueryTerm::new(false, Some("key"), "value"), QueryTerm::new(true, Some("key2"), "another value")]);
    }

    #[test]
    fn end_unexpectedly() {
        assert_eq!(parse(" -").terms, &[QueryTerm::new(false, None, "-")]);
        assert_eq!(parse("'hello").terms, &[QueryTerm::new(false, None, "'hello")]);
        assert_eq!(parse("\"hello ").terms, &[QueryTerm::new(false, None, "\"hello ")]);
        assert_eq!(parse("-'hello").terms, &[QueryTerm::new(false, None, "-'hello")]);
        assert_eq!(parse("-\"hello ").terms, &[QueryTerm::new(false, None, "-\"hello ")]);

        assert_eq!(parse("key:\"value ").terms, &[QueryTerm::new(false, Some("key"), "\"value ")]);
        assert_eq!(parse("-key:'value ").terms, &[QueryTerm::new(true, Some("key"), "'value ")]);
    }
}
