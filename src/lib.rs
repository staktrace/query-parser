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
}

impl ParseState {
    fn is_negated(&self) -> bool {
        match self {
            Self::Negated |
            Self::NegatedSingleQuote |
            Self::NegatedDoubleQuote |
            Self::NegatedRawToken |
            Self::NegatedValue => true,
            _ => false,
        }
    }
}

fn parse_tree(raw: &str) -> Vec<QueryTerm> {
    let mut result = Vec::new();
    let mut c = raw.chars();
    let mut state = ParseState::Initial;
    let mut key = None;
    let mut token = String::new();
    loop {
        match (state, c.next()) {
            // Initial state handlers
            (ParseState::Initial, None) => {
                return result;
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
                return result;
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
                return result;
            }
            (ParseState::DoubleQuote, None) |
            (ParseState::NegatedDoubleQuote, None) => {
                result.push(QueryTerm::from_value(format!("{}\"{}", if state.is_negated() { "-" } else { "" }, token)));
                return result;
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
                return result;
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
                return result;
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
            (ParseState::RawValue, None) => {
                result.push(QueryTerm::new(false, key, token));
                return result;
            }
            (ParseState::Value, Some(ref ch)) |
            (ParseState::RawValue, Some(ref ch))
                if ch.is_ascii_whitespace() =>
            {
                result.push(QueryTerm::new(false, key, token));
                key = None;
                token = String::new();
                state = ParseState::Initial;
            }
            (ParseState::Value, Some(ref ch)) |
            (ParseState::RawValue, Some(ref ch)) => {
                token.push(*ch);
                state = ParseState::RawValue;
            }
            (_, _) => {
                continue;
            }
        }
    }
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
    }

    #[test]
    fn raw_values() {
        assert_eq!(parse("key:value").terms, &[QueryTerm::new(false, Some("key"), "value")]);
        assert_eq!(parse("key:value key2:value2").terms, &[QueryTerm::new(false, Some("key"), "value"), QueryTerm::new(false, Some("key2"), "value2")]);
        assert_eq!(parse("key: anotherValue").terms, &[QueryTerm::new(false, Some("key"), ""), QueryTerm::new(false, None, "anotherValue")]);
    }

    #[test]
    fn end_unexpectedly() {
        assert_eq!(parse(" -").terms, &[QueryTerm::new(false, None, "-")]);
        assert_eq!(parse("'hello").terms, &[QueryTerm::new(false, None, "'hello")]);
        assert_eq!(parse("\"hello").terms, &[QueryTerm::new(false, None, "\"hello")]);
        assert_eq!(parse("-'hello").terms, &[QueryTerm::new(false, None, "-'hello")]);
        assert_eq!(parse("-\"hello").terms, &[QueryTerm::new(false, None, "-\"hello")]);
    }
}
