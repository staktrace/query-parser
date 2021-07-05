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
}

fn parse_tree(raw: &str) -> Vec<QueryTerm> {
    let mut result = Vec::new();
    let mut c = raw.chars();
    let mut state = ParseState::Initial;
    let mut token = String::new();
    loop {
        match (state, c.next()) {
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
            (ParseState::SingleQuote, None) => {
                result.push(QueryTerm::from_value(format!("'{}", token)));
                return result;
            }
            (ParseState::SingleQuote, Some('\'')) => {
                result.push(QueryTerm::from_value(token));
                token = String::new();
                state = ParseState::Initial;
            }
            (ParseState::DoubleQuote, None) => {
                result.push(QueryTerm::from_value(format!("\"{}", token)));
                return result;
            }
            (ParseState::DoubleQuote, Some('"')) => {
                result.push(QueryTerm::from_value(token));
                token = String::new();
                state = ParseState::Initial;
            }
            (ParseState::SingleQuote, Some(ref ch)) |
            (ParseState::DoubleQuote, Some(ref ch)) => {
                token.push(*ch);
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
        assert_eq!(parse("'hello' 'world'").terms, &[QueryTerm::from_value("hello"), QueryTerm::from_value("world")]);
        assert_eq!(parse(" 'hello''world' ").terms, &[QueryTerm::from_value("hello"), QueryTerm::from_value("world")]);
        assert_eq!(parse("\"hello\" \"world\"").terms, &[QueryTerm::from_value("hello"), QueryTerm::from_value("world")]);
        assert_eq!(parse(" \"hello\"\"world\" ").terms, &[QueryTerm::from_value("hello"), QueryTerm::from_value("world")]);
    }

    #[test]
    fn end_unexpectedly() {
        assert_eq!(parse(" -").terms, &[QueryTerm::from_value("-")]);
        assert_eq!(parse("'hello").terms, &[QueryTerm::from_value("'hello")]);
        assert_eq!(parse("\"hello").terms, &[QueryTerm::from_value("\"hello")]);
    }
}
