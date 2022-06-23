use std::borrow::Cow;

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
    allow_backslash_quotes: bool,
    allow_unicode_escapes: bool,
}

impl ParseOptions {
    pub fn default() -> Self {
        Self {
            allow_backslash_quotes: true,
            allow_unicode_escapes: true,
        }
    }

    /// Allows single- and double-quote characters to be escaped inside quoted strings by
    /// preceding them with a backslash. Similarly, backslash characters themselves can be
    /// escaped inside quoted strings by preceding them with a backslash. Note that if a
    /// backslash is followed by any another character, or is inside a non-quoted term, then
    /// it is treated as a literal backslash.
    pub fn allow_backslash_quotes(&mut self, allow: bool) -> &mut ParseOptions {
        self.allow_backslash_quotes = allow;
        self
    }

    /// Allows `\uXXXXXX` unicode escapes in term values. Each X must be a hexadecimal
    /// character, and all six are required (use zero-padding as needed). To represent
    /// a string like `\u001234` literally in a query string, one or more of the
    /// characters can be escaped via the unicode escape sequence; in this example
    /// `\u00005Cu001234` can be used to escape the initial backslash character
    /// with the equivalent unicode escape.
    pub fn allow_unicode_escapes(&mut self, allow: bool) -> &mut ParseOptions {
        self.allow_unicode_escapes = allow;
        self
    }
}

pub fn parse(raw: &str) -> Query {
    parse_with_options(raw, &ParseOptions::default())
}

pub fn parse_with_options(raw: &str, opts: &ParseOptions) -> Query {
    Query {
        raw_query: String::from(raw),
        terms: parse_terms(raw, opts).0,
    }
}

#[derive(Clone, Copy, Debug)]
enum ParseState {
    Initial, // before a new term is processed
    Negated, // term started with a '-' character
    SingleQuote, // term started with a single quote
    DoubleQuote, // term started with a double quote
    SingleQuoteEscape, // encountered backslash inside a single-quoted term
    DoubleQuoteEscape, // encountered backslash inside a double-quoted term
    RawToken, // term started without quoting
    NegatedSingleQuote, // term started with a '-' followed by a single quote
    NegatedDoubleQuote, // term started with a '-' followed by a double quote
    NegatedSingleQuoteEscape, // encountered backslash inside NegatedSingleQuote
    NegatedDoubleQuoteEscape, // encountered backslash inside NegatedDoubleQuote
    NegatedRawToken, // term started with a '-' followed by unquoted characters
    Value, // after encountering the ':' to separate key from value
    NegatedValue, // after encountering the ':' to separated negated key from value
    RawValue, // Once the value has been determined to be unquoted
    SingleQuotedValue, // Once the value has been determined to be single-quoted
    DoubleQuotedValue, // Once the value has been determined to be double-quoted
    SingleQuotedValueEscape, // Encountered backslash inside SingleQuotedValue
    DoubleQuotedValueEscape, // Encountered backslash inside DoubleQuotedValue
    NegatedRawValue, // Once the value for a negated term has been determined to be unquoted
    NegatedSingleQuotedValue, // Once the value for a negated term has been determined to be single-quoted
    NegatedDoubleQuotedValue, // Once the value for a negated term has been determined to be double-quoted
    NegatedSingleQuotedValueEscape, // Encountered backslash inside NegatedSingleQuotedValue
    NegatedDoubleQuotedValueEscape, // Encountered backslash inside NegatedDoubleQuotedValue
}

impl ParseState {
    fn is_negated(&self) -> bool {
        match self {
            Self::Negated |
            Self::NegatedSingleQuote |
            Self::NegatedDoubleQuote |
            Self::NegatedSingleQuoteEscape |
            Self::NegatedDoubleQuoteEscape |
            Self::NegatedRawToken |
            Self::NegatedValue |
            Self::NegatedRawValue |
            Self::NegatedSingleQuotedValue |
            Self::NegatedDoubleQuotedValue |
            Self::NegatedSingleQuotedValueEscape |
            Self::NegatedDoubleQuotedValueEscape => true,
            _ => false,
        }
    }

    fn is_single_quote(&self) -> bool {
        match self {
            Self::SingleQuote |
            Self::SingleQuoteEscape |
            Self::NegatedSingleQuote |
            Self::NegatedSingleQuoteEscape |
            Self::SingleQuotedValue |
            Self::SingleQuotedValueEscape |
            Self::NegatedSingleQuotedValue |
            Self::NegatedSingleQuotedValueEscape => true,
            _ => false,
        }
    }

    fn escape(&self) -> Self {
        match self {
            ParseState::SingleQuote => ParseState::SingleQuoteEscape,
            ParseState::DoubleQuote => ParseState::DoubleQuoteEscape,
            ParseState::NegatedSingleQuote => ParseState::NegatedSingleQuoteEscape,
            ParseState::NegatedDoubleQuote => ParseState::NegatedDoubleQuoteEscape,
            ParseState::SingleQuotedValue => ParseState::SingleQuotedValueEscape,
            ParseState::DoubleQuotedValue => ParseState::DoubleQuotedValueEscape,
            ParseState::NegatedSingleQuotedValue => ParseState::NegatedSingleQuotedValueEscape,
            ParseState::NegatedDoubleQuotedValue => ParseState::NegatedDoubleQuotedValueEscape,
            _ => panic!("Unescapable state"),
        }
    }

    fn unescape(&self) -> Self {
        match self {
            ParseState::SingleQuoteEscape => ParseState::SingleQuote,
            ParseState::DoubleQuoteEscape => ParseState::DoubleQuote,
            ParseState::NegatedSingleQuoteEscape => ParseState::NegatedSingleQuote,
            ParseState::NegatedDoubleQuoteEscape => ParseState::NegatedDoubleQuote,
            ParseState::SingleQuotedValueEscape => ParseState::SingleQuotedValue,
            ParseState::DoubleQuotedValueEscape => ParseState::DoubleQuotedValue,
            ParseState::NegatedSingleQuotedValueEscape => ParseState::NegatedSingleQuotedValue,
            ParseState::NegatedDoubleQuotedValueEscape => ParseState::NegatedDoubleQuotedValue,
            _ => panic!("Unescaped state"),
        }
    }

    fn is_escaped(&self) -> bool {
        match self {
            Self::SingleQuoteEscape |
            Self::DoubleQuoteEscape |
            Self::NegatedSingleQuoteEscape |
            Self::NegatedDoubleQuoteEscape |
            Self::SingleQuotedValueEscape |
            Self::DoubleQuotedValueEscape |
            Self::NegatedSingleQuotedValueEscape |
            Self::NegatedDoubleQuotedValueEscape => true,
            _ => false,
        }
    }
}

fn hex_to_nybble(hex: u8) -> u32 {
    match hex {
        b'0'..=b'9' => (hex - b'0').into(),
        b'a'..=b'f' => (hex - b'a' + 10).into(),
        b'A'..=b'F' => (hex - b'A' + 10).into(),
        _ => panic!("Not a hex character!"),
    }
}

fn decode_unicode_escape(s: &str, ix: usize) -> Option<char> {
    let bytes = s.as_bytes();
    if ix + 7 < s.len() &&
        bytes[ix + 1] == b'u' &&
        bytes[ix + 2].is_ascii_hexdigit() &&
        bytes[ix + 3].is_ascii_hexdigit() &&
        bytes[ix + 4].is_ascii_hexdigit() &&
        bytes[ix + 5].is_ascii_hexdigit() &&
        bytes[ix + 6].is_ascii_hexdigit() &&
        bytes[ix + 7].is_ascii_hexdigit()
    {
        let uchar = (hex_to_nybble(bytes[ix + 2]) << 20) |
            (hex_to_nybble(bytes[ix + 3]) << 16) |
            (hex_to_nybble(bytes[ix + 4]) << 12) |
            (hex_to_nybble(bytes[ix + 5]) << 8) |
            (hex_to_nybble(bytes[ix + 6]) << 4) |
            (hex_to_nybble(bytes[ix + 7]));
        return std::char::from_u32(uchar);
    }

    None
}

fn decode_unicode_escapes<'a>(mut s: &'a str) -> Cow<'a, str> {
    let mut ret = Cow::Borrowed(s);
    loop {
        if let Some(ix) = s.find('\\') {
            if let Some(ch) = decode_unicode_escape(s, ix) {
                match ret {
                    Cow::Borrowed(_) => {
                        let mut decoded = String::with_capacity(s.len());
                        decoded.push_str(&s[0..ix]);
                        decoded.push(ch);
                        ret = Cow::Owned(decoded);
                    }
                    Cow::Owned(ref mut owned) => {
                        owned.push_str(&s[0..ix]);
                        owned.push(ch);
                    }
                }
                s = &s[(ix + 8)..];
                continue;
            }
            s = &s[(ix + 1)..];
            continue;
        }

        match ret {
            Cow::Borrowed(_) => (),
            Cow::Owned(ref mut owned) => owned.push_str(s),
        }
        break;
    }
    return ret;
}

impl ParseOptions {
    fn decode_unicode(&self, s: String) -> String {
        if !self.allow_unicode_escapes {
            return s;
        }

        match decode_unicode_escapes(&s) {
            Cow::Borrowed(_) => s,
            Cow::Owned(owned) => owned,
        }
    }
}

/// Returns the terms parsed from the provided "raw" string, plus a proposed
/// corrected string that can be passed to this function again for parsing.  If
/// the "raw" string already looked unambiguously correct then None will be
/// returned instead of a corrected string.
///
/// Note that each correction pass may potentially may not be able to correct
/// all ambiguities in the query string, so it may be advisable to repeatedly
/// call this function until no more corrections are proposed.
fn parse_terms(raw: &str, opts: &ParseOptions) -> (Vec<Term>, Option<String>) {
    let mut result = Vec::new();

    let mut state = ParseState::Initial;
    let mut key = None;
    let mut token = String::new();

    // This is used to disable parsing after we make our first correction that
    // would cause a divergence in parsing between our correction and the
    // current parse.
    let mut correction_enabled = true;
    // The proposed correction string; if this ends up being the same as "raw"
    // then we return None for our correction.
    let mut corrected = String::new();
    // The number of input tokens to skip propagating to `corrected` because of
    // corrective actions taken.
    let mut skip_propagating = 0;

    let mut c = raw.chars();
    loop {
        let next_c = c.next();
        let write_as = match next_c {
            Some(val) => val,
            None => 0 as char,
        };
        match (state, next_c) {
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
                corrected.push(*ch);
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
            (ParseState::DoubleQuote, None) |
            (ParseState::SingleQuoteEscape, None) |
            (ParseState::DoubleQuoteEscape, None) |
            (ParseState::NegatedSingleQuote, None) |
            (ParseState::NegatedDoubleQuote, None) |
            (ParseState::NegatedSingleQuoteEscape, None) |
            (ParseState::NegatedDoubleQuoteEscape, None) => {
                result.push(Term::new(state.is_negated(), None, format!(
                    "{}{}{}",
                    if state.is_single_quote() { "'" } else { "\"" },
                    opts.decode_unicode(token),
                    if state.is_escaped() { "\\" } else { "" },
                )));
                break;
            }
            (ParseState::SingleQuoteEscape, Some(ref ch)) |
            (ParseState::DoubleQuoteEscape, Some(ref ch)) |
            (ParseState::NegatedSingleQuoteEscape, Some(ref ch)) |
            (ParseState::NegatedDoubleQuoteEscape, Some(ref ch)) => {
                if !(*ch == '\'' || *ch == '"' || *ch == '\\') {
                    token.push('\\');
                }
                token.push(*ch);
                state = state.unescape();
            }
            (ParseState::SingleQuote, Some(qc @ '\'')) |
            (ParseState::DoubleQuote, Some(qc @ '"')) |
            (ParseState::NegatedSingleQuote, Some(qc @ '\'')) |
            (ParseState::NegatedDoubleQuote, Some(qc @ '"')) |
            (ParseState::SingleQuotedValue, Some(qc @ '\'')) |
            (ParseState::DoubleQuotedValue, Some(qc @ '"')) |
            (ParseState::NegatedSingleQuotedValue, Some(qc @ '\'')) |
            (ParseState::NegatedDoubleQuotedValue, Some(qc @ '"')) => {
                // We have a simple heuristic for noting potential typos as it
                // relates to closing quotes: if there's not a space or the end
                // of the string after the quote (and we're currently in a
                // quoted string, as we are), then you probably made:
                // - A transposition typo.  Ex: `'fo'o` should be `'foo'`.
                // - A quoting error, like `'don't'` should have the single
                //   quote escaped if using single quotes, or it should have
                //   been quoted using double-quotes.  For simplicity, we just
                //   escape the quote.
                if correction_enabled {
                    let mut lookahead = c.clone();
                    match (lookahead.next(), lookahead.next()) {
                        // If the next character is whitespace or the end of the
                        // string, then this is a perfect parse, no action needed.
                        (None, _) => {}
                        (Some(ws), _) if ws.is_ascii_whitespace() => {}
                        // If there's a non-whitespace character followed by whitespace,
                        // that's transposition.
                        (Some(ch), Some (ws)) if !ch.is_ascii_whitespace() && ws.is_ascii_whitespace() => {
                            corrected.push(ch);
                            corrected.push(qc);
                            skip_propagating = 2;
                            // Correcting the transposition will not cause parse
                            // state to diverge so we can leave correction on.
                        }
                        // Anything else means neither character is whitespace,
                        // which means we should just escape the quoting char, which
                        // means injecting a backslash.
                        _ => {
                            corrected.push('\\');
                            // Escaping a quote will cause parse divergense, so
                            // this is the last correction we can make this pass.
                            correction_enabled = false;
                        }
                    }
                }
                result.push(Term::new(state.is_negated(), key, opts.decode_unicode(token)));
                key = None;
                token = String::new();
                state = ParseState::Initial;
            }
            (ParseState::SingleQuote, Some(ref ch)) |
            (ParseState::DoubleQuote, Some(ref ch)) |
            (ParseState::NegatedSingleQuote, Some(ref ch)) |
            (ParseState::NegatedDoubleQuote, Some(ref ch)) => {
                if opts.allow_backslash_quotes && *ch == '\\' {
                    state = state.escape();
                } else {
                    token.push(*ch);
                }
            }

            // Raw token state handlers
            (ParseState::RawToken, None) => {
                result.push(Term::from_value(opts.decode_unicode(token)));
                break;
            }
            (ParseState::RawToken, Some(':')) => {
                key = Some(token);
                token = String::new();
                state = ParseState::Value;
            }
            (ParseState::RawToken, Some(ref ch)) if ch.is_ascii_whitespace() => {
                result.push(Term::from_value(opts.decode_unicode(token)));
                token = String::new();
                state = ParseState::Initial;
            }
            (ParseState::RawToken, Some(ref ch)) => {
                token.push(*ch);
            }

            // Negated raw token state handlers
            (ParseState::NegatedRawToken, None) => {
                result.push(Term::new(true, None, opts.decode_unicode(token)));
                break;
            }
            (ParseState::NegatedRawToken, Some(':')) => {
                key = Some(token);
                token = String::new();
                state = ParseState::NegatedValue;
            }
            (ParseState::NegatedRawToken, Some(ref ch)) if ch.is_ascii_whitespace() => {
                result.push(Term::new(true, None, opts.decode_unicode(token)));
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
                result.push(Term::new(state.is_negated(), key, opts.decode_unicode(token)));
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
                result.push(Term::new(state.is_negated(), key, opts.decode_unicode(token)));
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
            (ParseState::DoubleQuotedValue, None) |
            (ParseState::SingleQuotedValueEscape, None) |
            (ParseState::DoubleQuotedValueEscape, None) |
            (ParseState::NegatedSingleQuotedValue, None) |
            (ParseState::NegatedDoubleQuotedValue, None) |
            (ParseState::NegatedSingleQuotedValueEscape, None) |
            (ParseState::NegatedDoubleQuotedValueEscape, None) => {
                result.push(Term::new(state.is_negated(), key, format!(
                    "{}{}{}",
                    if state.is_single_quote() { "'" } else { "\"" },
                    opts.decode_unicode(token),
                    if state.is_escaped() { "\\" } else { "" },
                )));
                break;
            }
            (ParseState::SingleQuotedValueEscape, Some(ref ch)) |
            (ParseState::DoubleQuotedValueEscape, Some(ref ch)) |
            (ParseState::NegatedSingleQuotedValueEscape, Some(ref ch)) |
            (ParseState::NegatedDoubleQuotedValueEscape, Some(ref ch)) => {
                if !(*ch == '\'' || *ch == '"' || *ch == '\\') {
                    token.push('\\');
                }
                token.push(*ch);
                state = state.unescape();
            }
            (ParseState::SingleQuotedValue, Some(ref ch)) |
            (ParseState::DoubleQuotedValue, Some(ref ch)) |
            (ParseState::NegatedSingleQuotedValue, Some(ref ch)) |
            (ParseState::NegatedDoubleQuotedValue, Some(ref ch)) => {
                if opts.allow_backslash_quotes && *ch == '\\' {
                    state = state.escape();
                } else {
                    token.push(*ch);
                }
            }
        }
        if skip_propagating > 0 {
            skip_propagating -= 1;
        } else {
            corrected.push(write_as);
        }
    }

    (result, if raw == corrected { None } else { Some(corrected) })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test-only function for ergonomics on writing correction test cases.
    pub fn correct(raw: &str) -> Option<String> {
        let opts = ParseOptions::default();
        if let Some(mut current)  = parse_terms(raw, &opts).1 {
            for _i in 1..10 {
                if let Some(next) = parse_terms(&current, &opts).1 {
                    current = next;
                } else {
                    break;
                }
            }
            Some(current)
        } else {
            None
        }
    }

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

        assert_eq!(correct("'hello' 'world'"), None);
        assert_eq!(correct(" 'hello''world' ").as_deref(), Some(" 'hello\\'\\'world' "));
        assert_eq!(correct("\"hello\" \"world\""), None);
        assert_eq!(correct(" \"hello\"\"world\" ").as_deref(), Some(" \"hello\\\"\\\"world\" "));

        assert_eq!(parse("-'hello' 'world'").terms, &[Term::new(true, None, "hello"), Term::new(false, None, "world")]);
        assert_eq!(parse(" 'hello'-'world' ").terms, &[Term::new(false, None, "hello"), Term::new(true, None, "world")]);
        assert_eq!(parse("\"hello\" -\"world\"").terms, &[Term::new(false, None, "hello"), Term::new(true, None, "world")]);
        assert_eq!(parse(" -\"hello\"-\"world\" ").terms, &[Term::new(true, None, "hello"), Term::new(true, None, "world")]);

        assert_eq!(correct("'I made a typ'o 'oh no!'").as_deref(), Some("'I made a typo' 'oh no!'"));
        assert_eq!(correct("'I made a typ'o'graphical error'").as_deref(), Some("'I made a typ\\'o\\'graphical error'"));

        // This is a test that we don't try and make more than one suggestion in
        // a single pass.  This is necessary because our suggestions about
        // escaping quotes inherently cause a conceptual divergence in parse
        // state, so any suggestions made after the first might be moot.  This
        // case does test that we perform at least 2 correction passes.
        assert_eq!(correct("'quote'\"danger\"'hat\"bat'").as_deref(), Some("'quote\\'\"danger\"\\'hat\"bat'"));
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
    fn double_colons() {
        assert_eq!(parse("Class::method").terms, &[Term::new(false, Some("Class"), ":method")]);
        assert_eq!(parse("'Class::method'").terms, &[Term::new(false, None, "Class::method")]);
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
        assert_eq!(parse("'hello\\").terms, &[Term::new(false, None, "'hello\\")]);
        assert_eq!(parse("\"hello ").terms, &[Term::new(false, None, "\"hello ")]);
        assert_eq!(parse("hello\\").terms, &[Term::new(false, None, "hello\\")]);
        assert_eq!(parse("-'hello").terms, &[Term::new(true, None, "'hello")]);
        assert_eq!(parse("-'hello\\").terms, &[Term::new(true, None, "'hello\\")]);
        assert_eq!(parse("-\"hello ").terms, &[Term::new(true, None, "\"hello ")]);
        assert_eq!(parse("-hello\\").terms, &[Term::new(true, None, "hello\\")]);

        assert_eq!(parse("key:\"value ").terms, &[Term::new(false, Some("key"), "\"value ")]);
        assert_eq!(parse("-key:'value ").terms, &[Term::new(true, Some("key"), "'value ")]);
    }

    #[test]
    fn parse_unicode() {
        let p = |x| parse_with_options(x, ParseOptions::default().allow_unicode_escapes(false));
        let pu = |x| parse_with_options(x, ParseOptions::default().allow_unicode_escapes(true));

        assert_eq!(p("\\u002021z").terms, &[Term::new(false, None, "\\u002021z")]);
        assert_eq!(pu("\\u002021z").terms, &[Term::new(false, None, "\u{2021}z")]);
        assert_eq!(pu("\\u00202xz").terms, &[Term::new(false, None, "\\u00202xz")]);
        assert_eq!(pu("\\u00202z").terms, &[Term::new(false, None, "\\u00202z")]);
        assert_eq!(pu("\\v002021z").terms, &[Term::new(false, None, "\\v002021z")]);

        assert_eq!(p("\\u002021:'\\u002022 \\u002023'").terms, &[Term::new(false, Some("\\u002021"), "\\u002022 \\u002023")]);
        assert_eq!(pu("\\u002021:'\\u002022 \\u002023'").terms, &[Term::new(false, Some("\\u002021"), "\u{2022} \u{2023}")]);
    }

    #[test]
    fn parse_escapes() {
        let p = |x| parse_with_options(x, ParseOptions::default().allow_backslash_quotes(false));
        let pe = |x| parse_with_options(x, ParseOptions::default().allow_backslash_quotes(true));

        assert_eq!(pe(r#"'fred says \'hi\''"#).terms, &[Term::new(false, None, "fred says 'hi'")]);
        assert_eq!(pe(r#""fred says \'hi\'""#).terms, &[Term::new(false, None, "fred says 'hi'")]);
        assert_eq!(pe(r#""fred says \"hi\"""#).terms, &[Term::new(false, None, "fred says \"hi\"")]);
        assert_eq!(pe(r#"'fred says \"hi\"'"#).terms, &[Term::new(false, None, "fred says \"hi\"")]);
        assert_eq!(pe(r#"'backslashes \\ \n'"#).terms, &[Term::new(false, None, "backslashes \\ \\n")]);
        assert_eq!(pe(r#""backslashes \\ \n""#).terms, &[Term::new(false, None, "backslashes \\ \\n")]);

        assert_eq!(p(r#"'fred says \'hi\''"#).terms, &[Term::new(false, None, "fred says \\"), Term::new(false, None, "hi\\''")]);
        assert_eq!(p(r#""fred says \'hi\'""#).terms, &[Term::new(false, None, "fred says \\'hi\\'")]);
        assert_eq!(p(r#""fred says \"hi\"""#).terms, &[Term::new(false, None, "fred says \\"), Term::new(false, None, "hi\\\"\"")]);
        assert_eq!(p(r#"'fred says \"hi\"'"#).terms, &[Term::new(false, None, "fred says \\\"hi\\\"")]);
        assert_eq!(p(r#"'backslashes \\ \n'"#).terms, &[Term::new(false, None, "backslashes \\\\ \\n")]);
        assert_eq!(p(r#""backslashes \\ \n""#).terms, &[Term::new(false, None, "backslashes \\\\ \\n")]);

        assert_eq!(pe(r#"in\"a\\raw\nword"#).terms, &[Term::new(false, None, "in\\\"a\\\\raw\\nword")]);
        assert_eq!(p(r#"in\"a\\raw\nword"#).terms, &[Term::new(false, None, "in\\\"a\\\\raw\\nword")]);

        assert_eq!(pe(r#"fred:'\'hi\''"#).terms, &[Term::new(false, Some("fred"), "'hi'")]);
        assert_eq!(pe(r#"fred:"\'hi\'""#).terms, &[Term::new(false, Some("fred"), "'hi'")]);
        assert_eq!(pe(r#"fred:"\"hi\"""#).terms, &[Term::new(false, Some("fred"), "\"hi\"")]);
        assert_eq!(pe(r#"fred:'\"hi\"'"#).terms, &[Term::new(false, Some("fred"), "\"hi\"")]);
        assert_eq!(pe(r#"back:'slashes \\ \n'"#).terms, &[Term::new(false, Some("back"), "slashes \\ \\n")]);
        assert_eq!(pe(r#"back:"slashes \\ \n""#).terms, &[Term::new(false, Some("back"), "slashes \\ \\n")]);

        assert_eq!(p(r#"fred:'\'hi\''"#).terms, &[Term::new(false, Some("fred"), "\\"), Term::new(false, None, "hi\\''")]);
        assert_eq!(p(r#"fred:"\'hi\'""#).terms, &[Term::new(false, Some("fred"), "\\'hi\\'")]);
        assert_eq!(p(r#"fred:"\"hi\"""#).terms, &[Term::new(false, Some("fred"), "\\"), Term::new(false, None, "hi\\\"\"")]);
        assert_eq!(p(r#"fred:'\"hi\"'"#).terms, &[Term::new(false, Some("fred"), "\\\"hi\\\"")]);
        assert_eq!(p(r#"back:'slashes \\ \n'"#).terms, &[Term::new(false, Some("back"), "slashes \\\\ \\n")]);
        assert_eq!(p(r#"back:"slashes \\ \n""#).terms, &[Term::new(false, Some("back"), "slashes \\\\ \\n")]);

        assert_eq!(pe(r#"raw:in\"a\\raw\nword"#).terms, &[Term::new(false, Some("raw"), "in\\\"a\\\\raw\\nword")]);
        assert_eq!(p(r#"raw:in\"a\\raw\nword"#).terms, &[Term::new(false, Some("raw"), "in\\\"a\\\\raw\\nword")]);
    }

    #[test]
    fn parse_unicode_and_escape() {
        let p = |x| parse_with_options(
            x,
            ParseOptions::default().allow_unicode_escapes(true).allow_backslash_quotes(true)
        );

        assert_eq!(p(r#"fred:"\'sup \u01F436""#).terms, &[Term::new(false, Some("fred"), "'sup \u{1F436}")]);
        assert_eq!(p(r#"fred:"\'sup \\u01F436""#).terms, &[Term::new(false, Some("fred"), "'sup \u{1F436}")]);
        assert_eq!(p(r#"fred:"\'sup \u00005cu01F436""#).terms, &[Term::new(false, Some("fred"), "'sup \\u01F436")]);
    }

    #[test]
    fn readme_example() {
        println!("{:?}", parse("from:foo -subject:'a long subject \\u00270c' baz"));
    }
}
