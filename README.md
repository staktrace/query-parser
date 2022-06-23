query-parser
===

A simple parser for search queries. This takes a string like `from:foo -subject:'a long subject \u00270c' baz` and returns it in a structured format:

```
Query {
    raw_query: "from:foo -subject:\'a long subject \\u00270c\' baz",
    terms: [
        Term { negated: false, key: Some("from"), value: Simple("foo") },
        Term { negated: true, key: Some("subject"), value: Simple("a long subject ✌") },
        Term { negated: false, key: None, value: Simple("baz") }
    ]
}
```

The primary entry point for this library are the following functions:

```rust
    parse(&str) -> Query;
    parse_with_options(&str, &ParseOptions) -> Query;
```

Refer to the [full documentation](https://docs.rs/query-parser/) for details.

## Automatic Parse Correction ##

The parser can optionally attempt to correct query syntax that looks like it may
involve typos or a misunderstanding of quoting rules.

Correction is enabled by specifying the number of correction passes to perform.
The parser operates in a single-pass without back-tracking or running multiple
speculative passes in parallel.  Because some correction operations like
escaping a quote character result in a divergence between how the correction
would parse and the current parse is occurring, each correction pass must stop
correcting once it makes a diverging correction.

### Correction Model ###

Correction is based around the idea that best practice for a query will have
spaces between all terms.  This assumption helps create more edit distance
between "best practice" queries.  Specifically, although the parser will parse
`'whoopsi'e` as 2 terms, our best practice guidance means that the best practice
formulations of this query would look like:
- corrected best practice: `'whoopsie'`
- alternate best practice: `'whoopsi' e` or `'whoopsi' 'e'`

We additionally add the assumption to our model that typos where characters are
transposed are more likely than accidental repeated keypresses or spurious
keypresses.

No attempt is made to support correction of key or value term string values.
This would require the parser to have dictionaries available and that is beyond
the scope of the parser.

### Implemented Corrections ###

The following corrections are currently implemented and tested.  More can be
added if they are consistent with the correction model described above and do
not substantially change the complexity of the parser.

- Transposition of the last character in a quoted string with the quote.  For
  example, `'typ'o` will be corrected to `'typo'`.
  - This can run into problems with contractions where the apostrophe is not
    escaped and single-quotes are in use.  If your queries involve a lot of
    prose, it may be advisable to favor an interactive query building model that
    can provide interactive visual feedback about term and phrase boundaries.
- Failure to quote inside a quoted string will result in the quotes being
  escaped.  For example, `'abc'de'fgh'` will have the interior quotes escaped
  as long as correction_passes is at least 2.  When this correction is made, it
  causes a parser divergence, so only a single escaping correction can happen
  per correction pass.
