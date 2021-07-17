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
