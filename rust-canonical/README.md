# miniKANREN

This is a Rust implementation of miniKANREN.

## Syntax mapping

Originally, miniKANREN was implemented as a DSL in Scheme, inheriting Scheme's S-expression syntax.

This crate implements miniKANREN as a DSL in Rust, with obviously different syntax.

The table below illustrates how to map from one to the other:

| Scheme                     | Rust                        |
| -------------------------- | --------------------------- |
| `(run* q (== q 1))`        | `run!(*, q { eq(q, 1); })`  |
