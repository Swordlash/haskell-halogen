# haskell-halogen-th-plugin

Normally, GHC expands quasi-quotes during type checking and reports errors at the start of the quote. This plugin expands `[hh| … |]`, `[hhs| … |]` and `[clay| … |]` while parsing the module instead. GHC parses the Haskell expressions inside each quote at their actual positions in the source file.

As a result, parse errors point to the expression's line and column, and type errors point to the expression that caused them. This includes incorrect handlers, unsupported child values and mismatched loop patterns. Template errors, such as unclosed expressions or children inside a void element, also point to the relevant line.

The plugin generates the same code as the quasi-quoters. Once it expands a quote, the quoter does not run again. The `parity-test` suite in `test/parity` reuses `th/test` to check that both approaches produce the same HTML and CSS.

Both approaches also reject the same unsupported expressions, such as splices and arrows. Each expression is parsed once: `haskell-halogen-th` passes the GHC syntax tree and its validation result to the plugin. Imports of the quasi-quoters still count as used.

```cabal
build-depends: haskell-halogen-th, haskell-halogen-th-plugin
ghc-options:   -fplugin=Halogen.TH.Plugin
```

To enable the plugin for a single module, add `{-# OPTIONS_GHC -fplugin=Halogen.TH.Plugin #-}`. GHC-based language servers can then show errors, hover information and go-to-definition targets inside quoted expressions.

The plugin runs inside the compiler and is available only for native builds. Put its flag behind a Cabal flag or in a project file that cross builds do not import. WebAssembly and JavaScript builds use the ordinary quasi-quoters and generate the same code. The plugin identifies quoters by name, so another quoter named `hh` would be treated as this package's `hh`.

`test/run.sh` tests the plugin after `cabal build all`. It compiles valid quotes and checks the locations of syntax errors, type errors, unsupported child values and expressions rejected by the quoter.
