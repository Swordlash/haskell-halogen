# haskell-halogen-th-plugin

GHC runs a quasi-quoter in the type checker and reports whatever goes wrong
inside an `[hh| … |]`, `[hhs| … |]` or `[clay| … |]` quote at the quote. This
plugin does the quoter's work as the module is parsed instead: each quote
of the three quoters becomes the code its quoter makes, with the Haskell of
every hole parsed by GHC itself at its place in the file. So

- a parse error in a hole is the parser's own, at its line and column;
- a type error in a hole is the type checker's, at the hole (a handler of
  the wrong type under `onClick`, a child no element can hold, a `for`
  pattern that does not fit the list);
- what the template itself gets wrong (an unclosed hole, an element that
  takes no children) is reported at its line.

The quoter never runs for a quote the plugin has expanded, and the code is
the same: the th package's own tests, compiled with the plugin, build the
same HTML and CSS (`plugin-test`). The import that brings the quoter stays
used.

```cabal
build-depends: haskell-halogen-th, haskell-halogen-th-plugin
ghc-options:   -fplugin=Halogen.TH.Plugin
```

or, in one module, `{-# OPTIONS_GHC -fplugin=Halogen.TH.Plugin #-}`. A
language server that builds on GHC shows the errors there too, and hover
and go-to-definition work inside the holes.

A plugin runs inside the compiler, so there is none for the WebAssembly or
JavaScript targets: put the flag in a native build only (a cabal flag, or a
`cabal.project` the cross builds do not import); there the quoters run as
before and make the same code. The quoters are known by their names, so
another `hh` in scope would be read as this one.

`test/run.sh` runs the plugin on a compiler, after `cabal build all`: a
module with every kind of broken quote (each error at its place), one with
good quotes, one whose quotes do not type check (each type error at its
hole) and one with a hole no element can hold.
