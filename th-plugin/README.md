# haskell-halogen-th-plugin

GHC reports an error inside an `[hh| … |]`, `[hhs| … |]` or `[clay| … |]`
quote at the quote, and the quoter can only write the place into the
message. This plugin reports it where it is: as a module is parsed, every
quote of the three quoters is run through the quoter's own checks
(`Halogen.TH.Check`), and what they find becomes a parse error at its line
and column, in the compiler's output and in any language server built on
GHC. A quote that passes is left to its quoter, which then generates the
code as usual.

```cabal
build-depends: haskell-halogen-th, haskell-halogen-th-plugin
ghc-options:   -fplugin=Halogen.TH.Plugin
```

or, in one module, `{-# OPTIONS_GHC -fplugin=Halogen.TH.Plugin #-}`.

A plugin runs inside the compiler, so there is none for the WebAssembly
or JavaScript targets: put the flag in a native build only (a cabal flag,
or a `cabal.project` the cross builds do not import). The quoters are
known by their names, so another `hh` in scope would be read as this one.

`test/run.sh` runs the plugin on a module with every kind of broken quote
and on one with good quotes, after `cabal build all`.
