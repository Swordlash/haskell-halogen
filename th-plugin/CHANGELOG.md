# Revision history for haskell-halogen-th-plugin

## Unreleased

* First version: a GHC plugin (`-fplugin=Halogen.TH.Plugin`) that expands
  every `hh`, `hhs` and `clay` quote as a module is parsed into the code
  its quoter makes, with the Haskell of every hole parsed by GHC at its
  place in the file. A parse error in a hole is the parser's own, a type
  error in a hole is the type checker's at the hole, and what the template
  gets wrong is reported at its line. The quoter never runs for an expanded
  quote; the th package's tests compiled with the plugin build the same
  HTML and CSS.
