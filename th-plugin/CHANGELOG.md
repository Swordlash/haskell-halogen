# Revision history for haskell-halogen-th-plugin

## Unreleased

* First version: a GHC plugin (`-fplugin=Halogen.TH.Plugin`) that reads
  every `hh`, `hhs` and `clay` quote as a module is parsed, runs the
  quoter's own checks on it and reports what they find at its line and
  column in the file, among the parser's errors, so the compiler and a
  language server show an error in a quote where it is.
