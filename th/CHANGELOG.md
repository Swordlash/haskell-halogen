# Revision history for haskell-halogen-th

## Unreleased

* First version: the `hh` and `hhs` quasi-quoters, HTML with Haskell inside
  as a Halogen `HTML w i` (one element, or a list of nodes), and `clay`, CSS
  with Haskell inside as Clay's `Css`. A `{{ … }}` is a Haskell expression,
  parsed by GHC's own parser with the extensions of the module it is in;
  `{% if %}`, `{% elif %}`, `{% else %}`, `{% for … in … %}`, `{% let … = … %}`
  and `{% end %}` are the statements, and `{# … #}` a comment. Elements,
  properties and event handlers compile to the typed functions of
  `Halogen.HTML`; CSS rules to Clay's, with selectors and values as written.
* A parse error in the Haskell of a hole names the line and the column in
  the file (`hh: Mod.hs:12: column 31: …`), counted from the quote's
  opening; a hole whose code starts on a later line than its `{{` names
  that line. A language server can put the error there.
