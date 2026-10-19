# Revision history for haskell-halogen-th

## Unreleased

* SVG: an `<svg>` in an `hh` quote is SVG down to a `<foreignObject>`, its
  elements those of `Halogen.Svg.Elements` (in SVG's namespace), its
  attributes those of `Halogen.Svg.Attributes` by their SVG or function
  names, typed (`cx={{ x }}`, `d={{ commands }}`,
  `viewBox={{ (0, 0, w, h) }}`), its `class` an attribute. The `svg` and
  `svgs` quoters start in SVG. A hole right before `/>` no longer takes the
  slash into its value.

* An element with no function of its own (`<stop>`, `<linearGradient>`, a
  custom element) takes the attributes any element has: its class, style,
  ref and handlers are typed, the rest is text. Its typed attributes used
  to be ambiguous (`x1="0"`, `style={{ … }}` failed to compile).

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
  that line. `Halogen.TH.Check` runs a quote through its quoter outside a
  splice, for the GHC plugin of `haskell-halogen-th-plugin`, which expands
  the quotes as a module is parsed so that every error, type errors in the
  holes included, is reported where it is.
