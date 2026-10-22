# Revision history for haskell-halogen-th

## Unreleased

* Whitespace in text follows JSX more closely. A hole or a statement now
  ends a text as a tag does, and each text is trimmed on its own, so a line
  break between two holes or statements, with nothing but whitespace
  between them, makes no text node: `{{ a }}` and `{{ b }}` on lines of
  their own used to have a `" "` between them, and every statement on a
  line of its own inside a `<ul>` left a `" "` among its items. A word and
  a hole on separate lines have no space between them either, as in JSX:
  put them on one line (`Hello, {{ name }}`) or write the space as a hole,
  `{{ " " :: Text }}`. A text over several lines now keeps the spaces at
  its first line's start and its last line's end, where it meets a tag on
  the same line: the spaces in `is an <code>x</code> quote` are kept when
  the text around goes on over several lines (they used to be trimmed with
  the indentation). A line break next to a tag is still nothing, as in
  JSX, and a `&nbsp;` at a line's edge is now kept. Text on one line is
  unchanged.

* SVG: an `<svg>` in an `hh` quote is SVG down to a `<foreignObject>`, its
  elements those of `Halogen.Svg.Elements` (in SVG's namespace), its
  attributes those of `Halogen.Svg.Attributes` by their SVG or function
  names, typed (`cx={{ x }}`, `d={{ commands }}`,
  `viewBox={{ (0, 0, w, h) }}`), its `class` an attribute. The `svg` and
  `svgs` quoters start in SVG. A hole right before the `/>` of a tag no
  longer takes the slash into its value (a `/>` in text or in a quoted
  value stays as written).

* In SVG, `fill` on an animation (`<animate>`, `<animateMotion>`,
  `<animateTransform>`, `<set>`) is the state it leaves (`SA.fillAnim`), not
  a colour; `fontStretch`, `fontVariant`, `path`, `markerUnits`,
  `maskUnits` and `maskContentUnits` are typed too, and an `<SVG>` written
  in upper case in HTML is the `svg` element. A `<circle>` may hold an
  animation (`SE.circleNode`), as may every other SVG element. In SVG, a
  numeric attribute written with a unit (`width="100%"`, `height="10em"`)
  is an attribute as written, not an error.

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
