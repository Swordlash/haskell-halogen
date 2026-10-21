# Changelog for `haskell-halogen-router`

## 0.1.0.0 -- unreleased

* First version. `Halogen.Router.Duplex` is a port of the core of
  `purescript-routing-duplex`: a `RouteDuplex` both parses an address into
  a route and prints a route as an address, built from `root`, `path`,
  `prefix`, `suffix`, `segment`, `param` (which takes the parameter it
  reads out of the address, so a route can read one name twice), `flag`,
  `int` (which refuses a number an `Int` cannot hold rather than wrap it),
  `string`, `boolean`, `as`, `optional`, `many`, `many1`, `rest` and `end`,
  with `lmap` and the `Applicative` instance for routes with several
  parts.
  Unlike the original, a route is read only when the whole path has been
  consumed, and the parser backtracks, so the order of alternatives
  matters only between two that both read an address.
* `Halogen.Router.Duplex.Generic.sum` derives the codec of a sum type with
  `GHC.Generics`, from a row-types record of one codec per constructor
  under the constructor's name (`#Home .== noArgs .+ #Post .== …`), the
  shape of PureScript's `sum`. A constructor without a codec, a codec of
  the wrong type and a label that names no constructor are type errors. On
  the JavaScript backend a record hashes its labels through the XXH3
  polyfill haskell-halogen-core links.
* `Halogen.Router` joins a codec to the browser's history: `newRouter` for
  paths under a base path (`pageBase` reads it from the page's
  `<base href>`), `newHashRouter` for the fragment, `current`, `navigate`
  and `redirect` (`pushState` and `replaceState`), `changes`, one emitter
  per router carrying its navigations and the browser's `popstate`, and
  `href`, `link` and `follow` for links a component follows without
  loading a page (a link's `href` and click handler are the router's,
  whatever the caller's properties say, and a click with a modifier key, on
  a link to another target or on a download is left to the browser). Natively the address is kept in memory, so a page can be
  rendered for any route with `Halogen.Static`.
