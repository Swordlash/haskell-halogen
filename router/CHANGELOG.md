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
  a link to another target or on a download is left to the browser).
  A link keeps the browser from following a plain click in the click's
  listener (`Halogen.HTML.Events.preventingDefault`), not in `follow`:
  `follow` runs when the tree's loop gets to it, which, for a link clicked
  while the loop was busy (right after a change that re-renders the page),
  was after the browser had loaded the link's page. Natively the address is kept in memory, so a page can be
  rendered for any route with `Halogen.Static`.
* `Halogen.Router.stop` takes a router's `popstate` listener off the window,
  after which the browser's back and forward no longer reach its `changes`
  (its own `navigate` and `redirect` still do). The listener otherwise
  stays for as long as the page does, which is what a program that is run
  again on the same page, as browser GHCi's `:main` runs it, needs to end
  with the run's tree.
* `Halogen.Router.linkProps` gives the properties that make an `<a>` a
  link to a route, the route's `href` and the click handler that prevents
  the default in its listener, for a link whose element is written some
  other way: in an `hh` quote, `<a {{ Router.linkProps router Followed
  route }}>`. `link` is `HH.a` with them after its own properties.
* A hash router (`newHashRouter`) writes whole addresses, the page's own
  path and query before the `#` (`/lesson/#/about`), rather than a bare
  `#/about`. A bare fragment resolves against the page's `<base href>`, not
  its address, so on a page with a base other than its own path (every page
  of a site under a `HALOGEN_BASE_PATH` but its root) a link to a route and
  a `navigate` went to the base's address instead: the page left, Back left
  it too, and a reload lost the route. `navigate` and `redirect` write the
  path the page is at as they go, which keeps a path another router changed
  meanwhile; `href`, a pure value, names the path the page was at when the
  router was made. `fragmentAddress` is how both are written. A hash router
  natively starts at `/`, so its `href`s now start with `/#`.
