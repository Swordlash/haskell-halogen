title: Routing
layers: router
args: -package haskell-halogen-router

Routing gives each view its own URL. `haskell-halogen-router` uses three parts: a route type, a codec that converts between routes and URLs, and a router that tracks navigation.

## Routes and their codec

`Route` has one constructor per view and derives `Generic`. Its codec, `route`, both parses URLs into `Route` values and prints routes as URLs.

- `RD.sum` takes a row-types record with a codec for each constructor: `(#Home .== RD.noArgs) .+ (#About .== RD.path "about" RD.noArgs)`. From `Data.Row`, `.==` creates a field and `.+` combines records. Labels such as `#Home` require `OverloadedLabels`.
- `RD.noArgs` handles constructors without arguments. `RD.path "about"` adds the `/about` segment.
- `RD.root` requires a leading `/`.

Both `Halogen.Router.Duplex` and `Halogen.Router.Duplex.Generic` are imported as `RD`. They can share an alias because the names used here do not clash.

## The router

The component receives a `Router.Router Route` as input and keeps it in state. `initialState` reads the current route with `Router.current`. On initialization, `watchRoutes` subscribes to `Router.changes`, producing an `Arrived` action for each new route.

`Router.link` builds an anchor with the route's URL and converts a plain click into `Go`. `Router.follow` updates the address without loading another page. The resulting `Router.changes` event updates the component.

The lesson wrapper creates the router with `Router.newHashRouter Home route`. It uses the fragment, such as `#/about`, because the tutorial already uses the path `/06-router/` for its own routing. Cleanup calls `Router.stop` before the next run.

## Exercise

Add numbered posts at `/post/1`, `/post/2` and so on:

1. Add `Post Int` to `Route`.
2. Add `.+ (#Post .== RD.path "post" (RD.int RD.segment))` to the codec record. `RD.segment` reads a segment and `RD.int` converts it to `Int`. GHC reports constructors missing from the codec.
3. Add links with `link (Post 1) "Post 1"` and `link (Post 2) "Post 2"`.

Compile and follow the links. The address ends in `#/post/1` or `#/post/2`. Back returns to the previous lesson route without leaving this tutorial page.
