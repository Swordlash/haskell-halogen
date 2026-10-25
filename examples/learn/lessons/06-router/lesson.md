title: Routing
layers: router
args: -package haskell-halogen-router

A page with several views gives each one an address. `haskell-halogen-router` does it in three parts: a type of routes, a codec between routes and addresses, and a router that watches the address.

## Routes and their codec

`Route` is an ordinary type with a constructor per view, deriving `Generic`. `route` is its codec: one value that both reads an address into a `Route` and writes a `Route` as an address.

- `RD.sum` takes a record of a codec for each constructor, under the constructor's name: `(#Home .== RD.noArgs) .+ (#About .== RD.path "about" RD.noArgs)`. It is a row-types record: `.==` makes a field and `.+` joins two records (both from `Data.Row`); a label such as `#Home` needs `OverloadedLabels`.
- `RD.noArgs` is a constructor with no arguments, and `RD.path "about"` puts a segment before it: `/about`.
- `RD.root` says the address starts with `/`.

`Halogen.Router.Duplex` and `Halogen.Router.Duplex.Generic`, which has `sum`, are both imported as `RD`: two modules can share a name when their exports do not clash.

## The router

The component gets a `Router.Router Route` as its input and keeps it in its state. `H.initialState` asks it where the page is, with `Router.current`. In `Initialize`, `H.subscribe` turns `Router.changes` into an `Arrived` action for every route the page goes to from then on.

`Router.link` makes an `<a>` whose `href` is the route's address, and hands a plain click on it to the component as `Go`. `Router.follow` goes there without loading a page, and `Router.changes` brings the new route back.

The program around your module makes the router, with `Router.newHashRouter Home route`. The tutorial has a router of its own, which reads the path (`/06-router/` is this lesson), and two routers reading the path would fight over it. A hash router keeps its route after the `#` instead (`#/about`). Before the next compile runs, the program calls `Router.stop`, so that the old router stops listening.

## Exercise

Add a route with a number in it, `Post Int`, at `/post/1`, `/post/2` and so on:

1. a constructor `Post Int` in `Route`;
2. its codec in the record: `.+ (#Post .== RD.path "post" (RD.int RD.segment))`. `RD.segment` reads a segment, and `RD.int` reads it as an `Int`. Without the codec, GHC says which constructor has none;
3. two links, `link (Post 1) "Post 1"` and `link (Post 2) "Post 2"`.

Compile and click them: the address ends in `#/post/1`, and the page says where you are. The browser's Back button then returns to the route before, through the lesson's own router, and the tutorial stays on this lesson.
