# Revision history for haskell-halogen-core

## Unreleased

- A program can watch what its component trees do. `Halogen.Trace.subscribe`
  takes a listener, which every tree of the program then tells, as it
  happens, of each component made (`Mounted`), initialized and finalized,
  each render and how long it took, each action handled, state change,
  query answered and output raised, and each subscription and fork. An
  event says which component it is about with a `Tag`: an id unique in the
  program, the parent's id, and a label, which is the name of the state's
  type (`Example.Counter.State`) unless `H.named "Counter"` gives the
  component another. A state change carries the new state as a `Dynamic`,
  and `Trace.restoreState` puts such a state back into its component, which
  renders it as a `put` would; `Trace.rootNode` finds the DOM node a
  component stands for on the page. With no listener the driver builds no
  event and keeps no registry of components, so a program that does not
  listen pays a read of a reference at each place it would report from.
  Only components made while someone listens can be restored or found:
  listen before mounting. `haskell-halogen-devtools` shows the trace in a
  panel over the page, and `hspec-halogen` prints its last events when a
  test fails.

- `H.mkComponent` and `H.component` ask for `Typeable` of the component's
  state, for the label and for the states a trace carries. A state declared
  with `data` or `newtype` has it already; a component whose state's type
  has a type variable in it (`H.Component q (Form a) o m` with a
  `State a`) needs `Typeable a` in its signature. A state that mentions the
  component's monad (one that keeps an `m` action, a component, or HTML of
  `m`'s) cannot reasonably ask for `Typeable m`: build such a component
  with the constructor instead, `H.Component H.ComponentInfo {label =
  "Widget", stateType = Nothing} (H.componentSpec initialState render &
  H.onAction handleAction)`, and the trace counts what it does without
  carrying its states. `Halogen.Portal`, `Halogen.Canvas`, material's lists
  and tabs, custom elements' wrappers and hooks components are built so.
  The action's type is not asked for, so a component that handles no
  action still compiles as it did. `Halogen.Component.Debounced`'s two
  functions ask for `Typeable` of the state too. Code that matches or
  builds the `Component` constructor gets its first field, the
  `ComponentInfo`, and `H.hoist` keeps it.

- `Halogen.IO.Driver.RenderSpec` has a field `rootNode`, the DOM node that
  stands for a component's rendering (for `Trace.rootNode`): a render spec
  of one's own whose output is not the DOM's gives `\_ -> pure Nothing`.

- A component is built from its handlers: `H.component initialState render
  [H.onAction handleAction, H.onInitialize start]`, with `H.onQuery`,
  `H.onFinalize` and `H.onReceive` for the rest, in any order. Each
  handler answers one of the five things a component is asked to do and
  leaves the others alone, so a component with nothing to handle is
  `H.component initialState render []`. Arriving, going and new input are
  handled by a `HalogenM` program, not by an action the component has to
  have a constructor for: `H.onInitialize (handleAction Initialize)` keeps
  the old shape where it is wanted. `H.componentSpec initialState render`
  is the same with the handlers added by `Data.Function`'s `&` and
  `mkComponent` at the end. `mkComponent`, `ComponentSpec`,
  `mkEval`, `defaultEval` and `EvalSpec` stay as they were, for an `eval`
  written as one function. The tutorial, the examples and the packages'
  own components use the new form. A module that imports `Halogen`
  unqualified and names something of its own `component` must now hide
  the new one.

- A component moved to another monad with `H.hoist` (every component of a
  tree hoisted from an application's `ReaderT`, say, or from
  `haskell-halogen-store`'s `StoreT`) no longer renders again after a
  `get`, a `gets` or any other state operation that leaves the state as it
  was. The hoisted program handed the state back as a new, unevaluated
  value, which the driver could not tell from a changed one; it is now
  handed back as the value it was.

- A subscription's action now runs nested in the synchronous effect that
  dispatched its event, as an element's own handler's does: a component
  that subscribes to a DOM listener (`Halogen.Query.Event.eventListener`,
  the emitters of `Halogen.Query.Event.Window`, or any emitter fed from a
  `mkEventListener` callback) and dispatches an event to it from
  `H.liftEffect` (`element.click()`, `dispatchEvent`) has the action handled
  before the dispatch returns, so the effect sees what the action did, and
  `preventDefault` or `stopPropagation` in the action, in `H.liftEffect`,
  still takes effect: a checkbox clicked that way stays unchecked. Before,
  the action was queued until the effect's action had ended, after the
  browser had acted on the event. This holds on both browser backends. Only
  a notification the page makes, from a callback it calls while it
  dispatches, is nested so: a notification from a `liftIO` body, a timer
  (`Window.interval`) or any other thread of the program is still queued
  behind the work under way, as before, since that thread is not under the
  effect and its action would run beside it rather than inside it.
  Natively nothing changes: an event an effect dispatches into the
  in-memory DOM was already handled on the loop's own thread, nested.

- On WebAssembly an event's action now runs inside the browser's dispatch
  of the event also when the tree's loop was busy with work the scheduler
  had preempted between two of the page's tasks, such as a long render the
  event arrived in the middle of: the event's listener lets that work
  finish, inside the dispatch, and then runs the action there, so
  `preventDefault`, reading a form's value or focusing from `H.liftEffect`
  in the action is still in time. The page waits for that work inside the
  dispatch, and work that mostly computes, rather than changes the page,
  can take a few times as long to finish this way, since each turn hands it
  only a short slice. An event a synchronous effect dispatches (an effect
  that clicks an element) is handled nested in the effect, as before. An
  event dispatched from the loop's own work outside an effect (a render
  that focuses an element), and every loop busy outside an effect on the
  JavaScript backend, still has its action queued until the dispatch is
  over; `HE.preventingDefault` and `HE.stoppingPropagation`
  remain the way to prevent the default or stop propagation in every case.

- `Halogen.Query.Event.Window` has emitters for what happens to the page
  as a whole, for a component to subscribe to rather than render a handler
  for: `keyDown` and `keyUp` anywhere on the page, `mouseMove`,
  `mouseDown` and `mouseUp`, `resize` (the viewport's new width and
  height), `visibility` (the document's `VisibilityState` each time the tab
  is hidden or shown), `online` (`True` and `False` as the browser goes
  online and offline), and `windowEvent` and `documentEvent` for any other
  event type, with a function that picks the events wanted. There are two
  clocks as well: `interval`, a tick every so many milliseconds from a
  thread of its own, which keeps to its start rather than drifting and
  skips ticks a slow handler had no time for, and `animationFrame`, the
  time of each frame the browser draws. Every one of them is an
  `Emitter IO`, which `H.subscribe` and the hooks' `subscribe` take as it
  is: those that listen to the page are made in the component's monad,
  best with `H.liftEffect` (`keys <- H.liftEffect Window.keyDown`), so
  that the subscription is in place once the component's `Initialize`
  has run its synchronous part, as `lift` would leave it until after a
  wait; the clocks are plain values
  (`H.subscribe (Tick <$ Window.interval 1000)`). Each subscription is a
  listener, frame request or thread of its own that unsubscribing takes
  away, so a component that goes away leaves nothing behind. Natively the
  page's emitters listen on the in-memory window, where nothing dispatches
  the page's events, and `animationFrame` never fires; `interval` ticks on
  every backend.
- `Web.UIEvent.KeyboardEvent` reads an event's `key` (what the key means,
  `"a"` or `"Enter"`) and `code` (which physical key it is, `"KeyA"`),
  and `Web.UIEvent.MouseEvent` its `clientX` and `clientY`, where in the
  viewport the pointer was. Both modules now have export lists, which
  export what they did before and these; code that imports either of them
  whole and has a `key`, `code`, `clientX` or `clientY` of its own may now
  have to hide the new names or import the module qualified.
- `Web.HTML.HTMLDocument.visibilityState` reads whether the page can be
  seen, a `VisibilityState` (`Visible` or `Hidden`, from the new
  `Web.HTML.HTMLDocument.VisibilityState`). `Web.HTML.Event.EventTypes`
  has `resize` and `visibilitychange`.

- A `ref` now follows its prop on an element a render keeps and patches.
  When the same element went from `HP.ref a` to `HP.ref b`, `getRef b`
  answered `Nothing` and `getRef a` went on answering the element; when
  the `ref` prop was dropped from an element that stayed, its label went
  on holding the element until the element itself was removed. A patch
  now records the element under the new label and clears it from the old
  one, and clears the label of a prop that goes. Adding a `ref` prop to an
  element that stays already recorded it, and still does. Nothing in the
  API changes; a function given to `Halogen.HTML.Core.ref` is now also
  called on every patch of its element, with `Removed el` from the old
  function and then `Created el` from the new one, which the driver turns
  into the same entry when the label has not changed.
- A `ref` on a keyed element now follows the element that replaces it.
  When a keyed child carrying `HP.ref label` was replaced by one under a
  new key with the same label, the component was left with nothing under
  the label, and every later `getRef`/`getHTMLElementRef` answered
  `Nothing`: the reconciler builds children under new keys before it
  removes those whose keys went, so the old element's removal came last
  and cleared the entry the new one had just written. A removal now clears
  the label only if it still holds the element that goes. This changes
  three public types. `Halogen.Query.Input.RefUpdate` carries the
  `ElemRef Element` (`Created el` or `Removed el`, from
  `Halogen.VDom.DOM.Prop`) instead of a `Maybe Element`, and
  `Halogen.HTML.Core.ref` takes a function of it, so the removed element
  is passed on rather than dropped. `MonadDOM` has a new method,
  `sameNode :: DomNode m -> DomNode m -> m Bool`, whether two handles are
  one node (`===` in a browser, the node's identity in `MemDOM`); a
  `ReaderT` or `IdentityT` instance gets it lifted, and a backend of your
  own has to answer it. And `Halogen.IO.Driver.RenderSpec` has a new field,
  `sameElement :: Element -> Element -> m Bool`, which
  `Halogen.VDom.Driver`'s render spec answers with `sameNode`; a render
  spec of your own that makes no elements (a test's, say) can answer
  `\_ _ -> pure False`, since it is never asked.
- Native HTML serialisation (`renderToText` and `Halogen.Static`) builds
  markup with a text builder, converting to strict `Text` once at the end,
  instead of copying each subtree at every ancestor. Escaping copies runs
  of ordinary text together rather than allocating one `Text` per character.
  The result type and HTML output are unchanged. A native `render-to-text`
  Cabal benchmark measures render time and allocation; see `bench/README.md`.
- A page served over plain HTTP (not a secure context, so its
  `crypto.randomUUID` is missing) no longer fails with
  `TypeError: globalThis.crypto.randomUUID is not a function` when it asks
  `MonadUUID` for a UUID, as a component does when it starts: the browser
  backends make an RFC 4122 version 4 UUID from `crypto.getRandomValues`
  there, without changing the page's `crypto`.
- A component's `Initialize` that waits (in `lift`/`liftIO`, or for a query
  to answer) now holds up only itself. A component counts as initialized
  once its `Initialize` has run its synchronous part, up to its first wait
  or its end: then the outputs it and its children raised meanwhile are
  delivered, its parent's `Initialize` starts, and the rest of the
  `Initialize` goes on while the tree does. The children's synchronous
  initialization still comes before the parent's. The program whose
  `modify` rendered new components goes on once they have run their
  `Initialize` up to its first wait, and `runUI` and `hydrateUI` return
  then too. This differs from purescript-halogen, which this driver
  followed until now: there, a component's initializer waits for its
  children's to end, and the program that rendered them and the mount wait
  for all of them. So one island of a page whose `Initialize` loaded
  something for seconds kept every ancestor made in the same render from
  answering queries and delivering outputs, held the action whose `modify`
  showed the page, and, on the first page, kept the root's own
  `Initialize` (and the router subscription it made) from running: a link
  clicked meanwhile changed the address and nothing else. Code that relied
  on a component having loaded once its parent's `Initialize` runs, or
  once the `modify` that created it or `runUI` returns, should ask it
  instead, with a query that waits until it has loaded (keep what loads in
  a fork or in its state, and have the query wait for it). A query to a
  component whose `Initialize` waits is answered as usual meanwhile.
  `Halogen.Static` still renders a page once every `Initialize`, and those
  of the components they render, has ended; nothing else waits for that.
- `Halogen.HTML.Events.preventingDefault` and `stoppingPropagation` wrap an
  event handler so that, whenever it makes an action, the event's default
  is prevented, or its propagation stopped, in the event's listener itself,
  while the browser is still dispatching it:
  `HE.preventingDefault (HE.onSubmit (const Save))`. A handler that makes
  no action (a key handler that ignores the key) leaves the event alone.
  Calling `preventDefault` from the action instead, in `H.liftEffect`, is
  only in time when the action runs inside the dispatch, which it does not
  always (see the entry above): when the loop is busy and its work cannot
  finish inside the dispatch, the action is queued and runs after the
  browser has followed the link, submitted the form or typed the key. Use
  them for a form's submit, a keyboard shortcut and a link of your own;
  `Halogen.Router`'s links do. `H.liftEffect` itself is unchanged.
- `Halogen.VDom.DOM.Prop.Handler` carries `HandlerOptions` (whether to
  prevent the default, whether to stop propagation; `handlerOptions` for
  neither) between the event type and the function, so code that builds or
  matches a `Handler` itself changes; `Halogen.HTML.Core.handler` is as it
  was, and `Halogen.HTML.Core.withHandlerOptions` changes a handler's
  options. `buildProp` asks for `MonadIO`, which every DOM monad has.
- New module `Web.Storage.IndexedDB`: a key-value store over the browser's
  IndexedDB, for what is too big, too binary or too much for
  `Web.Storage.Storage` (an offline cache, a document and its attachments).
  A `Database` names a database, its version and its stores (`StoreName`);
  stores are made when the database is created or opened at a later version,
  so adding one is a version bump. The class `MonadIndexedDB` has `get`,
  `put`, `delete`, `keys`, `getAll` and `clear` on one store, with keys of
  `Text` and values of `ByteString` kept as a `Uint8Array` (never base64),
  `writeMany`, which makes writes and deletions in several stores in one
  transaction, all or none of them, and `deleteDatabase`. `getItem`,
  `setItem` and `getAllItems` read and write values through
  `StorageSerialize`, as `Web.Storage.Storage` does, and `itemWrite` and
  `itemDeletion` make `writeMany`'s writes. Every call waits for the
  browser, so call it from a component's `lift`, not from `H.liftEffect`.
  What fails is thrown as an `IndexedDBError`, saying which database and why
  (`Unavailable`, `UpgradeBlocked` by another tab, `VersionTooLow`,
  `StoreNotFound`, `QuotaExceeded`, `NotBytes`, or the browser's own error).
  `BrowserDOM` has an instance on both browser backends, which keeps one
  open connection per database and closes it when another tab upgrades the
  database (natively it fails with `Unavailable`); `MemDOM` has one that
  keeps every database in memory, shared by the whole process as the
  browser's are by an origin, and behaves the same; `ReaderT` and
  `IdentityT` lift it, and so can any transformer, with an empty instance.
  The browser instance waits for each operation through
  haskell-halogen-jsbits' `Halogen.JSBits.Promise.await`, which hands back
  a rejection as a value, and turns that rejection's name and message into
  the `IndexedDBError`, so the same failure reads the same on WebAssembly
  and on the JavaScript backend.

- `Halogen.IO.Util.keepForNextRun` and `endPreviousRun`, for a program that
  is run again on the same page, as browser GHCi's `:main` runs it: the page
  and the runtime stay from one run to the next, and with them the last
  run's tree, still mounted, its subscriptions, forks and timers going. A
  `main` that starts with `endPreviousRun` and hands `keepForNextRun` how
  to end what it starts (the socket's `dispose`, and whatever else outlives
  the tree, such as a router's listener) ends the last run properly before
  starting its own: the tree's finalizers run, what it started ends and its
  root leaves the page. With nothing kept, `endPreviousRun` does nothing,
  so a program run once, anywhere else, is unaffected but for the one
  action it keeps.
- On the JavaScript backend, hashing a `Text`, a `ByteString` or a
  `ShortByteString` with hashable (a `HashMap Text`, a row-types `Rec`)
  no longer fails with `h$hs_XXH3_64bits_withSeed_offset is not defined`:
  `jsbits/polyfills.js` provides the XXH3 functions hashable calls in C, as
  a port of XXH3-64 that gives the same values as the C (see
  `POLYFILLS.md`). Every program linking core gets them unless the
  `js-polyfills` flag is off.
- `Halogen.Canvas.Properties.alpha` (`Alpha`): how opaque an element is,
  from 0 to 1, multiplying the alpha of its own colours and passing on to
  everything inside it, so a sprite, a label or a whole group can fade in
  and out, which `FillStyle` and `StrokeStyle` could not do. An element
  without it is opaque, as before.
- New module `Halogen.Static` (native only): `renderToHtml` renders a
  component to HTML with no browser, for a page a server sends or a build
  writes out ahead of its program. It is full server-side rendering: the
  component is mounted with the browser's driver into an in-memory document,
  its children and slots are built and its `Initialize` (and its
  children's) run to the end, `liftIO` included, before the HTML is taken;
  then the tree is disposed of, so its subscriptions end and its finalizers
  run. `renderToHtmlWith` does the same for a component in an application
  monad over `MemDOM`, given how to run it, and `fillMountPoint` puts the
  HTML into a page's empty `<div id="…"></div>`. A component that does
  browser things when it starts asks `Halogen.JSBits.inBrowser` first.
  `Halogen.VDom.Driver` exports `renderSpec` (and `RenderState`), which
  `Halogen.IO.Driver.runUI` takes to render into a document of one's own.
- `Halogen.VDom.DOM.Monad.Native.renderToText` writes what the browser
  would have to parse to build the same tree, and its output changes
  accordingly. Properties are written as the attributes they reflect
  (`className` as `class`, `htmlFor` as `for`, `tabIndex` as `tabindex`,
  boolean ones such as `checked`, `disabled`, `selected` and `hidden` as an
  attribute's presence, numbers as JavaScript's `Number::toString` spells
  them: `1`, `0.1`, `1e-7`, `1e+21`), where they used to be left out; a
  property and an attribute of the same name are written once, with the
  property's value. A `textarea`'s `value` is written as its text and a
  `select`'s marks the option it picks (an option without a `value` is
  matched by its text with ASCII white space collapsed, a non-breaking
  space kept, as in a browser). A property HTML has no attribute for
  (`selectedIndex`, a custom element's) is still left out. Two adjacent
  text nodes are separated by an empty comment, `<!---->`, so that a
  client adopting the page can tell where one ends, except in `script`,
  `style` and the other raw-text elements, where the comment would be
  text. No `xmlns` is made up for an `svg` or a `math` element, whose tag
  is what gives it its namespace, and an XLink attribute is written
  `xlink:href` where it was written with its namespace's URI. `>` and
  non-breaking spaces are escaped in attributes, and non-breaking spaces
  in text, and a carriage return in either is written `&#13;`, which the
  parser would otherwise turn into a line feed; the text of `script`,
  `style` and the other raw-text elements is no longer escaped, a newline
  that starts a `pre` or a `textarea` is kept, and `param`, `basefont`,
  `bgsound`, `frame` and `keygen` are written without an end tag, as the
  other void elements are.
  What no markup can parse back into is refused with an error naming it,
  so that a prerender fails rather than ship another page: a `plaintext`
  element; a raw-text element (`script` and `style` included) whose text
  holds its own end tag, which a script that needs one in a string writes
  `<\/script` itself; U+0000 in text or an attribute; an element in a
  namespace other than HTML's, SVG's and MathML's; and an element the
  parser would not make where it is (an SVG element outside an `svg`, a
  MathML one outside a `math`, HTML inside either other than at an
  integration point such as `foreignObject`). The haddock lists what HTML
  can only approximate until the program has started (a `select` whose
  value matches no option, a `tr` straight under a `table`), and says
  where the line is drawn: the markup is written as the browser's fragment
  serialisation writes it, and a tree the parser would build differently,
  an empty text node or a `p` holding a `div` for instance, comes back
  different. Snapshot tests over `renderToText` may need
  their expected HTML updated.
- In the in-memory DOM, setting an element's text content replaces its
  children with one text node (none for empty text), as in a browser,
  where it used to change nothing a serialisation could see.
- `Halogen.VDom.Driver.hydrateUI`, with `runUI`'s signature, mounts a
  component into a container that already holds its HTML, such as a page
  `Halogen.Static` rendered, and adopts the nodes it finds there instead of
  building its own: what the page shows stays on the page, and the program
  attaches its listeners and sets the properties HTML cannot carry. Child
  components adopt what is in their slots. Nothing a page holds is an
  error: a node of another kind or tag is replaced, a missing one built, a
  surplus one removed, a differing text set, a slot the page has nothing in
  filled, and the comments `renderToText` puts between adjacent texts are
  removed; attributes the page has and the component does not render are
  left as they are. The component starts from its `initialState`, so a
  page rendered from a state its `Initialize` changed is first rendered
  back to that state and then forward by the client's own initializer.
  `runUI` is unchanged.
- `Halogen.Static.embed ident json` writes a
  `<script type="application/json">` with that id holding the JSON text,
  every `<` in it written `\u003c` so that nothing in the text can end the
  script, and `Halogen.IO.Util.readEmbedded ident` reads the text of the
  element with that id back in the browser. With them a server can put the
  state it rendered a page in beside the page, for the component's
  `initialState`, which runs in its monad, to start from instead of taking
  the page back to its own first state. The format of the text is the
  application's.
- `Halogen.VDom.DOM.hydrateVDom` is `buildVDom` adopting a tree that is
  already there, from a node to start at; the machine it returns patches
  like any other. `VDomSpec` has a new field, `hydrateWidget`, its widgets'
  counterpart: a spec built by hand adds
  `hydrateWidget = \spec _ -> spec.buildWidget spec` for widgets that are
  never adopted, or `Halogen.VDom.Thunk.hydrateThunk` for thunks.
- `MonadDOM` can read the tree it builds, which adopting a prerendered
  page takes: `firstChild`, `nodeKind` (an element with its namespace and
  local name, a text with its text, a comment, or something else, as the
  new `DomNodeKind`) and `nodeToElement` are new methods, and
  `nextSibling` moved to `MonadDOM` from `MonadBrowserDOM`. All four have
  lifted defaults, like the other methods, so an instance for a
  transformer needs nothing new; a backend of one's own defines them, and
  one that is never hydrated, as Pixi's is not, can answer that a node has
  no children and is not an element, which makes hydrating build afresh.
  Code that called `nextSibling` under a `MonadBrowserDOM` constraint is
  unchanged, since `MonadDOM` is its superclass; an instance that defined
  it in `MonadBrowserDOM` defines it in `MonadDOM` instead, and an import
  list naming it under `MonadBrowserDOM` names it under `MonadDOM`.

- `Interaction` has a `fit` field: the size of a world rectangle that the
  camera at zoom 1 shows whole, centred on its focus, on whatever surface
  the renderer has, so a board of a fixed size fills a canvas the
  application never measures, and `zoom` and `zoomRange` count from "all of
  it in view". `Nothing`, the default, keeps zoom 1 at one world unit per
  pixel. `fitScale` is the factor a renderer multiplies the zoom by. Code
  that builds an `Interaction` with all its fields adds `fit = Nothing`.
- **Fix:** a program's worker (the thread running a `lift`/`liftIO`) no
  longer runs the tree's loop. A `liftIO` that notified a subscribed
  emitter, or called the socket's `query` or `dispose`, ran the handler on
  the worker itself when the loop was idle; if that handler killed the fork
  (or removed the component) the kill landed in the handler, which died with
  `AsyncCancelled` half way. Such work is now queued for the loop, as a
  callback from another thread is.
- `runUI` (both drivers), `MonadUI` and `Halogen.Portal` ask only for
  `MonadBrowserDOM m` and `MonadUnliftIO m`: the `MonadFork`, `MonadKill`,
  `MonadParallel`, `MonadMask` and `MonadUUID` constraints were left over
  from the driver before the loop and nothing used them. An application
  monad no longer has to derive them (the classes and the `BrowserDOM` and
  `MemDOM` instances remain). `Halogen.Subscription.create` tells
  subscribers apart by a counter instead of a UUID, so it needs only
  `MonadIO`. `mapAction`, `hoist`, `runDelayedStateT` and
  `Halogen.HTML.Layout.end` lost constraints they did not use.
- Every numeric SVG attribute (`x`, `width`, `r`, `strokeWidth`, `viewBox`,
  `transform`'s numbers, …) is written with `svgNumber`, as path commands
  and `points` already were: `1` where it was `1.0`. Code comparing the
  rendered attribute text sees the change.
- The browser `MonadDOM` instances are one module for both browser
  backends, `Halogen.VDom.DOM.Monad.JSFFI`, its bindings declared once
  through `browserJS`; `Halogen.VDom.DOM.Monad.JS` and `.WASM` are gone.
  `Halogen.JSBits.mkSyncCallback` is now what a DOM listener is on both
  backends (a sync export on wasm, which runs until it waits).
- Natively `Data.Foreign.readProp` finds nothing instead of panicking.
- `Halogen.join` on a fork that was killed cancels the joining program
  (purescript-halogen throws an error there); the haddock says so, and so
  does this entry, since a program cannot `catch` it.
- The warning set grew by `-Wredundant-constraints` and `-Wunused-packages`,
  in this and every other package of the repository; `async` and
  `unordered-containers` were dependencies nothing used.

SVG: `SE.animateTransform` and `SE.set` (their rows `SVGanimateTransform`
and `SVGset`, so `SA.fillAnim` types on them); `SE.mask` takes its own row,
`SVGmask`, whose field `maskContentsUnits` is now spelled
`maskContentUnits`, as `SA.maskContentUnits` wants; `SVGuse` and `SVGimage`
take `xlinkHref`. Every SVG element takes focus, touch and `onTransitionEnd`
handlers, `<svg>` and `<image>` `onLoad` and `onError`; `<path>`,
`<circle>`, `<ellipse>`, `<line>` and `<rect>` take `pathLength`.
`<marker>` takes `viewBox` and `preserveAspectRatio` (and lost the
`strokeWidth` it had twice, which made every typed attribute on it fail);
`<svg>`, `<polygon>`, `<polyline>`, `<image>` and `<foreignObject>` take
`transform`; the animations take `href` and `xlinkHref`; `<mpath>` takes the
global attributes and `href`. The basic shapes (`<rect>`, `<circle>`,
`<ellipse>`, `<line>`, `<polyline>`, `<polygon>`, `<path>`) take every
presentation attribute, as `<g>` does. `SA.xlinkHref` puts the attribute in
the XLink namespace (`http://www.w3.org/1999/xlink`), not one named `xlink`.

The browser bindings (`Web.Event.Event`, `Web.HTML.Window`,
`Web.HTML.HTMLDocument`, `Control.Monad.UUID`) are declared once, without CPP,
through `Halogen.JSBits.browserJS`; core depends on `haskell-halogen-jsbits` on
every architecture. Natively `Foreign` is `Halogen.JSBits.JSVal` and an
`EventListener` holds a `Halogen.JSBits.Callback`. Two native stubs changed:
`currentTarget` returns `Nothing` and `innerWidth`/`innerHeight` return 0,
where they panicked.

`onPointerDown`, `onPointerMove`, `onPointerUp` and the other pointer event
handlers, and pointer and drag and drop events on SVG elements, which only
accepted mouse and keyboard ones.

`currentTarget` now consistently returns the node running the handler on both
browser backends, including when an event bubbles from a descendant. Legacy DOM
tracing instrumentation and its Cabal flag have been removed.

- `Halogen.Canvas.Properties.cacheAsTexture` (`CacheAsTexture`): a group
  drawn once into a texture, at a given resolution, and shown as that
  texture until something inside it changes. Backends without such a thing
  may ignore it.
- SVG path commands (`Halogen.Svg.Attributes`, and so canvas paths) and
  `points` write their numbers with `svgNumber`, not `show`: to a thousandth,
  with no exponent and no trailing zeros (`M1, 2.5`, where it was
  `M1.0, 2.5`). `show` on a `Double` is slow, slowest in WebAssembly, and a
  scene of paths can hold tens of thousands of numbers: in a WebAssembly
  game, the first render of a board with fog of war (every hex's cloud a
  path) went from about 10 s to under 5 under an 8× slowed CPU. NaN, the
  infinities and numbers from 1e15 up are still written by `show`.
- `HitArea` has `PolygonHit`, a polygon given by its corners: a tile the
  shape of a hex is hit to its edges, where a circle left gaps between tiles.
- Canvas scenes have `Halogen.Canvas.Elements.memoized` and `lazy`, as HTML
  has: while the value a part of a scene is rendered from stays equal, a
  render skips building and diffing the part and updating its props. A
  scene's widget type is now that thunk (`Thunk (CanvasNode event) i`)
  instead of `Void`; code that matched on `unCanvasNode`'s type names it.
- Fix: mapping the actions of a memoized part (`fmap` on HTML or a canvas
  node holding it, or `hoist`) is part of the part's identity. A part whose
  input stayed equal kept the handlers built under the old mapping, and
  went on sending the old actions. Mappings are compared by reference, so a
  memoized part mapped by a function built anew in each render is built
  again in each render. **Breaking:** `Thunk` has a field for the mappings;
  build one with `Halogen.VDom.Thunk.mkThunk`, and map it with `mapThunk`,
  or `mapThunkBy` to name the mapping by something stabler than the
  function.
- **Breaking: `lift` and `liftIO` in `HalogenM` now suspend the program.** A
  tree's components run on one loop per tree
  (`Halogen.IO.Driver.Runtime`), as purescript-halogen's run on `Aff`: the
  synchronous parts of a program run there one piece at a time, and `lift`
  / `liftIO` is where a program waits, as `liftAff` is. The action runs on
  a worker thread while the rest of the tree goes on, and the program
  resumes through the loop afterwards. Anything that must happen at once
  (`preventDefault`, `stopPropagation`, focusing an element, reading the
  DOM as it is now, `localStorage`, handing a scene to a canvas in order)
  has to move to the new `Halogen.liftEffect` (hooks: `Hooks.liftEffect`),
  which runs synchronously on the loop and must not wait. Code that used
  `liftIO` for these still compiles, but now runs after the event handler
  has returned to the browser, too late to cancel anything.
- A program stops the moment it is cancelled: killed (by `kill`, its own
  fork's included), or its component removed, also by the program's own
  synchronous work. Nothing after that point runs, not even an error
  handler. A cancelled program's `lift` worker is killed.
- Parallel branches (`parallel`/`sequential` in `HalogenM`) are cancelled
  with the program they belong to, each with its own worker; the first
  branch to fail cancels the others, and one not started yet never starts.
- An event a component dispatches from `liftEffect` (`element.click()`,
  `dispatchEvent`) is handled before the dispatch returns, as in the
  browser, so its handler can still cancel it or stop its propagation.
  On the wasm backend DOM event listeners are sync exports (`"wrapper
  sync"`), which is what re-entering Haskell from a synchronous JavaScript
  call needs; a listener runs until it ends or waits, and the rest of it
  runs after it has returned, as on the JavaScript backend.
- A render pass that fails commits nothing and takes back the children it
  made, whatever fails before its commit. In the DOM backend, what a
  failed patch of a component left is taken off the page at once, an empty
  text node holding its place (which the parent moves and removes as it
  would the component's root), and the next render builds the HTML afresh
  there, instead of diffing against a record the failed patch made untrue.
- `HalogenM` has `MonadThrow` and `MonadCatch` instances; a failure after a
  suspension reaches the handler too.
- `runUI` returns once the tree's components are initialized, and a state
  change waits for the initializers of the children its render created,
  as in purescript-halogen.

- On the JavaScript backend, core defines the few functions other libraries
  import that GHC's JavaScript runtime lacks: `splitmix_init` from splitmix,
  and `readlink`, `geteuid`, `getpwuid_r` and `sysconf` from unix (hspec
  reaches all of them, through QuickCheck and directory), and `realloc`. A
  program that links core, a test suite included, no longer needs its own
  copies. They work in a page as well as under Node, and each is defined only
  if nothing else defines it, so a library that later ships its own keeps it.
  The `js-polyfills` flag (on by default) leaves them out when turned off.
  POLYFILLS.md in the repository says where each belongs upstream.
- A property that stops being rendered is cleared from the element. It used to
  be deleted, which does nothing to a DOM property: those are accessors on the
  element's prototype, not properties of the element itself, so a `title` or
  `disabled` rendered once stayed on the element after its prop was gone. It is
  now set back as purescript-halogen-vdom does: to `""` if it holds a string,
  to `1` for `rowSpan` and `colSpan`, and to `undefined` otherwise.
- `Halogen.newRefLabel` makes a ref label from a name and a fresh UUID, for a
  component to generate in `initialState` and keep in its state. A fixed label
  can collide: refs are looked up in the component that rendered them, and a
  component that renders HTML handed to it by its parent (a tab bar, a list)
  registers that HTML's refs among its own. `Halogen.Canvas` uses it.
- A component's refs are recorded as soon as their elements are created or
  removed, rather than queued with its actions. Queued handlers are forked,
  and a forked thread starts only when the scheduler gets to it, while
  initialisers run straight away, so an initialiser that looked up one of its
  own refs could run first and find nothing. Material's components look up
  their root element that way, and failed with "no HTMLElement found" when
  mounted after another component had just been disposed. purescript-halogen
  gets this order from Aff's `fork`, which starts a fiber at once.
- A patch writes a property only when its rendered value has changed, as
  purescript-halogen-vdom does. Old and new values were compared by pointer, and
  a render builds every value afresh, so each patch wrote every property back
  and undid whatever the page had changed in between: a JavaScript widget's
  classes (Material's notched outline lost its notch this way), or a checkbox
  the user had ticked while its rendered `checked` stayed the same. `value` is
  still compared against the element on every patch, so an input whose
  component turns an edit down gets its value put back. `PropScalar` and
  `propScalar`, the comparison both use, moved from
  `Halogen.VDom.DOM.Monad.Native` to `Halogen.VDom.DOM.Monad.Class`; the native
  module still exports `PropScalar`.
- `Halogen.HTML.Properties.hidden` sets the `hidden` property, as it does in
  purescript-halogen.
- The browser's own storage. `MonadBrowserDOM` gains `readStorageItem`,
  `writeStorageItem`, `removeStorageItem` and `storageItemKeys`, which read and
  write one key of a store at a time as text, and `Web.Storage.Storage` says
  what that text is: the base64 of whatever `Web.Storage.Serialize` turned a
  value into, kept under a key prefixed to say it is this program's. That class
  writes as JSON by default, so a type with `ToJSON` and `FromJSON` needs only
  an empty instance. One entry per key rather than one object holding every
  key, because a store is shared by every tab open on the page and an object
  would have to be read, changed and written back -- which loses whatever
  another tab wrote in between. The in-memory DOM has the two stores as well,
  kept in a global `IORef`, so what a page persists can be tested without a
  browser.
- `Web.HTML.Window` for the viewport size and the URL's fragment, and
  `Web.HTML.Cookie` for cookies one at a time — the parsing and rendering of
  the cookie string are pure, and percent-encode what a cookie cannot carry.
  `locationHash` together with a `hashchange` listener is enough to route a
  single-page app served from static files, back button included.
- `Halogen.Subscription.lowerEmitter` runs an emitter's registration in `IO`,
  which is what the driver subscribes in. `Halogen.Query.Event.eventListener`
  builds an emitter in the component's monad, so until now the two could not
  be put together.
- `Web.Event.Event` gains `preventDefault`, `stopPropagation` and
  `stopImmediatePropagation`, and every event newtype gains `toEvent`. Off the
  browser backends the three are no-ops rather than a `panic`, so a handler
  written for the browser can still be run against the in-memory DOM.
- JavaScript event callbacks start synchronously, so cancellation takes effect
  during dispatch. A callback that blocks continues asynchronously.
- **Fix.** A forked program is registered as running for as long as it runs.
  The bookkeeping that strikes a fork off the register was being run *before*
  the program instead of after it, which left `kill` and `join` with nothing to
  find — both were silently no-ops — and let a component's forks carry on after
  the component was finalized.

## 0.10.0 - 2026-09-14

- **Breaking.** `MonadDOM` no longer has an instance at `IO`. Each backend is
  its own newtype over `IO` — `BrowserDOM` for the browser, `MemDOM` for the
  in-memory document, `PixiDOM` in `haskell-halogen-pixi` — so more than one
  can exist in a single build, and a backend can say what its tree is made of.
  Applications that ran the driver directly should wrap the call:
  `runBrowserDOM HA.awaitBody >>= runUI component ()`.
- **Breaking.** The node types are associated type families on `MonadDOM`
  (`DomNode`, `DomElement`, `DomDocument`, `DomEventListener`,
  `DomEventTarget`), which is what lets a non-browser tree be a `MonadDOM` at
  all. The browser-specific operations — document splicing and the window
  globals — moved to `MonadBrowserDOM`, which carries the equalities back to
  the concrete `Node`, `Element` and friends as superclasses. Named attributes
  and properties are `MonadAttributes`, which only an HTML backend implements.
- **Breaking.** `MonadDOM` has `PrimMonad` as a superclass instead of its own
  mutable-cell operations; a `MutVar (PrimState m)` replaces the previous
  associated `Ref`. `log` was dropped.
- **Breaking.** The component monad is the DOM monad. `MonadDOM`,
  `MonadAttributes` and `MonadBrowserDOM` lift through transformers — the
  methods have defaults, and instances for `ReaderT` and `IdentityT` come with
  them — so an application monad is a stack over a backend and derives them:
  @newtype AppM a = AppM (ReaderT Config BrowserDOM a) deriving newtype
  (..., MonadBrowserDOM)@. `StateT` and friends are deliberately absent: a
  listener is called back by the DOM, so `mkEventListener` has to run the
  transformer, and a state update made inside a callback has nowhere to go.
  `runUI` therefore takes no natural transformations, and `runUIWith` is gone.
  `VDomSpec` lost its `dom` parameter and its `runDom` field; `buildProp`
  takes only the handler and the element.
- Add `Halogen.Canvas.Types`, `.Core`, `.Elements` and `.Properties`: a
  declarative scene language in the shape of `Halogen.HTML`, backend-neutral,
  and reconciled by `Halogen.VDom.DOM.buildVDom` like any other `VDom`. Shapes
  include an SVG `Path`, sharing the path commands in `Halogen.Svg.Attributes`.
  The cursor is an enumeration of the CSS keywords rather than free text: a
  misspelled keyword is silently ignored by the browser, which is the worst
  way to find out.
- Add `Clay.Extra.Pointer` with `touch-action`, which Clay does not cover.
  `Halogen.Canvas` styles its host element with Clay rather than a CSS string.
- Add `Halogen.Canvas`: a renderer-agnostic component that owns a canvas DOM
  node and delegates mounting, updating and teardown to a `Renderer` record.
  `haskell-halogen-pixi` implements that interface for PixiJS v8.
- Fix duplicate keys in `Halogen.VDom.Utils`. Two props with the same key, or
  two keyed children under the same key, each applied their effect while only
  one was recorded — and the build path recorded the first while the patch
  path recorded the last, so the second patch compared against the wrong one
  and settled on the losing value. Shadowed entries are now dropped before any
  effect runs, so the last one wins throughout. This affects HTML as much as
  the canvas.
- Re-export `~` from `HPrelude` via `Data.Type.Equality`.
- Move into the `haskell-halogen` monorepo alongside `haskell-halogen-material`
  and `haskell-halogen-pixi`. The library now lives in `core/`; the example app
  moved to `examples/vanilla`.

## 0.9.0 - 2025-05-30

- Update `clay`
- Insert `base` and `clay` version bounds
- Release version

## 0.8.0 - 2025-02-04

- Add `Halogen.Portal` for teleporting children across DOM elements (i.e. to body)
- Revert adding input to `Slot` type
- Add Java Swing - inspired CSS layout managers 

## 0.7.0 -- 2025-01-18

Remove dependency on `clock` which doesn't build on darwin

## 0.6.1 -- 2025-01-16

Fix `foreignToBool`

## 0.6.0 -- 2025-01-16

- Add `Halogen.Svg` modules
- Use `RequiredTypeArguments` for slot proxies

## 0.5.1 -- 2025-01-15

Fix `for` HTML property.

## 0.5.0 -- 2025-01-15

Allow monadic `initialState` function.

## 0.4.0 -- 2025-01-10

Fix unnecessary DOM replaces in foreign code

## 0.3.2 -- 2025-01-10

Add `-fexpose-all-unfoldings` to JS build and export `label` and `p` mistakenly hidden from `Halogen.HTML`

## 0.3.1 -- 2025-01-09

Use `isTrue#` for `reallyUnsafePtrEquality#`

## 0.3.0 -- 2025-01-08

Fix bugs related to style and class rendering, add IO specialisation, add `input` type to `H.Slot` to avoid `unsafeCoerce`.

## 0.2.0 -- 2024-12-28

More complete release, with events, properties and Clay integration

## 0.1.0 -- 2024-12-21

* First version. Released on an unsuspecting world.
