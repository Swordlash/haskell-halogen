# haskell-halogen-remote

Run a [haskell-halogen](https://github.com/Swordlash/haskell-halogen) component outside the page that displays it, similar to Phoenix LiveView. The program sends DOM operations to a small JavaScript client, which updates the page and sends events back to the component. A native server can hold application state while the browser displays it. The loopback mode runs both ends in one page and shows the messages they exchange.

```haskell
import Halogen.Remote.Server qualified as Server

main :: IO ()
main = Server.serve Server.defaultSettings {Server.pages = [("/", "web/live.html", "text/html")]} board ()
```

```html
<div id="app"></div>
<script src="/live.js"></script>
<script>halogen_remote_connect("/live", document.getElementById("app"));</script>
```

`board` is an ordinary component polymorphic in its monad. The entry point selects `RemoteDOM` instead of `BrowserDOM`; the component itself does not need to change.

## The backend: `Halogen.Remote.DOM`

`RemoteDOM` implements the same reconciler interface as the in-memory, Pixi and Three.js backends. It mirrors node types, children, parents, attributes, properties and listeners. The driver reads from that mirror, and every change is recorded for transmission to the client.

The monad provides the instances components need, including `MonadUnliftIO`, `MonadBrowserDOM`, forking, cancellation, parallelism and UUIDs. It also mirrors the page's URL, base URL and Halogen storage entries. The client supplies these at connection time and keeps them updated, allowing routers and storage APIs to work with the browser's state.

Node and element handles contain node numbers. On the native backend, `withNativeObject` lets those handles implement operations from the `Web.*` modules. Supported methods and assignments are recorded for the client, while checked casts inspect the mirrored node type. Existing ref-based component code can therefore work remotely.

Loopback handles are JavaScript numbers. They do not implement these native object operations; see the loopback limitations below.

## The session: `Halogen.Remote.Session`

```haskell
session <- Session.newSession send      -- send :: Text -> IO (), to the client
socket <- Session.mount session component input
Session.receive session message         -- each message from the client
Session.dispose session                 -- the client has gone
```

Each session owns one client connection and one component instance. It sends DOM changes in batches. The first operation wakes the sender, which waits for all nested renders to finish, yields once to collect nearby work, then sends the batch. The initial batch starts with `clear` followed by the first render.

A batch never splits a render. This matters because a render can temporarily remove and reinsert a node, for example when a parent replaces the element around a child slot. Both ends forget removed nodes only if they remain detached at the end of the batch.

Client events invoke the corresponding listeners. Events for listeners already removed are ignored. `dispose` stops transmission, disposes the component tree and clears the mirror. If initial state creation or the first render throws, mounting fails without sending anything, the session ends and `mount` rethrows the exception.

`Halogen.Remote.Server`, `Halogen.Remote.Loopback` and session tests all use this pipeline. Tests can replace the transport with a list; see `test/Test/Session.hs`. A session created without a `hello` message starts at `/`, with no base URL and empty stores.

## The page's address and stores

Existing routing and storage code works through the session. For example, create a router using the page's base URL before mounting:

```haskell
Server.serveWith settings $ \_ session -> do
  router <- Session.run session $ do
    base <- Router.pageBase                    -- the page's <base href>
    Router.newRouter base Home route           -- at the page's route
  void (Session.mount session app router)
```

The client first sends a `hello` containing `location.href`, `<base href>` and entries under core's `haskell-halogen:` prefix in `localStorage` and `sessionStorage`. The server applies it before calling connection code or mounting a component.

The session exposes this mirror through window location fields, `document.baseURI` and `MonadBrowserDOM` storage methods. `Router.newRouter` therefore starts at the browser's route. `navigate`, `redirect` and followed router links update the mirror immediately and send `pushState` or `replaceState` operations without reloading. Back and Forward send the new location before the router's `popstate` event. Reloading starts a new session at that same route. `newHashRouter` works through the fragment.

Storage writes update the mirror and the browser's own store. They survive session replacement and reloads according to the browser's storage rules. Changes from another same-origin tab arrive through the `storage` event and update the mirror.

Only prefixed Halogen storage entries are mirrored; other keys appear absent. History entries use `null` state. If the browser rejects a storage write or navigation, for example because of quota, denied storage or a cross-origin URL, it logs `console.warn`. The program is not notified and its mirror has already changed. Scroll restoration, window dimensions, IndexedDB and clipboard state are not mirrored.

## The protocol: `Halogen.Remote.Protocol`

The server sends one JSON object per batch: `{"n": 1, "ops": [...]}`. Each operation is an array beginning with its name:

| operation | meaning |
| --- | --- |
| `["clear"]` | empty the container, forget every node |
| `["create", node, namespace, name]` | an element (`namespace` is `null` for HTML) |
| `["text", node, text]` | a text node |
| `["set-text", node, text]` | a text node's text |
| `["insert", parent, index, child]` | put `child` at `index` among `parent`'s children |
| `["remove", parent, child]` | take `child` out of `parent` |
| `["attr", node, namespace, name, value]` | set an attribute |
| `["unattr", node, namespace, name]` | remove one |
| `["prop", node, name, value]` | set a property (a string, a number or a boolean) |
| `["unprop", node, name]` | reset one |
| `["listen", node, type, listener]` | listen for events of a type |
| `["unlisten", node, type, listener]` | stop |
| `["options", node, type, listener, options]` | what the listener's handler declared |
| `["call", node, method, [arguments]]` | call one of an element's methods, from the allowlist below |
| `["set", node, name, value]` | assign a property: the document's `title`, on node 0 |
| `["history", "push", address]` | `history.pushState(null, "", address)` (`null`: the page's own) |
| `["history", "replace", address]` | `history.replaceState(null, "", address)` |
| `["history", "go", n]` | `history.go(n)`, back for a negative `n` |
| `["storage", store, key, value]` | `localStorage` (`"local"`) or `sessionStorage` (`"session"`): set the item, or remove it for `null` |

Node 0 represents the window, and node 1 is the client container. The session assigns all other node numbers. An `insert` index refers to the child's final position after removal from its previous parent.

At the end of a batch, detached nodes and their descendants and listeners are forgotten on both sides and never referenced again. Redundant cleanup inside removed subtrees is omitted. Operations before the last `call` are retained, however, so methods see the DOM as it existed when called. For example, a download link can be clicked before being removed, and a form submitted before one of its fields is deleted.

`options` follows a listener's `listen` in the same batch and is resent when declarations change. It contains `preventDefault`, `stopPropagation` and `match`. A match can be `null`, `"any"`, `"plain-left-click"` or a list such as `[{"key": "/"}, {"key": "s", "ctrlKey": true}]`. Omitted modifiers match either state. The client evaluates these declarations synchronously during event dispatch.

`call` and `set` represent supported operations from the `Web.*` API. The client accepts only this allowlist:

| method | arguments |
| --- | --- |
| `focus` | none, or `{"preventScroll": true}` (`HTMLElement.focusWithoutScrolling`) |
| `blur`, `click`, `select`, `show`, `showModal` | none |
| `close` | none, or the dialog's return value |
| `scrollIntoView` | none, or `{"block": …, "inline": …, "behavior": …}` (`Element.scrollIntoViewBlock`) |

These methods operate on elements. The only supported `set` assignment is `title` on node 0, applied to `document.title`. Unknown methods, properties and nodes are ignored. Browser exceptions are logged with `console.warn`, and processing continues.

Batches run synchronously in order, so a method follows the operations that created its node. A focus requested in `Initialize` therefore reaches the element from the first render.

The client sends one JSON object per event:

```json
{"listener": 7, "node": 4, "type": "input",
 "event": {"type": "input", "inputType": "insertText", "data": "i",
           "target": {"value": "hi", "checked": false, "tagName": "INPUT"},
           "currentTarget": {"value": "hi", "checked": false, "tagName": "INPUT"}}}
```

`event` includes the string, numeric and boolean fields handlers need, such as key, code, modifiers, button, coordinates and detail. It reports `defaultPrevented` if an earlier page listener prevented the event. Target and current-target snapshots include `value`, `checked`, `selectedIndex`, `tagName` and `id`; links also include `target` and a download flag. Core's native `Data.Foreign` reads these JSON objects like browser events, so standard input, checkbox and keyboard handlers work unchanged.

The first client message is `hello`, containing the URL and prefixed storage entries. Denied storage is represented as empty. `base` is `document.baseURI` when a `<base href>` exists, otherwise `null`:

```json
{"hello": {"location": "https://example.com/blog/post/1?q=2#top", "base": "https://example.com/blog/",
           "storage": {"local": {"haskell-halogen:count": "Mw=="}, "session": {}}}}
```

The hello message includes all prefixed entries, so its size depends on stored data; browser storage is typically limited to about 5 MB per store and origin.

Back, Forward and manual fragment changes send a location message such as `{"location": "https://example.com/blog/"}`. The client's `popstate` and `hashchange` listeners run before program listeners so the mirror updates first. Cross-tab storage changes send `{"storage": {"kind": "local", "key": "haskell-halogen:count", "value": "NA=="}}`. A null value removes an item; a null key clears the store.

The activity probe uses the same socket. The client sends `{"probe": 3}` and receives `{"probe": 3, "trees": 1, "running": 0, "waiting": 0}`. `trees` is 1 while the component is mounted. `running` includes tree work, renders, queued operations and batches still being sent. `waiting` counts fibers suspended in `H.async`.

Probe replies are ordered with batches and follow all operations recorded before the query arrived. Both sides ignore unknown message types, and the client ignores unknown operations. The client recognises `ops` and `probe`; the program recognises `listener`, `probe`, `hello`, `location` and `storage`.

## The client: `jsbits/remote.js`

The client exposes global functions. The server serves the script at its socket path plus `.js`; its contents are also available as `Halogen.Remote.Server.clientScript`.

- `halogen_remote_connect(address, container, options)` connects to a server, shows what it sends and reconnects with a growing delay (`options.minDelay`, 0.5 s, doubling up to `options.maxDelay`, 10 s) when the connection is lost; each connection is a new session, which begins by emptying the container. With `options.reconnect` `false` it connects once, and a connection that closes stays closed. The container's `data-halogen-remote` is `connected` or `disconnected`, for the page's styling. It returns `{close()}`, which closes the connection and stops for good, a reconnection waiting for its delay included. When the page has a probe (`globalThis.__halogenProbe` an object), the client adds an asker to it, in the list core's programs use, so that a page with programs of both kinds sums them; with no open connection it answers at once that there is nothing (no tree, nothing running).
- `halogen_remote_attach(container, send)` is a client without a socket, whose events go to `send` as JSON text; `halogen_remote_apply(client, text)` applies a batch and `halogen_remote_detach(client)` stops listening and empties the container. `halogen_remote_hello_text()` is the page's first message, which `connect` sends as the socket opens and the loopback hands its session before it mounts.

## The server: `Halogen.Remote.Server` (native)

`serve settings component input` listens on port 8081 by default. The default WebSocket path is `/live`, with the client script at `/live.js`. `settings.pages` adds static files as path, filename and media-type triples. Use `application` to embed the same service in your own WAI server.

Every connection gets a separate component instance. Shared or persistent state, such as chat history, belongs outside that instance in a service it can read and subscribe to. A component failure ends only its session. Closing the connection disposes and finalizes the component. `examples/live` demonstrates a shared board.

`serveWith settings connect` and `applicationWith` call `connect request session` for each connection. The request exposes `path`, `query` and `headers`. Mount a component with `Session.mount` before returning; you may retain the session and socket afterward.

Exceptions end only that session. Returning without mounting closes the connection with "nothing was mounted for this connection". Disposing the session also closes its connection. The server handles activity-probe requests automatically.

Before calling `connect`, the server reads and applies the client's first `hello`. Code run through `Session.run session`, including router creation, can therefore read the page's URL and stores. A client that sends another message first starts with `/` and empty stores.

## The loopback: `Halogen.Remote.Loopback`

`Loopback.mountIn element component input` runs a session and client in one page. DOM messages go to the client in `element`, and events return to the session. The `traffic` emitter reports messages in both directions as `ToPage` and `ToProgram`.

The first render may be sent before callers can subscribe to `traffic`. Use `Loopback.mountWatched watch` to observe every message from the start. Tutorial lesson 21 uses loopback to display the protocol. It is also useful for browser tests of remotely rendered components. Native calls throw `NotAvailable`.

As with a socket connection, loopback applies the page's `hello` before mounting and reports it first on `traffic`. Storage uses the page's values. Routers created by the in-page program access the browser URL directly.

Loopback does not support ref-based `Web.*` operations the way a native session does. Its handles are JavaScript numbers, so checked casts such as `H.getHTMLElementRef` and `HTMLDialogElement.fromElement` return `Nothing`. Components using checked casts skip the operation.

Calling methods without a cast raises a `JSError` wrapping the browser's `TypeError`. Assigning the title may instead do nothing in non-strict JavaScript. Supporting these operations would require object handles and changes to browser casting behaviour.

## Testing a component in a real page: `Halogen.Remote.Test` (native)

Run the component in the native test process and display it in Chromium through the remote client. This makes rebuilds native-only while allowing the test to query the mounted component and inspect application state. Combine `Halogen.Remote.Test` with `haskell-halogen-playwright`; the former does not depend on the browser driver:

```haskell
import Halogen.Playwright
import Halogen.Playwright.Hspec
import Halogen.Remote.Test

main :: IO ()
main = withTestServer $ \server -> hspec $ playwright $
  it "saves" $ inPage $ \page -> do
    (address, connected) <- mounting server (editor env) input
    visit page address                           -- returns once the first render is shown
    mounted <- connected                         -- its session and socket
    click (byRoleNamed page "button" "Save")     -- settles: handled, rendered and sent
    textContent (locator page "#status") `shouldReturn` "Saved"
    queryComponent mounted (H.mkRequest IsSaved) `shouldReturn` Just True
```

`withTestServer` starts a server on a free loopback port for the supplied action. `mounting` registers a component under a unique key and returns a page URL plus an action that waits for its connection. The page connects once without retrying. Separate mounts can run in parallel on one server.

`withTestServerWith` accepts `TestSettings`: a `shell` for application styles and scripts, additional `pages`, and `connectTimeout`. The server inserts the mount's `<base href>` first in the head. Closing a page disposes its session; ending the server disposes all sessions. Disposing a mounted session explicitly closes the page's connection.

**Activity over the wire.** The client forwards activity queries to its own session, so a busy test does not block another. An idle reply means the component has handled earlier events and the browser has applied their DOM batches: both directions preserve socket order.

Some applied operations produce more browser events, such as a requested focus triggering `onFocus`. If those events were sent after a probe query, the client queries again before returning the answer. `waitQuiet` also waits for `H.async` fibers, but excludes `H.delay`, just as it does for an in-page program.

**Work started by the test.** Releasing a held backend response, querying the component or notifying an emitter does not trigger a browser action, so the driver does not automatically settle afterward. Use `waitSession mounted p` to wait for a session activity condition, then `settle page` to ensure the browser has applied the batches. `waitSession` wakes on activity changes rather than polling:

```haskell
release heldAnswer
waitSession mounted (\a -> a.running == 0 && a.waiting == stillHeld)
settle page
textContent (locator page "#count") `shouldReturn` "3"
```

**Disposal.** `Session.dispose` returns after finalizers complete their synchronous parts, even if the server is disposing the same session concurrently. Finalizers suspended in `H.async` may continue afterward. Use `waitSession mounted quiet` or `Session.waitActivity` to wait for those too.

**Routes and reloads.** Every path beneath a mount URL serves the same page with that mount's `<base href>`. Use `mountingEach server program` to create a component and input in each new session after its hello arrives. This supports routers and reloads: every connection gets fresh state, and the returned action waits for successive connections in order:

```haskell
(address, next) <- mountingEach server $ do
  base <- Router.pageBase
  router <- Router.newRouter base Home route
  pure (app, router)
visit page (address <> "blog/post/1")              -- shows the post
_ <- next
click (locator page "#to-about")                   -- the page's address is the route's
evaluate page "location.pathname" `shouldReturn` ("/mount/1/about" :: Text)
reload page >> waitStarted page
again <- next                                      -- a new session, at /about
```

`mounting` remains single-use. Use the driver's `goBack` and `goForward` for history navigation; they wait for navigation and settle through the probe.

**Limits of remote tests.** The browser provides real layout, CSS and input, and supported writes such as focus, dialog methods, scrolling and title updates cross the wire. However, refs refer to mirror nodes: the component cannot read browser measurements, `document.activeElement` or a dialog's current `open` state. Canvas APIs and hydration are unavailable, and handlers receive events after browser dispatch. Test browser-dependent component logic with `hspec-halogen` or test the built application with `haskell-halogen-playwright`.

## What crosses

Native sessions forward these `Web.*` operations: focus, `focusWithoutScrolling`, blur, click, select, dialog `showModal` and `close`, `scrollIntoView`, `Element.scrollIntoViewBlock` and document title assignment. Checked refs and dialog casts work against mirrored node types, so ordinary component code can use them.

Calls are recorded when made and included in the current or next batch. A call during initialization follows node creation. A call followed by removal in the same action still runs before the removal. Calls on forgotten nodes and operations outside the allowlist, such as `setAttribute` or `remove`, record nothing.

## What cannot cross a wire

The program and the page are apart, and the wire takes time:

`H.getRef` returns a mirror node. Reads that require the real page, including layout measurements, `document.activeElement`, `isConnected`, dialog `open`, canvas data and clipboard contents, return inert values such as `False`, 0 or `Nothing`. Unsupported methods do nothing. Mirror queries such as `querySelector` reflect what the program rendered, not independent browser state.

Event handlers run after browser dispatch, so calling `preventDefault` from an action is too late. Declare synchronous browser behaviour on the handler instead. `HE.matching` filters events using `HE.keys`, `HE.plainLeftClick` or `HE.anyEvent`; only matches cross the wire. Combine it with `HE.preventingDefault` or `HE.stoppingPropagation` to apply those effects in the browser:

  ```haskell
  HH.div [HE.preventingDefault (HE.matching (HE.keys ["/"]) (HE.onKeyDown (const FocusSearch)))] [...]
  ```

Without a match, `HE.preventingDefault` applies to every event of that type because the client cannot evaluate the Haskell handler. Declare the intended match explicitly. `Halogen.Router.link` already matches plain left clicks, preserving Ctrl-click for new tabs. For handlers without declarations, the client prevents listened-for submit events and plain same-tab anchor clicks, and no other defaults.

`HE.within` supports delegated handlers, such as one listener for all links in rendered Markdown. It matches a selector around the event target within the container. The client sends the matching element and its attributes so the handler can read them:

  ```haskell
  HH.div
    [HE.preventingDefault (HE.matching (HE.within "a[href]" HE.plainLeftClick) (HE.handler' (EventType "click") followed))]
    [rendered]
    where
      followed event = do
        target <- Event.target event >>= Element.fromEventTarget
        link <- Element.closestNow target "a[href]"
        Article <$> Element.getAttributeNow link "href"
  ```

Only matching plain link clicks are prevented and forwarded. Clicks elsewhere, Ctrl-clicks, new-tab links and downloads remain browser actions. Native `Element.closestNow` can answer only for the selector declared in the match, and `Element.getAttributeNow` only for the element supplied by the client.

Use `HE.carrying` to filter paste and drag events by data type. For example, `["Files"]` handles file pastes while allowing ordinary text paste. Forwarded events include data types and file names, media types and sizes. Drop and paste events also include text up to 262144 UTF-16 code units.

File contents are not transferred. `File.text` and `File.bytes` fail natively with `NotInBrowserError`; applications needing bytes must provide an upload channel. Drag text is unavailable before drop, as it is in the browser.

Focus and selection remain browser state unless the component requests a supported focus operation. The client updates `value` only when it differs, preserving ongoing typing. Events report current `value`, `checked` and `selectedIndex` into the mirror, so a component rejecting an edit can restore its intended value.

The client does not hydrate prerendered HTML; it starts with an empty container. URLs and Halogen storage entries are mirrored, but scroll restoration, window dimensions, IndexedDB and clipboard state are not.

## Tests

`Halogen-remote-test` runs natively and under Node on both browser backends. It tests session rendering, event patches, input values, child identity, listener removal, mount failures and disposal. Recorder tests verify that nodes survive temporary removal and that batches never split renders. Protocol tests cover JSON, session identity and activity replies before mounting, during pending work and after disposal.

Handler tests cover default prevention, declared matches, plain-click routing, delegated links, clipboard metadata and updated listener options. Native tests also check supported DOM calls and their ordering, dialog and scroll operations, both title access paths, ignored unsupported operations, real WebSocket serving through `application` and `applicationWith`, per-request mounts, empty connections and hello handling. Storage and router tests cover initial values, updates, cross-tab changes, history operations, hash routes and sessions without hello.

`Halogen-remote-live-test` drives Chromium from a native test through `haskell-halogen-playwright`. It exercises a server behind a disconnectable proxy and the `Halogen.Remote.Test` API, reading once after each settled action. Cases include busy handlers, `H.async`, uncounted delays, independent pages, disabled reconnection, direct queries, delegated navigation, real paste events, focus, modal dialogs, scrolling, title updates and unsupported methods. `mountingEach` tests deep links, Back/Forward, redirects, reloads, hash routing and local/session storage persistence.

`Halogen-remote-browser-test` runs loopback in headless Chromium on both browser backends using hspec-halogen. It checks real input, retained child elements, prevented form and link defaults, delegated events, key matching, loopback handle limitations, mount failure and disposal. It also checks that the probe counts the loopback tree once, hello arrives first, storage survives remounts and hash routing supports Back.
