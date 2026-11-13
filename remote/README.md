# haskell-halogen-remote

A [haskell-halogen](https://github.com/Swordlash/haskell-halogen) component whose program runs
somewhere other than the page that shows it, as in Phoenix LiveView: its DOM operations are
recorded and sent over a wire to a thin JavaScript client that applies them to a real page, and
the page's events travel back to the component's handlers. Natively a server holds the state and
the browser only shows it; in a page, a loopback runs the same pipeline with the wire in view.

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

`board` is an ordinary component, polymorphic in its monad. Nothing about it changes to be sent
over a wire: only the entry point names the monad, `RemoteDOM`, as a page's names `BrowserDOM`.

## The backend: `Halogen.Remote.DOM`

`RemoteDOM` is one more backend of the reconciler, as the in-memory DOM, Pixi's and Three.js's
are. It keeps a mirror of what the client shows (each node's kind, children, parent, attributes,
properties and listeners), which answers everything the reconciler and the driver read back, and
records every change it makes for the session to send. It has every instance a component monad
needs (`MonadUnliftIO`, `MonadBrowserDOM`, forking, killing, parallelism, UUIDs); storage is the
session's, in memory.

A handle the driver holds (a `Node`, an `Element`) is a value holding the node's number, so code
that hands one to the browser gets an error about a number rather than a crash.

## The session: `Halogen.Remote.Session`

```haskell
session <- Session.newSession send      -- send :: Text -> IO (), to the client
socket <- Session.mount session component input
Session.receive session message         -- each message from the client
Session.dispose session                 -- the client has gone
```

One session is one client and one instance of the component. Its operations are sent in
batches: the first operation after a message has gone wakes the session's sender, which waits
until no render of the tree is under way, yields once so that other work about to record joins
in, and sends what has been recorded as one message. A batch never ends in the middle of a render:
`mount` counts each render of the tree as under way while it runs (renders nest, a parent's
around its children's), and the sender takes nothing while one is. That is what the protocol's
rule for removed nodes needs: a node taken out and not put back by the end of the batch is gone,
on both sides, and only then, because a render may take a node out and put it back (when a
parent's render replaces the element around a child component's slot, the driver keeps the child
and moves its node into the new element). The first message begins with `clear`, then the first
render.
An event from the client runs its listener as a page's callback would, and is dropped if the
listener has gone (its removal crossed the wire after the event left). `dispose` sends nothing
more, disposes of the component's tree and forgets the mirror. A component that fails to mount
(its initial state or first render throws) ends the session, which has sent nothing, and `mount`
throws what it threw.

`Halogen.Remote.Server` and `Halogen.Remote.Loopback` are both made of this; so is a test, whose
wire can be a list (see `test/Test/Session.hs`).

## The protocol: `Halogen.Remote.Protocol`

To the page, one JSON object per batch, `{"n": 1, "ops": [...]}`, each operation an array with
its name first:

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

Node 0 is the window, node 1 the container the client was attached to (the component's body);
the session numbers every other node. `insert`'s index is where the child ends up once it has
been taken from wherever it was. A node taken out and not put back by the end of the batch is
gone, on both sides: after the batch's last operation, each node a `remove` took out that still
has no parent is forgotten with its subtree and their listeners, and is never named again. A
batch leaves out what would happen inside such a node (its children taken out of it, its
listeners stopped), which the client forgets with it.

`options` follows the `listen` of a handler that declares anything, in the same batch, and comes
again whenever a render changes what it declares: `{"preventDefault": true, "stopPropagation":
false, "match": m}`, where `m` is `null` (no match), `"any"`, `"plain-left-click"` or a list of
keys, `[{"key": "/"}, {"key": "s", "ctrlKey": true}]` (a modifier left out goes either way). The
client acts on it while the page dispatches the event (see "What cannot cross a wire", below).

To the program, one JSON object per event:

```json
{"listener": 7, "node": 4, "type": "input",
 "event": {"type": "input", "inputType": "insertText", "data": "i",
           "target": {"value": "hi", "checked": false, "tagName": "INPUT"},
           "currentTarget": {"value": "hi", "checked": false, "tagName": "INPUT"}}}
```

`event` holds what handlers read: the event's own string, number and boolean fields (`key`,
`code`, the modifiers, `button`, the pointer's coordinates, `detail`, …), `defaultPrevented:
true` when something in the page had prevented it before the client got it, and its `target`'s
and `currentTarget`'s `value`, `checked`, `selectedIndex`, `tagName` and `id`, and of a link its
`target` and `download: true`. Natively a handler reads it as
JSON (haskell-halogen-core's `Data.Foreign` reads an aeson object as a browser's event), so
`HE.onValueInput`, `HE.onChecked` and a key handler work as in a page.

The page's probe (below, "Testing a component in a real page") asks over the same socket. To
the program, `{"probe": 3}`, a number the client chose; to the page, a message that is not a
batch, `{"probe": 3, "trees": 1, "running": 0, "waiting": 0}`. `trees` is 1 while the session's
component is mounted, `running` is 1 while its tree is busy or the session has anything not yet
sent (operations not yet in a batch, a batch being sent, a render under way), and `waiting` its
fibers in a `liftIO`. The answer is sent in order with the batches, and only once everything
recorded when the question arrived has been sent. Either side ignores a message of a kind it does
not know (for the client, an object with neither `ops` nor `probe`; for the program, with
neither `listener` nor `probe`).

## The client: `jsbits/remote.js`

Plain functions on the global object, which the server serves at its path with `.js` after it
(`Halogen.Remote.Server.clientScript` holds it):

- `halogen_remote_connect(address, container, options)` connects to a server, shows what it sends
  and reconnects with a growing delay (`options.minDelay`, 0.5 s, doubling up to
  `options.maxDelay`, 10 s) when the connection is lost; each connection is a new session, which
  begins by emptying the container. With `options.reconnect` `false` it connects once, and a
  connection that closes stays closed. The container's `data-halogen-remote` is `connected` or
  `disconnected`, for the page's styling. It returns `{close()}`, which closes the connection and
  stops for good, a reconnection waiting for its delay included. When the page has a probe
  (`globalThis.__halogenProbe` an object), the client adds an asker to it, in the list core's
  programs use, so that a page with programs of both kinds sums them; with no open connection
  it answers at once that there is nothing (no tree, nothing running).
- `halogen_remote_attach(container, send)` is a client without a socket, whose events go to
  `send` as JSON text; `halogen_remote_apply(client, text)` applies a batch and
  `halogen_remote_detach(client)` stops listening and empties the container.

## The server: `Halogen.Remote.Server` (native)

`serve settings component input` listens on `settings.port` (8081), with the WebSocket at
`settings.path` (`/live`), the client at `/live.js` and `settings.pages` (a path, a file and its
media type each) beside them; `application` is the same as a WAI application, for a server of
one's own. Each connection is a session with an instance of the component of its own: state that
outlives a connection, or that connections share (a chat's lines), belongs outside the component,
in something its program reads and subscribes to. A program that throws ends its own session,
not the server; a connection that closes disposes of its session, which finalizes the component.
`examples/live` is a shared board served this way.

`serveWith settings connect` and `applicationWith` call `connect request session` for each
connection instead, with what it asked for (`request.path`, `request.query`, `request.headers`)
and its new session: it mounts what it wants (`Session.mount session component input`) before it
returns, and may keep the session and the socket. A `connect` that throws ends that session only;
one that mounts nothing has the connection closed with the reason "nothing was mounted for this
connection". A session the program disposes of itself has its connection closed. The server
answers the probe's questions by itself.

## The loopback: `Halogen.Remote.Loopback`

`Loopback.mountIn element component input` runs the whole pipeline in one page: a session whose
messages go to a client in `element`, whose events come back to the session, and every message,
both ways, emitted on `traffic` as `ToPage` and `ToProgram` for anyone watching. The first render
is sent as soon as the component is mounted, before a subscriber to `traffic` (which exists once
`mountIn` has returned) can usually subscribe: `Loopback.mountWatched watch` hands every message,
the first included, to `watch` as well, which a page showing the traffic subscribes to first. It is the server
minus the socket, which makes it the way to see the wire (the tutorial's lesson 21 shows its
traffic) and to test, in a browser, a component meant to be served. Natively it throws
`NotAvailable`.

## Testing a component in a real page: `Halogen.Remote.Test` (native)

A component's test can run it in the test's own process, natively, and show it in a real page
through the client, driven by `haskell-halogen-playwright`: edits rebuild one native test rather
than a WebAssembly program, and the test reaches into the program (a query through the socket,
the application's own state) because the program is its own. `Halogen.Remote.Test` is what such
a suite needs; it does not depend on the driver, a test combines the two:

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

`withTestServer` serves on a free port of the loopback interface for the action's duration.
Each `mounting` registers the component under a key of its own and gives the address of a page
that shows it: a bare HTML page with a container and the client, which connects once, without
reconnecting, with that key, and gets that component and no other. So examples do not share a
mount, and can run in parallel against one server. `withTestServerWith` takes `TestSettings`:
`shell`, the page around the client's HTML (the application's stylesheets, a `<base>`, scripts),
`pages`, files to serve beside it, and `connectTimeout`. When the page closes its connection the
session is disposed of; when `withTestServer` returns, every session is; and a test that
disposes of `mounted.session` itself has the page's connection closed, which leaves the page
disconnected.

**What the probe means over a wire.** The driver ends every action by asking the page's
`__halogenProbe.activity()` until it is idle, and `visit` waits for a tree and nothing running.
Here the client forwards the question to the session, whose answer is about that session alone,
so a busy example does not hold up another. The answer is idle only once the component has
handled every event the page sent before asking (one socket, in order: the event reached the
component's loop before the question was read) and the page shows what it rendered (the answer
is sent after the batches that carry it, and the client applies a batch before it reads the next
message). `waitQuiet` also waits for the fibers waiting in a `liftIO`; a pause in `H.delay` is not
waited for, as in a page.

**What such a test cannot see** is what only a browser does with the program in it: a ref is a
node of the session's mirror, so focusing, measuring or a canvas do nothing; `showModal`, the
document's title, the address bar and scrolling are not forwarded; the program has the page's
events only after the page is done with them (see below); and the page is not hydrated from
prerendered HTML. Layout and CSS are the page's and work, but a component that reads them back
cannot. Test those with `hspec-halogen`, which runs the real program in the page (on WebAssembly
or the JavaScript backend), or with `haskell-halogen-playwright` against the built application.

## What cannot cross a wire

The program and the page are apart, and the wire takes time:

- **Refs.** `H.getRef` gives a node of the mirror, not an element of the page: an effect that
  talks to the browser through it (focusing, measuring, drawing on a canvas) does nothing useful,
  as under the in-memory DOM.
- **What a handler decides.** A handler hears of an event after the page is done with it, too
  late to prevent its default: `preventDefault` called from an action does nothing, and neither
  does a choice only the handler's function can make. What the page must decide while it
  dispatches the event is declared instead, as data the page reads: a handler with a match,
  `HE.matching (HE.keys ["/"])`, `HE.matching HE.plainLeftClick` or `HE.matching HE.anyEvent`, is
  sent only the events that match (a `keydown` handler for two keys does not send every key over
  the wire), and the page prevents their default or stops their propagation as the handler
  declares with `HE.preventingDefault` and `HE.stoppingPropagation`:

  ```haskell
  HH.div [HE.preventingDefault (HE.matching (HE.keys ["/"]) (HE.onKeyDown (const FocusSearch)))] [...]
  ```

  A handler declared with `HE.preventingDefault` and no match has the default of every event of
  its type prevented, since the page cannot know which ones its function would make an action
  for: over the wire, declare the match. A router's link (`Halogen.Router.link`) declares the
  plain left click, so it works over the wire as in a page: a plain click is followed by the
  component, a ctrl-click opens a new tab. For a handler that declares nothing the client
  prevents the default of every `submit` it listens for and of a plain `click` on an `a` element
  (one the browser would follow in this tab), and of nothing else.
- **Focus and selection** are the page's. The client sets an element's `value` only when it
  differs from what the element holds, so text being typed is not undone, and each event reports
  its current target's `value`, `checked` and `selectedIndex`, which the mirror takes as the
  element's, so a component that turns an edit down still puts its own value back.
- **Hydration.** The client starts empty: it does not adopt HTML a server rendered ahead of time.

## Tests

`Halogen-remote-test` runs natively and, under Node, on both browser backends: a session whose
wire is a list (the first render, a click's patch and position, an input's value, a child removed,
a child kept while its parent replaces the element around it, a handler that prevents the default,
a link followed on a plain click and on no other, a listener's declared options sent with it and
when they change, an unknown listener, a component that fails to mount, disposal), the recorder on its own (a node
taken out is forgotten when the batch is taken and not before, no batch in the middle of a
render), the protocol's JSON, the probe's questions and answers (before the mount, after the
batches that carry what was recorded when it asked, what is unsent counted as running, a fiber
waiting, after disposal), a session's identity, and natively a component served over a real
WebSocket, by `application` and by `applicationWith` (a component per query string, a connection
nothing was mounted for closed with why, the probe answered over the socket).
`Halogen-remote-live-test` serves to Chromium through `haskell-halogen-playwright`, natively:
`Server.application` behind a proxy the test can cut, and `Halogen.Remote.Test` as a suite would
use it, reading the page once after each action (visit, a click whose work holds the loop,
`waitQuiet` for a `liftIO`, a pause not waited for, two pages in parallel with one kept busy, no
reconnection, a query from the test).
`Halogen-remote-browser-test` runs the loopback in headless Chromium (hspec-halogen) on both
browser backends: real clicks and typing through the client, a child's element kept across its
parent's new element, a form whose submission the page does not carry out (with and without
`HE.preventingDefault`), a link the page does not follow, a key a handler declares kept from
being typed, a component that fails to mount, disposal, and the page's probe, to which the
loopback's client adds nothing (the program in the page answers for the loopback's tree, once).
