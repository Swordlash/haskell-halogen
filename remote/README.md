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
batches: the first operation after a message has gone wakes the session's sender, which yields
once so that the render under way can finish, and sends what has been recorded as one message, so
a render is normally one message. The first message begins with `clear`, then the first render.
An event from the client runs its listener as a page's callback would, and is dropped if the
listener has gone (its removal crossed the wire after the event left). `dispose` sends nothing
more, disposes of the component's tree and forgets the mirror.

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
| `["remove", parent, child]` | take `child` out, and forget it and its subtree |
| `["attr", node, namespace, name, value]` | set an attribute |
| `["unattr", node, namespace, name]` | remove one |
| `["prop", node, name, value]` | set a property (a string, a number or a boolean) |
| `["unprop", node, name]` | reset one |
| `["listen", node, type, listener]` | listen for events of a type |
| `["unlisten", node, type, listener]` | stop |

Node 0 is the window, node 1 the container the client was attached to (the component's body);
the session numbers every other node. `insert`'s index is where the child ends up once it has
been taken from wherever it was. A removed node is never named again: nothing in the reconciler
puts a node back once it has taken it out (a move is an `insert`).

To the program, one JSON object per event:

```json
{"listener": 7, "node": 4, "type": "input",
 "event": {"type": "input", "inputType": "insertText", "data": "i",
           "target": {"value": "hi", "checked": false, "tagName": "INPUT"},
           "currentTarget": {"value": "hi", "checked": false, "tagName": "INPUT"}}}
```

`event` holds what handlers read: the event's own string, number and boolean fields (`key`,
`code`, the modifiers, `button`, the pointer's coordinates, `detail`, …) and its `target`'s and
`currentTarget`'s `value`, `checked`, `selectedIndex` and `tagName`. Natively a handler reads it as
JSON (haskell-halogen-core's `Data.Foreign` reads an aeson object as a browser's event), so
`HE.onValueInput`, `HE.onChecked` and a key handler work as in a page.

## The client: `jsbits/remote.js`

Plain functions on the global object, which the server serves at its path with `.js` after it
(`Halogen.Remote.Server.clientScript` holds it):

- `halogen_remote_connect(address, container, options)` connects to a server, shows what it sends
  and reconnects with a growing delay (`options.minDelay`, 0.5 s, doubling up to
  `options.maxDelay`, 10 s) when the connection is lost; each connection is a new session, which
  begins by emptying the container. The container's `data-halogen-remote` is `connected` or
  `disconnected`, for the page's styling. It returns `{close()}`.
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

## What cannot cross a wire

The program and the page are apart, and the wire takes time:

- **Refs.** `H.getRef` gives a node of the mirror, not an element of the page: an effect that
  talks to the browser through it (focusing, measuring, drawing on a canvas) does nothing useful,
  as under the in-memory DOM.
- **`preventDefault`.** A handler hears of an event after the page is done with it, too late to
  prevent its default, and `HE.preventingDefault` cannot help either: what a handler does to its
  event is decided in the listener, on the program's side. The client prevents the default itself
  for the two cases a component that listens never wants: every `submit` it listens for, and a
  `click` on an `a` element it listens for. Nothing else is prevented.
- **Focus and selection** are the page's. The client sets an element's `value` only when it
  differs from what the element holds, so text being typed is not undone, and each event reports
  its current target's `value`, `checked` and `selectedIndex`, which the mirror takes as the
  element's, so a component that turns an edit down still puts its own value back.
- **Hydration.** The client starts empty: it does not adopt HTML a server rendered ahead of time.

## Tests

`Halogen-remote-test` runs natively and, under Node, on both browser backends: a session whose
wire is a list (the first render, a click's patch, an input's value, a child removed, an unknown
listener, disposal), the protocol's JSON, and natively a component served over a real WebSocket.
`Halogen-remote-browser-test` runs the loopback in headless Chromium (hspec-halogen) on both
browser backends: real clicks and typing through the client, a form whose submission the page
does not carry out, and disposal.
