# Changelog for `haskell-halogen-remote`

## Unreleased

The first version, for a component whose program runs somewhere other than the page that shows
it, as in Phoenix LiveView: its DOM operations cross a wire to a thin JavaScript client that
applies them to a real page, and the page's events come back to its handlers.

`Halogen.Remote.DOM.RemoteDOM` is the backend: a monad with every instance a component monad
needs, whose DOM is a mirror of what the client shows, which answers what the reconciler reads
back and records each change. A component does not change to be sent over a wire; only its entry
point names `RemoteDOM`.

`Halogen.Remote.Session` is one client's side: `newSession` with what sends a message to the
client, `mount` of one component, `receive` of each message from the client and `dispose`. The
session sends its operations in batches, normally one per render and never part of one, the first
beginning with `clear`; an event runs its listener as a page's callback would, and one for a
listener that has gone is dropped. A component that fails to mount ends its session, which has sent
nothing. `Halogen.Remote.Protocol` says what crosses, as compact JSON: an operation is an array with
its name first (`["create", 3, null, "div"]`), an event an object naming its listener and node with
the fields handlers read. A node taken out and not put back by the end of a batch is forgotten, on
both sides, so a child component whose parent replaces the element around it keeps its node.
`Halogen.Remote.Internal` is the recorder itself, for tests and tools.

`Halogen.Remote.Server` (native) serves a component over WebSockets with warp and wai-websockets,
a session per connection, with the client at the socket's path and `.js` and the pages it is
given beside it; `application` is the same as a WAI application. A program that throws ends its
own session, and a connection that closes disposes of its session.

`Session.waitActivity` waits until the session's reading satisfies a predicate, woken by every
change of its tree and of what it has not sent (a batch taken or sent, a render, an operation
recorded), never on a timer; `Halogen.Remote.Test.waitSession` is the same for a mounted
component, for work the test sets off outside the page (an answer it held, let go), followed by
the driver's `settle`. After `dispose` the session's reading is no tree and what of it is still at
work, its finalizers' waits, so `waitActivity session quiet` returns once they have ended.
`dispose` returns once the component's finalizers have run their synchronous parts for every
caller: a second disposal at the same moment (the server's, as the connection the first closed
ends) returned at once before, while the first was still finalizing, so what a finalizer released
could still be held when it returned.

`Halogen.Remote.Loopback.mountIn` runs the same pipeline inside one page, with every message
emitted on `traffic` both ways, and `mountWatched` hands every message, the first render's
included, to a watcher given before anything crosses; natively they throw `NotAvailable`. A
component that fails to mount leaves the element without a client.

A handler hears of an event too late to prevent its default or stop it, so what the page must do
while it dispatches the event is declared as data and crosses the wire: a listener whose handler
declares anything (`HE.matching`, `HE.preventingDefault`, `HE.stoppingPropagation`) is followed
by an `options` operation, `["options", node, type, listener, {"preventDefault": …,
"stopPropagation": …, "match": …}]`, sent again whenever a render changes it. For a listener
with a match (`"any"`, `"plain-left-click"` or a list of keys), the client sends only the events
that match and prevents or stops each of them as declared, so a `keydown` handler for "/" keeps
the slash from being typed and does not send every other key; for one that declares
`HE.preventingDefault` with no match, the client prevents the default of every event of its type
on the element, since only the handler knows which it would make an action for. For a listener
that declares nothing, the client (`jsbits/remote.js`) prevents the default of every `submit` it
listens for, and of a `click` on an `a` element only when the browser would follow that click in
this tab, and nothing else. A router's link declares the plain left click.

Two matches are objects, `{"within": selector, "match": m}` and `{"carrying": types}` (core's
`HE.within` and `HE.carrying`). For a listener whose match has a `within`, the client judges each
event on the element the selector finds around the target, inside the listening element, and
sends that element under the target's `closest`, by selector, with all its attributes: one click
handler on a container of rendered Markdown, which reads the link from the event's target with
`Element.closestNow` and `Element.getAttributeNow`, follows the links it covers over the wire as
in a page, and the page follows the others and every ctrl-click itself. A drag or a clipboard
event carries its `dataTransfer` or `clipboardData`: the types it lists, its files' names, types
and sizes, and at a drop and a paste its text (up to 262144 UTF-16 code units; longer text does
not cross), so a paste handler declared for files takes a pasted file's name, type and size and
leaves a paste of text to the page. A file's bytes do not cross. A client older than these
matches takes them as `"any"`: it sends every event of the listener's type and prevents the
default of each as declared, and the program makes no action for them, since they carry nothing
the match finds; `Halogen.Remote.Server` serves the client it was built with, so only a page that
keeps a copy of its own meets this.

The client's rule for clicks on links changed: it used to prevent every click on an `a` it
listened for, a ctrl-click or a middle click included, so that nothing opened in a new tab. It now
prevents only a plain click (the main button, no modifier key, nothing in the page that prevented
it already, a link with no other `target` and no `download`), and leaves every other click to the
browser. An event also says more of itself: `defaultPrevented` when something in the page had
prevented it before the client got it, and, of a link that is its target or current target, its
`target` and `download: true`. That is what `Halogen.Router.isPlainLeftClick` now reads natively,
so a router's link clicked in a page served by a native program is followed, where it did
nothing before. Refs are nodes of the mirror, focus and selection stay the page's, and
nothing is hydrated. `halogen_remote_connect`'s `close()` also stops a reconnection that is
waiting for its delay.

A test that drives a page through haskell-halogen-playwright can wait on a component served this
way as on a program in the page. The client gives the page's probe (`__halogenProbe`) an asker
that forwards its question over the socket, `{"probe": n}`, and the session answers
`{"probe": n, "trees": …, "running": …, "waiting": …}`, a message that is not a batch, through
its sender, in order with its batches and only once what was recorded when the question came has
been sent: idle over a wire means the page shows what the program rendered.
`Session.activity` is that reading (its own tree's, with anything not yet sent counted as
running). Both sides ignore a message of a kind they do not know. The loopback's client adds no
asker, since the program in the page answers for its trees already.

`Server.applicationWith` and `Server.serveWith` call a function for each connection, with what it
asked for (`Request`: its path, query string and headers) and its new session, which mounts what
it wants and may keep the session and the socket; `application` and `serve` are the special case.
A function that mounts nothing has the connection closed with a reason, and one that throws ends
that session only. A session the program disposes of has its connection closed. Sessions have an
id unique in the process (`Session.sessionId`), which `Eq`, `Ord` and `Show` go by, and
`Session.ended` waits for one to be disposed of. `halogen_remote_connect(…, {reconnect: false})`
connects once and stays disconnected when the connection closes.

`Halogen.Remote.Test` (native) is what a test suite uses to test a component in a real page
while it runs in the test's own process: `withTestServer`, and per example `mounting`, which gives
the address of a page that shows that component alone and an action that waits for the page to
connect and gives the component's session and socket (`Mounted`, with `queryComponent`).

What a component does to the page through the `Web.*` types crosses the wire without the
component changing: a focus through a ref (`H.getHTMLElementRef`, then `HTMLElement.focus`, or
`focusWithoutScrolling`), `blur`, `click`, an input's `select`, `scrollIntoView` (and
`Element.scrollIntoViewBlock`), a dialog's `showModal`, `show` and `close` (through
`HTMLDialogElement.fromElement`), and the document's title (`doc.title_set`). Natively a handle
answers for itself (haskell-halogen-jsbits' `withNativeObject`): the call is recorded as an operation,
`["call", node, method, [arguments]]` or `["set", 0, "title", value]`, in the batch being recorded
(a render's, so a focus in an `Initialize` comes after the element's creation), and the client
performs it from an allowlist, logging one that throws. A cast of a handle asks what its node is, so
the casts that gave `Nothing` natively now find the element. Reads still cannot cross in time and
keep their inert answers (measuring, `document.activeElement`, a dialog's `open`, the clipboard);
nothing off the allowlist is recorded; a call on a node that has gone records nothing (one made
before the node went, in the same batch, is performed before its removal). An event such a call
makes the page send (a `focus` the component asked for, heard by its own `onFocus`) is waited for
by the page's probe: the client asks the session again when it has sent an event since its question. In the
loopback, where a handle is the engine's value, nothing of this is carried out.

**The page's address and stores cross.** The client's first message, as it connects, says where its
page is (`location.href`, and its `<base href>`) and what its `localStorage` and `sessionStorage`
hold under the library's `haskell-halogen:` prefix; then it sends where the page is whenever it goes
back or forward, before the `popstate` the program hears, and another tab's change to the library's
items. The session mirrors all of it: the window's `location` and the document's `baseURI` answer
from it, `history.pushState`, `replaceState`, `back`, `forward` and `go` are sent as `history`
operations the page performs (the mirror moving at once for a push or a replacement), and the
storage methods of `MonadBrowserDOM` read the mirror and send `storage` operations. So
haskell-halogen-router's router, made in the session's monad, starts at the page's route, changes
the page's address with no page load, hears Back and Forward, and `pageBase` is the page's base;
and what a component stores is the page's own, which outlives the session. The server waits for the
client's first message before it calls anything for a connection; a client that sends something
else first gets a session at `/` with empty stores. The loopback hands its session the page's
first message before it mounts, which appears first on its `traffic`.
`Halogen.Remote.Test` serves every address under a key's address as that key's page, with a
`<base href>` naming it (the address `mounting` gives now ends with `/`), and `mountingEach`
registers a program, run in each new session's monad, whose component every connection mounts
afresh, with an action that waits for the next connection: a test opens its page at a route and
reloads it.
