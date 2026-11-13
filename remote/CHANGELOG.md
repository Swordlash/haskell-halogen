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

`Halogen.Remote.Loopback.mountIn` runs the same pipeline inside one page, with every message
emitted on `traffic` both ways, and `mountWatched` hands every message, the first render's
included, to a watcher given before anything crosses; natively they throw `NotAvailable`. A
component that fails to mount leaves the element without a client.

The client (`jsbits/remote.js`) prevents the default of every `submit` it listens for, and of a
`click` on an `a` element it listens for only when the browser would follow that click in this
tab, since a handler hears of an event too late to; a handler declared with
`HE.preventingDefault` or `HE.stoppingPropagation` runs as any other, its preventing and stopping
doing nothing.

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
