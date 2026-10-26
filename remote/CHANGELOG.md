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
listener that has gone is dropped. `Halogen.Remote.Protocol` says what crosses, as compact JSON:
an operation is an array with its name first (`["create", 3, null, "div"]`), an event an object
naming its listener and node with the fields handlers read. A node taken out and not put back by
the end of a batch is forgotten, on both sides, so a child component whose parent replaces the
element around it keeps its node. `Halogen.Remote.Internal` is the recorder itself, for tests and
tools.

`Halogen.Remote.Server` (native) serves a component over WebSockets with warp and wai-websockets,
a session per connection, with the client at the socket's path and `.js` and the pages it is
given beside it; `application` is the same as a WAI application. A program that throws ends its
own session, and a connection that closes disposes of its session.

`Halogen.Remote.Loopback.mountIn` runs the same pipeline inside one page, with every message
emitted on `traffic` both ways, and `mountWatched` hands every message, the first render's
included, to a watcher given before anything crosses; natively they throw `NotAvailable`.

The client (`jsbits/remote.js`) prevents the default of every `submit`, and of every `click` on
an `a` element, that it listens for, since a handler hears of an event too late to; refs are
nodes of the mirror, focus and selection stay the page's, and nothing is hydrated.
