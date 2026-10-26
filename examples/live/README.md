# halogen-example-live

A board everyone who has it open writes on, sent over a wire with
[haskell-halogen-remote](../../remote): the component (`src/Example/Live.hs`) is an ordinary one,
and only its entry points say where its DOM is.

**Served natively.** One program holds the board and serves it to every page that opens it, a
session per tab, over a WebSocket; a line posted in one tab shows up in every other. From the
repository's root (the page's path is relative to it):

```sh
cabal run halogen-example-live-server --builddir=dist-newstyle/native
```

and open <http://localhost:8081/> in two tabs. The server serves `web/live.html`, the client
script at `/live.js`, and the socket at `/live`; the page needs nothing else.

**Looped back in a page.** `npm run serve-wasm -- live` builds `Main.hs` and serves it on :8080:
the same component, mounted in `RemoteDOM` with `Halogen.Remote.Loopback`, its operations going to
a client in the page and the page's events coming back, with every message shown beside it as it
crosses (`src/Example/Live/Wire.hs`). The tutorial's lesson 21 does the same with a component of
the reader's.
