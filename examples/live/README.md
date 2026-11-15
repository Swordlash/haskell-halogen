# halogen-example-live

A shared board built with [haskell-halogen-remote](../../remote). The component in `src/Example/Live.hs` is an ordinary Halogen component; its entry points choose whether to serve it over a WebSocket or run it in browser loopback mode.

**Native server.** One process stores the board and creates a session for each browser tab. Posting a line in one tab updates all others. Run from the repository root so the server can find the page:

```sh
cabal run halogen-example-live-server --builddir=dist-newstyle/native
```

Open <http://localhost:8081/> in two tabs. The server provides `web/live.html`, the client at `/live.js` and the WebSocket at `/live`. No additional page assets are needed.

**Browser loopback.** Run `npm run serve-wasm -- live` to build `Main.hs` and serve it on port 8080. `Halogen.Remote.Loopback` mounts the same component in `RemoteDOM` and exchanges messages with a client in the page. `src/Example/Live/Wire.hs` displays the traffic beside it. Tutorial lesson 21 uses the same approach for the reader's component.
