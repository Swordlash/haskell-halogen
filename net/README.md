# haskell-halogen-net

The browser's networking for [haskell-halogen](https://github.com/Swordlash/haskell-halogen):
`fetch`, in `Halogen.Net.Fetch`, on `Halogen.JSBits.Promise`; and WebSocket and server-sent
events, in `Halogen.Net.WebSocket` and `Halogen.Net.EventSource`, whose events arrive through
`Halogen.Subscription` emitters.

```haskell
import Halogen.Net.Fetch

data Action = Load | Save Text

handleAction = \case
  -- Waits on a worker thread, while the rest of the tree goes on. If the
  -- component goes away first, the request is aborted.
  Load -> do
    outcome <- fetch (request GET "/api/notes") >>= either (pure . Left) text
    modify $ \st -> st {notes = outcome}
  Save json -> do
    _ <-
      fetch
        (request POST "/api/notes")
          { headers = [("content-type", "application/json")]
          , body = Just (TextBody json)
          }
    pure ()
```

A `Request` is a value: `request` gives one with the browser's defaults, and record update sets
its `headers`, its `body` (`TextBody` or `BytesBody`, encoded by the caller: the package has no
JSON of its own), its `credentials` and its `mode`.

`fetch` waits for the response's status and headers, and `text` or `bytes` for its body, which
can be read once. All of them wait, so they belong in `lift`, never in `H.liftEffect`. A
request that must start inside the dispatch of the event that asked for it, or that another
action should be able to abort, is started with `start` in `H.liftEffect`, which gives a handle
to keep in the state; `response` waits for it in `lift`, and `abort` aborts it.

What the browser rejects is a `Left JSError` with the browser's error name: `TypeError` for a
request that got no response (a bad address, a host that does not answer, CORS, a header or body
`fetch` refuses), `AbortError` for an aborted one. An HTTP error status is a response like any
other, with `ok` set to `False`.

Killing the thread that waits in `fetch` (or in `text` or `bytes` on the response it gave), as a
component's worker is killed when the component goes away, aborts the request. A handle from
`start` is aborted only by `abort`.

Natively there is no engine to make a request: everything gives `NotInBrowserError`. The package
is no HTTP client of its own.

## WebSocket and server-sent events

```haskell
import Halogen.Net.WebSocket qualified as WS

data Action = Initialize | Received WS.Message | Ended WS.Close | Say Text | Finalize

handleAction = \case
  -- Opening waits for nothing, so it goes in an effect; what the socket
  -- does afterwards arrives through its emitters.
  Initialize ->
    H.liftEffect (WS.open "wss://example.com/chat" []) >>= \case
      Left refused -> modify $ \st -> st {problem = Just refused}
      Right socket -> do
        modify $ \st -> st {socket = Just socket}
        void $ H.subscribe (Received <$> WS.messages socket)
        void $ H.subscribe (Ended <$> WS.closed socket)
  Received message -> modify $ \st -> st {heard = st.heard <> [message]}
  Ended _ -> modify $ \st -> st {socket = Nothing}
  Say line -> gets (.socket) >>= traverse_ (\s -> H.liftEffect (WS.send s (WS.TextMessage line)))
  -- The component's subscriptions end with it; its socket does not.
  Finalize -> gets (.socket) >>= traverse_ (H.liftEffect . WS.close')
```

Nothing in either module waits. `WS.open url protocols` starts connecting and returns the
socket; `send` hands the browser a `TextMessage` or a `BinaryMessage` (a `ByteString`); `close`
starts the closing handshake with a code and a reason, and `close'` with neither. All of them
belong in `H.liftEffect`. The socket's events arrive through emitters for `H.subscribe`:
`opened`, `messages`, `errors` (the browser gives no detail, so neither does this) and `closed`,
which carries the close's `code`, `reason` and whether it was `clean`, and is the last event of
every socket, failed ones included. `readyState` and `protocol` say where it is now.

`Halogen.Net.EventSource` is the same shape for a stream of server-sent events:
`SSE.open url withCredentials names` starts the request, `events` carries every `ServerEvent`
(its `name`, `data_` and `lastEventId`) the server did not name and those it named with one of
`names`, `opened` fires on each (re)connection, `errors` says whether the browser will reconnect
(`Connecting`) or has given up (`Closed`), and `close` stops it for good.

Each emitter keeps what arrives before its first subscriber and hands it to that subscriber, so
a component that subscribes in a later action than the one that opened the connection, or is
handed one a program opened, misses nothing; after that, what arrives while nobody is subscribed
goes to nobody. The emitters are given the events while the browser dispatches them, in its
order. A component's subscriptions end when it goes away, but the connection is the program's,
and the component closes it in its `Finalize`. A connection's callbacks are freed once it has
ended: after a socket's `closed`, or when a source is closed or given up on.

What the browser refuses at once is a `Left JSError`: an address that is no WebSocket or event
stream address, a subprotocol asked for twice; a `send`
on a socket still connecting; a close code a page may not use. Natively nothing connects: the
socket or source is made, its emitters never fire, its `readyState` is `Closed`, and sending
and closing do nothing.

## Tests

`Halogen-net-test` checks requests on every backend, and natively what fetching, a socket and
an event source give. `Halogen-net-browser-test` makes real requests in headless Chromium,
through the `hspec-halogen` runner, against the files in `test/web/`; what the runner's file
server cannot do (hold a request until the test lets it go, stop a body halfway, echo a request
back) comes from `test/web/network-double.js`, which wraps the page's `fetch` before the suite
starts.

The runner's server is a static file server, and no WebSocket or event-stream server, so the
suite's sockets and sources are made against `test/web/socket-double.js`, which replaces the
page's `WebSocket` and `EventSource` for addresses on the host `double` with fakes the test
drives as a server would (opening, sending, closing, failing; or echoing what the page sends),
and which record what the page did. That tests this package's wiring (the order events arrive
in, bytes both ways, close codes, failures, what is kept before anyone subscribes, unsubscribing,
listeners taken off a finished connection, a component closing its socket as it goes) but not
the protocols themselves. Every other address goes to the browser's own classes, which the suite
uses for what the browser refuses and how a connection fails: a socket to a port nothing listens
on, a stream that is not there or is no event stream.
