# haskell-halogen-net

Browser networking for [haskell-halogen](https://github.com/Swordlash/haskell-halogen). `Halogen.Net.Fetch` wraps `fetch` using `Halogen.JSBits.Promise`. `Halogen.Net.WebSocket` and `Halogen.Net.EventSource` provide WebSockets and server-sent events through `Halogen.Subscription` emitters.

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

`request` creates a `Request` with browser defaults. Use record updates to set headers, credentials, mode and body. The body can be `TextBody` or `BytesBody`; encode JSON yourself if needed.

`fetch` waits for the response status and headers. `text` and `bytes` wait for the body, which can be read only once. These operations belong in `lift` because they wait.

To start a request during an event's dispatch, call `start` in `H.liftEffect`. It returns a handle you can keep in component state. Use `response` in `lift` to wait for the response, or `abort` to cancel the request from another action.

Browser failures return `Left JSError` with the original error name. `TypeError` covers failures such as invalid URLs, unreachable hosts, CORS restrictions and rejected headers or bodies. `AbortError` means the request was aborted. An HTTP error status is still a response, with `ok` set to `False`.

Interrupting a thread waiting in `fetch`, or in `text` or `bytes` for its response, aborts the request. This happens automatically when the waiting component is removed. Requests created with `start` require an explicit `abort` call.

On the native backend, request operations return `NotInBrowserError`. This package wraps browser networking; it does not provide a native HTTP client.

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

WebSocket and EventSource operations do not wait, so call them in `H.liftEffect`. `WS.open url protocols` begins connecting and returns a socket. `send` sends a `TextMessage` or a `BinaryMessage` containing a `ByteString`. `close` starts the closing handshake with a code and reason; `close'` uses neither.

Subscribe to the socket's `opened`, `messages`, `errors` and `closed` emitters with `H.subscribe`. The browser supplies no details for `errors`. The final event is always `closed`, including for failed connections, and contains the close `code`, `reason` and `clean` flag. Read `readyState` and `protocol` to inspect the connection.

`Halogen.Net.EventSource` provides a similar API for server-sent events. `SSE.open url withCredentials names` starts the connection. Its `events` emitter delivers unnamed events and events whose names appear in `names`, as `ServerEvent` values with `name`, `data_` and `lastEventId` fields. `opened` fires on every connection or reconnection. `errors` reports `Connecting` if the browser will retry, or `Closed` if it has stopped. Call `close` to stop the connection permanently.

Each emitter buffers events until its first subscriber arrives, then delivers them to that subscriber. You can therefore subscribe in a later action without losing startup events. After the first subscription, events arriving with no subscribers are discarded.

Emitters receive events in browser dispatch order. Removing a component ends its subscriptions but does not close its connections; close them in `Finalize`. The connection releases its callbacks after a socket closes or an event source closes or stops retrying.

Operations that the browser rejects immediately return `Left JSError`. Examples include invalid URLs, duplicate subprotocols, sending while a socket is still connecting and invalid close codes.

On the native backend, sockets and event sources remain `Closed`. Their emitters never fire, and sending or closing does nothing.

## Tests

`Halogen-net-test` checks request construction on every backend and native fallback behaviour. `Halogen-net-browser-test` uses `hspec-halogen` to make requests in headless Chromium against files in `test/web/`. For cases a static server cannot provide, `test/web/network-double.js` wraps `fetch`. It can hold a request, stop a body halfway or echo a request.

WebSocket and EventSource tests use `test/web/socket-double.js` because the runner only serves static files. For the host `double`, it substitutes connections the tests can open, send to, close or fail. It also records client operations.

These tests cover event ordering, binary data, close codes, errors, initial buffering, unsubscribe behaviour, callback cleanup and component finalization. They test the bindings rather than the wire protocols. Other hosts use the browser's real classes, which the suite exercises with invalid operations, an unavailable socket port, a missing stream and a response with the wrong content type.
