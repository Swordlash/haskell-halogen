# Changelog for `haskell-halogen-net`

## Unreleased

The first version, with the browser's `fetch` in `Halogen.Net.Fetch`, built on
`Halogen.JSBits.Promise`, and its WebSocket and server-sent events.

A `Request` is a value, made with `request method url` and changed with
record update: its `headers`, a `body` (`TextBody` or `BytesBody`, which the
caller has encoded; there is no JSON here), its `credentials` and its `mode`.
`fetch` makes it and waits for the response's status and headers; the
`Response` has `status`, `statusText`, `ok`, `url` and `headers`, and its body
is read once, with `text` or `bytes`. A rejection is a `Left JSError`, named
as the browser names it (`TypeError` for a request that got no response,
`AbortError` for an aborted one), and an HTTP error status is a response like
any other, whose `ok` is `False`.

`fetch` and the body reads wait, so they belong in a component's `lift`.
Killing the thread that waits, as a component's worker is killed when the
component goes away, aborts the request, on both browser backends. For a
request that has to start inside an event's dispatch, `start` begins it
without waiting, in `H.liftEffect`, and gives a `Fetch` handle: `response`
waits for it later, and `abort` aborts it. A handle is aborted only by
`abort`, since more than one wait may share it.

Natively there is no engine to make a request, and every request gives
`JSError "NotInBrowserError" _`.

The browser's WebSocket and server-sent events come with it, in
`Halogen.Net.WebSocket` and `Halogen.Net.EventSource`, which makes the package
depend on `haskell-halogen-core` for `Halogen.Subscription`. Both are meant to
be imported qualified, since their names (`open`, `close`, `opened`,
`errors`) are shared.

Neither waits for anything. `WebSocket.open url protocols` starts connecting
and returns the `Socket`, in `H.liftEffect`; `send` hands the browser a
`TextMessage` or a `BinaryMessage`, `close` starts the closing handshake with a
code and a reason (`close'` with neither), and `readyState` and `protocol` say
where it is. What the socket does afterwards arrives through emitters for
`H.subscribe`: `opened`, `messages`, `errors`, and `closed`, which carries a
`Close` with its `code`, `reason` and whether it was `clean`, and ends every
socket, including one that failed (an `errors` event first, then a close with
code 1006). `EventSource.open url withCredentials names` does the same for a
stream of server-sent events: `events` carries each `ServerEvent` (`name`,
`data_`, `lastEventId`) that the server left unnamed or named with one of
`names`, `opened` fires on every connection and reconnection, `errors` says
whether the browser will reconnect or has given up, and `close` stops the
source for good.

Each emitter keeps what arrives before its first subscriber and hands it to
that subscriber, so a component that subscribes in a later action, or is
handed a connection a program opened, still hears the `open` and the first
messages. Once an emitter has had a subscriber it keeps nothing more. The
emitters are given the events while the browser dispatches them, in its order.
A component's subscriptions end when it goes away but its connection does not:
close it in `Finalize`. The callbacks a connection hands the browser are freed
once it has ended.

What the browser refuses on the spot, which it throws in JavaScript, is a
`Left JSError` instead: an address that is no WebSocket or event-stream
address, a subprotocol asked for twice, a `send` on a socket still connecting, a close code a page may not
use. Natively nothing connects: `open` gives a connection whose emitters never
fire and whose `readyState` is `Closed`, and sending and closing do nothing.
