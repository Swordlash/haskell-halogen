# Changelog for `haskell-halogen-net`

## Unreleased

The first version, with the browser's `fetch` in `Halogen.Net.Fetch`, built on
`Halogen.JSBits.Promise`.

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
