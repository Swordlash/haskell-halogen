# haskell-halogen-net

The browser's networking for [haskell-halogen](https://github.com/Swordlash/haskell-halogen),
on `Halogen.JSBits.Promise`. So far that is `fetch`, in `Halogen.Net.Fetch`.

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

## Tests

`Halogen-net-test` checks requests on every backend, and natively what fetching gives.
`Halogen-net-browser-test` makes real requests in headless Chromium, through the `hspec-halogen`
runner, against the files in `test/web/`; what the runner's file server cannot do (hold a request
until the test lets it go, stop a body halfway, echo a request back) comes from
`test/web/network-double.js`, which wraps the page's `fetch` before the suite starts.
