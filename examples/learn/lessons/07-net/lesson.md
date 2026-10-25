title: Fetching data
layers: net
args: -package haskell-halogen-net

A component that shows data from a server has to wait for it, and say something while it waits. This one fetches a list of quotes, `data/quotes.json`, which sits next to this page, with `Halogen.Net.Fetch`, the browser's `fetch`.

## Three states

Until the data arrives there is nothing to show, and it may never arrive. The state says which of the three it is:

```haskell
data State a = Loading | Done a | Failed Text
```

`render` shows each one, and `handleAction` moves between them: `put Loading`, then the fetch, then `Done` with the quotes or `Failed` with the reason. `H.initialize` starts the first `Load` when the component is mounted.

## Fetching

`getQuotes` does the work, in `IO`:

1. `Fetch.request Fetch.GET url` describes the request. The address is relative, so it is read against the page's own.
2. `Fetch.fetch` sends it and waits for the response's status and headers. What the browser refuses (no network, an address that is no address) comes back as a `Left`, never an exception, with the error's `name` and `message`.
3. A 404 is still a response, a `Right`. Its fields are read with dot syntax (`OverloadedRecordDot`): `response.ok` says whether the status is a success, and `response.status` is the number.
4. `Fetch.text` waits for the body, and `Aeson.eitherDecodeStrictText` reads the JSON into a list of `Quote`. Its `FromJSON` instance comes from `Generic`: the record's field names are the JSON's keys.

## Waiting without stopping the page

`handleAction` calls `getQuotes` through `liftIO`. A component's handler runs on the page's loop only up to its first wait: a `liftIO` runs on a thread of its own, so while the request is out, the page goes on rendering and answering clicks, and the handler takes up where it left off when the response arrives.

## Exercise

Add two buttons above the quotes, with `HE.onClick` as in lesson 2 (import `Halogen.HTML.Events qualified as HE`):

- **Reload**, whose click is `Load "data/quotes.json"`: the page shows Loading..., briefly, and the quotes again;
- **Load a missing file**, whose click is `Load "data/missing.json"`, an address with nothing at it. The server answers 404, and the page shows `Failed` with the status.
