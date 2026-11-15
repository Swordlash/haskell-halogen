title: Fetching data
layers: net
args: -package haskell-halogen-net

A component fetching server data needs to display loading, success and failure states. This example uses `Halogen.Net.Fetch`, a wrapper around browser `fetch`, to load quotes from `data/quotes.json`.

## Three states

The state records whether the request is pending, succeeded or failed:

```haskell
data State a = Loading | Done a | Failed Text
```

`render` handles all three cases. `handleAction` first calls `put Loading`, fetches the data, then stores `Done` with the quotes or `Failed` with an explanation. `H.onInitialize (handleAction (Load "data/quotes.json"))` starts the initial request when the component mounts.

## Fetching

`getQuotes` performs the request in `IO`:

1. `Fetch.request Fetch.GET url` constructs a request. Relative URLs resolve against the page's base URL.
2. `Fetch.fetch` sends it and waits for status and headers. Browser failures, such as network errors, return `Left` with an error `name` and `message`.
3. HTTP errors such as 404 still return `Right response`. With `OverloadedRecordDot`, read `response.ok` to check success and `response.status` for the numeric code.
4. `Fetch.text` reads the body. `Aeson.eitherDecodeStrictText` decodes it into `[Quote]`. The generic `FromJSON` instance uses record field names as JSON keys.

## Waiting without stopping the page

`handleAction` calls `getQuotes` through `liftIO`. The request waits on a separate Haskell thread while the component loop keeps rendering and handling actions. Once the response arrives, the handler resumes.

## Exercise

Import `Halogen.HTML.Events qualified as HE` and add two buttons above the quotes:

- Reload triggers `Load "data/quotes.json"`. It briefly shows Loading... before displaying the quotes again.
- Load a missing file triggers `Load "data/missing.json"`. The server returns 404, and the component shows the failure status.
