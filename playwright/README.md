# haskell-halogen-playwright

Test a built program from outside the page, in Haskell. A native hspec suite
drives Chromium through [Playwright](https://playwright.dev): it opens pages,
navigates, finds elements, clicks and types, and checks what it reads. After
every action it waits for the page's
[haskell-halogen](https://github.com/Swordlash/haskell-halogen) program to
say it has finished reacting, so a test reads what a click rendered at once,
without sleeping and without retrying.

```haskell
import Halogen.Playwright
import Halogen.Playwright.Hspec

main :: IO ()
main = hspec $ playwright $ describe "the counter" $ do
  it "counts clicks" $ inPage $ \page -> do
    visit page "http://127.0.0.1:8080/"
    click (byRoleNamed page "button" "+")
    -- The click's handler has run and the page shows what it rendered.
    textContent (locator page ".count") `shouldReturn` "1"

  it "keeps the count in the address" $ inPage $ \page -> do
    visit page "http://127.0.0.1:8080/"
    click (byRoleNamed page "button" "+")
    shouldHaveURL page "http://127.0.0.1:8080/?count=1"
```

## Inside the page, or outside it

[hspec-halogen](../hspec-halogen/) runs the spec *inside* the page: it mounts
one component, holds it, queries it and sees its outputs, and is fast. It
cannot navigate, hold two pages, or test a program whose component is not in
the page at all.

This package runs the spec *outside*: the test is a native process and the
page is the program as it is built and served, prerendered page and all. Use
it for what a user meets: a built example or application, several tabs of
one, history, storage, a server whose component is drawn in the page over a
wire ([haskell-halogen-remote](../remote/), whose `Halogen-remote-live-test`
uses this package).

## Setup

It runs `node` with the npm package `playwright`, looked up from the working
directory upwards and then in `NODE_PATH`, as the project's own:

```sh
npm install --save-dev playwright
npx playwright install chromium
```

Without either, `withPlaywright` fails with a `PlaywrightError` that says
which is missing and how to install it. The package is native only (it does
not build for the browser backends) and depends on none of the
haskell-halogen packages: it drives any page, and speaks to a Halogen program
only through the probe below.

The driver's JavaScript is compiled into the library and written to a
temporary directory when it starts. It speaks a small protocol of JSON lines
on node's stdin and stdout (see `js/driver.mjs`); node's stderr is relayed to
the test's, each line prefixed with `[playwright]`. node ends itself, closing
Chromium first, when its stdin ends, which happens however the test process
ends, `kill -9` included; `withPlaywright` also stops it when its scope ends
for any reason.

`optionsFromEnvironment` (which `playwright` uses) reads:

| Variable | Meaning |
| --- | --- |
| `HALOGEN_PLAYWRIGHT_HEADED` | show the browser |
| `HALOGEN_PLAYWRIGHT_SLOWMO` | milliseconds to wait before every action |
| `HALOGEN_PLAYWRIGHT_TIMEOUT` | milliseconds an action, wait or expectation may take (5000) |
| `HALOGEN_PLAYWRIGHT_NODE` | the `node` to run |
| `HALOGEN_PLAYWRIGHT_ARTIFACTS` | where a failing test's screenshot is saved |
| `HALOGEN_PLAYWRIGHT_CONSOLE` | print what pages write to their console, and what they throw, as it arrives |
| `HALOGEN_PLAYWRIGHT_THROTTLE` | slow every page's processor down by this factor (`4`: four times as slow), popups too |
| `HALOGEN_PLAYWRIGHT_ARGS` | more arguments for Chromium, separated by blanks |

A run with `HALOGEN_PLAYWRIGHT_THROTTLE=6` is a run of the same tests on a slow
machine: a test that only passed because the program was quick enough
fails there, so races are found on purpose rather than in CI. `Options` also
has `browserArgs`, Chromium's command-line switches (a
`--host-resolver-rules` that sends a made-up host name to the loopback, say).

Chromium turns some of its behaviour on by machine, so a test can pass on one
and fail on another with the same browser. One that has mattered here: whether
a navigation gives the frame a new host (its `RenderDocument` feature), under
which the last requests of the document that goes (a beacon sent in `pagehide`)
are sent and reported to nobody. `HALOGEN_PLAYWRIGHT_ARGS` runs the tests the
other way: `--enable-features=RenderDocument:level/all-frames`, or
`--disable-features=RenderDocument`.

## The waits

- **`settle page`** waits for a task of the page's at background priority (a
  browser event reaches a WebAssembly program as a task of its own, so its
  handler may not have started when Playwright's click returns), then asks
  the page's program until it is idle, a background task between asks. Then
  it waits for a move through the history the page asked for and the browser
  has not made yet (a handler's `history.back()`, whose `popstate` comes a
  moment later), for the page's next animation frame and a task after it
  (the browser dispatches some events with the frame: a dialog's `close`
  after Escape or `close()`, `resize`, a `ResizeObserver`'s callback), and
  asks the program again, starting over if the event made it busy. Every
  action (`click`, `fill`, `typeText`, `press`, `goto`, ...) ends with it. A
  navigation that takes the document away is not an error. An action on an
  element in a frame (`inFrame`, `frameAt`) settles the frame's document
  first, which has a program of its own, and then the page's. On a page with no
  Halogen program it waits for the task, the move and the frame. A document
  that may not run scripts (a frame sandboxed without `allow-scripts`, as
  untrusted markup is shown) is left alone: nothing of the driver's runs in
  it, since the browser would refuse and write an error to the page's
  console each time, and an action in it settles at once. A hidden
  page draws no frames and is not waited for; a page whose frame does not
  come within 100 ms is waited for 20 ms at most from then on, until it
  draws one. The frame
  costs an action about a dozen milliseconds. To know of a move through the
  history as the page asks for it, the driver's init script wraps
  `History.prototype.back`, `forward` and `go` in every page: they call the
  page's own and keep their names and lengths, and their `toString()` is no
  longer native code's. JavaScript the test runs in the
  page (`evaluate`, `evaluateWith`, `evaluateOn`, `evaluateAll`) and `cdp` are
  not actions and do not settle: after one that dispatches an event to the
  program (a `blur`, a `mouseup`, a link clicked from script), call `settle`.
  Each fails after the timeout if its promise never settles.
- **`waitStarted page`** waits until a program in the page has mounted a
  tree and is idle: the prerendered page has been adopted and every
  `Initialize` has run its synchronous part. `visit` is `goto` and then this.
  It fails after the timeout on a page where no program answers.
- **`waitQuiet page`** waits for idle and for every fiber waiting in a
  `liftIO` to come back (a save's request, say), but not for one pausing in
  `H.delay` (below). Its timeout says how many were still waiting.
- **`activity page`** is the program's answer itself, `Nothing` where there
  is none.

What these do not cover is what is not the program's yet: a timer that has
not fired, a message on its way over a WebSocket, an animation, a Web Worker
starting or ending. For those the `should` expectations retry until the
timeout, `eventually page` retries any expectation, and
`waitForFunction page expression` polls a JavaScript expression in the page,
at its animation frames, until it holds (`waitForFunctionWith page function
argument` takes the value as an argument rather than spliced into the
JavaScript). `awaitText locator test` reads an element's text once it
satisfies a test, and its failure says what the text last was.
`waitForURL page address` waits for the page's address (exactly, or a glob)
after a link the program follows, and settles. `waitForEvent target type
action` runs an action and waits until the target (`OnWindow page`,
`OnDocument page` or `OnElement locator`) has heard an event of the type,
then settles: for an event the page causes later by itself (a timer of its
own that goes back, a worker's `message`). Its listener is added in the page
before the action and after the page's own; none within the timeout is a
`TimeoutError` naming the event, and a document that goes first is an error
named `NavigatedAway`.

```haskell
waitForEvent (OnWindow page) "popstate" (click (byRoleNamed page "button" "Back in a moment"))
```
 `waitForConsole page test` is
the first console message, from the page's start, that satisfies a test,
waiting for one if there is none yet: one written before the call counts, so
the message an action caused cannot be missed. A `ConsoleMessage` has the
address it was written at (`url`, a frame's for a message one of the page's
frames wrote) and its `line`, so a test can leave out what an interactive
frame's sandbox refuses: `not (maybe False ("?interactive" `Text.isInfixOf`) m.url)`.

Every wait works on a page sent with a Content Security Policy that refuses
`eval` (`script-src 'self'`, no `'unsafe-eval'`), as a real server's pages
are. Playwright's own `waitForFunction` compiles its expression in the page
with `eval`, which such a policy refuses (an uncaught `EvalError`, and a wait
that never ends); this package's compiles the expression together with the loop
that polls it, through the DevTools protocol, which the page's policy does not
govern. The probe is an init script, which the policy does not govern either,
and `evaluate` and its kin go through the protocol too.

`waitForWebSocket page test` is the same for the frames of the page's
WebSockets, which are kept as they cross, both ways (`webSocketFrames page`):
the first, from the page's start, that satisfies a test, so the server's
answer to a subscription the page made while it loaded is found however early
it came. A test that wants a later one tells it apart by its payload. On
timeout it lists the last frames that crossed.

```haskell
-- The second tab has subscribed: a comment made in the first is pushed to it.
_ <- waitForWebSocket watcher (\f -> not f.sent && "\"subscribed\"" `Text.isInfixOf` f.payload)
```

### Pauses: what `waitQuiet` waits for is the program's to say

`waitQuiet` waits for every fiber in a `liftIO`, and a timer written
`liftIO (threadDelay n)` is one: it waits for the timer. A program says which
of its pauses a test should wait for by how it writes them:

- a pause followed by work a test waits for (a debounce before a search
  request, a draft saved two seconds after the last key) is
  `liftIO (threadDelay n)`, and `waitQuiet` after the typing returns once the
  request has been answered;
- a pause that only takes something off the screen or ticks (a "Saved" notice
  hidden after three seconds, a clock, a ticker while a model works) is
  `H.delay` (`Hooks.delay` in hooks, a `NominalDiffTime` in seconds), which the
  probe does not count: `waitQuiet` returns while the notice is still shown,
  so the test can read it, and a `forever` ticker leaves the page quiet
  between ticks. Written with `liftIO`, the notice is gone by the time
  `waitQuiet` returns, and a ticker keeps it from ever returning.

A wait for a response (Playwright's `waitForResponse`) is not here, and
`waitQuiet` is not always one. After a save that shows a "Saved" notice for
three seconds, `waitQuiet` returns once the save has been answered only if the
notice's timer is an `H.delay`; with `liftIO (threadDelay n)` it waits for the
notice to go too, and the notice is never seen. Where the program cannot be
changed, check what the response did (the saved document, through the
server's API) after `waitQuiet`, rather than the notice; or wait for the notice
itself with `awaitText`, after the action's `settle`.

Every call has the timeout of the options (`HALOGEN_PLAYWRIGHT_TIMEOUT`, five
seconds). `withTimeout ms page` is the same page with another, for its calls
and those of the locators made from it, `settle` and the retrying
expectations included: for the one call that takes longer than the rest.

```haskell
visit (withTimeout 30000 page) address      -- a program of megabytes loading
click (locator (withTimeout 60000 page) "#count")
waitQuiet (withTimeout 60000 page)          -- a job of seconds in a Web Worker
```

## The probe

Every page the driver opens has `globalThis.__halogenProbe = {}` before its
own scripts run. A program built with a haskell-halogen-core that has
`Halogen.Activity` gives it, when it mounts its first tree,

```js
await __halogenProbe.activity()   // {trees, running, waiting}
```

`trees` is the trees mounted and not disposed of, `running` those running or
with work queued, `waiting` the fibers waiting in a `liftIO` (not those
pausing in `H.delay`). Idle is
`running == 0`; the answer reads the program's loops when asked, so it is
exact. Any tool can set the probe up and ask it; see `Halogen.Activity`'s
documentation for the contract.

A component need not be in the page for this: `haskell-halogen-remote`'s
`Halogen.Remote.Test` mounts a component in the test's own process, natively,
and shows it in a page through its client, which forwards the probe's question
to the component's session over the wire. The same driver then tests a
component that runs in the test's process, settling every action on that
component alone; see remote's README, "Testing a component in a real page".

## Serving a built directory

`Halogen.Playwright.Serve` serves a directory on a free port for a test's
duration, as a static host does: a directory's address serves its
`index.html` (without its last slash, it is redirected to the address with
one), files go with their media types (`application/wasm` among them, which
`WebAssembly.instantiateStreaming` insists on), `HEAD` gets what `GET`
would, and nothing else is sent: no cross-origin isolation, no caching
headers.

```haskell
main = serveDirectory "dist-newstyle/wasm/public/vanilla" $ \base ->
  hspec $ playwright $ it "starts" $ inPage $ \page -> visit page base
```

`serveDirectoryWith` takes a base path to serve it under (as GitHub Pages
serves a project's site, outside of which there is nothing), a `404.html` to
serve for an address with no file, and other directories mounted under the
base.

## A prerendered page, adopted

A page rendered ahead of time (`Halogen.Static`) is adopted by its program
(`hydrateUI`): the program takes the nodes the server sent as its own. If
the prerender got something wrong, the program repairs it, and the reader
sees the page change as it starts. `shouldAdoptCleanly page address` checks
that it does not:

```haskell
it "adopts the prerendered page with nothing repaired" $ inPage $ \page ->
  shouldAdoptCleanly page base
```

It holds the program's request (`**/app.wasm`), goes to the address, marks
every node under `#app` as the server sent it (its text, or its attributes),
lets the program go, waits for it to start (`waitStarted`: the adopting
render and every `Initialize`'s synchronous part have run), and compares.
The gate stays, released: the program's own requests start as it does, and
taking a route away while a page's requests are under way can leave them
stalled.
`adopt` and `adoptWith` give what changed as data, a `Repair` for each:
`Replaced` (a node gone, another where it was), `Removed`, `Built` (the
outermost of what went or was built), `TextChanged` and `AttributeChanged`,
each with a path to the node (`#app > div.learn > p:nth-child(2) > text()`)
and its HTML or text; `describeRepair` makes one a line of a report. The
space between tags and the comments that keep two texts apart, which the
HTML parser makes and a render does not, are not repairs.

`AdoptionOptions` say where to look and what to leave alone:

```haskell
defaultAdoptionOptions
  { root = "#app"
  , program = "**/app.wasm"
  , leaveOut = [".learn-editor"]                 -- not looked at: Initialize puts an editor there
  , leaveOutContents = [".wiki-math"]            -- kept by the program, filled by another library
  , ignoredAttributes = ["data-stuck"]           -- written by the page's own scripts
  }
```

What the program changes once it has started (a timer's render, a
response's) is not compared. What its `Initialize` changes at once is: a part
it fills on purpose (an editor in an empty element, a status line) goes in
`leaveOut`, which is then not looked at at all, or in `leaveOutContents`,
whose element must still be the one the server sent, with its attributes,
whatever is in it. `adoptWith` leaves its released gate on the page, so a
test that adopts many addresses on one page holds one gate of its own and
arms it again for each (`rearm`), around the two halves, which are also for
a test with more to do between marking and comparing (looking at what the
server sent, holding an API as well):

```haskell
gate <- hold page "**/app.wasm"
goto page address
waitHeld gate
-- the page as the server sent it
markServed defaultAdoptionOptions page
release gate
waitStarted page
repairsSinceMarked page >>= shouldHaveNoRepairs
```

## Contexts: init scripts, requests and pages

`addInitScript context js` runs a script of the test's own in every page of
the context before the page's own scripts, after the probe: to stand in for
something the page would find (a `Worker` that refuses to start, say).
`requests context` is what the context's pages and their Web Workers have
asked the network for so far (address, method, resource type), kept as they
are made: a service worker's requests are its context's and no page's, which
is why it is asked of the context. `workers page` is how many dedicated Web
Workers the page has alive.

Each `Request` also says who asked: `page` (a page's `pageNumber`;
`requestedBy page request` compares), `document`, the identity of the
document that asked, and `documentUrl`, its address. A document's identity
is the browser's (the DevTools protocol's loader id): new for every document
a frame loads, the same across a state the page pushed or a fragment. A
navigation's own request is the document it loads; a frame's requests are its
own document's; a request the document made before the page went elsewhere
stays the document's however late it is reported. `currentDocument page` is
the document the page shows now and `documents page` its frames' too, and
`requestsOf page` is the requests of the documents the page shows now: on a
page shared between examples, what the document a test went to asked for,
without going to `about:blank` first.

```haskell
goto page (base <> "no-picture.html")
fonts <- filter (\r -> r.resourceType == "font") <$> requestsOf page
fonts `shouldBe` []
```

How sure that is: the driver matches each request Playwright reports with the
browser's own report of it on a session of the page's (the same address and
method, both the main frame's or neither, both a navigation's own request or
neither, and never a report by a document that was not yet its frame's when
the request was heard; first come first matched), which names the document
exactly. What the
page's session does not report keeps the document its frame had when the
driver heard of it: a dedicated worker's requests (right unless the worker
asked just before its document went), and a frame's in another process (a
cross-site `iframe`). A service worker's requests have no page and no
document. And what a document asks for as it goes (a beacon sent in
`pagehide`) may be in no log at all: when a navigation gives the frame a new
host, which Chromium does on some machines and not on others, the browser
sends such a request and reports it to nobody.

A context opened with `recordExchanges = Just limit` also keeps each request,
once it has ended, with what was sent and what came back: `exchanges context`.
An `Exchange` has the request's address, method, resource type, page and
document, every header
it was sent with (`cookie` among them, which a page's script never sees), its
body, and the response's status, headers (a `set-cookie` that came twice is
there twice) and body, kept when it is at most `limit` bytes; or why it failed.
It is opt-in because it costs the driver two more questions to the browser per
request and the memory of what it keeps. `waitForExchange context test` is the
first that satisfies a test, from the context's start, waiting for one if there
is none yet: an exchange is kept a moment after the page has its response, once
its body has been read, so the one an action caused is waited for, not read.

```haskell
withContext pw defaultContextOptions {recordExchanges = Just 65536} $ \ctx -> do
  ...
  click (byRoleNamed page "button" "Save")
  saved <- waitForExchange ctx (\e -> e.method == "POST" && "/api/save" `Text.isSuffixOf` e.url)
  saved.status `shouldBe` Just 200
  lookup "cookie" saved.requestHeaders `shouldBe` Just ("session=" <> token)
  (decodeStrict =<< saved.responseBody) `shouldBe` Just (object ["saved" .= True])
```

`pages context` lists the context's pages, those its pages opened among them.
`waitForPopup page action` runs an action that opens a page (`window.open`, a
link to a new tab, a sign-in window) and gives the page it opened, once it has
loaded and settled. The wait is armed before the action, so the popup cannot be
missed. A popup is a page of the context like any other: it has the probe and
the context's init scripts, routes and exposed functions, the options'
throttle, a console of its own, and it closes with the context. A page that
closes itself as a click is handled (a sign-in window that posts back to its
opener) is no error for the click.

```haskell
(_, signIn) <- waitForPopup page (click (byRoleNamed page "button" "Sign in"))
fill (byLabel signIn "Name") "ada"
click (byRoleNamed signIn "button" "Sign in")   -- the window closes
locator page "#user" `shouldHaveText` "ada"
```

## Requests answered by the test

`route page glob handler` (or `route context glob handler`) hands each
request whose address matches a glob (Playwright's: `**` any characters, `*`
any but `/`) to a handler of the test's, which says what becomes of it:

```haskell
-- Another site's photograph answered here, so that the test needs no network.
_ <- route page "https://upload.wikimedia.org/**" $ \request ->
  pure $ if ".webm" `Text.isSuffixOf` request.url
    then fulfill 204 Nothing ""
    else fulfill 200 (Just "image/png") standInImage

-- The server out of reach.
unreachable <- route ctx (origin <> "/**") (\_ -> pure (Abort "connectionrefused"))
...
unroute unreachable
```

`Continue` lets the request go on (to the route added before, or the
network), `Fulfill response` answers it, `Abort code` fails it with
Playwright's error code. The handler is given the address, the method, the
resource type, the headers (`allHeaders` has every one the browser sends,
cookies included) and the body, so watching what was asked while letting it
through is a handler that records and continues.

To see what the server answered, a handler sends the request on and is given
the response, `fetchResponse request`: its status, its headers and its body (as
the page would read it, decoded, so without the headers that described the
bytes on the wire), to look at or to change before it answers `Fulfill` with
it. `passThrough` is the handler that only looks:

```haskell
-- The server's settings, with the dark theme on.
_ <- route page "**/api/settings" $ \request -> do
  r <- fetchResponse request
  pure (Fulfill r {body = "{\"theme\": \"dark\"}"})

-- Every answer of the API's, kept, and given to the page as it came.
_ <- route page "**/api/**" $ passThrough $ \request response ->
  atomicModifyIORef' answers (\as -> (as <> [(request.url, response.status, response.body)], ()))
```

`routeWith` and `holdWith` take a `RouteFilter`: a glob, and, when they are not
empty, the resource types (`fetch`, `image`, `document`, ...) and the methods
it takes. The driver decides them before the test is asked, so a request they
leave out costs nothing and goes on as if the route were not there. A gate on
an API's `fetch` requests holds them while the page's images under the same
path load, and `goto`, which waits for the page's `load`, returns:

```haskell
gate <- holdWith page (routeFilter "**/api/**") {resourceTypes = ["fetch"]}
_ <- routeWith page (routeFilter "**/api/**") {methods = ["POST"]} (\_ -> pure (fulfill 503 Nothing ""))
```

Each request's handler runs on a thread of its own, so a handler may block:
wait for the test to let it go, or for a moment. A handler that throws aborts
its request; what it threw is written to stderr and kept, and an `inPage`
failure lists it with the context's requests that failed. When the route is
taken away (`unroute`), or its page or context closes, or the driver ends,
the handlers still under way are killed and their requests go on as if the
route had not been there: the driver lets them go before Playwright's route is
removed, so nothing waits on a handler that will not answer. A request a
service worker makes, or answers from its cache, passes no page's route: open
a context with `serviceWorkers = False` when that matters.

A gate is a route whose handler waits:

```haskell
gate <- hold page "**/app.wasm"
goto page address
waitHeld gate                 -- the page asked for it
... what the server sent, before the program ran ...
release gate                  -- held requests go on, later ones pass straight through
waitStarted page
```

`heldCount gate` is how many requests it holds now; `waitHeld` fails after
the page's timeout if none comes. One gate serves a walk that holds every
load: `rearm gate` before the next `goto` or `reload` holds again, and
`closeGate gate` takes the route away at the end (what it holds goes on), so
released routes do not pile up.

On the wire this is the protocol's third kind of message: node asks
(`{"ask": 12, "route": 3, "request": {...}}`) and Haskell answers
(`{"answer": 12, "action": "fulfill", ...}`), bodies as base64; see the top of
`js/driver.mjs`. The thread that reads node's output never runs a handler and
never writes.

## Functions the page calls

`exposeFunction page name f` (or on a context, for all its pages) gives the page
`window[name]`, a function of the test's: the page calls it with one argument and
gets a promise of the result, both JSON, as a page calls a native app's bridge or
reports to a test harness. It is an ask too (`{"ask": 13, "exposed": 5,
"argument": ...}`), so each call runs on a thread of its own and may block while
others are answered. What the function throws (an argument that does not decode
too) rejects the page's promise with its message, is written to stderr and kept,
and an `inPage` failure lists it under "Exposed functions that failed". It is
there for every document the page loads after, until the page or the context
closes.

```haskell
exposeFunction page "reportScore" $ \(score :: Int) -> writeIORef best score
-- in the page: await window.reportScore(42)
```

## Files

`download page action` runs an action that downloads a file (a click on an
export button, or a `goto` to an address that is a download, which then does
not fail) and gives the download once it is saved whole: its suggested file
name, its address, `downloadBytes` and `saveDownload`. The wait for it is armed
before the action, so the event cannot be missed. `setInputFiles locator`
chooses the files of a file input, from disk (`UploadFile path`) or from
bytes the test made (`UploadBytes {name, mimeType, bytes}`).

```haskell
exported <- download page (click (locator page ".demo-export"))
zip <- downloadBytes exported
setInputFiles (locator page ".demo-import input")
  [UploadBytes {name = exported.suggestedFilename, mimeType = "application/zip", bytes = zip}]
```

## A context's state, and what the page is made to see

`cookies ctx`, `addCookies ctx` and `clearCookies ctx` are the context's
session; `cookie name value url` makes one for a site, so handing a session
to a fresh context is `addCookies fresh [cookie "wiki_session" token origin]`.
`setOffline ctx True` takes the context off the network (its pages hear
`offline`, and are settled). `emulateMedia page Print`, `Screen` or
`Scheme Dark` changes what media queries see, and settles once the page has
heard the change; `pdf page` prints the page with its CSS's page size.
`ContextOptions` have `permissions` (`["clipboard-read", "clipboard-write"]`),
`isMobile` and `hasTouch` for a phone (and `tap locator` there), and
`acceptDialogs`: every dialog a page opens is answered at once (dismissed, as
Playwright does, unless this says accept, which a `beforeunload` asking
whether to leave needs) and kept, `dialogs page`.

## Navigating, and pictures

`goBack page` and `goForward page` go one entry through the page's history and
return for every kind of entry, then settle: another document (once it has
loaded, as `goto`), a state the page pushed or a fragment (the same document,
which has had its `popstate` or `hashchange`), or a frame's own navigation
(Back undoes it in the frame alone, and the page does not load). No entry to go
to is a `PlaywrightError` named `NoHistoryEntry`.

`gotoStatus page address` is `goto` and the status of the response the
navigation ended with: `Just 404` for a server's not-found page, `Just 401`
for a page that is not the visitor's, `Nothing` for a navigation within the
document or an address that became a download. It works with JavaScript off.

`screenshot page path` is the whole page; `screenshotWith page
defaultScreenshotOptions {fullPage = False} path` is the window, the size of
the context's `viewport`. Neither settles. `reducedMotion = True` in the
context's options makes `prefers-reduced-motion: reduce` match, so a page that
animates only when it may shows its end state in the picture.

## Keys, the mouse and where things are

`typeKeys page text` types where the focus is (after a key that opened a
search box), `insertText page text` puts text there as a paste would.
`mouseMove page x y` and `mouseClick page x y` act at a point of the viewport,
`boundingBox locator` is where an element is drawn and `viewportSize page` the
viewport: a canvas, which no locator sees into, is clicked at a point of its
box. Each settles.

## Frames

`inFrame scope css` is a locator step into the frame an `iframe` element shows;
`frameAt page glob` starts in the frame whose address matches, wherever it has
navigated since. The steps after either are taken in the frame's document,
and every reading and action works there:

```haskell
locator (inFrame page "iframe.wiki-svg[title='Where the time goes']") "#status" `shouldHaveText` "ready"
innerText (locator (frameAt page "**/api/v1/session") "body")
```

`frames page` lists the frames' addresses, the page's own first.
`evaluateOn locator function argument` calls a function with the locator's
element, in its frame if it is in one.

## The Chrome DevTools Protocol

`cdp page method params` calls a method of the page's DevTools session (made on
first use, one per page) and decodes its result, for what Playwright has no call
for: a virtual authenticator for passkeys, Chromium's installability check, an
element's event listeners. `throttleCPU page rate` slows the page's processor
down, as the `throttle` option does for every page. Like `evaluate`, `cdp`
does not settle.

Its events are kept as the console is: `listenCDP page event` keeps every
occurrence from then on (on the same session, so the domain is enabled with a
`cdp` call), `cdpEvents page event` is their parameters so far, and
`waitForCDP page event test` the first that satisfies a test, waiting for one if
there is none yet.

```haskell
listenCDP page "Network.webSocketFrameReceived"
_ :: Value <- cdp page "Network.enable" ()
frame <- waitForCDP page "Network.webSocketFrameReceived" (const True)
```

```haskell
_ :: Value <- cdp page "WebAuthn.enable" ()
Authenticator {authenticatorId} <- cdp page "WebAuthn.addVirtualAuthenticator" (object ["options" .= options])
```

## What the browser costs

`browserProcesses page` is the processes of the browser the page is in, every
page's: each one's `kind` (Chromium's `--type`: `renderer`, `gpu-process`,
`utility`, `zygote`, or `browser` for its own), the bytes it has resident
(`rss`) and the most it has had (`peak`), read from `/proc` (on Linux; none
elsewhere). A page cannot see this from inside, and for a page that compiles
in the browser it is what the machine must have: the playground's test prints
its renderer's, which go into the tutorial's measurements.

```haskell
renderers <- filter (\p -> p.kind == "renderer") <$> browserProcesses page
```

## Locators

A `Locator` is a value: a page and the steps to an element, sent with each
call, so making one does nothing and it finds the element afresh each time.
`locator page css`, `byRole page role`, `byRoleNamed page role name`,
`byText page text`, `byLabel page label`; then `nth i`, `first`, `lastOf`,
`withText t` (contains it, ignoring case) and `withTextExact t` (the whole
text, exactly: `Version 2` and not `Version 2 (current)` or `Version 12`). Each of the first five starts from a page or from another
locator, as Playwright's do: `locator (locator page ".card") "button"` is the
buttons inside the cards. As in Playwright, an action on a locator that matches more than
one element fails. `evaluateAll locator function argument` calls a JavaScript
function in the page with every element the locator matches, and
`allTextContents locator` is their texts; like `count`, neither waits.
`isDisabled` is `isEnabled`'s opposite. There is no filter by another
locator (Playwright's `has`): the one script that uses it filters by a text
inside, which `withText` does.

## In hspec

`playwright :: SpecWith Playwright -> Spec` starts one node and Chromium for
a group; `inPage :: (Page -> IO a) -> Playwright -> IO a` gives each example
a fresh context and page. When an example fails, `inPage` adds to the failure
the page's address, its console errors, its uncaught errors, its context's
route handlers that failed and requests that failed, and saves a screenshot
under `HALOGEN_PLAYWRIGHT_ARTIFACTS` if it is set. A failed action (a
`PlaywrightError`, such as a click that timed out) becomes a failure the same
way, with Playwright's message, which names the locator.

A test that is one long walk shares a page across its examples:

```haskell
describe "the walk" $ onOnePage defaultContextOptions (\page -> addInitScript (pageContext page) script) $ do
  it "starts" $ \page -> visit page base
  it "counts" $ \page -> click (locator page "#count")   -- on the page "starts" left
```

The setup runs once with the page fresh; each example's failure says what the
page said; one example cannot run alone with `--match`. `pageContext page` is
the page's context, for what is the context's.

`Halogen.Playwright.Hspec` also exports the part of hspec a suite is written
with (`hspec`, `describe`, `it`, `shouldBe`, `shouldReturn` and the rest), as
hspec-halogen does, so a suite imports the two modules and nothing of
hspec's: hspec's own `focus` would otherwise meet the page's.

## Not here

Written against the smoke tests it is to run, it leaves out what they do not
use, and some of what they do: traces, video and other browsers; a wait for a
response of the page's own (see the pauses above; `waitForExchange` is one
after the fact); taking an exposed function away; and the mouse's buttons
pressed and released apart.

## Its own tests

`Halogen-playwright-test` drives pages in `test/web/` served by the suite;
`Halogen-playwright-example-test` drives a built WebAssembly example
(`dist-newstyle/wasm/public/vanilla`, or `HALOGEN_EXAMPLE_DIR`), and is
pending when it is not built. `npm run test-example` at the repository's root
runs it with the build required. The same pattern tests real programs in
`examples/worker/test` (`npm run test-worker`): jobs in Web Workers that are
copies of the page's binary; and in `examples/learn/playground-test`
(`npm run test-playground`): GHC compiling a program in the page and
mounting it, for every layer of the tutorial's rootfs.
