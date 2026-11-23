# haskell-halogen-playwright

Test built applications from a native Haskell hspec suite. [Playwright](https://playwright.dev) drives Chromium to open pages, navigate, click, type and inspect results. After each action, the driver waits for the [haskell-halogen](https://github.com/Swordlash/haskell-halogen) program to finish reacting. Tests can then read the rendered result without sleeps or polling.

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

[hspec-halogen](../hspec-halogen/) runs tests inside the page, where they can mount components, query them and inspect outputs directly. Use it for focused component tests. It cannot navigate between documents, control several pages or test components running outside the browser.

This package runs tests in a native process against the application as built and served, including prerendered HTML. Use it for navigation, storage, multiple tabs and remotely rendered applications. [haskell-halogen-remote](../remote/) uses it in `Halogen-remote-live-test`.

## Setup

Install Node and the npm `playwright` package. The driver resolves packages from the working directory and its parents, then from `NODE_PATH`:

```sh
npm install --save-dev playwright
npx playwright install chromium
```

`withPlaywright` reports a `PlaywrightError` with installation instructions if a dependency is missing. The package is native-only and has no dependencies on other Halogen packages. It can drive any page; integration with Halogen uses the activity probe described below.

The library embeds its JavaScript driver and writes it to a temporary directory at startup. Haskell and Node exchange JSON lines over stdin and stdout; see `js/driver.mjs`. Node's stderr is forwarded with a `[playwright]` prefix. `withPlaywright` closes the driver when its scope ends. If the Haskell process exits, even through `kill -9`, Node detects the closed stdin, closes Chromium and exits.

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

`HALOGEN_PLAYWRIGHT_THROTTLE=6` slows browser execution to expose timing assumptions and races. `Options.browserArgs` accepts additional Chromium switches, such as `--host-resolver-rules` to map a test hostname to loopback.

Chromium can enable different features on different machines. For example, `RenderDocument` can replace a frame's host during navigation, causing final requests such as `pagehide` beacons to be sent without appearing in request logs. Test both configurations through `HALOGEN_PLAYWRIGHT_ARGS`: `--enable-features=RenderDocument:level/all-frames` or `--disable-features=RenderDocument`.

## The waits

`settle page` waits for browser events and the work they trigger. It first yields to a background-priority task so WebAssembly handlers can start, then queries the activity probe until the program is idle. It also waits for pending history navigation, the next animation frame and a following task, then checks activity again. This catches delayed events such as dialog `close`, `resize` and `ResizeObserver` callbacks. Every normal action ends with this sequence.

Actions inside frames settle the frame first, then the page. Navigating away during an action is allowed. Pages without Halogen still wait for browser tasks, history and frames. Script-disabled sandbox frames settle immediately because the driver cannot run code there. Hidden pages do not wait for frames. After a frame timeout of 100 ms, later attempts wait at most 20 ms until frames resume. A normal settle adds roughly one frame of latency.

The init script tracks navigation by wrapping `History.prototype.back`, `forward` and `go`. These wrappers preserve names and argument counts but no longer stringify as native functions. `evaluate`, `evaluateWith`, `evaluateOn`, `evaluateAll` and `cdp` do not settle automatically. Call `settle` after using them to dispatch events. Unresolved promises fail at the configured timeout.

`waitStarted page` waits for a mounted, idle tree, including the synchronous parts of initialization. `visit` combines `goto` and `waitStarted`. It times out if no program answers.

`waitQuiet page` also waits for fibers suspended in `H.async`, but excludes `H.delay`. Timeout messages report the remaining waiters. `activity page` returns the raw activity reading, or `Nothing` if no program provides one.

Activity waits do not cover external events that have not reached the program, such as timers, incoming WebSocket messages, animations or worker startup. Use retrying `should` expectations or `eventually page` for those conditions. `waitForFunction` polls a JavaScript expression at animation frames; `waitForFunctionWith` passes an argument separately. `awaitText` waits for matching text and reports the last value on failure. `waitForURL` accepts an exact address or glob and settles after navigation.

`waitForEvent target type action` installs a listener, runs the action, waits for the event and then settles. Targets are `OnWindow page`, `OnDocument page` and `OnElement locator`. The listener is installed before the action but after the page's own listeners. Missing events fail with `TimeoutError`; navigating away first fails with `NavigatedAway`. This is useful when the page schedules later work itself:

```haskell
waitForEvent (OnWindow page) "popstate" (click (byRoleNamed page "button" "Back in a moment"))
```
`waitForConsole page test` returns the first matching console message since page startup, waiting if necessary. Earlier messages are retained, so it cannot miss one produced before the call. `ConsoleMessage.url` and `.line` identify its source, including frames. For example, filter an interactive frame with `not (maybe False ("?interactive" `Text.isInfixOf`) m.url)`.

Waits work under Content Security Policies that disallow `eval`. Unlike Playwright's own `waitForFunction`, this implementation compiles and polls expressions through the DevTools protocol. The probe runs as an init script, and `evaluate` also uses DevTools. These operations are not blocked by the page's `script-src` policy.

`waitForWebSocket page test` returns the first matching WebSocket frame since page startup. Both incoming and outgoing frames are retained and available through `webSocketFrames page`. Match payloads to distinguish later messages. On timeout, the error includes recent frames.

```haskell
-- The second tab has subscribed: a comment made in the first is pushed to it.
_ <- waitForWebSocket watcher (\f -> not f.sent && "\"subscribed\"" `Text.isInfixOf` f.payload)
```

### Pauses: what `waitQuiet` waits for is the program's to say

`waitQuiet` includes timers written with `H.async (threadDelay n)` or `H.delayCounted`. Choose the delay API according to what the timer represents:

- Use `H.delayCounted` or `Hooks.delayCounted` for delays before work that tests should await, such as a debounced search or autosave. `waitQuiet` then includes both the delay and the resulting request.
- Use `H.delay` or `Hooks.delay` for transient notices and repeating UI timers. These take seconds as `NominalDiffTime` and do not keep the program busy. Tests can inspect a notice before it disappears, and clocks remain quiet between ticks. Using a counted delay for a perpetual ticker would prevent `waitQuiet` from returning.

There is no direct equivalent of Playwright's `waitForResponse`. `waitQuiet` waits for all counted work, which may include a notice's dismissal timer. If a "Saved" notice uses `H.delayCounted`, it will be gone by the time `waitQuiet` returns; use `H.delay` for that timer. If you cannot change the program, use `awaitText` to observe the notice after settling, or check the saved data through the server API after `waitQuiet`.

Calls use `HALOGEN_PLAYWRIGHT_TIMEOUT`, five seconds by default. `withTimeout ms page` returns a page handle with a different timeout, also used by locators created from it, settling and retrying expectations:

```haskell
visit (withTimeout 30000 page) address      -- a program of megabytes loading
click (locator (withTimeout 60000 page) "#count")
waitQuiet (withTimeout 60000 page)          -- a job of seconds in a Web Worker
```

## The probe

Every page the driver opens has `globalThis.__halogenProbe = {}` before its own scripts run. A program built with a haskell-halogen-core that has `Halogen.Activity` gives it, when it mounts its first tree,

```js
await __halogenProbe.activity()   // {trees, running, waiting, asleep}
```

`trees` counts mounted trees, `running` counts trees executing or holding queued work, and `waiting` counts fibers suspended in `H.async`. `H.delay` does not contribute to `waiting`. `asleep` counts the waiting fibers in a counted pause asleep on a test's clock (core's `withTestClock`), which only the test's `advance` wakes; a program from before it answers without it, which counts as 0. The program is idle when `running == 0`, and quiet, as `waitQuiet` decides, when it is idle and `waiting - asleep == 0`, the rule of core's `Halogen.Activity.quiet`. Each reading inspects the current loops directly. Any tool can install and query this probe; see `Halogen.Activity` for its contract.

`Halogen.Remote.Test` supports the same probe for components running in the native test process. Its browser client forwards activity queries to the component's remote session. Actions then settle against that session alone. See remote's README, "Testing a component in a real page".

## Serving a built directory

`Halogen.Playwright.Serve` serves a directory on a free port for the duration of a test. Directory URLs serve `index.html` and redirect to a trailing slash when needed. Files use their proper media types, including `application/wasm` for streaming compilation. `HEAD` uses the same metadata as `GET`. The server adds no cache or cross-origin isolation headers.

```haskell
main = serveDirectory "dist-newstyle/wasm/public/vanilla" $ \base ->
  hspec $ playwright $ it "starts" $ inPage $ \page -> visit page base
```

`serveDirectoryWith` takes a base path to serve it under (as GitHub Pages serves a project's site, outside of which there is nothing), a `404.html` to serve for an address with no file, and other directories mounted under the base.

## A prerendered page, adopted

`hydrateUI` adopts the nodes generated by `Halogen.Static`. If the initial render differs from that HTML, hydration repairs the page and can cause visible changes. `shouldAdoptCleanly page address` checks that no repairs occur:

```haskell
it "adopts the prerendered page with nothing repaired" $ inPage $ \page ->
  shouldAdoptCleanly page base
```

The check holds `**/app.wasm`, opens the page and records every node under `#app`, including text and attributes. It then releases the program, waits for startup and compares the DOM. The released gate stays installed because removing routes while requests are active can stall them.

`adopt` and `adoptWith` return repairs as data: `Replaced`, `Removed`, `Built`, `TextChanged` and `AttributeChanged`. Structural repairs report the outermost affected node. Each includes a path such as `#app > div.learn > p:nth-child(2) > text()` and the relevant HTML or text. `describeRepair` formats it for reports. Parser-created whitespace and comments separating text nodes are ignored.

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

The comparison includes synchronous `Initialize` changes but excludes later timer and response updates. Use `leaveOut` for areas intentionally filled at initialization, such as an editor. `leaveOutContents` ignores contents while still checking the element's identity and attributes.

For several adoption checks on one page, reuse one gate and call `rearm` before each navigation. The lower-level functions also let you inspect prerendered content or hold an API request between marking and comparing:

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

`addInitScript context js` runs after the probe but before application scripts in every page of the context. Use it to replace browser features for a test. `requests context` records network requests from pages and workers with their URL, method and resource type. It belongs to the context because service-worker requests may have no page. `workers page` counts live dedicated workers.

Each `Request` includes the requesting `page`, `document` and `documentUrl`. Compare the page with `requestedBy page request`. Document identity uses the browser's loader ID: it changes for new documents but stays stable across history-state and fragment changes. Navigation requests belong to the document they load, and frame requests belong to the frame's document.

`currentDocument page` returns the main document, and `documents page` includes frame documents. `requestsOf page` filters requests to currently displayed documents. This lets tests sharing one page inspect the current navigation without first visiting `about:blank`.

```haskell
goto page (base <> "no-picture.html")
fonts <- filter (\r -> r.resourceType == "font") <$> requestsOf page
fonts `shouldBe` []
```

The driver matches Playwright requests to DevTools reports by URL, method, frame kind and navigation status, in arrival order. It excludes documents that were not yet active when the request arrived. Matched reports provide an exact document ID.

Requests without a matching report use the frame's document at observation time. This applies to dedicated workers and out-of-process cross-site frames, and can misattribute a worker request around navigation. Service-worker requests have no page or document. Chromium may also send unload beacons without reporting them when it replaces a frame host.

Set `recordExchanges = Just limit` to retain completed requests and responses. `exchanges context` includes the URL, method, resource type, page, document, complete request headers including cookies, request body, response status and headers, and either the response body or failure reason. Repeated headers such as `set-cookie` remain separate. Response bodies are retained only up to `limit` bytes.

Recording is opt-in because it adds two browser queries per request and uses memory. Use `waitForExchange context test` to wait for a match, including exchanges recorded before the call. Recording finishes after the response body is read, slightly later than the page receives the response.

```haskell
withContext pw defaultContextOptions {recordExchanges = Just 65536} $ \ctx -> do
  ...
  click (byRoleNamed page "button" "Save")
  saved <- waitForExchange ctx (\e -> e.method == "POST" && "/api/save" `Text.isSuffixOf` e.url)
  saved.status `shouldBe` Just 200
  lookup "cookie" saved.requestHeaders `shouldBe` Just ("session=" <> token)
  (decodeStrict =<< saved.responseBody) `shouldBe` Just (object ["saved" .= True])
```

`pages context` includes popups. `waitForPopup page action` arms its listener before the action and returns the opened page after load and settling. Popups inherit the context's probe, init scripts, routes, exposed functions and CPU throttle, have their own console log, and close with the context. A popup closing itself while handling a click does not make that click fail.

```haskell
(_, signIn) <- waitForPopup page (click (byRoleNamed page "button" "Sign in"))
fill (byLabel signIn "Name") "ada"
click (byRoleNamed signIn "button" "Sign in")   -- the window closes
locator page "#user" `shouldHaveText` "ada"
```

## Requests answered by the test

`route page glob handler` (or `route context glob handler`) hands each request whose address matches a glob (Playwright's: `**` any characters, `*` any but `/`) to a handler of the test's, which says what becomes of it:

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

`Continue` passes the request to an earlier route or the network. `Fulfill response` supplies a response, and `Abort code` fails it with a Playwright error code. Handlers receive the URL, method, resource type, headers and body; `allHeaders` includes cookies. To observe requests without changing them, record the data and return `Continue`.

`fetchResponse request` forwards the request and returns the server's status, headers and decoded body. Inspect or modify the response, then return `Fulfill`. Headers describing the original wire encoding are removed because the body is already decoded. Use `passThrough` to observe responses without modifying them:

```haskell
-- The server's settings, with the dark theme on.
_ <- route page "**/api/settings" $ \request -> do
  r <- fetchResponse request
  pure (Fulfill r {body = "{\"theme\": \"dark\"}"})

-- Every answer of the API's, kept, and given to the page as it came.
_ <- route page "**/api/**" $ passThrough $ \request response ->
  atomicModifyIORef' answers (\as -> (as <> [(request.url, response.status, response.body)], ()))
```

`routeWith` and `holdWith` accept a `RouteFilter` with a URL glob and optional resource-type and method lists. The driver filters before calling Haskell, so excluded requests continue without a handler call. For example, hold API fetches while allowing images at the same path to load:

```haskell
gate <- holdWith page (routeFilter "**/api/**") {resourceTypes = ["fetch"]}
_ <- routeWith page (routeFilter "**/api/**") {methods = ["POST"]} (\_ -> pure (fulfill 503 Nothing ""))
```

Each handler runs on its own Haskell thread and may block. If it throws, its request is aborted; the exception is logged to stderr and included in `inPage` failure diagnostics.

Removing a route, closing its page or context, or stopping the driver cancels active handlers. Their requests are released before Playwright removes the route so they do not remain stuck. Service-worker requests and cached responses bypass page routes; use `serviceWorkers = False` when interception must cover them.

A gate is a route whose handler waits:

```haskell
gate <- hold page "**/app.wasm"
goto page address
waitHeld gate                 -- the page asked for it
... what the server sent, before the program ran ...
release gate                  -- held requests go on, later ones pass straight through
waitStarted page
```

`heldCount gate` reports the current number of held requests. `waitHeld` waits for one using the page's timeout. Reuse a gate across navigations with `rearm` before `goto` or `reload`. `closeGate` removes the route and releases outstanding requests.

Route handlers use request/reply messages between Node and Haskell: `{"ask": 12, "route": 3, "request": {...}}` and `{"answer": 12, "action": "fulfill", ...}`. Bodies are base64-encoded; see `js/driver.mjs`. The Node-output reader never runs handlers or writes replies itself.

## Functions the page calls

`exposeFunction page name f` installs `window[name]` as a function accepting one JSON argument and returning a promise of a JSON result. Context-level registration applies to all its pages. Calls run on separate Haskell threads and may block independently. Decode errors and exceptions reject the page's promise, are logged to stderr and appear under "Exposed functions that failed" in failure reports. The function remains available across document loads until the page or context closes.

```haskell
exposeFunction page "reportScore" $ \(score :: Int) -> writeIORef best score
-- in the page: await window.reportScore(42)
```

## Files

`download page action` waits for a download triggered by an action, including navigation to a download URL. It registers before the action and returns after the file is fully saved. The result exposes its suggested filename, URL, `downloadBytes` and `saveDownload`. `setInputFiles` sets a file input from `UploadFile path` or in-memory `UploadBytes {name, mimeType, bytes}`.

```haskell
exported <- download page (click (locator page ".demo-export"))
zip <- downloadBytes exported
setInputFiles (locator page ".demo-import input")
  [UploadBytes {name = exported.suggestedFilename, mimeType = "application/zip", bytes = zip}]
```

## A context's state, and what the page is made to see

Manage cookies with `cookies`, `addCookies` and `clearCookies`. `cookie name value url` constructs one; for example, `addCookies fresh [cookie "wiki_session" token origin]` installs a session in a new context.

`setOffline ctx True` disables networking, delivers `offline` and settles the pages. `emulateMedia page Print`, `Screen` or `Scheme Dark` changes media-query results and settles. `pdf page` prints using the page size from CSS.

`ContextOptions` controls permissions, mobile emulation and touch support; use `tap` for touch input. Dialogs are answered immediately and recorded by `dialogs page`. They are dismissed by default; `acceptDialogs` accepts them, including `beforeunload` prompts.

## Navigating, and pictures

`goBack` and `goForward` navigate one history entry and settle afterward. They support full document loads, pushed states, fragments and frame-only navigation. With no destination they throw `PlaywrightError` named `NoHistoryEntry`.

`gotoStatus` navigates and returns the final HTTP status, such as `Just 404` or `Just 401`. It returns `Nothing` for same-document navigation or downloads, and works with JavaScript disabled.

`screenshot page path` captures the full page. Use `screenshotWith page defaultScreenshotOptions {fullPage = False} path` for the viewport. Screenshots do not settle automatically. Set `reducedMotion = True` in context options to activate `prefers-reduced-motion: reduce`.

## Keys, the mouse and where things are

`typeKeys page text` types into the focused element. `insertText` inserts text as a paste would. `mouseMove` and `mouseClick` use viewport coordinates; `boundingBox` and `viewportSize` help calculate them, for example to click inside a canvas. Input actions settle afterward.

## Frames

`inFrame scope css` is a locator step into the frame an `iframe` element shows; `frameAt page glob` starts in the frame whose address matches, wherever it has navigated since. The steps after either are taken in the frame's document, and every reading and action works there:

```haskell
locator (inFrame page "iframe.wiki-svg[title='Where the time goes']") "#status" `shouldHaveText` "ready"
innerText (locator (frameAt page "**/api/v1/session") "body")
```

`frames page` lists the frames' addresses, the page's own first. `evaluateOn locator function argument` calls a function with the locator's element, in its frame if it is in one.

## The Chrome DevTools Protocol

`cdp page method params` calls the Chrome DevTools Protocol and decodes its result. The driver creates one session per page on first use. Use it for APIs outside the wrapper, such as WebAuthn authenticators, installability checks or event-listener inspection. `throttleCPU page rate` changes that page's CPU throttle. Like `evaluate`, `cdp` does not settle.

`listenCDP page event` starts recording an event on the same DevTools session. Enable its domain separately with `cdp`. `cdpEvents` returns recorded parameters, and `waitForCDP page event test` waits for the first matching event, including earlier recorded events.

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

`browserProcesses page` reports all processes in the page's browser. Each has a Chromium process `kind`, current resident bytes (`rss`) and peak resident bytes (`peak`). On Linux these come from `/proc`; other platforms return no entries. The playground test uses renderer memory to measure the cost of compiling in the browser.

```haskell
renderers <- filter (\p -> p.kind == "renderer") <$> browserProcesses page
```

## Locators

A `Locator` stores a page and a sequence of lookup steps. Creating it does not query the DOM; each operation resolves it again. Start with `locator`, `byRole`, `byRoleNamed`, `byText` or `byLabel`, then narrow with `nth`, `first`, `lastOf`, `withText` or `withTextExact`. The first five can also start from another locator to search inside it. Actions fail if more than one element matches.

`withText` performs a case-insensitive substring match. `withTextExact` matches the entire text exactly. `evaluateAll` passes all matching elements to a JavaScript function; `allTextContents` returns their text. Neither waits, and neither does `count`. `isDisabled` is the opposite of `isEnabled`. Filtering by another locator, Playwright's `has`, is not implemented.

## In hspec

`playwright :: SpecWith Playwright -> Spec` starts one node and Chromium for a group; `inPage :: (Page -> IO a) -> Playwright -> IO a` gives each example a fresh context and page. When an example fails, `inPage` adds to the failure the page's address, its console errors, its uncaught errors, its context's route handlers that failed and requests that failed, and saves a screenshot under `HALOGEN_PLAYWRIGHT_ARTIFACTS` if it is set. A failed action (a `PlaywrightError`, such as a click that timed out) becomes a failure the same way, with Playwright's message, which names the locator.

A test that is one long walk shares a page across its examples:

```haskell
describe "the walk" $ onOnePage defaultContextOptions (\page -> addInitScript (pageContext page) script) $ do
  it "starts" $ \page -> visit page base
  it "counts" $ \page -> click (locator page "#count")   -- on the page "starts" left
```

`onOnePage` runs setup once and preserves the page between examples. Failures still include page diagnostics. These examples depend on earlier steps and cannot be run individually with `--match`. Use `pageContext page` for context-level operations.

`Halogen.Playwright.Hspec` also exports the part of hspec a suite is written with (`hspec`, `describe`, `it`, `shouldBe`, `shouldReturn` and the rest), as hspec-halogen does, so a suite imports the two modules and nothing of hspec's: hspec's own `focus` would otherwise meet the page's.

## Not here

The wrapper currently omits tracing, video, other browsers, direct response waits, removing exposed functions, and separate mouse-button press/release operations. `waitForExchange` can inspect completed network exchanges; see the pause discussion for response-wait alternatives.

## Its own tests

`Halogen-playwright-test` drives pages in `test/web/` served by the suite; `Halogen-playwright-example-test` drives a built WebAssembly example (`dist-newstyle/wasm/public/vanilla`, or `HALOGEN_EXAMPLE_DIR`), and is pending when it is not built. `npm run test-example` at the repository's root runs it with the build required. The same pattern tests real programs in `examples/worker/test` (`npm run test-worker`): jobs in Web Workers that are copies of the page's binary; and in `examples/learn/playground-test` (`npm run test-playground`): GHC compiling a program in the page and mounting it, for every layer of the tutorial's rootfs.
