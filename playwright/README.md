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
| `HALOGEN_PLAYWRIGHT_THROTTLE` | slow every page's processor down by this factor (`4`: four times as slow) |

A run with `HALOGEN_PLAYWRIGHT_THROTTLE=6` is a run of the same tests on a slow
machine: a test that only passed because the program was quick enough
fails there, so races are found on purpose rather than in CI. `Options` also
has `browserArgs`, Chromium's command-line switches (a
`--host-resolver-rules` that sends a made-up host name to the loopback, say).

## The waits

- **`settle page`** waits for a task of the page's at background priority (a
  browser event reaches a WebAssembly program as a task of its own, so its
  handler may not have started when Playwright's click returns), then asks
  the page's program until it is idle, a background task between asks. Every
  action (`click`, `fill`, `typeText`, `press`, `goto`, ...) ends with it. A
  navigation that takes the document away is not an error. On a page with no
  Halogen program it waits for the one task.
- **`waitStarted page`** waits until a program in the page has mounted a
  tree and is idle: the prerendered page has been adopted and every
  `Initialize` has run its synchronous part. `visit` is `goto` and then this.
  It fails after the timeout on a page where no program answers.
- **`waitQuiet page`** waits for idle and for every fiber waiting in a
  `liftIO` to come back (a save's request, say). Its timeout says how many
  were still waiting.
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
after a link the program follows, and settles. `waitForConsole page test` is
the first console message, from the page's start, that satisfies a test,
waiting for one if there is none yet: one written before the call counts, so
the message an action caused cannot be missed.

A wait for a response (Playwright's `waitForResponse`) is not here: the one
script that uses it waits for a save's `PUT` so that what is typed next is
typed after it, which is `waitQuiet` (the program's fiber waiting on the
request has come back), and needs no pattern for the request.

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
with work queued, `waiting` the fibers waiting in a `liftIO`. Idle is
`running == 0`; the answer reads the program's loops when asked, so it is
exact. Any tool can set the probe up and ask it; see `Halogen.Activity`'s
documentation for the contract.

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

## Contexts: init scripts and requests

`addInitScript context js` runs a script of the test's own in every page of
the context before the page's own scripts, after the probe: to stand in for
something the page would find (a `Worker` that refuses to start, say).
`requests context` is what the context's pages and their Web Workers have
asked the network for so far (address, method, resource type), kept as they
are made: a worker's requests are its context's, not always its page's, which
is why it is asked of the context. `workers page` is how many dedicated Web
Workers the page has alive.

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
resource type, the headers and the body, so watching what was asked while
letting it through is a handler that records and continues.

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
the page's timeout if none comes.

On the wire this is the protocol's third kind of message: node asks
(`{"ask": 12, "route": 3, "request": {...}}`) and Haskell answers
(`{"answer": 12, "action": "fulfill", ...}`), bodies as base64; see the top of
`js/driver.mjs`. The thread that reads node's output never runs a handler and
never writes.

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
does not settle. No events: none of the scripts this was written from listens
to one.

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
`byText page text`, `byLabel page label`; then `nth i`, `first`,
`withText t`. Each of the first five starts from a page or from another
locator, as Playwright's do: `locator (locator page ".card") "button"` is the
buttons inside the cards. As in Playwright, an action on a locator that matches more than
one element fails. `evaluateAll locator function argument` calls a JavaScript
function in the page with every element the locator matches.

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
use, and some of what they do: traces, video and other browsers; a check that
a hydration repaired nothing (a gate and an init script build one, and it comes
with the tutorial's test); `exposeFunction` (a page calling the test; it would
be one more kind of ask, answered with JSON); popups, a console message's
source location, a navigation's response status, and the mouse's buttons
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
