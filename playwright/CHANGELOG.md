# Revision history for haskell-halogen-playwright

## Unreleased

The first version. `Halogen.Playwright` drives Chromium from a native
Haskell program through Playwright: contexts and pages, navigation,
locators as values (CSS, role, text, label, and their refinements),
actions, reading, retrying expectations, JavaScript in the page, the page's
console and errors, and screenshots. Every action ends by waiting for the
page's Halogen program to say it is idle, through the probe
haskell-halogen-core's `Halogen.Activity` answers; `waitStarted`, `visit`
and `waitQuiet` wait on the same signal. `Halogen.Playwright.Hspec` gives a
group of tests one browser and each test a fresh page, and adds the page's
state and a screenshot to a failure.

What porting the Web Workers' smoke test (examples/worker) needed:
`withTimeout`, a page handle whose calls and locators take another timeout
than the options'; `Halogen.Playwright.Serve`, a built directory served as a
static host serves it (index pages, media types, `HEAD`, a base path, a
`404.html`, mounted directories); `addInitScript`, a context's script of the
test's own; `requests`, what a context's pages and their workers asked the
network for; `workers`, a page's live Web Workers; `evaluateAll`, a function
on a locator's elements; and `waitForFunction`, a JavaScript expression
polled in the page.

What the remaining smoke tests (the tutorial's, the playground's, and those
of a wiki built on haskell-halogen) need, each shaped by the lines that use
it:

- `route` and `unroute`: a page's or a context's requests answered by a
  handler of the test's (`Fulfill`, `Abort`, `Continue`), each on a thread of
  its own, so that a handler may block; a handler that throws aborts its
  request and is reported. The protocol's third kind of message, an ask from
  node that Haskell answers, carries them. `hold`, `release`, `heldCount` and
  `waitHeld`: a gate that holds requests (`app.wasm`) until the test lets them
  go.
- `download` (an action and the file it downloaded, a `goto` that becomes a
  download included), `downloadBytes`, `saveDownload`; `setInputFiles`.
- `cookies`, `addCookies`, `clearCookies` and `cookie`; `setOffline`;
  `emulateMedia` (print, screen, the colour scheme); `pdf`.
- `ContextOptions`' `permissions`, `isMobile`, `hasTouch` (and `tap`) and
  `acceptDialogs`; every dialog is answered at once and kept (`dialogs`).
- `typeKeys`, `insertText`, `mouseMove`, `mouseClick`, `boundingBox`,
  `viewportSize`.
- `waitForURL`, `waitForFunctionWith`, `waitForConsole`, `awaitText`,
  `evaluateOn`.
- Frames: the locator steps `inFrame` and `frameAt`, and `frames`.
- `cdp`, a method of the Chrome DevTools Protocol on the page's session, and
  `throttleCPU`; the options' `throttle` (`HALOGEN_PLAYWRIGHT_THROTTLE`) slows
  every page down as it is made.
- `pageContext`; `Halogen.Playwright.Hspec`'s `onOnePage`, a group of
  examples sharing one page; the console printed as it arrives
  (`relayConsole`, `HALOGEN_PLAYWRIGHT_CONSOLE`); a failure's context lists
  the route handlers and requests that failed. `Options` has `browserArgs`.

A `goto` to an address that turns into a download no longer fails.

What porting the playground's smoke test (learn/, now
`examples/learn/playground-test`) needed: `browserProcesses`, the browser's
processes with their memory now and at their peak, read from `/proc`, for
the renderers' memory the tutorial's measurements report, which no page can
read from inside.

What a wiki's smoke tests still lacked, each shaped by the lines that use it:

- `screenshotWith` and `ScreenshotOptions`: a picture of the window
  (`fullPage = False`) rather than of the whole page, which `screenshot`
  still takes; `ContextOptions`' `reducedMotion`, for pictures of pages that
  animate.
- `gotoStatus`: `goto`, and the status of the response the navigation ended
  with (`Nothing` within the document, or for a download).
- `webSocketFrames` and `waitForWebSocket`: the frames of the page's
  WebSockets, both ways, kept as they cross, and a wait for the first that
  satisfies a test, from the page's start as `waitForConsole` is (a
  subscription's answer that came while the page loaded is found).
- `ConsoleMessage` has `url` and `line`: where a message was written, so a
  frame's messages can be told from the page's. Code that builds a
  `ConsoleMessage` with two arguments now gives four, or reads the fields.
- `withTextExact`, a filter by the whole text; `lastOf`, the last match;
  `allTextContents`; `isDisabled`.
- `evaluate`, `evaluateWith`, `evaluateOn` and `evaluateAll` fail after the
  timeout when their promise never settles, rather than hold the test for
  ever. They still do not settle the page.
- `rearm` and `closeGate`: one gate held again for every load of a walk, and
  taken away at its end.

The README no longer says that `waitQuiet` is a wait for a response: it waits
for every fiber in a `liftIO`, a notice's timer among them unless the program
writes that timer with core's `H.delay`, which `waitQuiet` does not wait for.

The check that a prerendered page is adopted with nothing repaired, which
the tutorial's smoke test and a wiki's two each wrote by hand: `adopt` and
`adoptWith` go to an address with the program's request (`app.wasm`) held,
mark every node under the root as the server sent it, let the program go,
wait for it to start, and give what it changed as a list of `Repair`s
(`Replaced`, `Removed`, `Built`, `TextChanged`, `AttributeChanged`), each
with a path to the node and its HTML or text. `AdoptionOptions` name the
root, the program's request, what to leave out (`leaveOut`: parts the
program's `Initialize` changes on purpose, not looked at; `leaveOutContents`:
islands another library fills, whose own element is still compared) and the
attributes the page's own scripts write. `markServed` and
`repairsSinceMarked` are the two halves, for a test with a gate of its own;
`shouldAdoptCleanly`, `shouldAdoptCleanlyWith` and `shouldHaveNoRepairs`
fail with every repair listed (`describeRepair`).

`unroute` and `closeGate` now say when to use them: on a page at rest. A
route taken away while the page's own requests were starting once left one
of them unanswered, so a gate that has done its work is released and left
(`adoptWith` does that).

What porting a server's smoke test (a login, its API, a Content Security
Policy, an editor in an iframe) needed, each with a test of its own:

- A route's handler can send its request on and look at the response:
  `fetchResponse` gives the server's `Response` (status, headers, the body as
  the page reads it) to change or to answer with, and `passThrough` is the
  handler that only looks. `RoutedRequest` has two more fields, `allHeaders`
  (every header the browser sends, cookies included) and `asked` (the
  request as the driver holds it, which `fetchResponse` needs): code that
  builds a `RoutedRequest` itself gives them, code that reads its fields is
  unchanged.
- A context's exchanges, opt-in: `ContextOptions`' `recordExchanges = Just
  limit` keeps each request once it has ended with every header it was sent
  with, its body, and the response's status, headers and body (up to `limit`
  bytes), or why it failed; `exchanges` and `waitForExchange` read them.
  `requests` is as it was. `defaultContextOptions` records nothing, so a
  context made from it is unchanged; code that writes a `ContextOptions` out
  in full gives the field.
- `routeWith` and `holdWith`, a route and a gate that take a `RouteFilter`:
  a glob, resource types and methods, decided in the driver, so that a gate
  on an API's `fetch` requests lets its images, and the page's `load`,
  through. `route` and `hold` are as they were.
- The Chrome DevTools Protocol's events: `listenCDP` keeps an event of the
  page's session from then on, `cdpEvents` reads them and `waitForCDP` waits
  for one, as the console is kept and waited on.
- `waitForFunction` works on a page whose Content Security Policy refuses
  `eval`: it polls in the page at its animation frames as before, but the
  expression is compiled through the DevTools protocol rather than with
  `eval` in the page, which such a policy refuses (Playwright's own call
  failed with an `EvalError` once the expression was not true at once).
  `waitForFunctionWith` worked there only because Playwright evaluates a
  function's source once, inside the DevTools call; it now polls the same
  way. A navigation under either wait starts it again in the new document.
  Every other wait, and the probe, already worked there; the suite now says
  so.
- `goBack` and `goForward` return for every kind of history entry: a state the
  page pushed and a fragment (the same document), and a frame's own
  navigation, where they used to wait for a `load` of the page's that never
  came and time out; another document is waited for as before. With no entry
  to go to they fail with a `PlaywrightError` named `NoHistoryEntry`, where
  they did nothing.
- `exposeFunction`: a function of the test's that a page (or every page of a
  context) calls as `window[name]`, JSON both ways, each call on a thread of
  its own; what it throws rejects the page's promise and is listed in an
  `inPage` failure ("Exposed functions that failed").
- Popups: `waitForPopup` runs an action and gives the page it opened, loaded
  and settled, and `pages` lists a context's pages. A popup gets the probe,
  the context's init scripts and the options' throttle as every page does.
  An action whose page closes as it is handled (a sign-in window's button)
  no longer fails.

What the wiki's smoke tests found next, each with a test of its own:

- `settle`, and so every action, waits for the page's next animation frame
  as well as for a task, and asks the program again after it: the browser
  dispatches a dialog's `close` (after Escape, or a handler's `close()`)
  with the next frame, so a program idle when the action returned had not
  yet heard it, and a test read the dialog still open. It also waits for a
  move through the history the page asked for (`history.back()`, `forward()`,
  `go(n)`, which the driver's init script notes by wrapping those three
  methods of `History.prototype` in every page) until the page has its
  `popstate`. The frame costs an action about a dozen milliseconds; a hidden
  page, or one that draws no frames, is not held up. An action on an element
  in a frame now settles the frame's own program as well as the page's:
  before, only the page's document was asked.
- `waitForEvent`, with `EventTarget` (`OnWindow`, `OnDocument`, `OnElement`):
  an action, then an event on the target, waited for by a listener the page
  had before the action, then settled. For what the page does later by
  itself.
- Each logged request says who asked: `Request` (and `Exchange`) grew three
  fields, `page` (the page's number, `pageNumber`; `requestedBy` compares),
  `document` (the identity of the document that asked: new for every
  document a frame loads) and `documentUrl` (its address). A navigation's
  own request is the document it loads. `currentDocument` and `documents`
  are what a page shows now, and `requestsOf page` the requests of those
  documents: on a page shared between examples, a late request of the
  document before is no longer counted against the one a test went to, and
  the test need not go to `about:blank` first. Code that reads the records'
  fields by name is unchanged; code that builds a `Request` or an
  `Exchange` gives the new fields.
