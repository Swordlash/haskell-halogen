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
