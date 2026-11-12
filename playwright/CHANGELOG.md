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
