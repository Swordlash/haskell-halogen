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
not fired, a message on its way over a WebSocket, an animation. For those the
`should` expectations retry until the timeout, and `eventually page` retries
any expectation.

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

## Locators

A `Locator` is a value: a page and the steps to an element, sent with each
call, so making one does nothing and it finds the element afresh each time.
`locator page css`, `byRole page role`, `byRoleNamed page role name`,
`byText page text`, `byLabel page label`; then `nth i`, `first`,
`withText t`. Each of the first five starts from a page or from another
locator, as Playwright's do: `locator (locator page ".card") "button"` is the
buttons inside the cards. As in Playwright, an action on a locator that matches more than
one element fails.

## In hspec

`playwright :: SpecWith Playwright -> Spec` starts one node and Chromium for
a group; `inPage :: (Page -> IO a) -> Playwright -> IO a` gives each example
a fresh context and page. When an example fails, `inPage` adds to the failure
the page's address, its console errors and its uncaught errors, and saves a
screenshot under `HALOGEN_PLAYWRIGHT_ARTIFACTS` if it is set. A failed
action (a `PlaywrightError`, such as a click that timed out) becomes a
failure the same way, with Playwright's message, which names the locator.

`Halogen.Playwright.Hspec` also exports the part of hspec a suite is written
with (`hspec`, `describe`, `it`, `shouldBe`, `shouldReturn` and the rest), as
hspec-halogen does, so a suite imports the two modules and nothing of
hspec's: hspec's own `focus` would otherwise meet the page's.

## Not here yet

These are planned, written against real smoke tests, and not in this
version: request routes (answering a page's requests from the test),
downloads, uploads, cookies, init scripts of the test's own, the Chrome
DevTools Protocol (a slowed processor), traces, and a check that a hydration
repaired nothing.

## Its own tests

`Halogen-playwright-test` drives pages in `test/web/` served by the suite;
`Halogen-playwright-example-test` drives a built WebAssembly example
(`dist-newstyle/wasm/public/vanilla`, or `HALOGEN_EXAMPLE_DIR`), and is
pending when it is not built. `npm run test-example` at the repository's root
runs it with the build required.
