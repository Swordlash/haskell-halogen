// Open a browser test page in a Chromium that Playwright controls, and keep it
// open until the window is closed.
//
//   node toolchain/browser-test-open.mjs <url>
//
// dev-test.sh uses this for the browser-GHCi page a suite runs in. Opening it
// through Playwright rather than the desktop browser is what lets the suite's
// clicks and keystrokes be real input: the page gets the same bridge as under
// browser-test-runner.mjs. What the page logs, the suite's report included, is
// printed here. HALOGEN_TEST_HEADLESS=1 opens no window, and
// HALOGEN_TEST_SLOWMO=<ms> slows every action down so it can be followed.
import { chromium } from "playwright";

const url = process.argv[2];
if (!url) {
  console.error("usage: browser-test-open.mjs <url>");
  process.exit(1);
}

const browser = await chromium.launch({
  headless: Boolean(process.env.HALOGEN_TEST_HEADLESS),
  slowMo: Number(process.env.HALOGEN_TEST_SLOWMO ?? 0),
});
const page = await browser.newPage({ viewport: null });
page.setDefaultTimeout(5000);

// Set from the moment GHCi's socket closes until the page is loaded again: the
// old session's code fails as its connection goes, and that is not news.
let ghciGone = false;
page.on("pageerror", (error) => {
  if (!ghciGone) console.error(`[page error] ${error.stack ?? error.message}`);
});
// The suite's report arrives as console lines, since browser GHCi sends what
// the evaluated code prints to the page's console rather than the terminal.
page.on("console", (message) => {
  if (message.type() === "error" || message.type() === "warning") console.error(`[page ${message.type()}] ${message.text()}`);
  else console.log(message.text());
});

await page.exposeFunction("__halogenTestAct", async (action, selector, text) => {
  const target = page.locator(selector);
  if (action === "click") await target.click();
  else if (action === "type") await target.pressSequentially(text);
  else if (action === "clear") await target.clear();
  else throw new Error(`unknown action ${action}`);
});
await page.exposeFunction("__halogenTestPress", (key) => page.keyboard.press(key));
await page.addInitScript(() => {
  globalThis.__halogenTest = {
    act: (action, selector, text) => globalThis.__halogenTestAct(action, selector, text),
    press: (key) => globalThis.__halogenTestPress(key),
  };
});

// Browser GHCi talks to the page over a WebSocket. When ghciwatch restarts
// GHCi (a library or cabal file changed) that socket closes, and the new GHCi
// waits for a page to boot it: load the page again once it is served.
let reconnecting = false;
const reconnect = async () => {
  if (reconnecting || page.isClosed()) return;
  reconnecting = true;
  ghciGone = true;
  try {
    for (;;) {
      const served = await fetch(url).then((r) => r.ok, () => false);
      if (served) break;
      await new Promise((later) => setTimeout(later, 500));
    }
    if (!page.isClosed()) await page.goto(url);
    ghciGone = false;
  } finally {
    reconnecting = false;
  }
};
page.on("websocket", (socket) => socket.on("close", () => reconnect().catch(() => {})));

await page.goto(url);
await new Promise((closed) => {
  page.on("close", closed);
  browser.on("disconnected", closed);
});
await browser.close().catch(() => {});
