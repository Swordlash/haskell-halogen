// Drive the in-browser playground (learn/) in headless Chromium: GHC loads in
// the page, compiles the default program (a Halogen counter), and the counter
// it mounts counts; a second Run replaces it, and so does a third of an edited
// program, and a fourth of a counter written with haskell-halogen-hooks, from
// the hooks layer, when the build has one.
//
//   npm run test-learn                      # after: npm run build-learn
//   node toolchain/test-learn.mjs [dir]     # default dist-newstyle/wasm/learn
//
// It prints what the page measured (fetch, extract, loading the compiler, each
// run) and how much memory the page's renderer process used at its peak, and
// fails on any error in the page. The page imports browser_wasi_shim from
// esm.sh, so this needs the network.
import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import { createServer } from "node:http";
import { extname, join, normalize, resolve } from "node:path";
import { chromium } from "playwright";

const root = resolve(process.argv[2] ?? "dist-newstyle/wasm/learn");
// Loading takes seconds and the first run tens of them on a fast machine; be
// generous, for slow CI runners.
const timeout = Number(process.env.LEARN_TIMEOUT ?? 300) * 1000;
// The page's query, for trying other RTS options: LEARN_QUERY='rts=-c -H64m'.
const query = process.env.LEARN_QUERY ? `?${new URLSearchParams(process.env.LEARN_QUERY)}` : "";

const types = {
  ".html": "text/html",
  ".js": "text/javascript",
  ".mjs": "text/javascript",
  ".json": "application/json",
  ".wasm": "application/wasm",
};

// A static server, as Pages is, and nothing else: no COOP/COEP headers, so
// the page is not cross-origin isolated and has no SharedArrayBuffer.
const server = createServer(async (request, response) => {
  const path = decodeURIComponent(new URL(request.url, "http://x").pathname);
  const file = join(root, normalize(path.endsWith("/") ? `${path}index.html` : path));
  try {
    const body = await readFile(file);
    response.writeHead(200, { "content-type": types[extname(file)] ?? "application/octet-stream" });
    response.end(body);
  } catch {
    response.writeHead(404).end();
  }
});
await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
const url = `http://127.0.0.1:${server.address().port}/`;

// Launched as a server so that its process, and so its renderers', can be
// found in /proc for their peak memory.
const browserServer = await chromium.launchServer();
const browser = await chromium.connect(browserServer.wsEndpoint());
const errors = [];
let failed = false;

// The resident memory (VmRSS) and its peak (VmHWM) of each of the browser's
// renderer processes, on Linux.
async function rendererMemory() {
  const parents = new Map();
  const peaks = [];
  try {
    for (const pid of await readdir("/proc")) {
      if (!/^\d+$/.test(pid)) continue;
      try {
        const status = await readFile(`/proc/${pid}/status`, "utf8");
        const cmdline = await readFile(`/proc/${pid}/cmdline`, "utf8");
        parents.set(pid, { ppid: status.match(/^PPid:\s+(\d+)/m)?.[1], status, cmdline });
      } catch {
        // gone already
      }
    }
  } catch {
    return null;
  }
  const descends = (pid) => {
    for (let p = pid; p; p = parents.get(p)?.ppid) if (p === String(browserServer.process().pid)) return true;
    return false;
  };
  for (const [pid, { status, cmdline }] of parents) {
    if (descends(pid) && cmdline.includes("--type=renderer")) {
      const kB = (field) => Number(status.match(new RegExp(`^${field}:\\s+(\\d+) kB`, "m"))?.[1] ?? 0) * 1024;
      peaks.push({ rss: kB("VmRSS"), peak: kB("VmHWM") });
    }
  }
  return peaks;
}

// A counter written with haskell-halogen-hooks, for the hooks layer.
const hooksCounter = `{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module Main (main) where

import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.Hooks qualified as Hooks
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

counter :: H.Component q () o BrowserDOM
counter = Hooks.component @Empty $ \\_ -> Hooks.do
  (n, nId) <- Hooks.useState (0 :: Int)
  Hooks.pure $
    HH.button [HE.onClick (const (Hooks.modify_ nId (+ 1)))] [HH.text ("Hooked " <> T.pack (show n) <> " times")]

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI counter () app
  HA.keepForNextRun socket.dispose
`;

const mb = (bytes) => `${(bytes / 1e6).toFixed(0)} MB`;

try {
  const page = await browser.newPage();
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
    if (message.text().startsWith("learn:")) console.log(`    ${message.text()}`);
  });
  const noErrors = (step) => assert.deepEqual(errors, [], `errors in the page ${step}`);

  // The page's JS heap, as DevTools counts it (performance.memory, which the
  // page reports, is rounded and lags), after a collection: what the page
  // keeps. The extracted archive's files are on it.
  const devtools = await page.context().newCDPSession(page);
  await devtools.send("Performance.enable");
  const heaps = {};
  const rendererAt = {};
  const measureHeap = async (when) => {
    const renderers = await rendererMemory();
    if (renderers?.length) rendererAt[when] = renderers;
    await devtools.send("HeapProfiler.collectGarbage");
    const { metrics } = await devtools.send("Performance.getMetrics");
    heaps[when] = metrics.find((m) => m.name === "JSHeapUsedSize")?.value;
  };

  await page.goto(url + query);
  await page.waitForFunction(() => window.learn?.ready || window.learn?.error, null, { timeout });
  assert.equal(await page.evaluate(() => window.learn.error), null, "the page failed to load the compiler");
  noErrors("while loading");
  assert.equal(
    await page.evaluate(() => crossOriginIsolated || typeof SharedArrayBuffer !== "undefined"),
    false,
    "the page is not cross-origin isolated",
  );
  console.log("ok  the compiler loaded, without cross-origin isolation");
  await measureHeap("loaded");

  const run = async (n) => {
    await page.locator("#run:enabled").click();
    await page.waitForFunction((n) => window.learn.runs >= n, n, { timeout });
    const { timings, stderr } = await page.evaluate(() => ({
      timings: window.learn.timings,
      stderr: document.getElementById("stderr").textContent,
    }));
    assert.ok(`run ${n}` in timings, `run ${n} failed: ${await page.locator("#status").textContent()}\n${stderr}`);
    assert.equal(stderr, "", `run ${n}: GHC wrote to stderr`);
    noErrors(`in run ${n}`);
    await measureHeap(`run ${n}`);
  };
  const counter = page.locator("#app button");

  await run(1);
  await page.locator("#app button", { hasText: "Clicked 0 times" }).waitFor({ timeout: 10000 });
  console.log("ok  run 1 compiled the counter and mounted it");

  await counter.click();
  await counter.click();
  await page.locator("#app button", { hasText: "Clicked 2 times" }).waitFor({ timeout: 10000 });
  console.log("ok  the interpreted counter counts clicks");

  // A second Run ends the first run's tree (Halogen.IO.Util.endPreviousRun)
  // before it mounts its own, so there is one counter, a new one, at 0.
  await counter.evaluate((button) => (button.dataset.first = "1"));
  await run(2);
  await page.locator("#app button", { hasText: "Clicked 0 times" }).waitFor({ timeout: 10000 });
  assert.equal(await counter.count(), 1, "one counter after the second run");
  assert.equal(await page.locator("#app button[data-first]").count(), 0, "the first run's counter is gone");
  await counter.click();
  await page.locator("#app button", { hasText: "Clicked 1 times" }).waitFor({ timeout: 10000 });
  console.log("ok  run 2 replaced the counter with a new one, which counts");

  // Run 2 compiled the same source again, which GHC finds unchanged; an edit
  // is what a reader does, and has Main compiled anew.
  await page.locator("#source").evaluate((area) => (area.value = area.value.replace('"Clicked "', '"Pressed "')));
  await run(3);
  await page.locator("#app button", { hasText: "Pressed 0 times" }).waitFor({ timeout: 10000 });
  assert.equal(await counter.count(), 1, "one counter after the third run");
  console.log("ok  run 3 compiled the edited program and replaced the counter");

  // A layer: haskell-halogen-hooks in an archive and a package DB of its
  // own, which the page extracted on top of the base one and passes to GHC.
  const layers = (await (await fetch(`${url}rootfs.json`)).json()).layers;
  if (layers.some((layer) => layer.name === "hooks")) {
    await page.locator("#ghcArgs").evaluate((input) => (input.value += " -package haskell-halogen-hooks"));
    await page.locator("#source").fill(hooksCounter);
    await run(4);
    const hooked = page.locator("#app button", { hasText: "Hooked 0 times" });
    await hooked.waitFor({ timeout: 10000 });
    assert.equal(await counter.count(), 1, "one counter after the hooks run");
    await counter.click();
    await page.locator("#app button", { hasText: "Hooked 1 times" }).waitFor({ timeout: 10000 });
    console.log("ok  run 4 compiled a hooks counter from the hooks layer, which counts");
  }

  noErrors("at the end");
  console.log("ok  no errors in the page");

  const { timings, memory } = await page.evaluate(() => window.learn);
  console.log("\nTimings (ms, as the page measured them):");
  for (const [name, ms] of Object.entries(timings)) console.log(`  ${name.padEnd(28)} ${String(ms).padStart(7)}`);
  console.log("Memory:");
  for (const [when, { wasm }] of Object.entries(memory)) {
    const heap = heaps[when] === undefined ? "?" : mb(heaps[when]);
    console.log(`  after ${when.padEnd(22)} JS heap ${heap}, wasm memory ${wasm === null ? "?" : mb(wasm)}`);
  }
  for (const [when, renderers] of Object.entries(rendererAt)) {
    const shown = renderers.map(({ rss, peak }) => `${mb(rss)} (peak ${mb(peak)})`).join(", ");
    console.log(`  renderer RSS ${when.padEnd(16)} ${shown}`);
  }
} catch (error) {
  failed = true;
  console.error(`FAIL ${error.message}`);
  if (errors.length) console.error("page errors:", errors);
} finally {
  await browser.close();
  await browserServer.close();
  server.close();
}

process.exit(failed ? 1 : 0);
