// Drive the in-browser playground (learn/) in headless Chromium: GHC loads in
// the page, compiles the default program (a Halogen counter), and the counter
// it mounts counts; a second Run replaces it, and so does a third of an edited
// program. Then, for each layer the build has (rootfs.json), the page loads
// it and runs a program that uses it, which loads its libraries and calls
// their JavaScript: a hooks counter, a router link, a fetch from this server,
// a Material button with its CSS, a Pixi scene on a canvas, and an hh quote
// from haskell-halogen-th.
//
//   npm run test-learn                      # after: npm run build-learn
//   node toolchain/test-learn.mjs [dir]     # default dist-newstyle/wasm/learn
//   LEARN_ONLY=router,net npm run test-learn   # only those layers
//
// It prints what the page measured (fetch, extract, loading the compiler, each
// layer, each run) and how much memory the page's renderer process used at its
// peak, and fails on any error in the page. The page imports browser_wasi_shim
// from esm.sh, and the pixi layer PixiJS from jsDelivr, so this needs the
// network; if jsDelivr cannot be reached, the pixi run is skipped and says so.
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

const only = process.env.LEARN_ONLY?.split(",").filter(Boolean);

const types = {
  ".css": "text/css",
  ".html": "text/html",
  ".js": "text/javascript",
  ".mjs": "text/javascript",
  ".json": "application/json",
  ".wasm": "application/wasm",
};

// A static server, as Pages is, and nothing else: no COOP/COEP headers, so
// the page is not cross-origin isolated and has no SharedArrayBuffer.
// It also answers learn-test/hello.txt, which the net layer's program
// fetches.
const hello = "Hello from the test server.";
const server = createServer(async (request, response) => {
  const path = decodeURIComponent(new URL(request.url, "http://x").pathname);
  if (path === "/learn-test/hello.txt") {
    response.writeHead(200, { "content-type": "text/plain" }).end(hello);
    return;
  }
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
const layerTimes = [];
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

// The program for each layer, and what shows that it worked: something
// that loads the layer's libraries and calls into their JavaScript. Each
// replaces the last run's tree, as the counter does.
const mainOf = (component) => String.raw`
main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI ${component} () app
  HA.keepForNextRun socket.dispose
`;

const mounting = String.raw`import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))`;

const appText = (page, text) => page.locator("#app", { hasText: text }).waitFor({ timeout: 10000 });

const layerRuns = {
  // A counter written with haskell-halogen-hooks.
  hooks: {
    what: "a hooks counter from the hooks layer, which counts",
    source:
      String.raw`{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module Main (main) where

import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.Hooks qualified as Hooks
${mounting}

counter :: H.Component q () o BrowserDOM
counter = Hooks.component @Empty $ \_ -> Hooks.do
  (n, nId) <- Hooks.useState (0 :: Int)
  Hooks.pure $
    HH.button [HE.onClick (const (Hooks.modify_ nId (+ 1)))] [HH.text ("Hooked " <> T.pack (show n) <> " times")]
` + mainOf("counter"),
    async verify(page) {
      await page.locator("#app button", { hasText: "Hooked 0 times" }).waitFor({ timeout: 10000 });
      await page.locator("#app button").click();
      await page.locator("#app button", { hasText: "Hooked 1 times" }).waitFor({ timeout: 10000 });
    },
  },

  // A hash router: where the page is, and a link that goes elsewhere.
  router: {
    what: "a router link from the router layer, followed in the page",
    source:
      String.raw`{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.State (put)
import Data.Row ((.+), (.==))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Router qualified as Router
import Halogen.Router.Duplex (RouteDuplex', noArgs, path, root)
import Halogen.Router.Duplex.Generic qualified as Duplex
${mounting}

data Route = Home | About
  deriving (Eq, Show, Generic)

route :: RouteDuplex' Route
route = root $ Duplex.sum $ #Home .== noArgs .+ #About .== path "about" noArgs

data Action = Start | Go (Router.Follow Route)

page :: Router.Router Route -> H.Component q () o BrowserDOM
page router =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure Home
      , H.render = \r ->
          HH.div_
            [ HH.p_ [HH.text ("route: " <> T.pack (show r))]
            , Router.link router Go About [] [HH.text "About"]
            ]
      , H.eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = \case
                  Start -> put =<< Router.current router
                  Go follow -> do
                    Router.follow router follow
                    put =<< Router.current router
              , H.initialize = Just Start
              }
      }

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  router <- Router.newHashRouter Home route
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI (page router) () app
  HA.keepForNextRun (socket.dispose >> Router.stop router)
`,
    async verify(page) {
      await appText(page, "route: Home");
      await page.locator("#app a", { hasText: "About" }).click();
      await appText(page, "route: About");
      const where = await page.evaluate(() => ({ hash: location.hash, path: location.pathname }));
      if (where.hash !== "#/about" || where.path !== "/") throw new Error(`the link went to ${JSON.stringify(where)}`);
    },
  },

  // A fetch of a file this server serves.
  net: {
    what: "a fetch from the net layer, its text shown",
    source:
      String.raw`{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.State (put)
import Data.Text (Text)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Net.Fetch qualified as Fetch
${mounting}

data Action = Load

fetched :: H.Component q () o BrowserDOM
fetched =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure ("waiting" :: Text)
      , H.render = \t -> HH.p_ [HH.text ("fetched: " <> t)]
      , H.eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = \Load -> do
                  result <- Fetch.fetch (Fetch.request Fetch.GET "learn-test/hello.txt")
                  body <- either (pure . Left) Fetch.text result
                  put (either (\e -> "failed: " <> e.name <> ": " <> e.message) id body)
              , H.initialize = Just Load
              }
      }
` + mainOf("fetched"),
    async verify(page) {
      await appText(page, `fetched: ${hello}`);
    },
  },

  // A Material button, whose ripple is material.js's and whose look is
  // material.css's.
  material: {
    what: "a Material button from the material layer, with its CSS and JavaScript, clicked",
    source:
      String.raw`{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad.State (modify)
import Data.Row (type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.Material.Button qualified as Button
${mounting}

type Slots = "button" .== H.Slot H.VoidF Button.ButtonClicked ()

data Action = Clicked

clicks :: H.Component q () o BrowserDOM
clicks =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure (0 :: Int)
      , H.render = render
      , H.eval = H.mkEval H.defaultEval {H.handleAction = \Clicked -> modify (+ 1)}
      }
  where
    render :: Int -> H.ComponentHTML Action Slots BrowserDOM
    render n =
      HH.div_
        [ HH.slot "button" () Button.button Button.emptyButtonSpec {Button.label = "Material", Button.style = Just Button.Raised} (const Clicked)
        , HH.p_ [HH.text ("material clicks: " <> T.pack (show n))]
        ]
` + mainOf("clicks"),
    async verify(page) {
      const button = page.locator("#app button.mdc-button");
      await button.waitFor({ timeout: 10000 });
      // MDCRipple, from material.js, marks the button it took.
      await page.locator("#app button.mdc-ripple-upgraded").waitFor({ timeout: 10000 });
      const background = await button.evaluate((b) => getComputedStyle(b).backgroundColor);
      if (background !== "rgb(98, 0, 238)") throw new Error(`the button's background is ${background}: no material.css?`);
      await button.click();
      await appText(page, "material clicks: 1");
    },
  },

  // A rectangle on a Pixi canvas, tapped.
  pixi: {
    what: "a Pixi scene from the pixi layer, on a canvas, tapped",
    async skip(page) {
      const { layers } = await (await fetch(`${url}rootfs.json`)).json();
      const cdn = layers.find((l) => l.name === "pixi")?.external?.[0]?.url;
      if (!cdn) return null;
      // Probed from a page of its own: a fetch that fails logs a console
      // error, which the test page counts as an error of the page.
      const probe = await page.context().browser().newPage();
      const reachable = await probe.evaluate(
        (cdn) =>
          fetch(cdn).then(
            (r) => r.ok,
            () => false,
          ),
        cdn,
      );
      await probe.close();
      return reachable ? null : `${cdn} cannot be reached from the browser, and the pixi layer loads PixiJS from there`;
    },
    source:
      String.raw`{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Clay qualified as C
import Control.Monad.State (modify)
import Data.Row (type (.==))
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.Canvas.Elements qualified as CE
import Halogen.Canvas.Pixi qualified as Pixi
import Halogen.Canvas.Properties qualified as CP
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
${mounting}

type Slots = "canvas" .== H.Slot H.VoidF (Pixi.CanvasEvent ()) ()

newtype Action = Canvas (Pixi.CanvasEvent ())

scene :: Pixi.View ()
scene =
  Pixi.view
    Pixi.defaultCamera
    Pixi.defaultInteraction
    [CE.rectangle (Pixi.Point 0 0) (Pixi.Point 400 400) (Just (Pixi.FillStyle 0x0ea5e9 1)) Nothing [CP.onClick (const (Just ()))]]

taps :: H.Component q () o BrowserDOM
taps =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure (0 :: Int)
      , H.render = render
      , H.eval = H.mkEval H.defaultEval {H.handleAction = handle}
      }
  where
    render :: Int -> H.ComponentHTML Action Slots BrowserDOM
    render n =
      HH.div_
        [ HH.div
            [HP.style (C.position C.relative >> C.width (C.px 300) >> C.height (C.px 200))]
            [HH.slot "canvas" () Pixi.component scene Canvas]
        , HH.p_ [HH.text ("pixi taps: " <> T.pack (show n))]
        ]
    handle (Canvas (Pixi.Fired ())) = modify (+ 1)
    handle (Canvas _) = pure ()
` + mainOf("taps"),
    async verify(page) {
      const canvas = page.locator("#app canvas");
      await canvas.waitFor({ timeout: 10000 });
      // PixiJS arrives from the CDN and initialises asynchronously; until it
      // has, a tap goes nowhere. Tap until one is counted, below the scene's
      // origin (the canvas's centre) and inside the rectangle wherever its
      // corner is.
      const deadline = Date.now() + 60000;
      for (;;) {
        const box = await canvas.boundingBox();
        if (box && box.width > 0) await page.mouse.click(box.x + box.width / 2 + 40, box.y + box.height / 2 + 40);
        try {
          await page.locator("#app", { hasText: /pixi taps: [1-9]/ }).waitFor({ timeout: 1000 });
          break;
        } catch (error) {
          if (Date.now() > deadline) throw new Error(`no tap reached the Pixi scene (${error.message})`);
        }
      }
    },
  },

  // An hh quote, which runs haskell-halogen-th's quasi-quoter (and so
  // ghc-lib-parser, for the Haskell in it) in the page's interpreter.
  th: {
    what: "an hh quote from the th layer, with Haskell inside",
    source:
      String.raw`{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text qualified as T
import Halogen qualified as H
import Halogen.TH (hh)
${mounting}

quoted :: H.Component q () o BrowserDOM
quoted =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \_ -> pure (6 :: Int)
      , -- Whitespace on the quote's one line would be a text beside the
        -- element (JSX's rule), and hh takes one element: hence the lines.
        H.render = \n ->
          [hh|
            <p class="quoted">hello from hh, {{ T.pack (show (n * 7)) }}</p>
          |]
      , H.eval = H.mkEval H.defaultEval
      }
` + mainOf("quoted"),
    async verify(page) {
      await page.locator("#app p.quoted", { hasText: "hello from hh, 42" }).waitFor({ timeout: 10000 });
    },
  },
};

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

  // The layers, each in an archive and a package DB of its own, which the
  // page extracts on top of the base one when asked to and passes to GHC.
  const { layers } = await (await fetch(`${url}rootfs.json`)).json();
  let n = 3;
  for (const layer of layers) {
    if (only && !only.includes(layer.name)) continue;
    const check = layerRuns[layer.name];
    const skip = check?.skip && (await check.skip(page));
    if (skip) {
      console.log(`skip the ${layer.name} layer: ${skip}`);
      continue;
    }
    await page.locator(`#layer-${layer.name}:enabled`).click();
    await page.waitForFunction((name) => window.learn.layers[name], layer.name, { timeout });
    noErrors(`while loading the ${layer.name} layer`);
    const loaded = await page.evaluate((name) => window.learn.layers[name], layer.name);
    if (!check) {
      console.log(`??  no program for the ${layer.name} layer: only loaded it`);
      layerTimes.push({ name: layer.name, load: loaded.ms, run: null });
      continue;
    }
    await page.locator("#source").fill(check.source);
    n += 1;
    await run(n);
    await check.verify(page);
    noErrors(`in the ${layer.name} layer's run`);
    const runMs = await page.evaluate((n) => window.learn.timings[`run ${n}`], n);
    layerTimes.push({ name: layer.name, load: loaded.ms, run: runMs });
    console.log(`ok  run ${n}: ${check.what}`);
  }

  noErrors("at the end");
  console.log("ok  no errors in the page");

  const { timings, memory } = await page.evaluate(() => window.learn);
  if (layerTimes.length) {
    console.log("\nLayers (ms; loading is fetching, extracting and adding assets; the first run loads its libraries):");
    for (const { name, load, run } of layerTimes)
      console.log(`  ${name.padEnd(10)} load ${String(load).padStart(6)}  first run ${String(run ?? "-").padStart(6)}`);
    const total = layerTimes.reduce((sum, t) => sum + t.load, 0);
    console.log(`  ${"all".padEnd(10)} load ${String(total).padStart(6)}`);
  }
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
