// GHC in the page: unpack rootfs.tar.zst into an in-memory file system, load
// libplayground.so (learn/Playground.hs) with GHC's wasm dynamic linker, and
// on Run have it compile the text area's Main to bytecode and run Main.main,
// which mounts into #app.
//
// The further layers that rootfs.json lists (hooks, router, ..., th) are
// loaded when asked for, by their buttons, by ?layers=name,name (or
// ?layers=all) on the page's address, or by window.learn.loadLayer(name):
// the archive is extracted on top of the base one, the layer's CSS and
// JavaScript are added to the page, and its package DB and packages to the
// GHC arguments. The playground rereads the package DBs on every run, so a
// layer loaded after a run is there for the next.
//
// A run's program can start Web Workers of itself (haskell-halogen-worker's
// pools): before each run the page names worker.js as halogenWorker.script,
// with the run's arguments, its source and the layers loaded, and each worker
// starts a compiler of its own from it, compiles the same program and runs
// its main, which serves the pool's jobs (worker.js says how).
//
// browser_wasi_shim is the copy beside this page (from learn/vendor/), which
// the build also points dyld.mjs at, so that the page and dyld share one
// instance of the module (dyld checks the file system's objects against its
// own classes). compiler.js imports both, and is what this page and
// worker.js share.
import { Directory, File, emptyRoot, extract, fetchArchive, startPlayground } from "./compiler.js";

// -Wno-missed-extra-shared-lib: the RTS's package names libdl, which wasi
// has no shared library for and nothing needs, and the interpreter warns
// that it could not load it on every run (try-miso passes the same).
const defaultArgs = "-package haskell-halogen-core -Wno-missed-extra-shared-lib";

const defaultSource = `{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- A counter: a button that counts its clicks. Edit it and press Run.
module Main (main) where

import Control.Monad.State (modify)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.IO.Util qualified as HA
import Halogen.VDom.DOM.Monad (BrowserDOM, runBrowserDOM)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector (..))

data Action = Increment

counter :: H.Component q () o BrowserDOM
counter =
  H.mkComponent
    H.ComponentSpec
      { H.initialState = \\_ -> pure (0 :: Int)
      , H.render = \\n ->
          HH.button
            [HE.onClick (const Increment)]
            [HH.text ("Clicked " <> T.pack (show n) <> " times")]
      , H.eval = H.mkEval H.defaultEval {H.handleAction = \\Increment -> modify (+ 1)}
      }

main :: IO ()
main = runBrowserDOM $ do
  -- Run replaces the last run's counter rather than add another.
  HA.endPreviousRun
  app <- maybe (error "the page has no #app") pure =<< HA.selectElement (QuerySelector "#app")
  socket <- runUI counter () app
  HA.keepForNextRun socket.dispose
`;

const $ = (id) => document.getElementById(id);

// What the page measured, for the playground's test
// (examples/learn/playground-test) to read.
const learn = (window.learn = {
  ready: false,
  runs: 0,
  timings: {},
  memory: {},
  layers: {},
  error: null,
  loadLayer: (name) => Promise.reject(new Error("the compiler has not loaded yet")),
});
let dyld;

const mb = (bytes) => `${(bytes / 1e6).toFixed(1)} MB`;

function measureMemory(when) {
  const heap = performance.memory?.usedJSHeapSize;
  // dyld gives every .so one shared memory, which is the only wasm memory
  // alive once bsdtar has finished.
  const wasm = dyld?.exportFuncs.memory.buffer.byteLength;
  learn.memory[when] = { jsHeap: heap ?? null, wasm: wasm ?? null };
  return [heap === undefined ? null : `JS heap ${mb(heap)}`, wasm === undefined ? null : `wasm memory ${mb(wasm)}`]
    .filter(Boolean)
    .join(", ");
}

function record(name, ms, extra) {
  learn.timings[name] = Math.round(ms);
  const line = `${name}: ${Math.round(ms)} ms${extra ? ` (${extra})` : ""}`;
  $("timings").textContent += `${line}\n`;
  console.log(`learn: ${line}`);
}

function status(text) {
  $("status").textContent = text;
}

// A layer's CSS and JavaScript: a <link> or a <script>, resolved once it has
// loaded, so that the first run finds its globals.
function addAsset(asset) {
  return new Promise((resolve, reject) => {
    const element =
      asset.kind === "stylesheet"
        ? Object.assign(document.createElement("link"), { rel: "stylesheet", href: asset.path })
        : Object.assign(document.createElement("script"), { src: asset.path });
    element.addEventListener("load", resolve);
    element.addEventListener("error", () => reject(new Error(`${asset.path} did not load`)));
    document.head.append(element);
  });
}

async function load() {
  $("source").value = defaultSource;
  $("ghcArgs").value = defaultArgs;

  // RTS options, for trying others: ?rts=-c+-H64m
  const query = new URLSearchParams(location.search);
  const rts = (query.get("rts") ?? "-c").split(/\s+/).filter(Boolean);

  const config = await (await fetch("./rootfs.json")).json();
  const base = config.rootfs ?? { archive: "rootfs.tar.zst" };
  const rootfs = emptyRoot();

  status(`Downloading the compiler${base.compressedSize ? ` (${mb(base.compressedSize)})` : ""}…`);
  let start = performance.now();
  const [bsdtar, bytes] = await Promise.all([
    WebAssembly.compileStreaming(fetch("./bsdtar.wasm")),
    fetchArchive(document.baseURI, base, (received, total) =>
      status(`Downloading the compiler: ${mb(received)}${total ? ` of ${mb(total)}` : ""}…`),
    ),
  ]);
  record(`fetch ${base.archive}`, performance.now() - start, mb(bytes.byteLength));

  status("Unpacking…");
  start = performance.now();
  await extract(bsdtar, rootfs, bytes, base.archive);
  record(`extract ${base.archive}`, performance.now() - start);

  status("Loading the compiler…");
  start = performance.now();
  const started = await startPlayground(config, rootfs, {
    rts,
    stdout: (msg) => {
      $("stdout").textContent += `${msg}\n`;
    },
    stderr: (msg) => {
      $("stderr").textContent += `${msg}\n`;
    },
  });
  dyld = started.dyld;
  const run = started.run;
  // For Halogen.Lazy.Dyld, which links further libraries into a program
  // through this dyld (the lazy layer's demo library).
  globalThis.halogenDyld = { dyld, rootfs, shim: { Directory, File } };
  record("dyld load", performance.now() - start, measureMemory("loaded"));

  // One thing at a time: a run, or a layer being loaded.
  let busy = Promise.resolve();
  const exclusive = (task) => {
    const next = busy.then(async () => {
      $("run").disabled = true;
      try {
        return await task();
      } finally {
        $("run").disabled = false;
      }
    });
    busy = next.catch(() => {});
    return next;
  };

  const loading = new Map();
  // The layers extracted, in order, for a worker to extract too.
  const loaded = [];
  const loadLayer = (name) => {
    const layer = config.layers.find((l) => l.name === name);
    if (!layer) return Promise.reject(new Error(`no layer ${name} in rootfs.json`));
    if (!loading.has(name)) {
      const button = $(`layer-${name}`);
      const load = exclusive(async () => {
          if (button) button.disabled = true;
          const start = performance.now();
          const size = layer.compressedSize ? ` (${mb(layer.compressedSize)})` : "";
          status(`Downloading the ${name} layer${size}…`);
          const bytes = await fetchArchive(document.baseURI, layer, (received, total) =>
            status(`Downloading the ${name} layer: ${mb(received)}${total ? ` of ${mb(total)}` : ""}…`),
          );
          const fetched = performance.now() - start;
          status(`Unpacking the ${name} layer…`);
          await extract(bsdtar, rootfs, bytes, layer.archive);
          await Promise.all((layer.assets ?? []).map(addAsset));
          const ms = performance.now() - start;
          learn.layers[name] = { ms: Math.round(ms), fetchMs: Math.round(fetched), bytes: bytes.byteLength };
          loaded.push(name);
          record(`layer ${name}`, ms, `${mb(bytes.byteLength)}, fetched in ${Math.round(fetched)} ms`);
          // Its package DB, and its packages, for the next run.
          const packages = (layer.packages ?? []).map((p) => `-package ${p}`);
          $("ghcArgs").value = [$("ghcArgs").value, `-package-db ${layer.packageDb}`, ...packages].join(" ");
          if (button) button.textContent = `${name} ✓`;
          status(`Loaded the ${name} layer.`);
        });
      // A load that fails (a dropped connection, a 503) is forgotten, so the
      // next call fetches again; one under way or done is shared.
      loading.set(
        name,
        load.catch((error) => {
          loading.delete(name);
          if (button) button.disabled = false;
          status(`Could not load the ${name} layer: ${error.message}`);
          throw error;
        }),
      );
    }
    return loading.get(name);
  };
  learn.loadLayer = loadLayer;

  const failedLayer = (name) => (error) => {
    status(`Could not load the ${name} layer: ${error}`);
    console.warn(`learn: layer ${name} failed:`, error);
  };
  for (const layer of config.layers) {
    const button = document.createElement("button");
    button.id = `layer-${layer.name}`;
    button.textContent = layer.compressedSize ? `${layer.name} (${mb(layer.compressedSize)})` : layer.name;
    button.title = (layer.packages ?? []).join(", ");
    button.addEventListener("click", () => loadLayer(layer.name).catch(failedLayer(layer.name)));
    $("layers").append(button);
  }

  $("run").addEventListener("click", () =>
    exclusive(async () => {
      $("stdout").textContent = "";
      $("stderr").textContent = "";
      const n = learn.runs + 1;
      status(`Compiling and running (run ${n})…`);
      const start = performance.now();
      // Where a worker of this run's program is: a compiler of its own,
      // which compiles the same source with the same arguments (so that its
      // static pointers have the page's keys) and runs its main.
      const args = $("ghcArgs").value;
      const source = $("source").value;
      globalThis.halogenWorker = {
        script: new URL("./worker.js", import.meta.url).href,
        data: { args, source, layers: [...loaded], rts },
      };
      try {
        await run(args, source);
        record(`run ${n}`, performance.now() - start, measureMemory(`run ${n}`));
        status(`Ran (run ${n}).`);
      } catch (error) {
        record(`run ${n} (failed)`, performance.now() - start);
        status(`Run ${n} failed: ${error}`);
        console.warn(`learn: run ${n} failed:`, error);
      } finally {
        learn.runs = n;
      }
    }),
  );
  $("run").disabled = false;
  learn.ready = true;
  status(`Ready (${config.ghc}).`);

  const wanted = query.get("layers");
  if (wanted) {
    const names = wanted === "all" ? config.layers.map((l) => l.name) : wanted.split(",").filter(Boolean);
    for (const name of names) await loadLayer(name).catch(failedLayer(name));
  }
}

load().catch((error) => {
  learn.error = String(error?.stack ?? error);
  status(`Failed to load: ${error}`);
  console.error("learn:", error);
});
