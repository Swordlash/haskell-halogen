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
// browser_wasi_shim is the copy beside this page (from learn/vendor/), which
// the build also points dyld.mjs at, so that the page and dyld share one
// instance of the module (dyld checks the file system's objects against its
// own classes).
import { ConsoleStdout, File, OpenFile, PreopenDirectory, WASI } from "./browser_wasi_shim.js";
import { DyLDBrowserHost, main } from "./dyld.mjs";

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

// What the page measured, for toolchain/test-playground.mjs to read.
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

// An archive as the manifest lists it: its parts, fetched in order and
// joined (an archive larger than a file served may be is split), with the
// bytes so far against the total, for a progress line. The first manifest had
// only `archive`.
async function fetchArchive(entry, progress) {
  const parts = entry.parts ?? [{ path: entry.archive, size: entry.compressedSize ?? 0 }];
  const total = entry.compressedSize ?? parts.reduce((sum, part) => sum + part.size, 0);
  const chunks = [];
  let received = 0;
  for (const part of parts) {
    const response = await fetch(part.path);
    if (!response.ok) throw new Error(`${part.path}: ${response.status}`);
    const reader = response.body.getReader();
    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;
      chunks.push(value);
      received += value.byteLength;
      progress?.(received, total);
    }
  }
  const bytes = new Uint8Array(received);
  let offset = 0;
  for (const chunk of chunks) {
    bytes.set(chunk, offset);
    offset += chunk.byteLength;
  }
  return bytes;
}

// bsdtar -x reads the archive from stdin and extracts it into the directory
// it is given as its working directory: the root, each layer's on top of the
// base one's.
async function extract(bsdtar, rootfs, bytes, what) {
  const wasi = new WASI(
    ["bsdtar.wasm", "-x"],
    [],
    [
      new OpenFile(new File(bytes, { readonly: true })),
      ConsoleStdout.lineBuffered((msg) => console.info(`bsdtar: ${msg}`)),
      ConsoleStdout.lineBuffered((msg) => console.warn(`bsdtar: ${msg}`)),
      rootfs,
    ],
    { debug: false },
  );
  const instance = await WebAssembly.instantiate(bsdtar, { wasi_snapshot_preview1: wasi.wasiImport });
  const code = wasi.start(instance);
  if (code !== 0) throw new Error(`bsdtar exited with ${code} on ${what}`);
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
  const rootfs = new PreopenDirectory("/", []);

  status(`Downloading the compiler${base.compressedSize ? ` (${mb(base.compressedSize)})` : ""}…`);
  let start = performance.now();
  const [bsdtar, bytes] = await Promise.all([
    WebAssembly.compileStreaming(fetch("./bsdtar.wasm")),
    fetchArchive(base, (received, total) =>
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
  dyld = await main({
    rpc: new DyLDBrowserHost({
      rootfs,
      stdout: (msg) => {
        $("stdout").textContent += `${msg}\n`;
      },
      stderr: (msg) => {
        $("stderr").textContent += `${msg}\n`;
      },
    }),
    searchDirs: config.searchDirs,
    mainSoPath: config.mainSoPath,
    args: ["libplayground.so", "+RTS", ...rts, "-RTS"],
    isIserv: false,
  });
  const run = await dyld.exportFuncs.playground(config.libdir);
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
          const bytes = await fetchArchive(layer, (received, total) =>
            status(`Downloading the ${name} layer: ${mb(received)}${total ? ` of ${mb(total)}` : ""}…`),
          );
          const fetched = performance.now() - start;
          status(`Unpacking the ${name} layer…`);
          await extract(bsdtar, rootfs, bytes, layer.archive);
          await Promise.all((layer.assets ?? []).map(addAsset));
          const ms = performance.now() - start;
          learn.layers[name] = { ms: Math.round(ms), fetchMs: Math.round(fetched), bytes: bytes.byteLength };
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
      try {
        await run($("ghcArgs").value, $("source").value);
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
