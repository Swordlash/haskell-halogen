// GHC in the page: unpack rootfs.tar.zst into an in-memory file system, load
// libplayground.so (learn/Playground.hs) with GHC's wasm dynamic linker, and
// on Run have it compile the text area's Main to bytecode and run Main.main,
// which mounts into #app.
//
// browser_wasi_shim comes from the address dyld.mjs imports it from, so that
// the page and dyld share one copy of it. Vendoring it means rewriting that
// import in the copy of dyld.mjs the build ships.
import { ConsoleStdout, File, OpenFile, PreopenDirectory, WASI } from "https://esm.sh/gh/haskell-wasm/browser_wasi_shim";
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

// What the page measured, for toolchain/test-learn.mjs to read.
const learn = (window.learn = { ready: false, runs: 0, timings: {}, memory: {}, error: null });
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

async function load() {
  $("source").value = defaultSource;

  // RTS options, for trying others: ?rts=-c+-H64m
  const rts = (new URLSearchParams(location.search).get("rts") ?? "-c").split(/\s+/).filter(Boolean);

  const config = await (await fetch("./rootfs.json")).json();
  // Each layer has a package DB of its own, for GHC to read beside the
  // global one.
  $("ghcArgs").value = [defaultArgs, ...config.layers.map((layer) => `-package-db ${layer.packageDb}`)].join(" ");
  const rootfs = new PreopenDirectory("/", []);

  status("Downloading the compiler…");
  let start = performance.now();
  const download = async (path) => {
    const response = await fetch(path);
    if (!response.ok) throw new Error(`${path}: ${response.status}`);
    const bytes = new Uint8Array(await response.arrayBuffer());
    record(`fetch ${path}`, performance.now() - start, mb(bytes.byteLength));
    return bytes;
  };
  const [bsdtar, ...archives] = await Promise.all([
    WebAssembly.compileStreaming(fetch("./bsdtar.wasm")),
    download("rootfs.tar.zst"),
    ...config.layers.map((layer) => download(layer.archive)),
  ]);
  record("fetch", performance.now() - start);

  // bsdtar -x reads the archive from stdin and extracts it into the
  // directory it is given as its working directory: the root, each layer's on
  // top of the base one's.
  status("Unpacking…");
  const paths = ["rootfs.tar.zst", ...config.layers.map((layer) => layer.archive)];
  for (const [i, bytes] of archives.entries()) {
    start = performance.now();
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
    if (code !== 0) throw new Error(`bsdtar exited with ${code} on ${paths[i]}`);
    record(`extract ${paths[i]}`, performance.now() - start);
  }
  archives.length = 0;

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

  $("run").addEventListener("click", async () => {
    $("run").disabled = true;
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
      $("run").disabled = false;
    }
  });
  $("run").disabled = false;
  learn.ready = true;
  status(`Ready (${config.ghc}).`);
}

load().catch((error) => {
  learn.error = String(error?.stack ?? error);
  status(`Failed to load: ${error}`);
  console.error("learn:", error);
});
