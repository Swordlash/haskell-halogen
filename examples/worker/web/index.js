import { WASI, OpenFile, File, ConsoleStdout } from "./wasi.js";
import ghcWasmImports from "./ghc_wasm_jsffi.js";

// Where the program is, for Halogen.Worker to start a worker of it: the
// binary, its JSFFI glue and the WASI shim this page imports (bundle.sh puts
// it beside the page as wasi.js). It must be set before Haskell starts.
//
// With ?workers=none in the address the page says nothing, and the jobs run
// on the page instead, as wherever no worker can be made. With
// ?workers=script it names a worker script of its own instead of the files,
// worker-script.js, which loads them itself: the form a program that is not
// a binary uses (the tutorial's lessons, run in GHC's interpreter).
const workers = new URLSearchParams(location.search).get("workers");
if (workers === "script") {
  globalThis.halogenWorker = { script: new URL("./worker-script.js", import.meta.url).href };
} else if (workers !== "none") {
  globalThis.halogenWorker = {
    wasm: new URL("./app.wasm", import.meta.url).href,
    jsffi: new URL("./ghc_wasm_jsffi.js", import.meta.url).href,
    wasi: new URL("./wasi.js", import.meta.url).href,
  };
}

const wasi = new WASI([], [], [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((line) => console.log(line)),
  ConsoleStdout.lineBuffered((line) => console.error(line)),
]);
const exports = {};
const { instance } = await WebAssembly.instantiateStreaming(
  fetch(new URL("./app.wasm", import.meta.url)),
  {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghcWasmImports(exports),
  },
);
Object.assign(exports, instance.exports);
wasi.initialize(instance);
await instance.exports.hs_start();
