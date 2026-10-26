import { WASI, OpenFile, File, ConsoleStdout } from "./wasi.js";
import ghcWasmImports from "./ghc_wasm_jsffi.js";

// Where the program is, for Halogen.Worker to start a worker of it: the
// binary, its JSFFI glue and the WASI shim this page imports (bundle.sh puts
// it beside the page as wasi.js). It must be set before Haskell starts.
//
// With ?workers=none in the address the page says nothing, and the jobs run
// on the page instead, as wherever no worker can be made (the tutorial's
// lesson 17 is such a place).
if (new URLSearchParams(location.search).get("workers") !== "none") {
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
