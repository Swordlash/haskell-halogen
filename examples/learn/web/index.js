// The source of the page's index.js, which bundle.sh bundles with the shim
// from node_modules (the version package-lock.json pins), so that the page
// loads nothing from a CDN; it is not served as it is.
import { WASI, OpenFile, File, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import ghcWasmImports from "./ghc_wasm_jsffi.js";
// The compiler and the editor, which the program's bindings call
// (examples/learn/js/learn.js, bundled by bundle.sh).
import "./learn.js";

const wasi = new WASI([], [], [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((line) => console.log(line)),
  ConsoleStdout.lineBuffered((line) => console.error(line)),
], { debug: false });
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
