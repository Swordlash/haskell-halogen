import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";
import ghcWasmImports from "./ghc_wasm_jsffi.js";

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
