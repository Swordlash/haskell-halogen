// A worker script of the page's own, for halogenWorker.script (the page's
// ?workers=script): what the script Halogen.Worker makes for a wasm program
// does, written out, so that the form a program with a loader of its own
// uses (the tutorial's, whose program is not a binary) is tested end to end
// here. Halogen.Worker imports it in each worker it starts, once
// globalThis.halogenWorker says which worker this is; it loads the WASI
// shim, the program's JSFFI glue and app.wasm from beside itself and runs
// main, whose Worker.program serves the page's requests. The import of this
// module ends once main has, which is when the worker checks that main
// served.
import { WASI, OpenFile, File, ConsoleStdout } from "./wasi.js";
import jsffi from "./ghc_wasm_jsffi.js";

const prefix = `[worker ${globalThis.halogenWorker.worker}] `;
const wasi = new WASI([], [], [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((line) => console.log(prefix + line)),
  ConsoleStdout.lineBuffered((line) => console.error(prefix + line)),
]);
const exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch(new URL("./app.wasm", import.meta.url)), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: jsffi(exports),
});
Object.assign(exports, instance.exports);
wasi.initialize(instance);
await instance.exports.hs_start();
