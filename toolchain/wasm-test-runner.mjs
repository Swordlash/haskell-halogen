import fs from "node:fs";
import { pathToFileURL } from "node:url";
import { WASI } from "node:wasi";

const [wasmPath, jsffiPath, ...testArgs] = process.argv.slice(2);
const wasi = new WASI({
  args: [wasmPath, ...testArgs],
  env: process.env,
  preopens: { "/": "/" },
  version: "preview1",
  returnOnExit: true,
});
const exports = {};
const ghcWasmImports = (await import(pathToFileURL(jsffiPath))).default;
const module = await WebAssembly.compile(fs.readFileSync(wasmPath));
const instance = await WebAssembly.instantiate(module, {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghcWasmImports(exports),
});

Object.assign(exports, instance.exports);

// A test that waits for promises is a reactor exporting hs_test, an async
// export: a command's main cannot wait for a promise, since start has to
// return before the event loop can settle one. Its promise resolves once the
// test has passed and rejects with what it failed with.
if (typeof instance.exports.hs_test === "function") {
  wasi.initialize(instance);
  try {
    await instance.exports.hs_test();
  } catch (error) {
    console.error(`${wasmPath}: ${error?.message ?? error}`);
    process.exit(1);
  }
  process.exit(0);
}

// Exit before the event loop runs again. A JavaScript callback into Haskell
// can leave the RTS with rts_schedulerLoop queued on setImmediate, which
// cannot run until start returns and fails once the RTS has shut down.
process.exit(wasi.start(instance));
