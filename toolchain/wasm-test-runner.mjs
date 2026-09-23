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
process.exitCode = wasi.start(instance);
