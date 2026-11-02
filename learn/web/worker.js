// A Web Worker of a program the page ran: the page names this module as
// globalThis.halogenWorker.script before each run (index.js), and
// haskell-halogen-worker imports it in each worker a pool of that program
// starts. The program is not a binary a worker could load, it is the
// source the page compiled in its GHC; so the worker starts a GHC of its
// own, as the page did (the same archives, fetched again, the layers the
// page had loaded, the same RTS options), compiles
// the same source with the same arguments and runs its main. Compiled
// alike, the program has the same static pointer table as the page's, whose
// keys are a hash of the module and the static's place in it, so a job the
// page names by its key is found here. main's Worker.program serves the
// pool's requests and returns, which is when this module's import ends.
//
// halogenWorker.data is { args, source, layers, rts }. What GHC and the
// program write goes to the console, each line prefixed with the worker's
// number; a program that does not compile, or whose main fails, fails the
// import, and the pool fails the worker's jobs with why.
import { emptyRoot, extract, fetchArchive, startPlayground } from "./compiler.js";

const { worker, data } = globalThis.halogenWorker;
const prefix = `[worker ${worker}] `;
const base = new URL("./", import.meta.url).href;

const config = await (await fetch(new URL("rootfs.json", base))).json();
const rootfs = emptyRoot();
const archives = [config.rootfs ?? { archive: "rootfs.tar.zst" }, ...config.layers.filter((layer) => data.layers.includes(layer.name))];
const [bsdtar, ...bytes] = await Promise.all([
  WebAssembly.compileStreaming(fetch(new URL("bsdtar.wasm", base))),
  ...archives.map((archive) => fetchArchive(base, archive)),
]);
for (const [i, archive] of archives.entries()) await extract(bsdtar, rootfs, bytes[i], archive.archive);

// Only GHC's diagnostics are kept until the run is over, to say why it
// failed; what the program writes once it serves is logged as it comes.
let diagnostics = [];
const { run } = await startPlayground(config, rootfs, {
  rts: data.rts,
  stdout: (line) => console.log(prefix + line),
  stderr: (line) => (diagnostics ? diagnostics.push(line) : console.warn(prefix + line)),
});
try {
  await run(data.args, data.source);
} catch (error) {
  const last = diagnostics.filter((line) => line.trim() !== "").slice(-20).join("\n");
  throw new Error(`the program did not run in the worker: ${last || error?.message || error}`);
} finally {
  diagnostics = null;
}
