// A Web Worker of a lesson's program. Before each compile the page names
// this module, bundled by bundle.sh beside learn.js, as
// globalThis.halogenWorker.script (playground.js's offerWorkers), and
// haskell-halogen-worker imports it in each worker a pool of that program
// starts. The program is no binary a worker could load: it is the lesson,
// run in the page's GHC. So the worker is what an application's worker is,
// a second copy of the program, made the only way this program can be: a
// second compiler, started here as the page started its own (the same
// rootfs, read from the Cache API where the page kept it, and the same
// layers), compiling the same Lesson.hs and Main.hs with the same arguments
// and flags, then running that main. Compiled alike, the program has the
// page's static pointer table, whose keys are a hash of the module and
// each static's place in it, so a job the page names by its key is found
// here. Lesson 17's main is Worker.program, which in a worker serves the
// pool's requests and returns, which is when this module's import ends.
//
// halogenWorker.data is { root, layers, lesson, main, args }: the rootfs'
// address as start takes it, the layers the compile used, the two modules'
// sources and the lesson's own GHC arguments, as compile's request has them.
// What the program writes once it serves goes to the console, each line
// prefixed with the worker's number. A lesson that does not compile here, or
// whose main fails (the default Main, which mounts into a page this worker
// has not), fails the import, and the pool fails the worker's jobs with why.
import { compile, ensureLayers, logTo, start } from "./playground.js";

const { worker, data } = globalThis.halogenWorker;
const prefix = `[worker ${worker}] `;
logTo((stream, line) => (stream === "stderr" ? console.warn : console.log)(prefix + line));

await start(data.root, data.layers);
// A layer that did not load while the compiler started is reported here.
await ensureLayers(data.layers);
const result = await compile({ lesson: data.lesson, main: data.main, args: data.args, layers: data.layers });
if (!result.ok) {
  throw new Error(`the lesson did not ${result.compiled ? "run" : "compile"} in the worker: ${result.error ?? result.stderr}`);
}
