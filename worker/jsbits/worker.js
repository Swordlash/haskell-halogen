// Web Workers for Halogen.Worker. The GHC JavaScript backend links this file
// and calls these by name; the wasm backend embeds it into its JSFFI module
// (Halogen.JSBits.browserJS).
//
// A worker is a second instance of the program itself: the page's loader
// says where the program is, in globalThis.halogenWorker, before it starts
// Haskell, and a pool starts each worker from a script made here, which
// loads the program the way the page did and runs its main. That main calls
// Halogen.Worker.serve first, which finds itself in a worker and answers
// messages rather than going on to mount the page.
//
// The messages are plain objects holding strings:
//   worker to page   { ready: true }                 serve is listening
//                    { failed: message }             the program did not start
//                    { id, result: json }            a job's result
//                    { id, error: message }          a job that failed
//   page to worker   { id, job: name, args: json }   run a job
//
// A worker is told which it is by halogenWorker.worker, a number, which is
// also how serve tells a worker of a pool from the page.

// Whether a pool can start workers here: the page has said where the
// program is, for the backend it was built with ("wasm32" or "javascript"),
// and has Worker. Inside a worker of a pool, never: a job runs where it is.
function halogen_worker_available(arch) {
  const config = globalThis.halogenWorker;
  if (typeof Worker !== "function" || config === null || typeof config !== "object") return false;
  if (typeof config.worker === "number") return false;
  return arch === "wasm32"
    ? typeof config.wasm === "string" && typeof config.jsffi === "string" && typeof config.wasi === "string"
    : typeof config.js === "string";
}

function halogen_worker_pool(size, arch) {
  return { size: Math.max(1, size), arch: arch, workers: [], next: 0, ids: 0 };
}

// An address the page gave, made absolute: a worker started from a blob has
// no address of its own to resolve a relative one against.
function halogen_worker_absolute(address) {
  return new URL(address, globalThis.document?.baseURI ?? globalThis.location?.href).href;
}

// The worker's script. On WebAssembly a module that loads the WASI shim, the
// program's JSFFI glue and its binary, as the page's loader does, and calls
// hs_start; with the JavaScript backend a classic script that imports the
// program's all.js, which runs main itself.
function halogen_worker_script(pool, index) {
  const page = globalThis.halogenWorker;
  if (pool.arch !== "wasm32") {
    const config = { js: halogen_worker_absolute(page.js), worker: index };
    return {
      type: "classic",
      source: `self.halogenWorker = ${JSON.stringify(config)};
try {
  importScripts(self.halogenWorker.js);
} catch (error) {
  postMessage({ failed: String(error?.message ?? error) });
}
`,
    };
  }
  const config = {
    wasm: halogen_worker_absolute(page.wasm),
    jsffi: halogen_worker_absolute(page.jsffi),
    wasi: halogen_worker_absolute(page.wasi),
    worker: index,
  };
  return {
    type: "module",
    source: `const config = ${JSON.stringify(config)};
globalThis.halogenWorker = config;
const prefix = "[worker " + config.worker + "] ";
try {
  const { WASI, OpenFile, File, ConsoleStdout } = await import(config.wasi);
  const { default: jsffi } = await import(config.jsffi);
  const wasi = new WASI([], [], [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered((line) => console.log(prefix + line)),
    ConsoleStdout.lineBuffered((line) => console.error(prefix + line)),
  ]);
  const exports = {};
  const { instance } = await WebAssembly.instantiateStreaming(fetch(config.wasm), {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: jsffi(exports),
  });
  Object.assign(exports, instance.exports);
  wasi.initialize(instance);
  await instance.exports.hs_start();
  if (!config.serving) throw new Error("the program's main did not call Halogen.Worker.serve");
} catch (error) {
  postMessage({ failed: String(error?.message ?? error) });
}
`,
  };
}

// Reject everything the worker was given and end it, and forget it, so that
// the next job for its place in the pool starts a new one.
function halogen_worker_fail(pool, index, entry, message) {
  entry.worker.terminate();
  URL.revokeObjectURL(entry.url);
  for (const { reject } of entry.pending.values()) reject(new Error(message));
  entry.pending.clear();
  entry.queue = [];
  if (pool.workers[index] === entry) pool.workers[index] = undefined;
}

function halogen_worker_start(pool, index) {
  const { type, source } = halogen_worker_script(pool, index);
  const url = URL.createObjectURL(new Blob([source], { type: "text/javascript" }));
  let worker;
  try {
    worker = new Worker(url, { type: type, name: `halogen-worker-${index}` });
  } catch (error) {
    // A browser that refuses a Blob worker (a SecurityError under some
    // policies) throws here, before any message: the URL goes with it.
    URL.revokeObjectURL(url);
    throw error;
  }
  const entry = { worker: worker, url: url, ready: false, queue: [], pending: new Map() };
  worker.onmessage = (event) => {
    const data = event.data;
    if (data.ready) {
      // Messages posted before serve listened would have gone to nobody, so
      // the jobs given until now were kept for this.
      entry.ready = true;
      URL.revokeObjectURL(url);
      for (const message of entry.queue) worker.postMessage(message);
      entry.queue = [];
    } else if (data.failed !== undefined) {
      halogen_worker_fail(pool, index, entry, `the worker did not start: ${data.failed}`);
    } else {
      const waiting = entry.pending.get(data.id);
      if (waiting === undefined) return;
      entry.pending.delete(data.id);
      if (data.error !== undefined) waiting.reject(new Error(data.error));
      else waiting.resolve(data.result);
    }
  };
  // An error the worker's script did not catch (one that did not load at
  // all, say): reported to the program as its jobs' failure, rather than to
  // the page's console as well.
  worker.onerror = (event) => {
    event.preventDefault();
    halogen_worker_fail(pool, index, entry, `the worker failed: ${event.message || "its script did not load"}`);
  };
  return entry;
}

// Run a job in the next worker of the pool, starting that worker if it has
// not been, and give a promise of the result's JSON, rejected with the job's
// error. The promise is marked as handled here, since the program may stop
// waiting for it (its fiber killed), and its rejection should not be
// reported as unhandled then.
function halogen_worker_run(pool, job, args) {
  const index = pool.next;
  pool.next = (pool.next + 1) % pool.size;
  let entry = pool.workers[index];
  if (entry === undefined) {
    // A worker that cannot be made is this job's failure, through the
    // promise like any other, not an exception out of this call: available
    // only knows that Worker and the configuration exist.
    try {
      entry = pool.workers[index] = halogen_worker_start(pool, index);
    } catch (error) {
      const failed = Promise.reject(new Error(`the worker did not start: ${error?.message ?? error}`));
      failed.catch(() => {});
      return failed;
    }
  }
  const id = ++pool.ids;
  const promise = new Promise((resolve, reject) => entry.pending.set(id, { resolve: resolve, reject: reject }));
  promise.catch(() => {});
  const message = { id: id, job: job, args: args };
  if (entry.ready) entry.worker.postMessage(message);
  else entry.queue.push(message);
  return promise;
}

function halogen_worker_terminate(pool) {
  pool.workers.forEach((entry, index) => {
    if (entry !== undefined) halogen_worker_fail(pool, index, entry, "the pool was terminated");
  });
  pool.workers = [];
  pool.next = 0;
}

// The worker's side.

function halogen_worker_in_worker() {
  return typeof globalThis.halogenWorker?.worker === "number";
}

function halogen_worker_listen(callback) {
  globalThis.halogenWorker.serving = true;
  self.onmessage = (event) => callback(event.data);
  self.postMessage({ ready: true });
}

function halogen_worker_request_id(request) {
  return request.id;
}

function halogen_worker_request_job(request) {
  return String(request.job);
}

function halogen_worker_request_args(request) {
  return String(request.args);
}

function halogen_worker_reply(id, ok, text) {
  self.postMessage(ok ? { id: id, result: text } : { id: id, error: text });
}
