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
// The messages are plain objects, a job's data in them as bytes, each in an
// ArrayBuffer of its own that is transferred (postMessage's second
// argument) rather than copied:
//   worker to page   { ready: true }                     serve is listening
//                    { failed: message }                 the program did not start
//                    { id, ok: true, reply: buffer }     a request's results
//                    { id, ok: false, error: message }   a request that failed
//   page to worker   { id, request: buffer }             run a job
//                    { cancel: id }                      stop it at its next checkpoint
// What the bytes say is Haskell's business (Halogen.Worker.Internal): a
// request is the job's static key and a list of arguments, a reply the list
// of their results, in order, up to the first call that failed, and that
// one's failure. A list, so that a batch of calls of one job
// (Halogen.Worker.Par's) is one request; Halogen.Worker.start sends one.
//
// A worker is told which it is by halogenWorker.worker, a number, which is
// also how serve tells a worker of a pool from the page.
//
// The page keeps every request it has not had an answer to, under its id,
// with a copy of its bytes: a worker that is killed takes the requests it
// was given with it, and those are sent again to the worker started in its
// place. An answer to an id the page no longer keeps (a cancelled or killed
// job's) is dropped.

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

// started: which worker each promise the pool gave was sent to, and under
// which id, for cancel and kill to find the job from the promise alone.
function halogen_worker_pool(size, arch) {
  return { size: Math.max(1, size), arch: arch, workers: [], next: 0, ids: 0, started: new WeakMap() };
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

// A rejection's error, named as the Haskell side reads it (JSError's name).
function halogen_worker_error(name, message) {
  const error = new Error(message);
  error.name = name;
  return error;
}

// End the worker and forget it, so that the next job for its place in the
// pool starts a new one; give back what it had not answered, in the order
// it was given.
function halogen_worker_end(pool, index, entry) {
  entry.worker.terminate();
  URL.revokeObjectURL(entry.url);
  const unanswered = [...entry.pending];
  entry.pending.clear();
  entry.queue = [];
  if (pool.workers[index] === entry) pool.workers[index] = undefined;
  return unanswered;
}

// Reject everything the worker was given and end it.
function halogen_worker_fail(pool, index, entry, message) {
  for (const [, { reject }] of halogen_worker_end(pool, index, entry)) reject(new Error(message));
}

// Post a request the worker keeps, a copy of its bytes transferred, the
// page's own staying for a worker that may have to be sent it again.
function halogen_worker_post(entry, id) {
  const waiting = entry.pending.get(id);
  if (waiting === undefined) return;
  const buffer = waiting.request.slice().buffer;
  entry.worker.postMessage({ id: id, request: buffer }, [buffer]);
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
      // the jobs given until now were kept for this; those cancelled
      // meanwhile are no longer pending, and are not posted.
      entry.ready = true;
      URL.revokeObjectURL(url);
      for (const id of entry.queue) halogen_worker_post(entry, id);
      entry.queue = [];
    } else if (data.failed !== undefined) {
      halogen_worker_fail(pool, index, entry, `the worker did not start: ${data.failed}`);
    } else {
      const waiting = entry.pending.get(data.id);
      if (waiting === undefined) return;
      entry.pending.delete(data.id);
      if (data.ok === true) waiting.resolve(new Uint8Array(data.reply));
      else waiting.reject(new Error(data.error));
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

// Give a request to a worker: posted at once to one that is ready, kept in
// its queue until it is otherwise.
function halogen_worker_give(entry, id, waiting) {
  entry.pending.set(id, waiting);
  if (entry.ready) halogen_worker_post(entry, id);
  else entry.queue.push(id);
}

// A Uint8Array's bytes as an ArrayBuffer of their own, to transfer: the
// array itself when it is the whole of its buffer (the arrays Haskell makes
// are), a copy otherwise, so that nothing else's bytes are given away.
function halogen_worker_buffer(bytes) {
  return bytes.byteOffset === 0 && bytes.byteLength === bytes.buffer.byteLength ? bytes.buffer : bytes.slice().buffer;
}

// Send a request, the Uint8Array of its bytes, to the next worker of the
// pool, starting that worker if it has not been, and give a promise of the
// reply's bytes (a Uint8Array), rejected with what went wrong. The promise
// is marked as handled here, since the program may stop waiting for it (its
// fiber killed), and its rejection should not be reported as unhandled then.
function halogen_worker_run(pool, request) {
  const index = pool.next;
  pool.next = (pool.next + 1) % pool.size;
  return halogen_worker_run_on(pool, index, request);
}

// The same, to the pool's worker of this number rather than the next in
// turn: Halogen.Worker.Par gives each batch to the worker that is free. A
// number outside the pool is taken modulo its size.
function halogen_worker_run_on(pool, given, request) {
  const index = ((given % pool.size) + pool.size) % pool.size;
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
  let waiting;
  const promise = new Promise((resolve, reject) => (waiting = { resolve: resolve, reject: reject, request: request }));
  promise.catch(() => {});
  pool.started.set(promise, { index: index, id: id });
  halogen_worker_give(entry, id, waiting);
  return promise;
}

// The worker a promise's job was given to and the job's entry there, if it
// is still waiting for an answer.
function halogen_worker_find(pool, promise) {
  const place = promise !== null && typeof promise === "object" ? pool.started.get(promise) : undefined;
  if (place === undefined) return undefined;
  const entry = pool.workers[place.index];
  const waiting = entry?.pending.get(place.id);
  return waiting === undefined ? undefined : { index: place.index, id: place.id, entry: entry, waiting: waiting };
}

// Reject the job's promise and forget it; a worker that has been posted it
// is told, and stops it at its next checkpoint. One still in a queue is
// simply not posted.
function halogen_worker_cancel(pool, promise) {
  const found = halogen_worker_find(pool, promise);
  if (found === undefined) return;
  const { id, entry, waiting } = found;
  entry.pending.delete(id);
  waiting.reject(halogen_worker_error("Cancelled", "the job was cancelled"));
  if (entry.ready) entry.worker.postMessage({ cancel: id });
}

// End the worker the job runs in, rejecting the job's promise, and give what
// else the worker had to a new one in its place, in order. A worker with
// nothing else to do is not replaced until the pool gives its place a job.
function halogen_worker_kill(pool, promise) {
  const found = halogen_worker_find(pool, promise);
  if (found === undefined) return;
  const { index, id, entry, waiting } = found;
  entry.pending.delete(id);
  waiting.reject(halogen_worker_error("Killed", "the job was killed, and the worker it ran in"));
  const unanswered = halogen_worker_end(pool, index, entry);
  if (unanswered.length === 0) return;
  let fresh;
  try {
    fresh = pool.workers[index] = halogen_worker_start(pool, index);
  } catch (error) {
    for (const [, { reject }] of unanswered) reject(new Error(`the worker did not start: ${error?.message ?? error}`));
    return;
  }
  for (const [other, rest] of unanswered) halogen_worker_give(fresh, other, rest);
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

// Listen to the page. A cancel is noted here, for the job's next
// checkpoint to find; only requests go to Haskell.
function halogen_worker_listen(callback) {
  const config = globalThis.halogenWorker;
  config.serving = true;
  config.cancelled = new Set();
  self.onmessage = (event) => {
    const data = event.data;
    if (data.cancel !== undefined) config.cancelled.add(data.cancel);
    else callback(data);
  };
  self.postMessage({ ready: true });
}

// A turn of the worker's event loop, for a checkpoint: a macrotask, so that
// the messages that arrived meanwhile are delivered first.
function halogen_worker_turn() {
  return new Promise((resolve) => setTimeout(resolve, 0));
}

function halogen_worker_is_cancelled(id) {
  return globalThis.halogenWorker.cancelled?.has(id) === true;
}

// The request is answered: a cancel of it is of no more use.
function halogen_worker_forget(id) {
  globalThis.halogenWorker.cancelled?.delete(id);
}

// Which of its pool's workers this is; asked only inside one.
function halogen_worker_which_worker() {
  return globalThis.halogenWorker.worker;
}

function halogen_worker_request_id(message) {
  return message.id;
}

function halogen_worker_request_bytes(message) {
  return new Uint8Array(message.request);
}

function halogen_worker_reply(id, bytes) {
  const buffer = halogen_worker_buffer(bytes);
  self.postMessage({ id: id, ok: true, reply: buffer }, [buffer]);
}

function halogen_worker_reply_error(id, message) {
  self.postMessage({ id: id, ok: false, error: message });
}
