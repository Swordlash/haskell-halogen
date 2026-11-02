// What the examples marked inWorker ask of wherever they run: which shard
// the runner gave this copy of the suite, how many workers it started, and
// whether there is a document, which a Web Worker has not. And a timer as a
// promise, to see that a worker's program can wait for one.
function halogen_worker_test_shard() {
  return globalThis.__halogenTestShard ?? "";
}

function halogen_worker_test_workers() {
  return globalThis.__halogenTestWorkers ?? 0;
}

function halogen_worker_test_has_document() {
  return typeof document !== "undefined";
}

function halogen_worker_test_later(ms, text) {
  return new Promise((resolve) => setTimeout(() => resolve(text), ms));
}
