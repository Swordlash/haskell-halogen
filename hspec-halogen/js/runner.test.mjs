// Exercise the actual runner and Chromium with a small JavaScript suite.
// A rendezvous proves that the parallel pages and the workers run at once;
// input and localStorage checks prove that the pages' browser contexts are
// independent, and a worker checks it has no document.
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { mkdtempSync, mkdirSync, writeFileSync, rmSync } from "node:fs";
import { createServer } from "node:http";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";

const runner = fileURLToPath(new URL("./runner.mjs", import.meta.url));

// Run the fixture suite with this many pages and workers. Every parallel
// page and every worker fetches from the server, which answers none of them
// until all have asked: `meet` of them, by default all.
async function exercise(mode, jobs = "2", workers = "0", meet = Number(jobs) + Number(workers)) {
  const dir = mkdtempSync(join(tmpdir(), "halogen-runner-test-"));
  const suite = join(dir, "suite");
  mkdirSync(`${suite}.jsexe`);
  writeFileSync(suite, "// __halogenTestArgs\n");
  const waiting = [];
  let arrived = 0;
  const server = createServer((request, response) => {
    response.setHeader("Access-Control-Allow-Origin", "*");
    waiting.push(response);
    arrived++;
    if (arrived === meet) waiting.splice(0).forEach((r) => r.end("ready"));
  });
  await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
  const url = `http://127.0.0.1:${server.address().port}/`;
  writeFileSync(`${suite}.jsexe/all.js`, `
(async () => {
  const shard = globalThis.__halogenTestShard;
  const mode = ${JSON.stringify(mode)};
  if (mode !== "unsupported") globalThis.__halogenTestShardSupported = true;
  const check = (condition, text) => { if (!condition) throw new Error(text); };
  check(globalThis.__halogenTestWorkers === ${Number(workers)}, "the shard was not told how many workers there are");
  if (shard.startsWith("worker/")) {
    check(typeof document === "undefined", "a worker had a document");
    check(typeof globalThis.__halogenTest.act === "undefined", "a worker was given the page's actions");
    // A URL of its own: Chromium holds a second request for a URL until the
    // first has its answer, and two workers share a context.
    await fetch(${JSON.stringify(url)} + "?" + shard);
    if (mode === "worker-error" && shard === "worker/0/2") setTimeout(() => { throw new Error("fixture worker error"); });
    if (mode === "worker-error") await new Promise((resolve) => setTimeout(resolve, 100));
    if (mode === "worker-missing" && shard === "worker/0/2") {
      postMessage({ kind: "finished", crash: null, supported: true });
      return;
    }
    if (mode === "worker-timeout" && shard === "worker/0/2") return;
    console.log("finished " + shard + "\\n");
    await globalThis.__halogenTest.done(mode === "worker-failure" && shard === "worker/0/2" ? 1 : 0);
    return;
  }
  if (shard === "serial") {
    localStorage.setItem("serial", "finished");
  } else if (shard) {
    check(localStorage.getItem("serial") === null, "storage leaked from serial page");
    check(localStorage.getItem("parallel") === null, "storage leaked between parallel pages");
    localStorage.setItem("parallel", shard);
    const input = document.body.appendChild(document.createElement("input"));
    input.id = "field";
    let trusted = false;
    input.addEventListener("input", (event) => { trusted = event.isTrusted; });
    await fetch(${JSON.stringify(url)});
    await globalThis.__halogenTest.act("type", "#field", shard);
    await globalThis.__halogenTest.press("End");
    check(input.value === shard, "input crossed pages");
    check(trusted, "input was not trusted");
  }
  if (mode === "crash" && shard === "0/2") throw new Error("fixture crashed");
  if (mode === "missing" && shard === "0/2") {
    await globalThis.__halogenTestFinished(null);
    return;
  }
  if (mode === "timeout" && shard === "0/2") return;
  console.log("finished " + shard + "\\n");
  await globalThis.__halogenTest.done(mode === "failure" && shard === "0/2" ? 1 : 0);
})().catch((error) => globalThis.__halogenTestFinished
  ? globalThis.__halogenTestFinished(String(error))
  : postMessage({ kind: "finished", crash: String(error), supported: true }));
`);
  try {
    const result = await new Promise((resolve, reject) => {
      const child = spawn(process.execPath, [runner, suite, "-"], {
        env: { ...process.env, HSPEC_HALOGEN_JOBS: jobs, HSPEC_HALOGEN_WORKERS: workers, HSPEC_HALOGEN_TIMEOUT: "3" },
        stdio: ["ignore", "pipe", "pipe"],
      });
      let stdout = "", stderr = "";
      child.stdout.on("data", (text) => { stdout += text; });
      child.stderr.on("data", (text) => { stderr += text; });
      const deadline = setTimeout(() => { child.kill("SIGKILL"); reject(new Error("runner did not exit")); }, 20000);
      child.on("error", reject);
      child.on("exit", (code) => { clearTimeout(deadline); resolve({ code, stdout, stderr, arrived }); });
    });
    return result;
  } finally {
    waiting.splice(0).forEach((r) => r.end());
    server.closeAllConnections();
    server.close();
    rmSync(dir, { recursive: true, force: true });
  }
}

test("parallel pages overlap and isolate storage and trusted input", async () => {
  const result = await exercise("pass");
  assert.equal(result.code, 0, result.stderr);
  assert.equal(result.arrived, 2);
  assert.match(result.stdout, /Browser shard serial[\s\S]*Browser shard 0\/2[\s\S]*Browser shard 1\/2/);
});
for (const mode of ["failure", "crash", "missing", "timeout"]) {
  test(`${mode} in one parallel page fails the whole run`, async () => {
    const result = await exercise(mode);
    assert.equal(result.code, 1, result.stdout + result.stderr);
    if (mode === "crash") assert.match(result.stderr, /fixture crashed/);
    if (mode === "missing") assert.match(result.stderr, /without reporting a result/);
    if (mode === "timeout") assert.match(result.stderr, /no result after/);
  });
}
test("one context retains the original suite protocol", async () => {
  const result = await exercise("pass", "1");
  assert.equal(result.code, 0, result.stderr);
  assert.equal(result.arrived, 0);
  assert.doesNotMatch(result.stdout, /Browser shard/);
});

test("an older suite cannot silently run every example in every context", async () => {
  const result = await exercise("unsupported");
  assert.equal(result.code, 1, result.stdout + result.stderr);
  assert.equal(result.arrived, 0);
  assert.match(result.stderr, /does not support browser shards/);
});
test("invalid context counts fail before launching a browser", async () => {
  const result = await exercise("pass", "0");
  assert.equal(result.code, 1);
  assert.match(result.stderr, /must be a positive integer/);
});

test("workers run beside the parallel pages, each reported on its own", async () => {
  const result = await exercise("pass", "2", "2");
  assert.equal(result.code, 0, result.stdout + result.stderr);
  assert.equal(result.arrived, 4);
  assert.match(
    result.stdout,
    /Browser shard serial\n[\s\S]*Browser shard 0\/2\nfinished 0\/2\n[\s\S]*Browser shard 1\/2\nfinished 1\/2\n[\s\S]*Browser shard worker\/0\/2\nfinished worker\/0\/2\n[\s\S]*Browser shard worker\/1\/2\nfinished worker\/1\/2\n/,
  );
});
test("workers with one context divide the suite into a serial and a parallel page", async () => {
  const result = await exercise("pass", "1", "2");
  assert.equal(result.code, 0, result.stdout + result.stderr);
  assert.equal(result.arrived, 3);
  const shards = [...result.stdout.matchAll(/^Browser shard (\S+)$/gm)].map((match) => match[1]);
  assert.deepEqual(shards, ["serial", "0/1", "worker/0/2", "worker/1/2"]);
});
for (const mode of ["worker-failure", "worker-error", "worker-missing", "worker-timeout"]) {
  test(`${mode.slice("worker-".length)} in one worker fails the whole run`, async () => {
    const result = await exercise(mode, "2", "2");
    assert.equal(result.code, 1, result.stdout + result.stderr);
    if (mode === "worker-error") assert.match(result.stderr, /Errors in the worker:[\s\S]*fixture worker error/);
    if (mode === "worker-missing") assert.match(result.stderr, /without reporting a result/);
    if (mode === "worker-timeout") assert.match(result.stderr, /no result after/);
  });
}
test("invalid worker counts fail before launching a browser", async () => {
  const result = await exercise("pass", "1", "-1", 0);
  assert.equal(result.code, 1);
  assert.match(result.stderr, /HSPEC_HALOGEN_WORKERS must be a non-negative integer/);
});
