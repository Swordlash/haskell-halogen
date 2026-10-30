// Exercise the actual runner and Chromium with a small JavaScript suite.
// A rendezvous proves that both parallel pages run at once; input and
// localStorage checks prove that their browser contexts are independent.
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { mkdtempSync, mkdirSync, writeFileSync, rmSync } from "node:fs";
import { createServer } from "node:http";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";

const runner = fileURLToPath(new URL("./runner.mjs", import.meta.url));

async function exercise(mode, jobs = "2") {
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
    if (arrived === 2) waiting.splice(0).forEach((r) => r.end("ready"));
  });
  await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
  const url = `http://127.0.0.1:${server.address().port}/`;
  writeFileSync(`${suite}.jsexe/all.js`, `
(async () => {
  const shard = globalThis.__halogenTestShard;
  const mode = ${JSON.stringify(mode)};
  if (mode !== "unsupported") globalThis.__halogenTestShardSupported = true;
  const check = (condition, text) => { if (!condition) throw new Error(text); };
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
})().catch((error) => globalThis.__halogenTestFinished(String(error)));
`);
  try {
    const result = await new Promise((resolve, reject) => {
      const child = spawn(process.execPath, [runner, suite, "-"], {
        env: { ...process.env, HSPEC_HALOGEN_JOBS: jobs, HSPEC_HALOGEN_TIMEOUT: "3" },
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
