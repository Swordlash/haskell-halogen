// Drive the worker example (examples/worker) in headless Chromium: jobs run
// in Web Workers that are copies of the page's own app.wasm, while the page
// goes on drawing.
//
//   npm run test-worker                     # after: sh toolchain/build-wasm.sh worker
//   node toolchain/test-worker.mjs [dir]    # default dist-newstyle/wasm/public/worker
//
// It builds nothing. It serves the built example and checks: the pool says
// workers are available; counting in a worker gives the right count while
// the component's spinner, which it turns a frame at a time, goes on turning
// (the frames the page showed meanwhile are counted from the page itself, a
// sample at each of its own animation frames), in each of the pool's two
// workers as it starts and in one already running; counting on the page
// gives the same count, and the component draws no frame until it is done;
// a job that throws is reported with its message; terminating the pool ends
// its workers, and the next job starts one again; nothing is logged as an
// error, and nothing is loaded from another site. WORKER_DEBUG=1 prints
// what the page and its workers log.
import assert from "node:assert/strict";
import { readFile, stat } from "node:fs/promises";
import { createServer } from "node:http";
import { extname, join, normalize, resolve } from "node:path";
import { chromium } from "playwright";

const root = resolve(process.argv[2] ?? "dist-newstyle/wasm/public/worker");
// The primes below two million, which the example counts.
const expected = 148933;

const types = {
  ".html": "text/html",
  ".js": "text/javascript",
  ".mjs": "text/javascript",
  ".css": "text/css",
  ".json": "application/json",
  ".wasm": "application/wasm",
};

// A static server and nothing else, as any site the example is put on: no
// COOP/COEP headers, so the page is not cross-origin isolated and a worker
// shares no memory with it.
const server = createServer(async (request, response) => {
  const path = decodeURIComponent(new URL(request.url, "http://x").pathname);
  const file = join(root, normalize(path.endsWith("/") ? `${path}index.html` : path));
  try {
    await stat(file);
    const body = await readFile(file);
    response.writeHead(200, { "content-type": types[extname(file)] ?? "application/octet-stream" });
    response.end(body);
  } catch {
    response.writeHead(404).end();
  }
});
await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
const url = `http://127.0.0.1:${server.address().port}/`;

try {
  await stat(join(root, "app.wasm"));
} catch {
  console.error(`FAIL no app.wasm in ${root}: build the example first (sh toolchain/build-wasm.sh worker)`);
  process.exit(1);
}

const browser = await chromium.launch();
const errors = [];
const offSite = [];
let failed = false;
const ms = (n) => `${Math.round(n)} ms`;

try {
  const context = await browser.newContext();
  // A worker's requests are the context's, not always the page's.
  context.on("request", (request) => {
    if (/^https?:/.test(request.url()) && !request.url().startsWith(url)) offSite.push(request.url());
  });
  // The page's workers, as they start and end.
  const workers = new Set();
  const open = async (address, init) => {
    const opened = await context.newPage();
    if (init) await opened.addInitScript(init);
    // The workers' console is the page's too, each line of theirs prefixed
    // with the worker's number.
    opened.on("pageerror", (error) => errors.push(error.message));
    opened.on("console", (message) => {
      if (message.type() === "error") errors.push(message.text());
      if (process.env.WORKER_DEBUG) console.log(`    [${message.type()}] ${message.text()}`);
    });
    opened.on("worker", (worker) => {
      workers.add(worker);
      worker.on("close", () => workers.delete(worker));
    });
    await opened.goto(address);
    return opened;
  };
  const noErrors = (step) => assert.deepEqual(errors, [], `errors in the page ${step}`);

  let page = await open(url);
  await page.locator("#available", { hasText: "Web Workers are available" }).waitFor({ timeout: 30000 });
  const frames = page.locator("#frames");
  const first = Number(await frames.textContent());
  await page.waitForFunction((n) => Number(document.getElementById("frames").textContent) > n + 5, first, { timeout: 10000 });
  assert.equal(workers.size, 0, "no worker before the first job");
  console.log("ok  the page is up, the spinner turning, workers available and none started");

  // Click a button, then sample the frame count the page shows at each of
  // the page's own frames while the component says it is counting: how many
  // different counts the reader saw while the job ran, in how many frames
  // of the page's (none, when the page's thread was busy throughout).
  const runJob = async (button, place) => {
    await page.evaluate(() => {
      window.seen = new Set();
      window.samples = 0;
      const sample = () => {
        if (document.getElementById("status").textContent.startsWith("Counting")) {
          window.seen.add(document.getElementById("frames").textContent);
          window.samples += 1;
        }
        window.sampling = requestAnimationFrame(sample);
      };
      sample();
    });
    const previous = await page.locator("#result").textContent();
    const started = Date.now();
    await page.locator(`#${button}`).click();
    // A new result: the component empties the last one as the job starts.
    await page.waitForFunction(
      ({ place, previous }) => {
        const text = document.getElementById("result").textContent;
        return text !== previous && text.startsWith(`${place}:`);
      },
      { place, previous },
      { timeout: 60000 },
    );
    const elapsed = Date.now() - started;
    const { shown, samples } = await page.evaluate(() => {
      cancelAnimationFrame(window.sampling);
      return { shown: window.seen.size, samples: window.samples };
    });
    const text = await page.locator("#result").textContent();
    return { text, shown, samples, elapsed };
  };
  const counted = (text) => {
    const match = text.match(/: (\d+) primes below \d+ in (\d+) ms, while the page drew (\d+) frames/);
    assert.ok(match, `a count: ${text}`);
    return { count: Number(match[1]), millis: Number(match[2]), drew: Number(match[3]) };
  };

  const inWorker = async (what) => {
    const run = await runJob("worker", "in a worker");
    const { count, millis, drew } = counted(run.text);
    assert.equal(count, expected, "the worker's count");
    assert.ok(workers.size >= 1, "a worker runs the job");
    // At 60 frames a second a job of a second or more shows tens of them;
    // ten is a floor a loaded machine still clears.
    assert.ok(run.shown >= 10, `the spinner turned while the worker counted: ${run.shown} different counts shown in ${ms(run.elapsed)}`);
    assert.ok(drew >= 10, `the component drew while the worker counted: ${drew} frames`);
    console.log(
      `ok  in a worker, ${what}: ${count} primes in ${ms(millis)}, the component drew ${drew} frames, the page showed ${run.shown} different counts in ${run.samples} of its frames`,
    );
    noErrors(`counting in a worker, ${what}`);
    return millis;
  };

  // The pool has two workers, which take turns: the first two jobs start
  // one each, and the third finds the first already running.
  await inWorker("the first, starting it");
  await inWorker("the second, starting it");
  await inWorker("the first again, already running");

  {
    const run = await runJob("here", "here");
    const { count, millis, drew } = counted(run.text);
    assert.equal(count, expected, "the page's own count");
    assert.equal(drew, 0, "the component draws no frame while it counts on the page");
    assert.ok(run.shown <= 1, `the spinner stopped while the page counted: ${run.shown} different counts shown`);
    console.log(
      `ok  on the page: ${count} primes in ${ms(millis)}, the component drew ${drew} frames, the page showed ${run.shown} different counts in ${run.samples} of its frames`,
    );
    noErrors("counting on the page");
  }

  {
    const run = await runJob("fail", "the failing job");
    assert.match(run.text, /^the failing job: failed: .*this job always fails/, "the job's exception, with its message");
    console.log(`ok  a job that throws is reported: ${run.text}`);
    noErrors("in the failing job");
  }

  const before = workers.size;
  await page.locator("#terminate").click();
  await page.locator("#status", { hasText: "terminated" }).waitFor({ timeout: 10000 });
  for (const deadline = Date.now() + 10000; workers.size > 0 && Date.now() < deadline; ) await page.waitForTimeout(50);
  assert.equal(workers.size, 0, `terminate ends the workers (${before} before)`);
  console.log(`ok  terminate ended the pool's ${before} workers`);
  await inWorker("started again after terminate");
  await page.close();

  // The page with ?workers=none, whose loader leaves halogenWorker unset, as
  // wherever no worker can be made: Worker.run counts on the page, on the
  // thread of the component's liftIO, which leaves the component's loop
  // free, and the page's thread is shared between them.
  page = await open(`${url}?workers=none`);
  await page.locator("#available", { hasText: "No Web Worker can be made here" }).waitFor({ timeout: 30000 });
  {
    const run = await runJob("worker", "with Worker.run");
    const { count, millis, drew } = counted(run.text);
    assert.equal(count, expected, "the fallback's count");
    assert.equal(workers.size, 0, "no worker without the page's configuration");
    console.log(
      `ok  with Worker.run and no worker: ${count} primes in ${ms(millis)}, the component drew ${drew} frames, the page showed ${run.shown} different counts in ${run.samples} of its frames`,
    );
    noErrors("in the fallback");
  }

  await page.close();

  // A page whose Worker constructor throws, as a browser refusing a Blob
  // worker does: available still says yes, and the failure to start must
  // reach the program as the job's WorkerError, not as an exception out of
  // the binding.
  page = await open(url, () => {
    globalThis.Worker = class {
      constructor() {
        throw new DOMException("workers are refused here", "SecurityError");
      }
    };
  });
  await page.locator("#available", { hasText: "Web Workers are available" }).waitFor({ timeout: 30000 });
  {
    const run = await runJob("worker", "in a worker");
    assert.match(run.text, /failed: the worker did not start: .*workers are refused here/, "the start's failure, as the job's");
    assert.equal(workers.size, 0, "no worker started");
    console.log(`ok  a worker that cannot be made fails the job: ${run.text}`);
    noErrors("with a worker that cannot start");
  }

  noErrors("at the end");
  console.log("ok  no errors in the page");
  assert.deepEqual(offSite, [], "requests to another site than the test server's");
  console.log("ok  nothing loaded from another site");
} catch (error) {
  failed = true;
  console.error(`FAIL ${error.message}`);
  if (errors.length) console.error("page errors:", errors);
} finally {
  await browser.close();
  server.close();
}

process.exit(failed ? 1 : 0);
