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
// the range counted in four parts with Worker.start and mapConcurrently
// comes back in order, from both of the pool's workers, while the page
// draws, and on the page where no worker can be made; the range counted in
// sixteen tasks with Halogen.Worker.Par's runPar comes back in order, each
// part's count right, from both workers; a slow count that
// checkpoints stops soon after Cancel (its log of chunks, which the
// workers' console relays to the page's, stops), a count that does not is
// stopped at once by Kill, which ends its worker, and the pool's places
// still run jobs afterwards; a race between a quick count and the slow one
// cancels the slow one; a job that throws is reported with its message;
// terminating the pool ends its workers, and the next job starts one again;
// a page whose loader names a worker script of its own (?workers=script)
// counts in a worker and in sixteen tasks over both through it;
// nothing is logged as an error, and nothing is loaded from another site. WORKER_DEBUG=1 prints
// what the page and its workers log.
import assert from "node:assert/strict";
import { readFile, stat } from "node:fs/promises";
import { createServer } from "node:http";
import { extname, join, normalize, resolve } from "node:path";
import { chromium } from "playwright";

const root = resolve(process.argv[2] ?? "dist-newstyle/wasm/public/worker");
// The primes below two million, which the example counts.
const expected = 148933;
// And in each quarter of that range, which the example counts in parts.
const quarters = [
  { from: 0, to: 500000, count: 41538 },
  { from: 500000, to: 1000000, count: 36960 },
  { from: 1000000, to: 1500000, count: 35657 },
  { from: 1500000, to: 2000000, count: 34778 },
];

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
// Everything the page and its workers log, for the slow count's chunks.
const logs = [];
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
      logs.push(message.text());
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

  // The range in four parts, each a Worker.start, waited for together with
  // mapConcurrently: the parts come back in the range's order, whichever
  // finished first, and the pool's two workers took turns with them.
  const inParts = async (place, where) => {
    const run = await runJob("spread", place);
    const { count, millis, drew } = counted(run.text);
    assert.equal(count, expected, "the parts' total");
    const parts = await page.locator("#parts li").evaluateAll((items) =>
      items.map((item) => ({ text: item.textContent, worker: item.dataset.worker })),
    );
    assert.equal(parts.length, quarters.length, `a line for each part: ${JSON.stringify(parts)}`);
    parts.forEach((part, index) => {
      const quarter = quarters[index];
      assert.ok(
        part.text.startsWith(`${quarter.count} primes from ${quarter.from} up to ${quarter.to}, counted `),
        `part ${index} in its place, with its count: ${part.text}`,
      );
    });
    const workersUsed = [...new Set(parts.map((part) => part.worker))].sort();
    assert.deepEqual(workersUsed, where, `the parts were counted ${where.join(" and ")}: ${JSON.stringify(parts)}`);
    return { count, millis, drew, run, parts };
  };
  {
    const { count, millis, drew, run, parts } = await inParts("in both workers", ["0", "1"]);
    assert.ok(drew >= 10, `the component drew while the workers counted: ${drew} frames`);
    console.log(
      `ok  in both workers, a part each in turn: ${count} primes in ${ms(millis)}, the parts in order from workers ${parts.map((part) => part.worker).join(", ")}, the component drew ${drew} frames, the page showed ${run.shown} different counts`,
    );
    noErrors("counting in parts");
  }

  // The range in sixteen tasks, which runPar sends in batches to whichever
  // worker is free: each part's count, against a sieve's, in the range's
  // order, and both workers used.
  const inTasks = async (what) => {
    const sixteenth = 2000000 / 16;
    const composite = new Uint8Array(2000000);
    for (let i = 2; i * i < 2000000; i++) if (!composite[i]) for (let j = i * i; j < 2000000; j += i) composite[j] = 1;
    const primesIn = (from, to) => {
      let n = 0;
      for (let k = Math.max(2, from); k < to; k++) if (!composite[k]) n++;
      return n;
    };
    const run = await runJob("tasks", "in tasks");
    const { count, millis, drew } = counted(run.text);
    assert.equal(count, expected, "the tasks' total");
    const parts = await page.locator("#parts li").evaluateAll((items) =>
      items.map((item) => ({ text: item.textContent, worker: item.dataset.worker })),
    );
    assert.equal(parts.length, 16, `a line for each task: ${JSON.stringify(parts)}`);
    parts.forEach((part, index) => {
      const from = index * sixteenth;
      const to = from + sixteenth;
      assert.ok(
        part.text.startsWith(`${primesIn(from, to)} primes from ${from} up to ${to}, counted `),
        `task ${index} in its place, with its count: ${part.text}`,
      );
    });
    const used = [...new Set(parts.map((part) => part.worker))].sort();
    assert.deepEqual(used, ["0", "1"], `the tasks were counted in both workers: ${JSON.stringify(parts)}`);
    console.log(
      `ok  runPar${what}: ${count} primes in 16 tasks in ${ms(millis)}, in order, from workers ${parts.map((part) => part.worker).join("")}, the component drew ${drew} frames`,
    );
    noErrors(`counting in tasks${what}`);
  };
  await inTasks("");

  // The slow count logs each chunk it counts, and checkpoints after each.
  const progress = () => logs.filter((line) => line.includes("the slow count has counted up to")).length;
  const until = async (done, timeout, what) => {
    for (const deadline = Date.now() + timeout; !done(); ) {
      if (Date.now() > deadline) throw new Error(`timed out waiting until ${what}`);
      await page.waitForTimeout(50);
    }
  };

  {
    const started = progress();
    await page.locator("#slow").click();
    await until(() => progress() >= started + 3, 30000, "the slow count has logged three chunks");
    const clicked = Date.now();
    await page.locator("#cancel").click();
    await page.locator("#result", { hasText: /^the slow count: failed: .*the job was cancelled/ }).waitFor({ timeout: 5000 });
    const rejectedIn = Date.now() - clicked;
    // The chunk under way when the cancel came is finished, then the
    // checkpoint after it throws: half a second is several chunks' time.
    await page.waitForTimeout(500);
    const atCancel = progress();
    await page.waitForTimeout(2000);
    assert.equal(progress(), atCancel, "the slow count logs no chunk once it has stopped");
    console.log(
      `ok  cancel: the promise was rejected ${ms(rejectedIn)} after the click, and the slow count stopped at its next checkpoint, after ${atCancel - started} chunks`,
    );
    noErrors("cancelling the slow count");
  }

  {
    const before = workers.size;
    await page.locator("#worker").click();
    await page.locator("#status", { hasText: "Counting in a worker" }).waitFor();
    await page.waitForTimeout(300);
    const clicked = Date.now();
    await page.locator("#kill").click();
    await page.locator("#result", { hasText: /^in a worker: failed: .*killed/ }).waitFor({ timeout: 5000 });
    const rejectedIn = Date.now() - clicked;
    await until(() => workers.size < before, 5000, "the killed job's worker has ended");
    console.log(`ok  kill: the promise was rejected ${ms(rejectedIn)} after the click, and the worker that counted ended (${before} workers before, ${workers.size} after)`);
    noErrors("killing a count");
    const { count, parts } = await inParts("in both workers", ["0", "1"]);
    console.log(`ok  after kill, both of the pool's places run jobs: ${count} primes, the parts from workers ${parts.map((part) => part.worker).join(", ")}`);
  }

  {
    const started = progress();
    const run = await runJob("race", "the race");
    const { count, millis } = counted(run.text);
    assert.equal(count, 78498, "the quick count wins the race");
    const atEnd = progress();
    assert.ok(atEnd > started, "the slow count ran during the race");
    await page.waitForTimeout(2000);
    // At most the chunk under way when the race ended.
    assert.ok(progress() <= atEnd + 1, `the slow count stopped when it lost: ${progress() - atEnd} chunks after the race`);
    console.log(`ok  race: the quick count won in ${ms(millis)}, and the slow one, after ${atEnd - started} chunks, was cancelled and stopped`);
    noErrors("in the race");
  }

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
  // wherever no worker can be made: Worker.start counts on the page, on a
  // thread it forks, which leaves the component's loop free, and the page's
  // thread is shared between them.
  page = await open(`${url}?workers=none`);
  await page.locator("#available", { hasText: "No Web Worker can be made here" }).waitFor({ timeout: 30000 });
  {
    const run = await runJob("worker", "with Worker.start");
    const { count, millis, drew } = counted(run.text);
    assert.equal(count, expected, "the fallback's count");
    assert.equal(workers.size, 0, "no worker without the page's configuration");
    console.log(
      `ok  with Worker.start and no worker: ${count} primes in ${ms(millis)}, the component drew ${drew} frames, the page showed ${run.shown} different counts in ${run.samples} of its frames`,
    );
    noErrors("in the fallback");
  }
  {
    const { count, millis, parts } = await inParts("in parts with Worker.start", ["page"]);
    assert.equal(workers.size, 0, "no worker for the parts without the page's configuration");
    console.log(`ok  in parts with no worker: ${count} primes in ${ms(millis)}, all ${parts.length} parts counted on the page, in order`);
    noErrors("counting in parts in the fallback");
  }

  await page.close();

  // The page with ?workers=script, whose loader names a worker script of its
  // own (web/worker-script.js) rather than the program's files, as a program
  // that is not a binary does: each worker imports it, and it loads app.wasm
  // and runs main itself. The count in a worker while the page draws, and
  // the sixteen tasks over both workers.
  page = await open(`${url}?workers=script`);
  await page.locator("#available", { hasText: "Web Workers are available" }).waitFor({ timeout: 30000 });
  assert.equal(workers.size, 0, "no worker before the first job, with a script of the page's own");
  await inWorker("from the page's own script, starting it");
  await inTasks(", from the page's own script");
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
