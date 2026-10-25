// Drive the tutorial (examples/learn) in headless Chromium, compiler and all.
//
//   npm run test-learn-app                     # after: sh toolchain/build-wasm.sh learn
//   node toolchain/test-learn-app.mjs [dir]    # default dist-newstyle/wasm/public/learn
//
// It builds nothing. It serves the built app, with the compiler's rootfs
// (toolchain/build-learn-rootfs.sh) at rootfs/ under it, where the page looks
// for it: LEARN_ROOTFS_DIR names the rootfs directory (default
// dist-newstyle/wasm/learn). The site is served under HALOGEN_BASE_PATH
// (default /), the base path the pages were built for.
//
// What it checks: each lesson's page arrives with its text already in it;
// the program adopts that page; the compiler loads (the timings are
// printed); the first two lessons compile and run as their text says, and a
// type error is reported at its line with the last result still showing;
// every lesson in examples/learn/lessons compiles its Solution.hs without a
// diagnostic and shows something (one line per lesson, which is how a new
// lesson is checked); the routes survive back and forward, the reader's code
// a reload, and the compiler a second visit, from the Cache API; on a narrow
// screen nothing large is downloaded until asked; and nothing is logged as an
// error.
import assert from "node:assert/strict";
import { readFile, readdir, stat } from "node:fs/promises";
import { createServer } from "node:http";
import { extname, join, normalize, resolve } from "node:path";
import { chromium } from "playwright";

const root = resolve(process.argv[2] ?? "dist-newstyle/wasm/public/learn");
const rootfs = resolve(process.env.LEARN_ROOTFS_DIR ?? "dist-newstyle/wasm/learn");
const lessonsDir = resolve("examples/learn/lessons");
const inner = (process.env.HALOGEN_BASE_PATH ?? "/").replace(/^\/+|\/+$/g, "");
const base = inner ? `/${inner}/` : "/";

// The lessons, as the app reads them: the directory's name and the header's
// title, and the code.
const lessons = [];
for (const slug of (await readdir(lessonsDir)).sort()) {
  const dir = join(lessonsDir, slug);
  if (!(await stat(dir)).isDirectory()) continue;
  const markdown = await readFile(join(dir, "lesson.md"), "utf8");
  const title = markdown.match(/^title:\s*(.+)$/m)?.[1].trim();
  lessons.push({
    slug,
    number: Number(slug.slice(0, 2)),
    title,
    solution: await readFile(join(dir, "Solution.hs"), "utf8"),
  });
}
assert.ok(lessons.length >= 2, "at least the two first lessons");

const types = {
  ".html": "text/html",
  ".js": "text/javascript",
  ".mjs": "text/javascript",
  ".css": "text/css",
  ".json": "application/json",
  ".wasm": "application/wasm",
};

// A static server, as Pages is: a directory's address serves its
// index.html, rootfs/ is the rootfs directory, and a HEAD gets the length
// and the modification time a GET would.
const requests = [];
const server = createServer(async (request, response) => {
  const path = decodeURIComponent(new URL(request.url, "http://x").pathname);
  requests.push(`${request.method} ${path}`);
  if (!path.startsWith(base)) {
    response.writeHead(404).end();
    return;
  }
  const relative = path.slice(base.length);
  const file = relative.startsWith("rootfs/")
    ? join(rootfs, normalize(relative.slice("rootfs/".length)))
    : join(root, normalize(relative === "" || relative.endsWith("/") ? `${relative}index.html` : relative));
  try {
    const info = await stat(file);
    const headers = {
      "content-type": types[extname(file)] ?? "application/octet-stream",
      "content-length": info.size,
      "last-modified": info.mtime.toUTCString(),
    };
    if (request.method === "HEAD") {
      response.writeHead(200, headers).end();
      return;
    }
    const body = await readFile(file);
    response.writeHead(200, headers);
    response.end(body);
  } catch {
    response.writeHead(404).end();
  }
});
await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
const url = `http://127.0.0.1:${server.address().port}${base}`;

const browser = await chromium.launch();
const errors = [];
let failed = false;
const seconds = (ms) => `${(ms / 1000).toFixed(1)} s`;

try {
  // Every lesson's page holds its text before any script runs.
  for (const lesson of lessons) {
    const response = await fetch(`${url}${lesson.slug}/`);
    assert.equal(response.status, 200, `${lesson.slug}: status`);
    const html = await response.text();
    assert.ok(html.includes(`<h1>${lesson.number}. ${escapeHtml(lesson.title)}</h1>`), `${lesson.slug}: its heading in the page`);
    assert.ok(html.includes(`<base href="${base}">`), `${lesson.slug}: the base path`);
    assert.ok(html.includes('id="learn-result"'), `${lesson.slug}: the result's element`);
  }
  const home = await (await fetch(url)).text();
  for (const lesson of lessons) assert.ok(home.includes(`href="${base}${lesson.slug}/"`), `the introduction links ${lesson.slug}`);
  console.log(`ok  every page is prerendered (${lessons.length} lessons and the introduction)`);

  const context = await browser.newContext({ viewport: { width: 1400, height: 900 } });
  const page = await context.newPage();
  watch(page);
  page.on("dialog", (dialog) => dialog.accept());
  const ready = [];
  page.on("console", (message) => {
    const text = message.text();
    if (text.startsWith("learn: compiler ready")) ready.push(text);
  });

  // The lesson shows before app.wasm, and the program adopts the page.
  let release;
  const held = new Promise((resolve) => (release = resolve));
  await page.route(/app\.wasm$/, async (route) => {
    await held;
    await route.continue();
  });
  const t0 = Date.now();
  await page.goto(`${url}01-hello/`, { waitUntil: "domcontentloaded" });
  await page.locator("article h1", { hasText: lessons[0].title }).waitFor({ timeout: 5000 });
  await page.evaluate(() => {
    document.querySelector("article").dataset.prerendered = "1";
    window.learnNoReload = 1;
  });
  console.log("ok  lesson 1 shows before app.wasm has loaded");
  release();
  await page.locator(".learn-editor .cm-editor").waitFor({ timeout: 30000 });
  // The route stays: taking it away while the page's other requests are
  // under way (the compiler's) can leave them stalled.
  assert.equal(await page.locator('article[data-prerendered="1"]').count(), 1, "the prerendered text is adopted");
  console.log(`ok  hydrated, the editor is up (${seconds(Date.now() - t0)} after navigation)`);

  // The compiler loads by itself on a wide screen; each step it shows is
  // printed with when it showed.
  await untilReady(page, t0);
  const loaded = Date.now() - t0;
  console.log(`ok  compiler ready ${seconds(loaded)} after navigation: ${await page.locator(".learn-status").textContent()}`);
  if (ready.length) console.log(`    ${ready[0]}`);

  const compile = async (what) => {
    const start = Date.now();
    await page.locator("button.learn-compile:not([disabled])").click();
    await page.locator("button.learn-compile:not([disabled])").waitFor({ timeout: 120000 });
    const outcome = await page.locator(".learn-outcome").textContent();
    console.log(`    compile ${what}: ${seconds(Date.now() - start)} (${outcome})`);
    return outcome;
  };

  // Lesson 1: the start renders Hello; the solution adds a paragraph.
  assert.match(await compile("lesson 1, Start.hs"), /^Compiled and ran/);
  await page.locator("#learn-result p", { hasText: /^Hello$/ }).waitFor({ timeout: 5000 });
  console.log("ok  lesson 1's start shows Hello");
  await page.locator("button.learn-solution").click();
  assert.match(await compile("lesson 1, Solution.hs"), /^Compiled and ran/);
  await page.locator("#learn-result p", { hasText: "My name is Ada." }).waitFor({ timeout: 5000 });
  assert.equal(await page.locator("#learn-result p", { hasText: /^Hello$/ }).count(), 1, "the last run's tree is replaced, not added to");
  console.log("ok  lesson 1's solution shows its paragraph");

  // Lesson 2, by its Next link: the address changes, the page does not
  // reload, and the counter counts.
  await page.locator("header a.learn-next").click();
  await page.waitForURL(`${url}02-counter/`);
  await page.locator("article h1", { hasText: lessons[1].title }).waitFor({ timeout: 5000 });
  assert.equal(await page.evaluate(() => window.learnNoReload), 1, "Next does not reload the page");
  assert.equal(await page.locator("#learn-result").innerHTML(), "", "a new lesson starts with an empty result");
  await page.locator(".learn-editor .cm-editor").waitFor({ timeout: 5000 });
  await page.locator("button.learn-solution").click();
  assert.match(await compile("lesson 2, Solution.hs"), /^Compiled and ran/);
  const plus = page.locator("#learn-result button", { hasText: /^\+$/ });
  await plus.click();
  await plus.click();
  await page.locator("#learn-result p", { hasText: "Count: 2" }).waitFor({ timeout: 5000 });
  console.log("ok  lesson 2 via Next: the counter counts to 2");

  // A type error: reported at its line, marked in the editor, and the last
  // result stays on the page.
  // A number where HH.text wants text: there is no Num Text.
  const broken = lessons[1].solution.replace('HH.text ("Count: " <> T.pack (show count))', "HH.text 42");
  assert.notEqual(broken, lessons[1].solution, "lesson 2's solution renders the count with T.pack");
  const line = broken.split("\n").findIndex((l) => l.includes("HH.text 42")) + 1;
  await setEditor(page, broken);
  assert.match(await compile("a type error"), /did not compile/);
  const diagnostic = page.locator(".learn-diagnostics li.learn-error").first();
  await diagnostic.waitFor({ timeout: 5000 });
  const where = await diagnostic.locator(".learn-location").textContent();
  assert.ok(where.startsWith(`Lesson.hs:${line}:`), `the diagnostic is at line ${line}: ${where}`);
  await page.locator(".cm-lintRange-error").first().waitFor({ timeout: 5000 });
  await page.locator("#learn-result p", { hasText: "Count: 2" }).waitFor({ timeout: 1000 });
  await diagnostic.locator("button.learn-location").click();
  assert.match(await page.locator(".cm-activeLine").textContent(), /HH\.text 42/, "clicking the diagnostic moves the cursor to its line");
  console.log(`ok  a type error is reported at ${where.trim()}, and the counter is still there`);

  // Every lesson: its start compiles (warnings allowed, since an exercise
  // may leave something unused), and its solution compiles with no
  // diagnostic at all and shows something.
  const runLesson = async (button) => {
    await page.locator(button).click();
    const start = Date.now();
    await page.locator("button.learn-compile").click();
    await page.locator("button.learn-compile:not([disabled])").waitFor({ timeout: 120000 });
    return {
      ms: Date.now() - start,
      outcome: await page.locator(".learn-outcome").textContent(),
      errors: await page.locator(".learn-diagnostics li.learn-error").allTextContents(),
      diagnostics: await page.locator(".learn-diagnostics li").allTextContents(),
      shown: (await page.locator("#learn-result").innerHTML()).length > 0,
    };
  };
  for (const lesson of lessons) {
    await page.locator("header a.learn-brand").click();
    await page.locator(".learn-home ol a", { hasText: lesson.title }).click();
    await page.waitForURL(`${url}${lesson.slug}/`);
    await page.locator("article h1", { hasText: lesson.title }).waitFor({ timeout: 5000 });
    await page.locator(".learn-editor .cm-editor").waitFor({ timeout: 5000 });
    // The lesson's layers may be loading still.
    await page.locator("button.learn-compile:not([disabled])").waitFor({ timeout: 120000 });
    const start = await runLesson("button.learn-reset");
    const solution = await runLesson("button.learn-solution");
    const startOk = /^Compiled and ran/.test(start.outcome) && start.errors.length === 0;
    const solutionOk = /^Compiled and ran/.test(solution.outcome) && solution.diagnostics.length === 0 && solution.shown;
    const problems = [
      startOk ? "" : `; Start.hs: ${start.outcome} ${start.errors.join(" | ")}`,
      solutionOk ? "" : `; Solution.hs: ${solution.outcome} ${solution.diagnostics.join(" | ")}${solution.shown ? "" : " (nothing shown)"}`,
    ].join("");
    console.log(`${startOk && solutionOk ? "ok " : "FAIL"} ${lesson.slug}: Start.hs ${seconds(start.ms)}, Solution.hs ${seconds(solution.ms)}${problems}`);
    if (!startOk || !solutionOk) failed = true;
  }
  assert.ok(!failed, "every lesson's solution compiles and shows something");
  assert.equal(await page.evaluate(() => window.learnNoReload), 1, "following the lessons' links never reloads");

  // Back and forward.
  const last = lessons[lessons.length - 1];
  await page.goBack();
  await page.waitForURL(url);
  await page.locator(".learn-home h1").waitFor({ timeout: 5000 });
  await page.goForward();
  await page.waitForURL(`${url}${last.slug}/`);
  await page.locator("article h1", { hasText: last.title }).waitFor({ timeout: 5000 });
  console.log("ok  back and forward");

  // The reader's code survives a reload, and the compiler comes from the
  // cache the second time.
  // A second visit, in a page of its own, as after closing the tab.
  await page.close();
  const again = await context.newPage();
  watch(again);
  const revisit = Date.now();
  const fetched = requests.length;
  await again.goto(`${url}02-counter/`);
  await again.locator(".learn-editor .cm-editor").waitFor({ timeout: 30000 });
  assert.ok((await again.locator(".learn-editor .cm-content").textContent()).includes("modify (subtract 1)"), "the reader's code on a second visit");
  await untilReady(again, revisit);
  assert.ok(!requests.slice(fetched).some((r) => r.startsWith("GET ") && r.endsWith("rootfs.tar.zst")), "the archive comes from the cache");
  console.log(`ok  the reader's code is kept for the next visit; the compiler was ready again ${seconds(Date.now() - revisit)} after it, from the cache`);
  await again.locator("button.learn-reset").click();
  await again.waitForFunction(() => !document.querySelector(".learn-editor .cm-content")?.textContent.includes("subtract"));
  assert.equal(await again.evaluate(() => localStorage.getItem("learn:02-counter")), null, "Reset forgets the reader's code");
  console.log("ok  Reset brings back Start.hs and forgets the saved code");
  await context.close();

  // A narrow screen downloads nothing large until asked.
  const phone = await browser.newContext({ viewport: { width: 400, height: 800 } });
  const small = await phone.newPage();
  watch(small);
  const before = requests.length;
  await small.goto(`${url}01-hello/`);
  await small.locator("button.learn-load").waitFor({ timeout: 30000 });
  await small.waitForTimeout(1000);
  // A HEAD, for the archive's size, is all.
  assert.ok(!requests.slice(before).some((r) => r.startsWith("GET ") && r.endsWith(".tar.zst")), "no archive downloaded on a narrow screen");
  assert.equal(await small.locator('.cm-content[contenteditable="false"]').count(), 1, "the code is read-only there");
  console.log("ok  on a narrow screen the compiler waits to be asked, and the code is read-only");
  const asked = Date.now();
  await small.locator("button.learn-load").click();
  await untilReady(small, asked);
  await small.locator('.cm-content[contenteditable="true"]').waitFor({ timeout: 5000 });
  console.log("ok  loading it anyway makes the code editable");
  await phone.close();

  // A compiler that does not load says why, and Retry tries again.
  const flaky = await browser.newContext({ viewport: { width: 1400, height: 900 } });
  const retried = await flaky.newPage();
  watch(retried, /rootfs\.json|503/);
  let refused = 0;
  await retried.route(/rootfs\/rootfs\.json$/, (route) => (refused++ === 0 ? route.fulfill({ status: 503, body: "" }) : route.continue()));
  await retried.goto(`${url}01-hello/`);
  await retried.locator("button.learn-retry").waitFor({ timeout: 30000 });
  assert.match(await retried.locator(".learn-compiler .learn-error").textContent(), /rootfs\.json: 503/, "the reason is shown");
  const retry = Date.now();
  await retried.locator("button.learn-retry").click();
  await untilReady(retried, retry);
  console.log("ok  a compiler that fails to load says why, and Retry loads it");
  await flaky.close();

  assert.deepEqual(errors, [], "errors in the page");
  console.log("ok  no errors in the page");
} catch (error) {
  failed = true;
  console.error(`FAIL ${error.message}`);
  if (errors.length) console.error("page errors:", errors);
} finally {
  await browser.close();
  server.close();
}

process.exit(failed ? 1 : 0);

// Collect the page's errors, but those matching `expected`.
function watch(page, expected) {
  // LEARN_DEBUG=1 prints everything the page logs.
  if (process.env.LEARN_DEBUG) {
    page.on("console", (message) => console.log(`    [${message.type()}] ${message.text().slice(0, 300)}`));
    page.on("request", (request) => console.log(`    > ${request.method()} ${request.url()}`));
    page.on("requestfinished", (request) => console.log(`    < ${request.url()}`));
    page.on("requestfailed", (request) => console.log(`    ! ${request.url()} ${request.failure()?.errorText}`));
  }
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("console", (message) => {
    if (message.type() === "error" && !(expected && expected.test(message.text()))) errors.push(message.text());
  });
}

// Wait for the compiler, printing each phase the page shows as it changes.
async function untilReady(page, since) {
  let last = "";
  for (const deadline = Date.now() + 300000; ; await page.waitForTimeout(100)) {
    const status = await page.evaluate(() =>
      [document.querySelector(".learn-status")?.textContent, document.querySelector(".learn-compiler p")?.textContent]
        .filter(Boolean)
        .join(": "),
    );
    const phase = status.replace(/[\d.]+ of /, "… of ").replace(/: Downloading GHC: [\d.]+/, ": Downloading GHC");
    if (phase !== last) {
      console.log(`    ${seconds(Date.now() - since)}  ${status}`);
      last = phase;
    }
    if (status.includes("in your browser")) return;
    assert.ok(Date.now() < deadline, `the compiler loads (last: ${status})`);
  }
}

function escapeHtml(text) {
  return text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

// Replace the editor's text as the reader would paste it.
async function setEditor(page, text) {
  await page.locator(".learn-editor .cm-content").click();
  await page.keyboard.press("ControlOrMeta+a");
  await page.keyboard.insertText(text);
  await page.waitForFunction(
    (expected) => document.querySelector(".learn-editor .cm-content").innerText.replace(/​/g, "").trim().length >= expected.trim().length - 5,
    text,
  );
}
