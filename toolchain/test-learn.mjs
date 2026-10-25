// Drive the tutorial (examples/learn) in headless Chromium, compiler and all.
//
//   npm run test-learn                         # after: sh toolchain/build-wasm.sh learn
//   node toolchain/test-learn.mjs [dir]        # default dist-newstyle/wasm/public/learn
//
// It builds nothing. It serves the built tree as Pages serves what it
// deploys: the app, its pages, and the compiler's rootfs in rootfs/ inside
// it, where examples/learn/bundle.sh copied it, under HALOGEN_BASE_PATH
// (default /), the base path the pages were built for. CI runs it on the tree
// it deploys, built for the site's base path. LEARN_ROOTFS_DIR, when set,
// serves that rootfs directory at rootfs/ in place of the tree's own (for a
// tree built before the rootfs was, or another rootfs).
//
// What it checks: each lesson's page arrives with its text already in it;
// the program adopts that page; the compiler loads (the timings are
// printed), with the manifest's version and sizes and so without a HEAD
// request; the first two lessons compile and run as their text says, and a
// type error is reported at its line with the last result still showing;
// every lesson in examples/learn/lessons compiles its Solution.hs without a
// diagnostic and shows something (one line per lesson, which is how a new
// lesson is checked); an error GHC throws rather than reports is still a
// failed compile; lesson 13's spec runs in the page, its Start.hs with one
// failure and its Solution.hs with none, by hspec's report under the editor,
// and leaves nothing mounted; the routes survive back and forward; lesson 6's hash router
// keeps the lesson's address under the page's <base href>, Back included;
// what lesson 5's Finalize prints shows on every compile after the first;
// Escape then Tab leaves the editor; the reader's code survives a reload, and
// the compiler a second visit, from the Cache API; on a narrow screen nothing
// large is downloaded until asked; a compiler that fails to load can be
// retried, and loads from a manifest without a version or sizes too (asking
// the server for them); a layer that fails to load is its lessons' failure
// alone, and can be retried; nothing is logged as an error; and nothing is
// loaded from another site but what a layer loads itself.
import assert from "node:assert/strict";
import { lstat, readFile, readdir, stat } from "node:fs/promises";
import { createServer } from "node:http";
import { extname, join, normalize, resolve } from "node:path";
import { chromium } from "playwright";

const root = resolve(process.argv[2] ?? "dist-newstyle/wasm/public/learn");
const rootfs = resolve(process.env.LEARN_ROOTFS_DIR ?? join(root, "rootfs"));
const lessonsDir = resolve("examples/learn/lessons");
const inner = (process.env.HALOGEN_BASE_PATH ?? "/").replace(/^\/+|\/+$/g, "");
const base = inner ? `/${inner}/` : "/";

// The manifest the page will read, for what it says the page needs.
const manifest = await readFile(join(rootfs, "rootfs.json"), "utf8")
  .then(JSON.parse)
  .catch((error) => {
    console.error(`FAIL no rootfs at ${rootfs} (${error.message}): build it (npm run build-rootfs), then the app (sh toolchain/build-wasm.sh learn), or set LEARN_ROOTFS_DIR`);
    process.exit(1);
  });
assert.ok(manifest.version, "rootfs.json has a version");
if (!process.env.LEARN_ROOTFS_DIR) {
  // What Pages is given is the files themselves: a link would be followed
  // by the upload or not, and either way is not what this tests.
  assert.ok((await lstat(rootfs)).isDirectory(), `${rootfs} is a directory, not a link`);
}
console.log(`    rootfs ${rootfs === join(root, "rootfs") ? "in the tree" : `from ${rootfs}`}, version ${manifest.version}`);
// A request for an archive or a part of one.
const archive = /\.tar\.zst(\.\d\d)?$/;

// What a page may load from another site: what a layer says it loads itself
// (its `external` entries in rootfs.json, which learn/README.md lists), and
// nothing else, so that neither the app nor the compiler depends on a CDN.
// A layer's external is allowed only on the lessons that use that layer
// (PixiJS, when lesson 11's canvas mounts), but for one named by a layer's
// stylesheet (the Material Icons font, named by material.css): the
// stylesheet stays on the page once the layer has loaded, whichever lesson
// loaded it, and the browser fetches the font when it first needs it.
const external = new Map();
for (const layer of manifest.layers) {
  const anywhere = (layer.assets ?? []).some((asset) => asset.kind === "stylesheet");
  for (const { url } of layer.external ?? []) external.set(url, { layer: layer.name, anywhere });
}
const offSite = [];
const externalSeen = new Set();

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
    layers: (markdown.match(/^layers:\s*(.+)$/m)?.[1] ?? "").split(/[\s,]+/).filter(Boolean),
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
// index.html, rootfs/ is the rootfs directory (the tree's own, unless
// LEARN_ROOTFS_DIR says otherwise), and a HEAD gets the length and the
// modification time a GET would.
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
  // The manifest has the version and the sizes, so the page asks the server
  // for neither.
  const heads = requests.filter((r) => r.startsWith("HEAD "));
  assert.deepEqual(heads, [], "no HEAD request with a manifest that has a version and sizes");
  console.log(`ok  the manifest's version (${manifest.version}) and sizes are used: no HEAD request`);
  // A layer with assets (material's script and stylesheet) has them on the
  // page once it has loaded, whether or not a lesson uses it yet.
  const withAssets = manifest.layers.find((layer) => layer.assets?.length);
  if (withAssets) {
    await page.evaluate((name) => globalThis.halogenLearn.playground.ensureLayers([name]), withAssets.name);
    for (const asset of withAssets.assets) {
      const selector = asset.kind === "stylesheet" ? `link[rel=stylesheet][href$="/${asset.path}"]` : `script[src$="/${asset.path}"]`;
      assert.equal(await page.locator(selector).count(), 1, `${asset.path} is on the page`);
    }
    console.log(`ok  the ${withAssets.name} layer's assets are on the page once it has loaded`);
  }

  // A compile is over once the outcome is a new one (its data-run counts
  // the compiles started) and Compile is enabled again: the click's action
  // may run a moment after the click, so the button alone could still show
  // the last compile's state.
  const compiled = async () => {
    const before = await page.locator(".learn-outcome").getAttribute("data-run");
    await page.locator("button.learn-compile:not([disabled])").click();
    await page.locator(`.learn-outcome:not([data-run="${before}"])`).waitFor({ timeout: 120000 });
    await page.locator("button.learn-compile:not([disabled])").waitFor({ timeout: 120000 });
  };
  const compile = async (what) => {
    const start = Date.now();
    await compiled();
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

  // Leaving a lesson while its layer is still downloading: the lesson the
  // reader arrives at gets its own layer once that download ends, and
  // Compile is enabled there (it stayed disabled once). Both lessons are
  // reached inside the app, so the compiler is not started over.
  if (lessons.some((l) => l.slug === "06-router") && lessons.some((l) => l.slug === "07-net")) {
    let releaseLayer;
    const layerHeld = new Promise((resolve) => (releaseLayer = resolve));
    let held = 0;
    const router = /layers\/router[^/]*\.tar\.zst/;
    await page.route(router, async (route) => {
      if (held++ === 0) await layerHeld;
      await route.continue();
    });
    await page.locator("header a.learn-brand").click();
    await page.locator(".learn-home ol a", { hasText: lessons.find((l) => l.slug === "06-router").title }).click();
    await page.waitForURL(`${url}06-router/`);
    await page.locator(".learn-compiler", { hasText: /libraries/ }).waitFor({ timeout: 20000 });
    await page.locator("a.learn-next").first().click();
    await page.waitForURL(`${url}07-net/`);
    releaseLayer();
    await page.locator("button.learn-compile:not([disabled])").waitFor({ timeout: 120000 });
    await page.unroute(router);
    console.log("ok  a lesson entered while another's layer downloads gets its own layer, and Compile");
  }

  // An error GHC throws rather than reports (one in the module's header) is
  // a failed compile too, not a program that compiled and stopped.
  await setEditor(page, "module Lesson (component) where\n\nimport Data.Row (type (.==))\n");
  assert.match(await compile("an import GHC cannot parse"), /did not compile/);
  console.log("ok  an error in the module's header is reported as a failed compile");

  // Every lesson: its start compiles (warnings allowed, since an exercise
  // may leave something unused), and its solution compiles with no
  // diagnostic at all and shows something: a component in the result, or
  // what the program printed, for a lesson whose program runs a spec.
  const runLesson = async (button) => {
    await page.locator(button).click();
    const start = Date.now();
    await compiled();
    const result = await page.locator("#learn-result").innerHTML();
    const stdout = (await page.locator(".learn-stdout").allTextContents()).join("");
    return {
      ms: Date.now() - start,
      outcome: await page.locator(".learn-outcome").textContent(),
      errors: await page.locator(".learn-diagnostics li.learn-error").allTextContents(),
      diagnostics: await page.locator(".learn-diagnostics li").allTextContents(),
      result,
      stdout,
      shown: result.length > 0 || stdout.length > 0,
    };
  };
  // The lessons whose program runs an hspec-halogen spec in the page rather
  // than mounting the component: what hspec's summary says for Start.hs and
  // for Solution.hs. Each test takes down what it mounted, so the result is
  // empty once the run is over, and the second compile runs on what the
  // first left, nothing.
  const specLessons = {
    "13-testing": {
      start: { summary: /^2 examples, 1 failure$/m, lines: [/^ {2}greets by name \[✔\]$/m, /^ {2}counts clicks \[✘\]$/m, /No element matches "button\.increment"/] },
      solution: { summary: /^3 examples, 0 failures$/m, lines: [/^ {2}counts clicks \[✔\]$/m, /^ {2}greets a new name once the old one is cleared \[✔\]$/m] },
    },
  };
  const reportProblems = (run, expected, file) => {
    const missing = [expected.summary, ...expected.lines].filter((pattern) => !pattern.test(run.stdout));
    return [
      missing.length ? `; ${file}'s report lacks ${missing.join(", ")}:\n${run.stdout}` : "",
      run.result === "" ? "" : `; ${file} left something mounted: ${run.result.slice(0, 200)}`,
    ].join("");
  };
  // A lesson, from the introduction's list, once it can compile.
  const openLesson = async (lesson, on = page) => {
    await on.locator("header a.learn-brand").click();
    await on.locator(".learn-home ol a", { hasText: lesson.title }).click();
    await on.waitForURL(`${url}${lesson.slug}/`);
    await on.locator("article h1", { hasText: lesson.title }).waitFor({ timeout: 5000 });
    await on.locator(".learn-editor .cm-editor").waitFor({ timeout: 5000 });
    // The lesson's layers may be loading still.
    await on.locator("button.learn-compile:not([disabled])").waitFor({ timeout: 120000 });
  };
  for (const lesson of lessons) {
    await openLesson(lesson);
    const start = await runLesson("button.learn-reset");
    const solution = await runLesson("button.learn-solution");
    const startOk = /^Compiled and ran/.test(start.outcome) && start.errors.length === 0;
    const solutionOk = /^Compiled and ran/.test(solution.outcome) && solution.diagnostics.length === 0 && solution.shown;
    const report = specLessons[lesson.slug];
    const reported = report ? reportProblems(start, report.start, "Start.hs") + reportProblems(solution, report.solution, "Solution.hs") : "";
    const problems = [
      startOk ? "" : `; Start.hs: ${start.outcome} ${start.errors.join(" | ")}`,
      solutionOk ? "" : `; Solution.hs: ${solution.outcome} ${solution.diagnostics.join(" | ")}${solution.shown ? "" : " (nothing shown)"}`,
      reported,
    ].join("");
    const summaries = report ? ` (${[start, solution].map((run) => run.stdout.match(/^\d+ examples?, \d+ failures?$/m)?.[0] ?? "no summary").join("; then ")}, nothing left mounted)` : "";
    const ok = startOk && solutionOk && !reported;
    console.log(`${ok ? "ok " : "FAIL"} ${lesson.slug}: Start.hs ${seconds(start.ms)}, Solution.hs ${seconds(solution.ms)}${ok ? summaries : problems}`);
    if (!ok) failed = true;
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

  // Lesson 6's hash router, under the page's <base href>: its links and
  // what it pushes are the lesson's own address with the route after the #,
  // not the base's, and Back goes to its route before without leaving the
  // lesson.
  const routing = lessons.find((lesson) => lesson.slug === "06-router");
  if (routing) {
    await openLesson(routing);
    await page.locator("button.learn-solution").click();
    assert.match(await compile("lesson 6, Solution.hs"), /^Compiled and ran/);
    const about = page.locator("#learn-result a", { hasText: "About" });
    assert.equal(await about.getAttribute("href"), `${base}06-router/#/about`, "the link names the lesson's address");
    await about.click();
    await page.waitForURL(`${url}06-router/#/about`);
    await page.locator("#learn-result p", { hasText: "You are at About" }).waitFor({ timeout: 5000 });
    await page.goBack();
    await page.waitForURL(`${url}06-router/`);
    await page.locator("#learn-result p", { hasText: "You are at Home" }).waitFor({ timeout: 5000 });
    assert.equal(await page.locator("article h1", { hasText: routing.title }).count(), 1, "Back stays in the lesson");
    console.log("ok  lesson 6's links keep the lesson's address, and Back returns to the route before, in the lesson");
  }

  // What lesson 5's Finalize prints, as the next compile ends the last
  // run's component, shows under the editor, every time.
  const lifecycle = lessons.find((lesson) => lesson.slug === "05-lifecycle");
  if (lifecycle) {
    await openLesson(lifecycle);
    await page.locator("button.learn-solution").click();
    assert.match(await compile("lesson 5, Solution.hs"), /^Compiled and ran/);
    for (let i = 1; i <= 4; i++) {
      await page.waitForTimeout(1100);
      assert.match(await compile(`lesson 5 again, ${i}`), /^Compiled and ran/);
      const stdout = (await page.locator(".learn-stdout").allTextContents()).join("");
      assert.match(stdout, /Finalized after \d+ seconds\./, `compile ${i} shows what the last run's Finalize printed`);
    }
    console.log("ok  what lesson 5's Finalize prints shows on every compile after the first");
  }

  // Tab indents in the editor, and Escape then Tab leaves it.
  await page.locator(".learn-editor .cm-content").click();
  const typed = await page.evaluate(() => document.querySelector(".learn-editor .cm-content").innerText.length);
  await page.keyboard.press("Tab");
  await page.waitForFunction((n) => document.querySelector(".learn-editor .cm-content").innerText.length === n + 2, typed);
  assert.ok(await page.evaluate(() => !!document.activeElement.closest(".cm-editor")), "Tab stays in the editor");
  await page.keyboard.press("Escape");
  await page.keyboard.press("Tab");
  assert.ok(await page.evaluate(() => !document.activeElement.closest(".cm-editor")), "Escape then Tab leaves the editor");
  await page.locator("button.learn-reset").click();
  console.log("ok  Tab indents by two spaces, and Escape then Tab moves the focus out of the editor");

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
  assert.ok(!requests.slice(fetched).some((r) => r.startsWith("GET ") && archive.test(r)), "the archives come from the cache");
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
  // Not even a HEAD: the manifest has the archive's size, which the page shows.
  assert.ok(!requests.slice(before).some((r) => archive.test(r)), "no archive downloaded, or asked about, on a narrow screen");
  assert.equal(await small.locator('.cm-content[contenteditable="false"]').count(), 1, "the code is read-only there");
  console.log("ok  on a narrow screen the compiler waits to be asked, and the code is read-only");
  const asked = Date.now();
  await small.locator("button.learn-load").click();
  await untilReady(small, asked);
  await small.locator('.cm-content[contenteditable="true"]').waitFor({ timeout: 5000 });
  console.log("ok  loading it anyway makes the code editable");
  await phone.close();

  // A compiler that does not load says why, and Retry tries again. The
  // second time, the manifest is one without a version or sizes (as an older
  // build's), and the page asks the server for them instead.
  const flaky = await browser.newContext({ viewport: { width: 1400, height: 900 } });
  const retried = await flaky.newPage();
  watch(retried, /rootfs\.json|503/);
  let refused = 0;
  const bare = structuredClone(manifest);
  for (const entry of [bare, bare.rootfs, ...bare.layers]) {
    for (const field of ["version", "size", "compressedSize"]) delete entry[field];
    // An archive in one part is the file itself, as in the first manifests,
    // which had no parts.
    if (entry.parts?.length === 1 && entry.parts[0].path === entry.archive) delete entry.parts;
    for (const part of entry.parts ?? []) delete part.size;
  }
  await retried.route(/rootfs\/rootfs\.json$/, (route) =>
    refused++ === 0
      ? route.fulfill({ status: 503, body: "" })
      : route.fulfill({ status: 200, contentType: "application/json", body: JSON.stringify(bare) }),
  );
  await retried.goto(`${url}01-hello/`);
  await retried.locator("button.learn-retry").waitFor({ timeout: 30000 });
  assert.match(await retried.locator(".learn-compiler .learn-error").textContent(), /rootfs\.json: 503/, "the reason is shown");
  const retry = Date.now();
  const beforeRetry = requests.length;
  await retried.locator("button.learn-retry").click();
  await untilReady(retried, retry);
  assert.ok(
    requests.slice(beforeRetry).some((r) => r === `HEAD ${base}rootfs/${manifest.rootfs.parts[0].path}`),
    "without a version in the manifest, the page asks the server for the archive's",
  );
  console.log("ok  a compiler that fails to load says why, and Retry loads it, from a manifest without a version or sizes");
  await flaky.close();

  // A layer that does not load is its lessons' failure, with a Retry of its
  // own: the compiler is ready, and a lesson without the layer compiles.
  const layered = lessons.find((lesson) => lesson.layers.length);
  if (layered) {
    const name = layered.layers[0];
    const partly = await browser.newContext({ viewport: { width: 1400, height: 900 } });
    const lacking = await partly.newPage();
    watch(lacking, /503/);
    let refusals = 0;
    await lacking.route(new RegExp(`rootfs/layers/${name}\\.tar\\.zst`), (route) =>
      refusals++ < 2 ? route.fulfill({ status: 503, body: "" }) : route.continue(),
    );
    const since = Date.now();
    await lacking.goto(`${url}${layered.slug}/`);
    await lacking.locator("button.learn-retry-layers").waitFor({ timeout: 300000 });
    await untilReady(lacking, since);
    assert.match(await lacking.locator(".learn-result-column .learn-error").textContent(), /503/, "the layer's failure is shown");
    await lacking.locator("button.learn-retry-layers").click();
    for (const deadline = Date.now() + 30000; refusals < 2; await lacking.waitForTimeout(100)) {
      assert.ok(Date.now() < deadline, "Retry fetches the layer again");
    }
    await lacking.locator("button.learn-retry-layers").waitFor({ timeout: 30000 });
    await openLesson(lessons[0], lacking);
    await lacking.locator("button.learn-compile").click();
    await lacking.locator("button.learn-compile:not([disabled])").waitFor({ timeout: 120000 });
    assert.match(await lacking.locator(".learn-outcome").textContent(), /^Compiled and ran/, "a lesson without the layer compiles");
    await openLesson(layered, lacking);
    assert.equal(await lacking.locator("button.learn-retry-layers").count(), 0, "coming back loads the layer");
    console.log(`ok  a layer (${name}) that fails to load is ${layered.slug}'s failure alone, and loads when tried again`);
    await partly.close();
  }

  assert.deepEqual(errors, [], "errors in the page");
  console.log("ok  no errors in the page");
  assert.ok(offSite.length === 0, `the page loaded from another site than the test server's: ${offSite.join(", ")}`);
  console.log(`ok  nothing loaded from another site but the layers' external dependencies (${[...externalSeen].join(", ") || "none of them"})`);
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
  page.on("request", (request) => {
    const target = new URL(request.url());
    if (!/^https?:$/.test(target.protocol) || request.url().startsWith(url)) return;
    const allowed = external.get(request.url());
    const where = new URL(page.url()).pathname;
    const lesson = lessons.find((l) => where.startsWith(`${base}${l.slug}/`));
    if (allowed && (allowed.anywhere || lesson?.layers.includes(allowed.layer))) {
      externalSeen.add(request.url());
      return;
    }
    offSite.push(`${request.url()} (from ${where})`);
  });
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
