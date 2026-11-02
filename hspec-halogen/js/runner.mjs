// Run a test suite for the hspec-halogen executable: one built with
// hspec-halogen in headless Chromium, any other under Node.
//
//   hspec-halogen test <suite> [hspec args...]
//
// The executable runs this with node, from the directory cabal runs the suite
// in (the package's). A suite built by the WebAssembly backend comes
// post-linked:
//
//   node runner.mjs <suite.wasm> <ghc_wasm_jsffi.mjs> [hspec args...]
//
// and one built by the JavaScript backend (a Node script, with the program
// itself in <suite>.jsexe/all.js) does not:
//
//   node runner.mjs <suite> - [hspec args...]
//
// A browser suite is loaded into a page and runs there, so it can mount
// components into the real DOM; this side only serves it, relays what it
// prints, and performs its clicks and keystrokes as trusted input through
// Playwright. On wasm that is a reactor exporting hs_start, which the page
// calls; on the JavaScript backend it is a program using hspec-halogen's page
// functions, bundled with esbuild and started by loading it. Any other suite
// runs here, as it would under a plain test wrapper.
//
// A package's test/web/ directory is served next to a browser suite, and every
// .css and .js file in it is loaded before the suite starts. If it holds a
// bundle.sh, that is run first with a fresh output directory as its argument,
// and what it writes there is served and loaded the same way.
//
// The npm packages it needs -- playwright, and @bjorn3/browser_wasi_shim for
// wasm or esbuild for JavaScript -- are looked up from the working directory
// upwards, as node itself would for a
// script there: they belong to the project under test, not to this file.
//
// HSPEC_HALOGEN_HEADED=1 shows the browser; HSPEC_HALOGEN_SLOWMO=<ms> slows
// every Playwright action down so it can be followed; HSPEC_HALOGEN_TIMEOUT is
// how many seconds the whole suite may take (600 by default).
//
// HSPEC_HALOGEN_JOBS=<n> (1 by default) runs the examples marked parallel on
// n pages, each in a browser context of its own, after the unmarked ones
// have run on a page by themselves. HSPEC_HALOGEN_WORKERS=<m> (0 by
// default) also starts m Web Workers of the suite, in a context of their
// own, beside those pages: they run the examples marked inWorker, which
// need no page. Each page and worker is a shard of the suite, told which by
// a global the library reads (Test.Hspec.Halogen.Internal.Shard), and its
// report is buffered and printed on its own.
import { execFileSync, spawnSync } from "node:child_process";
import { existsSync, mkdtempSync, readdirSync, readFileSync, rmSync } from "node:fs";
import { createServer } from "node:http";
import { createRequire } from "node:module";
import { tmpdir } from "node:os";
import { dirname, extname, join, normalize, resolve } from "node:path";
import { pathToFileURL } from "node:url";
import { WASI } from "node:wasi";

const [suitePath, jsffiPath, ...hspecArgs] = process.argv.slice(2);

// The directory holding node_modules/<name>, from here upwards, else from
// NODE_PATH.
const packageDir = (name) => {
  const candidates = [];
  for (let dir = process.cwd(); ; dir = dirname(dir)) {
    candidates.push(join(dir, "node_modules", name));
    if (dirname(dir) === dir) break;
  }
  for (const dir of (process.env.NODE_PATH ?? "").split(":").filter(Boolean)) candidates.push(join(dir, name));
  const found = candidates.find((dir) => existsSync(join(dir, "package.json")));
  if (!found) {
    process.stderr.write(
      `hspec-halogen: cannot find the npm package ${name} from ${process.cwd()}.\n` +
        `Install it in the project: npm install --save-dev ${name}\n` +
        "and a browser for it: npx playwright install chromium\n",
    );
    process.exit(1);
  }
  return found;
};

const bytes = readFileSync(suitePath);
const isWasm = bytes.subarray(0, 4).equals(Buffer.from([0x00, 0x61, 0x73, 0x6d]));
// A JavaScript program that uses hspec-halogen's page functions carries the
// name of the global its arguments arrive in.
const isBrowserSuite = isWasm
  ? WebAssembly.Module.exports(new WebAssembly.Module(bytes)).some((e) => e.name === "hs_start")
  : bytes.includes("__halogenTestArgs");

if (!isBrowserSuite && !isWasm) {
  // A Node script, as cabal would run it.
  const run = spawnSync(process.execPath, [suitePath, ...hspecArgs], { stdio: "inherit" });
  process.exit(run.status ?? 1);
}

if (!isBrowserSuite) {
  // A WASI command: run it here. Exit before the event loop runs again: a
  // JavaScript callback into Haskell can leave the RTS scheduler queued, and it
  // fails if it runs once the RTS has shut down.
  const wasi = new WASI({ args: [suitePath, ...hspecArgs], env: process.env, preopens: { "/": "/" }, version: "preview1", returnOnExit: true });
  const exports = {};
  const ghcWasmImports = (await import(pathToFileURL(resolve(jsffiPath)))).default;
  const instance = await WebAssembly.instantiate(await WebAssembly.compile(bytes), {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghcWasmImports(exports),
  });
  Object.assign(exports, instance.exports);
  process.exit(wasi.start(instance));
}

const jobs = Number(process.env.HSPEC_HALOGEN_JOBS ?? 1);
if (!Number.isSafeInteger(jobs) || jobs < 1) {
  process.stderr.write("HSPEC_HALOGEN_JOBS must be a positive integer\n");
  process.exit(1);
}
const workers = Number(process.env.HSPEC_HALOGEN_WORKERS ?? 0);
if (!Number.isSafeInteger(workers) || workers < 0) {
  process.stderr.write("HSPEC_HALOGEN_WORKERS must be a non-negative integer\n");
  process.exit(1);
}
// One page and no workers is the original protocol: the whole suite on one
// page, its report written as it comes. Anything else divides the suite.
const sharded = jobs > 1 || workers > 0;

const { chromium } = createRequire(join(packageDir("playwright"), "package.json"))("playwright");
const wasiShim = isWasm ? join(packageDir("@bjorn3/browser_wasi_shim"), "dist") : null;

// A JavaScript-backend program is bundled for the page, as the repository's
// build-ghcjs.sh bundles an app: its js-sources may import npm packages, and
// the runtime require()s these Node modules only when it finds itself in Node.
const suiteDir = mkdtempSync(join(tmpdir(), "hspec-halogen-suite-"));
let suiteScript = null;
if (!isWasm) {
  const program = `${suitePath}.jsexe/all.js`;
  if (!existsSync(program)) {
    process.stderr.write(`hspec-halogen: ${program} is missing; is ${suitePath} a JavaScript-backend program?\n`);
    process.exit(1);
  }
  const esbuild = createRequire(join(packageDir("esbuild"), "package.json"))("esbuild");
  esbuild.buildSync({
    entryPoints: [program],
    bundle: true,
    outfile: join(suiteDir, "suite.js"),
    logLevel: "warning",
    logOverride: { "direct-eval": "silent" },
    external: ["os", "fs", "child_process", "path", "ghcjs-profiling"],
  });
  suiteScript = readFileSync(join(suiteDir, "suite.js"));
}

// hspec colours its report only for a terminal, and the suite writes to a page.
const args =
  process.stdout.isTTY && !hspecArgs.some((arg) => /^--(no-)?colou?r$/.test(arg)) ? ["--color", ...hspecArgs] : hspecArgs;

const webDir = resolve("test/web");
const bundleDir = mkdtempSync(join(tmpdir(), "hspec-halogen-"));
if (existsSync(join(webDir, "bundle.sh"))) {
  execFileSync("sh", [join(webDir, "bundle.sh"), bundleDir], { stdio: "inherit" });
}
const assetDirs = [bundleDir, webDir].filter(existsSync);
const assets = assetDirs.flatMap((dir) => readdirSync(dir)).filter((name) => [".css", ".js"].includes(extname(name)));

const page = `<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>halogen browser test</title>
    ${assets.filter((a) => a.endsWith(".css")).map((a) => `<link rel="stylesheet" href="/${a}">`).join("\n    ")}
    ${assets.filter((a) => a.endsWith(".js")).map((a) => `<script src="/${a}"></script>`).join("\n    ")}
  </head>
  <body>
    ${isWasm ? '<script type="module" src="/__runner/boot.js"></script>' : '<script src="/__runner/suite.js"></script>'}
  </body>
</html>`;

// Instantiate the suite the way the examples' index.js does, with its output
// sent to this process as it is written rather than a line at a time.
const boot = `
try {
  const { WASI, OpenFile, File, ConsoleStdout } = await import("/__runner/wasi/index.js");
  const { default: ghcWasmImports } = await import("/__runner/ghc_wasm_jsffi.js");
  const decoders = [null, new TextDecoder(), new TextDecoder()];
  const output = (fd) => new ConsoleStdout((bytes) => __halogenTestWrite(fd, decoders[fd].decode(bytes, { stream: true })));
  const wasi = new WASI(["test", ...globalThis.__halogenTestArgs], [], [new OpenFile(new File([])), output(1), output(2)], { debug: false });
  const exports = {};
  const { instance } = await WebAssembly.instantiateStreaming(fetch("/__runner/test.wasm"), {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghcWasmImports(exports),
  });
  Object.assign(exports, instance.exports);
  wasi.initialize(instance);
  await instance.exports.hs_start();
  __halogenTestFinished(null);
} catch (error) {
  __halogenTestFinished(String(error?.stack ?? error));
}
`;

// A worker of the suite, a second copy of it with no page: on wasm a module
// that instantiates the binary as the page's boot does, on the JavaScript
// backend a classic script that imports the bundle (the runtime starts main
// itself). Its name is its shard. What it would print, and how it ends, it
// posts to the page that started it, which passes it on to this process:
//   { kind: "write", fd, text }          output, to the shard's report
//   { kind: "done", count, supported }   the suite's failures (reportDone)
//   { kind: "finished", crash, supported }  hs_start returned or threw
//   { kind: "error", message }           an error the program did not catch
// The console is the worker's own, which Playwright does not hand to the
// page's console event in every version, so it goes the same way: on the
// JavaScript backend console.log is where the runtime writes stdout and
// stderr, a chunk at a time, so it is the report as it is on a page; any
// other kind of message is labelled, as the page's are.
const workerPrelude = `
const shard = self.name;
const post = (message) => self.postMessage(message);
const supported = () => globalThis.__halogenTestShardSupported === true;
globalThis.__halogenTestArgs = ${JSON.stringify(args)};
globalThis.__halogenTestShard = shard;
globalThis.__halogenTestWorkers = ${workers};
globalThis.__halogenTest = { done: (count) => post({ kind: "done", count, supported: supported() }) };
const runtimeWritesToLog = ${!isWasm};
for (const type of ["log", "info", "warn", "error", "debug"]) {
  console[type] = runtimeWritesToLog && type === "log"
    ? (...values) => post({ kind: "write", fd: 1, text: values.join(" ") })
    : (...values) => post({ kind: "write", fd: type === "error" ? 2 : 1, text: \`[worker \${type}] \${values.join(" ")}\\n\` });
}
// What escapes the program fails the shard, as an error in a page does. The
// error event is kept from the page's Worker object, which would otherwise
// count it again as the worker failing to start.
self.addEventListener("error", (event) => {
  event.preventDefault();
  post({ kind: "error", message: String(event.error?.stack ?? event.message) });
});
self.addEventListener("unhandledrejection", (event) => post({ kind: "error", message: String(event.reason?.stack ?? event.reason) }));
`;
// Each in a block, so that its names stay out of the global scope the
// JavaScript backend's bundle is imported into.
const workerScript = isWasm
  ? `{${workerPrelude}
try {
  const { WASI, OpenFile, File, ConsoleStdout } = await import("/__runner/wasi/index.js");
  const { default: ghcWasmImports } = await import("/__runner/ghc_wasm_jsffi.js");
  const decoders = [null, new TextDecoder(), new TextDecoder()];
  const output = (fd) => new ConsoleStdout((bytes) => post({ kind: "write", fd, text: decoders[fd].decode(bytes, { stream: true }) }));
  const wasi = new WASI(["test", ...globalThis.__halogenTestArgs], [], [new OpenFile(new File([])), output(1), output(2)], { debug: false });
  const exports = {};
  const { instance } = await WebAssembly.instantiateStreaming(fetch("/__runner/test.wasm"), {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghcWasmImports(exports),
  });
  Object.assign(exports, instance.exports);
  wasi.initialize(instance);
  await instance.exports.hs_start();
  post({ kind: "finished", crash: null, supported: supported() });
} catch (error) {
  post({ kind: "finished", crash: String(error?.stack ?? error), supported: supported() });
}
}
`
  : `{${workerPrelude}
try {
  importScripts("/__runner/suite.js");
} catch (error) {
  post({ kind: "finished", crash: String(error?.stack ?? error), supported: supported() });
}
}
`;

// The page the workers are started from: nothing on it, since they have no
// use for it beyond being started there and heard from.
const workersPage = `<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>halogen browser test workers</title>
  </head>
  <body></body>
</html>`;

const types = { ".html": "text/html", ".js": "text/javascript", ".mjs": "text/javascript", ".css": "text/css", ".wasm": "application/wasm" };
const routes = {
  "/": [page, ".html"],
  "/__runner/boot.js": [boot, ".js"],
  "/__runner/test.wasm": [bytes, ".wasm"],
  "/__runner/ghc_wasm_jsffi.js": [() => readFileSync(jsffiPath), ".js"],
  "/__runner/suite.js": [() => suiteScript, ".js"],
  "/__runner/workers.html": [workersPage, ".html"],
  "/__runner/worker.js": [workerScript, ".js"],
};

const server = createServer((request, response) => {
  const path = decodeURIComponent(new URL(request.url, "http://x").pathname);
  let body;
  let type;
  if (routes[path]) {
    const [content, extension] = routes[path];
    body = typeof content === "function" ? content() : content;
    type = types[extension];
  } else {
    const file = path.startsWith("/__runner/wasi/") && wasiShim
      ? join(wasiShim, normalize(path.slice("/__runner/wasi/".length)))
      : assetDirs.map((dir) => join(dir, normalize(path))).find(existsSync);
    if (file && existsSync(file)) {
      body = readFileSync(file);
      type = types[extname(file)] ?? "application/octet-stream";
    }
  }
  if (body === undefined) {
    response.writeHead(404).end();
  } else {
    response.writeHead(200, { "content-type": type });
    response.end(body);
  }
});
await new Promise((ready) => server.listen(0, "127.0.0.1", ready));

const browser = await chromium.launch({
  headless: !process.env.HSPEC_HALOGEN_HEADED,
  slowMo: Number(process.env.HSPEC_HALOGEN_SLOWMO ?? 0),
});
const timeoutSeconds = Number(process.env.HSPEC_HALOGEN_TIMEOUT ?? 600);
const deadline = Date.now() + timeoutSeconds * 1000;
// A suite that never finishes (a test waiting on something that will not
// happen, with no timeout of its own) fails the run rather than hanging it.
const untilDeadline = (finished) =>
  Promise.race([
    finished,
    new Promise((resolve) => setTimeout(() => resolve(`no result after ${timeoutSeconds}s`), Math.max(0, deadline - Date.now())).unref()),
  ]);
// Where a shard's output goes: straight through when the suite is not
// divided, otherwise kept for its report, so that concurrent shards do not
// interleave.
const output = () => {
  const stdout = [];
  const stderr = [];
  const write = (fd, text) => {
    if (!sharded) (fd === 2 ? process.stderr : process.stdout).write(text);
    else (fd === 2 ? stderr : stdout).push(text);
  };
  return { stdout, stderr, write };
};
// How a shard went, from what it said: a crash or an error in the program
// is fatal, and a shard that reported no failures, and whose library knew
// it was one, passed.
const outcome = (shard, { stdout, stderr, write }, { crash, errors, failures, supported }) => {
  if (crash) write(2, `The suite stopped with an exception:\n${crash}\n`);
  if (errors.length) write(2, `Errors in the ${shard.startsWith("worker/") ? "worker" : "page"}:\n${errors.join("\n")}\n`);
  if (failures === null && !crash) write(2, "The suite finished without reporting a result.\n");
  if (!supported) write(2, "The suite library does not support browser shards. Rebuild with the current hspec-halogen library or set HSPEC_HALOGEN_JOBS=1 and HSPEC_HALOGEN_WORKERS=0.\n");
  const fatal = Boolean(crash) || errors.length > 0;
  return { fatal, supported, shard, stdout: stdout.join(""), stderr: stderr.join(""), ok: supported && !fatal && failures === 0 };
};
async function runShard(shard) {
  const out = output();
  const { write } = out;
  const context = await browser.newContext();
  const tab = await context.newPage();
  // Long enough for a real page, short enough that an element which never
  // becomes clickable fails its test rather than the run.
  tab.setDefaultTimeout(5000);

  const pageErrors = [];
  tab.on("pageerror", (error) => pageErrors.push(error.stack ?? error.message));
  tab.on("console", (message) => {
    // The JavaScript backend's runtime writes the suite's stdout to the
    // console a chunk at a time, newlines and all: that is the report.
    if (!isWasm && message.type() === "log") {
      write(1, message.text());
      return;
    }
    const line = `[page ${message.type()}] ${message.text()}\n`;
    write(message.type() === "error" ? 2 : 1, line);
  });

  let failures = null;
  let finish;
  const finished = new Promise((resolve) => {
    finish = resolve;
  });
  await tab.exposeFunction("__halogenTestFinished", (crash) => finish(crash));
  await tab.exposeFunction("__halogenTestWrite", (fd, text) => write(fd, text));
  await tab.exposeFunction("__halogenTestDone", (count) => {
    failures = count;
    // A JavaScript-backend suite starts itself, so its report is its end.
    if (!isWasm) finish(null);
  });
  // The suite acts on an element through a selector the harness tagged it
  // with, so Playwright's locator does the waiting and the actionability checks.
  await tab.exposeFunction("__halogenTestAct", async (action, selector, text) => {
    const target = tab.locator(selector);
    if (action === "click") await target.click();
    else if (action === "type") await target.pressSequentially(text);
    else if (action === "clear") await target.clear();
    else throw new Error(`unknown action ${action}`);
  });
  // Playwright's mouse drag: pointer and mouse events along the way, and in
  // Chromium HTML5 drag and drop's too.
  await tab.exposeFunction("__halogenTestDrag", (source, target) => tab.locator(source).dragTo(tab.locator(target)));
  await tab.exposeFunction("__halogenTestPress", (key) => tab.keyboard.press(key));
  await tab.addInitScript(({ args, shard, workers }) => {
    globalThis.__halogenTestArgs = args;
    globalThis.__halogenTestShard = shard;
    globalThis.__halogenTestWorkers = workers;
    globalThis.__halogenTest = {
      act: (action, selector, text) => globalThis.__halogenTestAct(action, selector, text),
      drag: (source, target) => globalThis.__halogenTestDrag(source, target),
      press: (key) => globalThis.__halogenTestPress(key),
      done: (count) => globalThis.__halogenTestDone(count),
    };
  }, { args, shard, workers });

  const { port } = server.address();
  await tab.goto(`http://127.0.0.1:${port}/`);
  const crash = await untilDeadline(finished);
  const supported = !shard || Boolean(crash) || await tab.evaluate(() => globalThis.__halogenTestShardSupported === true);
  await context.close();
  return outcome(shard, out, { crash, errors: pageErrors, failures, supported });
}

// Start a worker of the suite for each of these shards, all from one blank
// page in a context of their own, and give each one's outcome. The page
// passes what each worker posts on to this process with the shard it came
// from, so the workers share one page and its bindings.
async function runWorkers(shards) {
  const context = await browser.newContext();
  const tab = await context.newPage();
  const states = new Map(shards.map((shard) => {
    let finish;
    const finished = new Promise((resolve) => {
      finish = resolve;
    });
    return [shard, { out: output(), errors: [], failures: null, supported: false, finished, finish }];
  }));
  const pageErrors = [];
  tab.on("pageerror", (error) => pageErrors.push(error.stack ?? error.message));
  await tab.exposeFunction("__halogenTestWorkerMessage", (shard, message) => {
    const state = states.get(shard);
    if (!state) return;
    if (message.kind === "write") state.out.write(message.fd, message.text);
    else if (message.kind === "error") state.errors.push(message.message);
    else if (message.kind === "done") {
      state.failures = message.count;
      state.supported = message.supported;
      // A JavaScript-backend suite starts itself, so its report is its end.
      if (!isWasm) state.finish(null);
    } else if (message.kind === "finished") {
      state.supported ||= message.supported;
      state.finish(message.crash);
    }
  });
  // The worker's script did not load or did not parse: nothing in it ran to
  // say so.
  await tab.exposeFunction("__halogenTestWorkerFailed", (shard, message) => states.get(shard)?.finish(`the worker failed: ${message}`));
  const { port } = server.address();
  await tab.goto(`http://127.0.0.1:${port}/__runner/workers.html`);
  await tab.evaluate(({ shards, type }) => {
    globalThis.halogenTestWorkers = shards.map((shard) => {
      const worker = new Worker("/__runner/worker.js", { type, name: shard });
      worker.onmessage = (event) => globalThis.__halogenTestWorkerMessage(shard, event.data);
      worker.onerror = (event) => {
        event.preventDefault();
        globalThis.__halogenTestWorkerFailed(shard, event.message || "its script did not load");
      };
      return worker;
    });
  }, { shards, type: isWasm ? "module" : "classic" });
  const results = await Promise.all(shards.map(async (shard) => {
    const state = states.get(shard);
    const crash = await untilDeadline(state.finished);
    return { shard, state, crash };
  }));
  await context.close();
  return results.map(({ shard, state, crash }) =>
    outcome(shard, state.out, { crash, errors: [...state.errors, ...pageErrors], failures: state.failures, supported: Boolean(crash) || state.supported }));
}
const report = (result) => {
  if (result.shard) process.stdout.write(`\nBrowser shard ${result.shard}\n`);
  process.stdout.write(result.stdout);
  process.stderr.write(result.stderr);
  return result.ok;
};
let exitCode = 1;
try {
  // Unmarked and sequential examples finish before parallel pages and the
  // workers start.
  const serial = await runShard(sharded ? "serial" : "");
  const rest = !sharded || !serial.supported || serial.fatal
    ? []
    : [
      ...Array.from({ length: jobs }, (_, i) => runShard(`${i}/${jobs}`)),
      ...(workers > 0 ? [runWorkers(Array.from({ length: workers }, (_, i) => `worker/${i}/${workers}`))] : []),
    ];
  const results = (await Promise.all(rest)).flat();
  exitCode = [serial, ...results].map(report).every(Boolean) ? 0 : 1;
} finally {
  await browser.close();
  server.close();
  rmSync(bundleDir, { recursive: true, force: true });
  rmSync(suiteDir, { recursive: true, force: true });
}
process.exit(exitCode);
