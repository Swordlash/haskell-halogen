// Run a wasm hspec suite built with hspec-halogen inside
// headless Chromium.
//
//   node toolchain/browser-test-runner.mjs <app.wasm> <ghc_wasm_jsffi.mjs> [hspec args...]
//
// wasm-test-wrapper.sh calls this for a test binary built as a reactor (one
// exporting hs_start), from the package directory, as cabal runs test suites.
// The suite is loaded into a page and runs there, so it can mount components
// into the real DOM; this side only serves it, relays what it prints, and
// performs its clicks and keystrokes as trusted input through Playwright.
//
// A package's test/web/ directory is served next to the suite, and every .css
// and .js file in it is loaded before the suite starts. If it holds a bundle.sh,
// that is run first with a fresh output directory as its argument, and what it
// writes there is served and loaded the same way.
//
// HALOGEN_TEST_HEADED=1 shows the browser; HALOGEN_TEST_SLOWMO=<ms> slows
// every Playwright action down so it can be followed; HALOGEN_TEST_TIMEOUT is
// how many seconds the whole suite may take (600 by default).
import { execFileSync } from "node:child_process";
import { existsSync, mkdtempSync, readdirSync, readFileSync, rmSync } from "node:fs";
import { createServer } from "node:http";
import { tmpdir } from "node:os";
import { dirname, extname, join, normalize, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium } from "playwright";

const [wasmPath, jsffiPath, ...hspecArgs] = process.argv.slice(2);
const toolchain = dirname(fileURLToPath(import.meta.url));
const wasiShim = resolve(toolchain, "../node_modules/@bjorn3/browser_wasi_shim/dist");

// hspec colours its report only for a terminal, and the suite writes to a page.
const args =
  process.stdout.isTTY && !hspecArgs.some((arg) => /^--(no-)?colou?r$/.test(arg)) ? ["--color", ...hspecArgs] : hspecArgs;

const webDir = resolve("test/web");
const bundleDir = mkdtempSync(join(tmpdir(), "halogen-browser-test-"));
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
    <script type="module" src="/__runner/boot.js"></script>
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

const types = { ".html": "text/html", ".js": "text/javascript", ".mjs": "text/javascript", ".css": "text/css", ".wasm": "application/wasm" };
const routes = {
  "/": [page, ".html"],
  "/__runner/boot.js": [boot, ".js"],
  "/__runner/test.wasm": [() => readFileSync(wasmPath), ".wasm"],
  "/__runner/ghc_wasm_jsffi.js": [() => readFileSync(jsffiPath), ".js"],
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
    const file = path.startsWith("/__runner/wasi/")
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
  headless: !process.env.HALOGEN_TEST_HEADED,
  slowMo: Number(process.env.HALOGEN_TEST_SLOWMO ?? 0),
});
let exitCode = 1;
try {
  const context = await browser.newContext();
  const tab = await context.newPage();
  // Long enough for a real page, short enough that an element which never
  // becomes clickable fails its test rather than the run.
  tab.setDefaultTimeout(5000);

  const pageErrors = [];
  tab.on("pageerror", (error) => pageErrors.push(error.stack ?? error.message));
  tab.on("console", (message) => {
    const line = `[page ${message.type()}] ${message.text()}\n`;
    (message.type() === "error" ? process.stderr : process.stdout).write(line);
  });

  let failures = null;
  let finish;
  const finished = new Promise((resolve) => {
    finish = resolve;
  });
  await tab.exposeFunction("__halogenTestFinished", (crash) => finish(crash));
  await tab.exposeFunction("__halogenTestWrite", (fd, text) => (fd === 2 ? process.stderr : process.stdout).write(text));
  await tab.exposeFunction("__halogenTestDone", (count) => {
    failures = count;
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
  await tab.exposeFunction("__halogenTestPress", (key) => tab.keyboard.press(key));
  await tab.addInitScript((args) => {
    globalThis.__halogenTestArgs = args;
    globalThis.__halogenTest = {
      act: (action, selector, text) => globalThis.__halogenTestAct(action, selector, text),
      press: (key) => globalThis.__halogenTestPress(key),
      done: (count) => globalThis.__halogenTestDone(count),
    };
  }, args);

  const { port } = server.address();
  await tab.goto(`http://127.0.0.1:${port}/`);
  // A suite that never finishes (a test waiting on something that will not
  // happen, with no timeout of its own) fails the run rather than hanging it.
  const timeoutSeconds = Number(process.env.HALOGEN_TEST_TIMEOUT ?? 600);
  const crash = await Promise.race([
    finished,
    new Promise((resolve) => setTimeout(() => resolve(`no result after ${timeoutSeconds}s`), timeoutSeconds * 1000).unref()),
  ]);

  if (crash) process.stderr.write(`The suite stopped with an exception:\n${crash}\n`);
  if (pageErrors.length) process.stderr.write(`Errors in the page:\n${pageErrors.join("\n")}\n`);
  if (failures === null && !crash) process.stderr.write("The suite finished without reporting a result.\n");
  exitCode = !crash && !pageErrors.length && failures === 0 ? 0 : 1;
} finally {
  await browser.close();
  server.close();
  rmSync(bundleDir, { recursive: true, force: true });
}
process.exit(exitCode);
