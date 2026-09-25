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

const types = { ".html": "text/html", ".js": "text/javascript", ".mjs": "text/javascript", ".css": "text/css", ".wasm": "application/wasm" };
const routes = {
  "/": [page, ".html"],
  "/__runner/boot.js": [boot, ".js"],
  "/__runner/test.wasm": [bytes, ".wasm"],
  "/__runner/ghc_wasm_jsffi.js": [() => readFileSync(jsffiPath), ".js"],
  "/__runner/suite.js": [() => suiteScript, ".js"],
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
    // The JavaScript backend's runtime writes the suite's stdout to the
    // console a chunk at a time, newlines and all: that is the report.
    if (!isWasm && message.type() === "log") {
      process.stdout.write(message.text());
      return;
    }
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
  const timeoutSeconds = Number(process.env.HSPEC_HALOGEN_TIMEOUT ?? 600);
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
  rmSync(suiteDir, { recursive: true, force: true });
}
process.exit(exitCode);
