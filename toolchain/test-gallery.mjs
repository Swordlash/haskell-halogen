// Drive the deployed examples gallery (examples/all) in headless Chromium.
//
//   npm run test-gallery                      # after: npm run build-wasm -- all
//   node toolchain/test-gallery.mjs [dir]     # default dist-newstyle/wasm/public/all
//
// This is what the unit tests cannot see: the size-optimised app.wasm loading
// in a real browser, each example mounting and unmounting as the route changes,
// the path routing surviving back, forward and a reload, and every route's
// page arriving with the gallery's shell already rendered into it. Needs a
// Chromium that Playwright can launch (`npx playwright install chromium`), and
// the network, since the Pixi example loads PixiJS from a CDN.
//
// The site is served under HALOGEN_BASE_PATH (default /), the base path the
// pages were built for, as GitHub Pages serves it under the project's name.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { createServer } from "node:http";
import { extname, join, normalize, resolve } from "node:path";
import { chromium } from "playwright";

const root = resolve(process.argv[2] ?? "dist-newstyle/wasm/public/all");
const inner = (process.env.HALOGEN_BASE_PATH ?? "/").replace(/^\/+|\/+$/g, "");
const base = inner ? `/${inner}/` : "/";

// instantiateStreaming refuses a .wasm served without its media type, so the
// server has to get these right.
const types = {
  ".html": "text/html",
  ".js": "text/javascript",
  ".css": "text/css",
  ".wasm": "application/wasm",
};

// A static server, as Pages is: a directory's address serves its index.html,
// and there is nothing outside the base.
const server = createServer(async (request, response) => {
  const path = decodeURIComponent(new URL(request.url, "http://x").pathname);
  if (!path.startsWith(base)) {
    response.writeHead(404).end();
    return;
  }
  const relative = path.slice(base.length);
  const file = join(root, normalize(relative === "" || relative.endsWith("/") ? `${relative}index.html` : relative));
  try {
    const body = await readFile(file);
    response.writeHead(200, { "content-type": types[extname(file)] ?? "application/octet-stream" });
    response.end(body);
  } catch {
    response.writeHead(404).end();
  }
});
await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
const url = `http://127.0.0.1:${server.address().port}${base}`;

// What each route shows, by its path under the base, and a selector that
// finds it only there.
const routes = {
  "": { name: "landing", marker: "main :text-is('haskell-halogen examples')" },
  "vanilla/": { name: "Vanilla", marker: "main :text-is('Test sentinel element')" },
  "hooks/": { name: "Hooks", marker: "main :text-is('Halogen Hooks')" },
  "pixi/": { name: "Pixi", marker: "main canvas" },
  "material/": { name: "Material", marker: "main .mdc-tab-bar" },
  "th/": { name: "Quotes", marker: "main :text-is('A list, quoted')" },
};

const browser = await chromium.launch();
const errors = [];
let failed = false;

try {
  const page = await browser.newPage();
  // Hold web fonts back, so text is always drawn before its face arrives, as
  // it is on a slow connection. How that race goes otherwise depends on the
  // network, and a test that only sometimes loses it proves nothing.
  await page.route(/\.woff2$/, async (route) => {
    await new Promise((later) => setTimeout(later, 1500));
    await route.continue();
  });
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
  });

  // The route is on screen, and no other route's marker is: the example that
  // was there before has been taken down, not just covered.
  const expectRoute = async (path, step) => {
    assert.equal(await page.evaluate(() => location.pathname + location.hash), base + path, `${step}: location`);
    await page.locator(routes[path].marker).first().waitFor({ timeout: 15000 });
    for (const [other, { marker }] of Object.entries(routes)) {
      if (other !== path) {
        assert.equal(await page.locator(marker).count(), 0, `${step}: ${routes[other].name} still mounted`);
      }
    }
    console.log(`ok  ${step} -> ${routes[path].name}`);
  };

  // Every route has a page of its own, with the shell already rendered for
  // that route: the page's base, its link in the navigation marked as the
  // current one, and on the landing page the landing content itself.
  for (const path of Object.keys(routes)) {
    const response = await fetch(url + path);
    assert.equal(response.status, 200, `${base}${path}: status`);
    const html = await response.text();
    assert.ok(html.includes(`<base href="${base}">`), `${base}${path}: the base path`);
    assert.match(html, new RegExp(`<a href="${base}${path}" style="[^"]*font-weight:bold`), `${base}${path}: its own link, current`);
    if (path === "") assert.ok(html.includes("<h1>haskell-halogen examples</h1>"), "the landing page's content");
  }
  console.log("ok  every route's page is prerendered for it");

  // And it shows before the program does: hold app.wasm back, and the
  // landing page is on screen all the same.
  let release;
  const held = new Promise((resolve) => (release = resolve));
  await page.route(/app\.wasm$/, async (route) => {
    await held;
    await route.continue();
  });
  await page.goto(url, { waitUntil: "domcontentloaded" });
  await page.locator(routes[""].marker).waitFor({ timeout: 5000 });
  assert.ok(await page.locator("nav a", { hasText: "Pixi" }).isVisible(), "the navigation before app.wasm");
  console.log("ok  the landing page shows before app.wasm has loaded");
  // Then the program renders the page afresh in its place.
  await page.evaluate(() => document.querySelector("nav").setAttribute("data-prerendered", ""));
  release();
  await page.locator("nav[data-prerendered]").waitFor({ state: "detached", timeout: 15000 });
  await page.unroute(/app\.wasm$/);
  await expectRoute("", "load");

  // Pixi sizes a text's texture from font metrics it caches per font string.
  // Measured while an asset font was still loading, they are the fallback's,
  // and the real glyphs come out with their tops cut off, which no selector can
  // see. So compare what Pixi has cached with a fresh measurement, through the
  // very module the page imported, until the font's load has been handled.
  const expectFreshFontMetrics = async () => {
    const compare = () =>
      page.evaluate(async () => {
        const url = performance
          .getEntriesByType("resource")
          .map((entry) => entry.name)
          .find((name) => /pixi.*\.mjs$/.test(name));
        // The canvas is on the page before PixiJS has been fetched, and until
        // the face itself has loaded a fresh measurement is of the fallback
        // too, so there is nothing to compare yet.
        const loaded = [...document.fonts].some((face) => face.family.includes("Press Start 2P") && face.status === "loaded");
        if (!url || !loaded) return {};
        const { CanvasTextMetrics } = await import(url);
        const font = Object.keys(CanvasTextMetrics._fonts).find((key) => key.includes("Press Start 2P"));
        if (!font) return { font };
        const cached = CanvasTextMetrics._fonts[font];
        CanvasTextMetrics.clearMetrics(font);
        const fresh = CanvasTextMetrics.measureFont(font);
        CanvasTextMetrics._fonts[font] = cached;
        return { font, cached, fresh };
      });
    let last;
    for (const deadline = Date.now() + 10000; Date.now() < deadline; await page.waitForTimeout(250)) {
      last = await compare();
      if (last.font && JSON.stringify(last.cached) === JSON.stringify(last.fresh)) {
        console.log("ok  Pixi measured the title's font after it loaded");
        return;
      }
    }
    assert.fail(`Pixi kept stale metrics for the title's font: ${JSON.stringify(last)}`);
  };

  const visited = ["vanilla/", "hooks/", "pixi/", "material/", "th/"];
  for (const path of visited) {
    await page.locator("nav a", { hasText: routes[path].name }).click();
    await expectRoute(path, `click ${routes[path].name}`);
    if (path === "pixi/") await expectFreshFontMetrics();
  }

  for (const path of [...visited].reverse().slice(1).concat("")) {
    await page.goBack();
    await expectRoute(path, "back");
  }

  await page.goForward();
  await expectRoute("vanilla/", "forward");

  // An example keeps working once mounted inside the gallery.
  const plus = page.locator("main button", { hasText: /^\+$/ });
  await plus.click();
  await plus.click();
  await page.locator("main :text-is('2')").waitFor({ timeout: 5000 });
  console.log("ok  vanilla counter counts");

  // The quoted list adds what is typed: the hh quote's handlers and the
  // {% for %} over the items work once mounted.
  await page.goto(`${url}th/`);
  await expectRoute("th/", "goto the quoted list");
  await page.locator("main input.draft").fill("Quote some CSS");
  await page.locator("main button", { hasText: /^Add$/ }).click();
  await page.locator("main li.item", { hasText: "Quote some CSS" }).waitFor({ timeout: 5000 });
  assert.equal(await page.locator("main li.item").count(), 3, "three items after adding one");
  console.log("ok  quoted list adds an item");

  // Each route is a page of its own now, so going to one loads it: let the
  // program mount it before reloading, or the reload cuts its app.wasm off.
  await page.goto(`${url}material/`);
  await expectRoute("material/", "goto a deep link");
  await page.reload();
  await expectRoute("material/", "reload on a deep link");

  // A link from when the gallery routed by fragment lands on the path, once
  // the program has started and redirected it.
  await page.goto(`${url}#/pixi`);
  await page.waitForURL(`${url}pixi/`, { timeout: 15000 });
  await expectRoute("pixi/", "a fragment link from before");

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
