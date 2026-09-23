// Drive the deployed examples gallery (examples/all) in headless Chromium.
//
//   npm run test-gallery                      # after: npm run build-wasm -- all
//   node toolchain/test-gallery.mjs [dir]     # default dist-newstyle/wasm/public/all
//
// This is what the unit tests cannot see: the size-optimised app.wasm loading
// in a real browser, each example mounting and unmounting as the route changes,
// and the fragment routing surviving back, forward and a reload. Needs a
// Chromium that Playwright can launch (`npx playwright install chromium`), and
// the network, since the Pixi example loads PixiJS from a CDN.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { createServer } from "node:http";
import { extname, join, normalize, resolve } from "node:path";
import { chromium } from "playwright";

const root = resolve(process.argv[2] ?? "dist-newstyle/wasm/public/all");

// instantiateStreaming refuses a .wasm served without its media type, so the
// server has to get these right.
const types = {
  ".html": "text/html",
  ".js": "text/javascript",
  ".css": "text/css",
  ".wasm": "application/wasm",
};

const server = createServer(async (request, response) => {
  const path = decodeURIComponent(new URL(request.url, "http://x").pathname);
  const file = join(root, normalize(path.endsWith("/") ? `${path}index.html` : path));
  try {
    const body = await readFile(file);
    response.writeHead(200, { "content-type": types[extname(file)] ?? "application/octet-stream" });
    response.end(body);
  } catch {
    response.writeHead(404).end();
  }
});
await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
const url = `http://127.0.0.1:${server.address().port}/`;

// What each route shows, and a selector that finds it only there.
const routes = {
  "": { name: "landing", marker: "main :text-is('haskell-halogen examples')" },
  "#/vanilla": { name: "Vanilla", marker: "main :text-is('Test sentinel element')" },
  "#/hooks": { name: "Hooks", marker: "main :text-is('Halogen Hooks')" },
  "#/pixi": { name: "Pixi", marker: "main canvas" },
  "#/material": { name: "Material", marker: "main .mdc-tab-bar" },
};

const browser = await chromium.launch();
const errors = [];
let failed = false;

try {
  const page = await browser.newPage();
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
  });

  // The route is on screen, and no other route's marker is: the example that
  // was there before has been taken down, not just covered.
  const expectRoute = async (hash, step) => {
    assert.equal(await page.evaluate(() => location.hash), hash, `${step}: location.hash`);
    await page.locator(routes[hash].marker).first().waitFor({ timeout: 15000 });
    for (const [other, { marker }] of Object.entries(routes)) {
      if (other !== hash) {
        assert.equal(await page.locator(marker).count(), 0, `${step}: ${routes[other].name} still mounted`);
      }
    }
    console.log(`ok  ${step} -> ${routes[hash].name}`);
  };

  await page.goto(url);
  await expectRoute("", "load");

  const visited = ["#/vanilla", "#/hooks", "#/pixi", "#/material"];
  for (const hash of visited) {
    await page.locator("nav a", { hasText: routes[hash].name }).click();
    await expectRoute(hash, `click ${routes[hash].name}`);
  }

  for (const hash of [...visited].reverse().slice(1).concat("")) {
    await page.goBack();
    await expectRoute(hash, "back");
  }

  await page.goForward();
  await expectRoute("#/vanilla", "forward");

  // An example keeps working once mounted inside the gallery.
  const plus = page.locator("main button", { hasText: /^\+$/ });
  await plus.click();
  await plus.click();
  await page.locator("main :text-is('2')").waitFor({ timeout: 5000 });
  console.log("ok  vanilla counter counts");

  await page.goto(`${url}#/material`);
  await page.reload();
  await expectRoute("#/material", "reload on a deep link");

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
