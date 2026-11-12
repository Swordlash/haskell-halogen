// The node side of haskell-halogen-playwright: Playwright's Chromium,
// driven by the Haskell program that started this file, over a protocol
// of JSON lines on stdin and stdout.
//
//   node driver.mjs '<options as JSON>'
//
// Haskell to node, a call:
//   {"id": 7, "method": "click", "locator": {"page": 2, "steps": [...]}, ...}
// node to Haskell, its reply, one per call, in any order:
//   {"id": 7, "result": ...}
//   {"id": 7, "error": {"name": "TimeoutError", "message": "..."}}
// node to Haskell, something that happened, unasked:
//   {"event": "ready"}                       Chromium is up; calls may come
//   {"event": "fatal", "message": "..."}     nothing will work; node exits
//   {"event": "console", "page": 2, "type": "error", "text": "..."}
//   {"event": "pageerror", "page": 2, "message": "..."}
//   {"event": "closed", "page": 2}
// Messages are told apart by their keys, so a later kind (a request node
// makes for Haskell to answer, say, as a routed request's handler would)
// can be added beside these without changing them.
//
// Nothing else writes to stdout: console.log is sent to stderr, which the
// Haskell side relays. When stdin ends, the browser is closed and node
// exits.
//
// Contexts and pages are numbered handles. A locator is not kept here: it
// arrives with each call as the page and a list of steps, and is rebuilt.
import { existsSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { createInterface } from "node:readline";

const options = JSON.parse(process.argv[2] ?? "{}");
const timeout = options.timeout ?? 5000;

const send = (message) => process.stdout.write(JSON.stringify(message) + "\n");
console.log = (...values) => process.stderr.write(values.join(" ") + "\n");
console.info = console.log;

const fatal = (message) => {
  send({ event: "fatal", message });
  process.exit(1);
};

// The directory holding node_modules/<name>, from the working directory
// upwards, else from NODE_PATH, as hspec-halogen's runner looks for it: the
// package belongs to the project under test, not to this file.
const packageDir = (name) => {
  const candidates = [];
  for (let dir = process.cwd(); ; dir = dirname(dir)) {
    candidates.push(join(dir, "node_modules", name));
    if (dirname(dir) === dir) break;
  }
  for (const dir of (process.env.NODE_PATH ?? "").split(":").filter(Boolean)) candidates.push(join(dir, name));
  const found = candidates.find((dir) => existsSync(join(dir, "package.json")));
  if (!found) {
    fatal(
      `cannot find the npm package ${name} from ${process.cwd()}.\n` +
        `Install it in the project: npm install --save-dev ${name}\n` +
        "and a browser for it: npx playwright install chromium",
    );
  }
  return found;
};

const { chromium } = createRequire(join(packageDir("playwright"), "package.json"))("playwright");

let browser;
try {
  // The signals are this file's to handle (below), so that each closes the
  // browser the same way.
  browser = await chromium.launch({
    headless: options.headless ?? true,
    slowMo: options.slowMo ?? 0,
    handleSIGINT: false,
    handleSIGTERM: false,
    handleSIGHUP: false,
  });
} catch (error) {
  fatal(
    `Chromium did not start: ${error.message}\n` +
      "If it is not installed for this version of Playwright: npx playwright install chromium",
  );
}

const contexts = new Map();
const pages = new Map();
let nextHandle = 1;

const contextOf = (id) => {
  const context = contexts.get(id);
  if (!context) throw Object.assign(new Error(`no context ${id}: it was closed`), { name: "ClosedError" });
  return context;
};
const pageOf = (id) => {
  const page = pages.get(id);
  if (!page) throw Object.assign(new Error(`no page ${id}: it was closed`), { name: "ClosedError" });
  return page;
};

// Every page of every context gets the probe before any of its scripts
// runs, so that a Halogen program in it answers (Halogen.Activity).
const probeScript = () => {
  if (globalThis.__halogenProbe === undefined) globalThis.__halogenProbe = {};
};

const locate = ({ page, steps }) =>
  steps.reduce((at, step) => {
    if ("css" in step) return at.locator(step.css);
    if ("nth" in step) return at.nth(step.nth);
    if ("hasText" in step) return at.filter({ hasText: step.hasText });
    if ("role" in step) return at.getByRole(step.role, step.name == null ? {} : { name: step.name, exact: true });
    if ("text" in step) return at.getByText(step.text);
    if ("label" in step) return at.getByLabel(step.label);
    throw new Error(`unknown locator step ${JSON.stringify(step)}`);
  }, pageOf(page));

// A task of the page's at background priority, then, if a program in this
// document answers the probe, its answer until it is idle, a background
// task between asks: a browser event reaches a wasm program as a task of
// its own, so its handler may not have started when the action returns,
// and the program says when it has finished. No clock is involved except
// the deadline.
const inPageSettle = async ({ deadline, quiet, pause }) => {
  const task = () =>
    new Promise((resolve) =>
      globalThis.scheduler?.postTask ? scheduler.postTask(resolve, { priority: "background" }) : setTimeout(resolve, 0),
    );
  const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
  await task();
  const probe = globalThis.__halogenProbe;
  if (typeof probe?.activity !== "function") return { program: false };
  for (;;) {
    const a = await probe.activity();
    if (a.running === 0 && (!quiet || a.waiting === 0)) return { program: true, activity: a };
    if (Date.now() > deadline) return { program: true, activity: a, timedOut: true };
    await (pause > 0 ? sleep(pause) : task());
  }
};

// Whether an error says the document went away under a call: a
// navigation the action started, which leaves nothing to settle.
const navigatedAway = (error) =>
  /Execution context was destroyed|most likely because of a navigation|Cannot find context with specified id/.test(
    String(error?.message ?? ""),
  );

const timeoutError = (message) => Object.assign(new Error(message), { name: "TimeoutError" });

// A question put to the page, with a deadline kept here: a program that
// has crashed leaves the probe's promise pending for ever, and a page
// whose thread is held runs no timer of its own, so the page cannot be
// trusted to give up by itself. The question lost is left to end with its
// page; the race has taken its outcome, so it rejects nobody.
const answeredWithin = (ms, question, message) => {
  let timer;
  const late = new Promise((_, reject) => {
    timer = setTimeout(() => reject(timeoutError(message())), ms);
  });
  return Promise.race([question, late]).finally(() => clearTimeout(timer));
};
const silent = (page, ms) => () =>
  `the page ${page.url()} did not answer within ${ms}ms: its thread is held, or its program stopped and left the probe's question open`;

// The pages whose context has JavaScript disabled: no program runs there,
// and no task or timer of the page's would ever fire, so they settle at
// once.
const scriptless = new WeakSet();

const settle = async (page, { quiet = false, timeout: ms = timeout } = {}) => {
  if (scriptless.has(page)) return null;
  const started = Date.now();
  let outcome;
  try {
    // The page gives up at the deadline by itself when it can; a second
    // later it is taken not to be answering at all.
    outcome = await answeredWithin(
      ms + 1000,
      page.evaluate(inPageSettle, { deadline: started + ms, quiet, pause: quiet ? 5 : 0 }),
      silent(page, ms),
    );
  } catch (error) {
    if (navigatedAway(error)) return null;
    throw error;
  }
  if (outcome.timedOut) {
    const a = outcome.activity;
    throw timeoutError(
      quiet
        ? `the page's program was not quiet after ${ms}ms: ${a.running} tree(s) running, ${a.waiting} fiber(s) still waiting`
        : `the page's program was still running after ${ms}ms: ${a.running} tree(s) running`,
    );
  }
  return outcome.activity ?? null;
};

const waitStarted = async (page, ms = timeout) => {
  const deadline = Date.now() + ms;
  let last = null;
  for (;;) {
    try {
      last = await answeredWithin(
        Math.max(0, deadline - Date.now()) + 1000,
        page.evaluate(async () => {
          const probe = globalThis.__halogenProbe;
          return typeof probe?.activity === "function" ? await probe.activity() : null;
        }),
        silent(page, ms),
      );
    } catch (error) {
      if (!navigatedAway(error)) throw error;
      last = null;
    }
    if (last && last.trees >= 1 && last.running === 0) return last;
    if (Date.now() > deadline) {
      throw timeoutError(
        last === null
          ? `no Halogen program in ${page.url()} answered the probe within ${ms}ms (is it built with a haskell-halogen-core that has Halogen.Activity, and did it mount a tree?)`
          : `the Halogen program in ${page.url()} had not started within ${ms}ms: ${last.trees} tree(s), ${last.running} running`,
      );
    }
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
};

const activity = async (page) => {
  try {
    return await answeredWithin(
      timeout,
      page.evaluate(async () => {
        const probe = globalThis.__halogenProbe;
        return typeof probe?.activity === "function" ? await probe.activity() : null;
      }),
      silent(page, timeout),
    );
  } catch (error) {
    if (navigatedAway(error)) return null;
    throw error;
  }
};

const watch = (id, page) => {
  page.on("console", (message) => send({ event: "console", page: id, type: message.type(), text: message.text() }));
  page.on("pageerror", (error) => send({ event: "pageerror", page: id, message: String(error.stack ?? error.message) }));
  page.on("close", () => {
    pages.delete(id);
    send({ event: "closed", page: id });
  });
};

// An action on a locator, then settle.
const acting = (act) => async (m) => {
  const target = locate(m.locator);
  await act(target, m);
  await settle(pageOf(m.locator.page));
  return null;
};
// An action on a page, then settle.
const pageActing = (act) => async (m) => {
  const page = pageOf(m.page);
  const result = await act(page, m);
  await settle(page);
  return result ?? null;
};

const methods = {
  async newContext(m) {
    const o = m.options ?? {};
    const context = await browser.newContext({
      baseURL: o.baseURL ?? undefined,
      viewport: o.viewport ?? undefined,
      colorScheme: o.colorScheme ?? undefined,
      javaScriptEnabled: o.javaScriptEnabled ?? true,
      offline: o.offline ?? false,
      serviceWorkers: o.serviceWorkers ?? "allow",
    });
    context.setDefaultTimeout(timeout);
    context.setDefaultNavigationTimeout(timeout);
    await context.addInitScript(probeScript);
    context.halogenScriptless = o.javaScriptEnabled === false;
    const id = nextHandle++;
    contexts.set(id, context);
    return id;
  },
  async closeContext(m) {
    const context = contexts.get(m.context);
    contexts.delete(m.context);
    if (context) await context.close();
    return null;
  },
  async newPage(m) {
    const context = contextOf(m.context);
    const page = await context.newPage();
    if (context.halogenScriptless) scriptless.add(page);
    const id = nextHandle++;
    pages.set(id, page);
    watch(id, page);
    return id;
  },
  async closePage(m) {
    const page = pages.get(m.page);
    if (page) await page.close();
    pages.delete(m.page);
    return null;
  },

  goto: pageActing((page, m) => page.goto(m.url, { waitUntil: "load" }).then(() => null)),
  reload: pageActing((page) => page.reload({ waitUntil: "load" }).then(() => null)),
  goBack: pageActing((page) => page.goBack({ waitUntil: "load" }).then(() => null)),
  goForward: pageActing((page) => page.goForward({ waitUntil: "load" }).then(() => null)),
  async url(m) {
    return pageOf(m.page).url();
  },
  async title(m) {
    return await pageOf(m.page).title();
  },
  async content(m) {
    return await pageOf(m.page).content();
  },
  async setViewport(m) {
    await pageOf(m.page).setViewportSize({ width: m.width, height: m.height });
    return null;
  },
  press: pageActing((page, m) => page.keyboard.press(m.key)),

  async settle(m) {
    await settle(pageOf(m.page));
    return null;
  },
  async waitQuiet(m) {
    await settle(pageOf(m.page), { quiet: true });
    return null;
  },
  async waitStarted(m) {
    await waitStarted(pageOf(m.page));
    return null;
  },
  async activity(m) {
    return await activity(pageOf(m.page));
  },

  click: acting((target) => target.click()),
  doubleClick: acting((target) => target.dblclick()),
  hover: acting((target) => target.hover()),
  fill: acting((target, m) => target.fill(m.text)),
  typeText: acting((target, m) => target.pressSequentially(m.text)),
  clear: acting((target) => target.clear()),
  pressOn: acting((target, m) => target.press(m.key)),
  check: acting((target) => target.check()),
  uncheck: acting((target) => target.uncheck()),
  selectOption: acting((target, m) => target.selectOption(m.values)),
  focus: acting((target) => target.focus()),
  blur: acting((target) => target.blur()),
  dragTo: acting((target, m) => target.dragTo(locate(m.target))),

  async textContent(m) {
    return (await locate(m.locator).textContent()) ?? "";
  },
  async innerText(m) {
    return await locate(m.locator).innerText();
  },
  async inputValue(m) {
    return await locate(m.locator).inputValue();
  },
  async getAttribute(m) {
    return await locate(m.locator).getAttribute(m.name);
  },
  async isVisible(m) {
    return await locate(m.locator).isVisible();
  },
  async isChecked(m) {
    return await locate(m.locator).isChecked();
  },
  async isEnabled(m) {
    return await locate(m.locator).isEnabled();
  },
  async count(m) {
    return await locate(m.locator).count();
  },
  async outerHTML(m) {
    return await locate(m.locator).evaluate((element) => element.outerHTML);
  },
  async waitFor(m) {
    await locate(m.locator).waitFor({ state: m.state });
    return null;
  },

  async evaluate(m) {
    const result = await pageOf(m.page).evaluate(m.expression);
    return result === undefined ? null : result;
  },
  async evaluateWith(m) {
    // Made a function here, and sent to the page as its source, which is
    // how Playwright sends any function.
    const fn = new Function(`return (${m.function});`)();
    const result = await pageOf(m.page).evaluate(fn, m.argument);
    return result === undefined ? null : result;
  },
  async screenshot(m) {
    await pageOf(m.page).screenshot({ path: m.path, fullPage: true });
    return null;
  },
};

const handle = async (message) => {
  const method = methods[message.method];
  try {
    if (!method) throw new Error(`unknown method ${message.method}`);
    const result = await method(message);
    send({ id: message.id, result: result === undefined ? null : result });
  } catch (error) {
    send({ id: message.id, error: { name: error?.name ?? "Error", message: String(error?.message ?? error) } });
  }
};

// Closing the browser and leaving: when stdin ends (the Haskell side is
// done, or it died, which closes the pipe however it died) and on SIGTERM,
// SIGINT or SIGHUP. Chromium also leaves when node does (its pipe to node
// closes), but closing it first lets it go cleanly; and if it takes too
// long, node leaves anyway.
let closing = false;
const close = async () => {
  if (closing) return;
  closing = true;
  setTimeout(() => process.exit(0), 3000).unref();
  try {
    await browser.close();
  } finally {
    process.exit(0);
  }
};
for (const signal of ["SIGTERM", "SIGINT", "SIGHUP"]) process.on(signal, close);

const input = createInterface({ input: process.stdin });
input.on("line", (line) => {
  if (line.trim() === "") return;
  let message;
  try {
    message = JSON.parse(line);
  } catch (error) {
    process.stderr.write(`haskell-halogen-playwright: not a message: ${line}\n`);
    return;
  }
  if ("id" in message) handle(message);
  else process.stderr.write(`haskell-halogen-playwright: not a call: ${line}\n`);
});
input.on("close", close);
// Chromium closing by itself (it crashed, say) ends the session too.
browser.on("disconnected", () => {
  if (!closing) fatal("Chromium went away");
});

send({ event: "ready" });
