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
//   {"event": "console", "page": 2, "type": "error", "text": "...", "url": "..." | null, "line": 3 | null}
//   {"event": "websocket", "page": 2, "url": "ws://...", "sent": true, "payload": "...", "binary": false}
//   {"event": "pageerror", "page": 2, "message": "..."}
//   {"event": "dialog", "page": 2, "type": "beforeunload", "message": "..."}
//   {"event": "closed", "page": 2}
//   {"event": "request", "context": 1, "n": 9, "url": "...", "method": "GET", "resourceType": "script",
//    "page": 2 | null, "document": "<loader id>" | null, "documentUrl": "..." | null}
//   {"event": "requestDocument", "context": 1, "n": 9, "document": "...", "documentUrl": "..."}
//                                            request 9 was another document's
//                                            than the request event said (see
//                                            attribute)
//   {"event": "requestfailed", "context": 1, "url": "...", "method": "GET", "failure": "net::ERR_FAILED"}
//   {"event": "exchange", "context": 1, "url": "...", "method": "POST", "resourceType": "fetch",
//    "requestHeaders": [["cookie", "..."], ...], "postData": "<base64>" | null,
//    "status": 200 | null, "responseHeaders": [[name, value], ...],
//    "responseBody": "<base64>" | null, "failure": "net::..." | null,
//    "page": 2 | null, "document": "..." | null, "documentUrl": "..." | null}
//                                            a request of a context that records
//                                            its exchanges, once it has ended
//   {"event": "cdp", "page": 2, "name": "Network.requestWillBeSent", "params": {...}}
//                                            a DevTools event the test listens to
//   {"event": "unrouted", "routes": [3, 4]}  these routes (or exposed functions)
//                                            went with their page or context; no
//                                            ask of theirs is waiting for an
//                                            answer any more
// node to Haskell, a question, which Haskell answers once:
//   {"ask": 12, "route": 3, "request": {"url": "...", "method": "GET", "resourceType": "fetch",
//                                       "headers": {...}, "allHeaders": {...}, "postData": "<base64>" | null}}
//   {"ask": 13, "exposed": 5, "argument": <JSON>}   the page called a function the test exposed
// Haskell to node, its answer:
//   {"answer": 12, "action": "continue"}
//   {"answer": 12, "action": "fulfill", "status": 200, "headers": {...}, "contentType": "..." | null, "body": "<base64>"}
//   {"answer": 12, "action": "abort", "error": "connectionrefused"}
//   {"answer": 13, "result": <JSON>}                 the page's promise resolved with it
//   {"answer": 13, "error": "..."}                   the page's promise rejected with it
// While a route's ask waits for its answer, Haskell may call "fetchRouted"
// with the ask's number: the request is sent to the server, and the reply is
// the response (status, headers, body), which the handler then looks at or
// changes before it answers "fulfill" with it.
// Messages are told apart by their keys: "id" (a call, or its reply),
// "event", "ask" and "answer". Bytes cross as base64.
//
// A route's id (and an exposed function's) is chosen by Haskell, which
// registers its handler before it calls "route" ("expose"), so that no ask
// can arrive for a route Haskell does not know. A route may carry
// "resourceTypes" and "methods": a request of another kind is passed on here
// without asking Haskell. An ask waits here until its answer comes, or until
// its route goes (unrouted by Haskell, or with its page or context),
// whichever is first: then the request is let go on as if the route had not
// been there (an exposed function's call is rejected), and an answer that
// comes later is ignored. So neither side is ever left waiting for the
// other.
//
// Nothing else writes to stdout: console.log is sent to stderr, which the
// Haskell side relays. When stdin ends, the browser is closed and node
// exits.
//
// Contexts and pages are numbered handles. A locator is not kept here: it
// arrives with each call as the page and a list of steps, and is rebuilt.
// A call may carry "timeout", milliseconds that take the place of the
// options' for this call alone (the Haskell side's withTimeout): it is
// passed to Playwright's own call and to the waits below, so no state is
// kept for it here and calls with different timeouts can be in flight at
// once.
import { existsSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { createInterface } from "node:readline";

const options = JSON.parse(process.argv[2] ?? "{}");
const timeout = options.timeout ?? 5000;
// The timeout a call carries, else the options'.
const within = (m) => m.timeout ?? timeout;

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
    args: options.args ?? [],
    // Downloads are saved in the Haskell side's temporary directory, which
    // goes when the driver does.
    downloadsPath: options.downloadsPath ?? undefined,
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
// The downloads being waited for, by the number expectDownload gave; the
// popups, by the number expectPopup gave.
const downloads = new Map();
const popups = new Map();
// The events being waited for, by the number expectEvent gave: each the
// handle of its listener's entry in the page.
const events = new Map();
// The DevTools events listened to, by page.
const listening = new WeakMap();

// A context's exchanges, recorded when the context's options ask for it:
// each request once it has ended (finished, or failed), with every header
// it was sent with (cookies included, which requests leave out), its body,
// and the response: status, headers, and its body when it is at most the
// limit's bytes (none for a redirect). Reading the request's actual headers
// and the response's body is a round trip to the browser each, which is why
// a context only does this when asked.
const recordExchanges = (context, id, limit) => {
  const record = async (request, failure) => {
    const exchange = {
      event: "exchange",
      context: id,
      url: request.url(),
      method: request.method(),
      resourceType: request.resourceType(),
      requestHeaders: headerList(await request.headersArray().catch(() => [])),
      postData: request.postDataBuffer()?.toString("base64") ?? null,
      status: null,
      responseHeaders: [],
      responseBody: null,
      failure,
    };
    // The attribution as it is when the request has ended, its match with
    // the protocol's report long made.
    const a = attributions.get(request);
    exchange.page = a?.page ?? null;
    exchange.document = a?.document ?? null;
    exchange.documentUrl = a?.documentUrl ?? null;
    if (failure === null) {
      const response = await request.response().catch(() => null);
      if (response) {
        exchange.status = response.status();
        exchange.responseHeaders = headerList(await response.headersArray().catch(() => []));
        const body = await response.body().catch(() => null);
        if (body !== null && body.length <= limit) exchange.responseBody = body.toString("base64");
      }
    }
    send(exchange);
  };
  const recording = (request, failure) =>
    record(request, failure).catch((error) => process.stderr.write(`haskell-halogen-playwright: recording ${request.url()}: ${error?.message ?? error}\n`));
  context.on("requestfinished", (request) => recording(request, null));
  context.on("requestfailed", (request) => recording(request, request.failure()?.errorText ?? ""));
};

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

// Every page of every context, the ones the test opens and the ones a page
// opens (popups), gets a handle, is watched (its console, its errors), and
// is slowed down by the options' throttle, as soon as it is seen: the
// context's "page" event, or newPage's result, whichever comes first. A
// page's entry is {id, ready}: ready is the throttle applied.
const known = new WeakMap();
const register = (page) => {
  let entry = known.get(page);
  if (entry) return entry;
  const id = nextHandle++;
  pages.set(id, page);
  if (page.context().halogenScriptless) scriptless.add(page);
  watch(id, page);
  // The options' processor throttle, before the page loads anything (for a
  // popup, as soon as it is seen), and the documents its requests are
  // attributed to.
  const ready = Promise.all([
    options.throttle
      ? sessionOf(page).then((session) => session.send("Emulation.setCPUThrottlingRate", { rate: options.throttle }))
      : Promise.resolve(),
    trackDocuments(page),
  ]);
  ready.catch(() => {});
  entry = { id, ready };
  known.set(page, entry);
  return entry;
};

// Every page of every context gets the probe before any of its scripts
// runs, so that a Halogen program in it answers (Halogen.Activity). An init
// script is the DevTools protocol's (Page.addScriptToEvaluateOnNewDocument),
// which a page's Content Security Policy does not govern.
//
// The same script notes the moves through the history the page asks for
// (history.back(), forward(), go(n)), which the browser makes a moment
// later, out of the page's hands: settle waits for one under way, so that
// an action whose handler goes back has had its popstate (or hashchange)
// when it returns. A move counts as under way from the call until the
// document hears where it went (popstate, hashchange), leaves (pagehide),
// or half a second has passed (a move into a frame's entries, which this
// document never hears of). A move the Navigation API says has nowhere to
// go in this document's entries (go(0), which reloads; back from the
// first) is not counted. The methods keep their names and lengths, and
// call the page's own History's.
const probeScript = () => {
  if (globalThis.__halogenProbe === undefined) globalThis.__halogenProbe = {};
  const key = Symbol.for("halogen.playwright.traversals");
  if (typeof History !== "function" || typeof addEventListener !== "function" || globalThis[key]) return;
  // Each move under way is a token of its own, so that one's half second
  // ends that one and not a later move.
  const moves = { pending: new Set(), waiters: [] };
  Object.defineProperty(globalThis, key, { value: moves });
  const arrived = () => {
    moves.pending.clear();
    for (const resolve of moves.waiters.splice(0)) resolve();
  };
  for (const event of ["popstate", "hashchange", "pagehide"]) addEventListener(event, arrived, true);
  const somewhere = (delta) => {
    const navigation = globalThis.navigation;
    if (!delta) return false;
    if (typeof navigation?.entries !== "function" || !navigation.currentEntry) return true;
    const to = navigation.currentEntry.index + delta;
    return to >= 0 && to < navigation.entries().length;
  };
  const wrap = (name, deltaOf) => {
    const original = History.prototype[name];
    if (typeof original !== "function") return;
    const wrapped = {
      [name](...args) {
        if (somewhere(deltaOf(args))) {
          const move = {};
          moves.pending.add(move);
          setTimeout(() => {
            if (moves.pending.delete(move) && moves.pending.size === 0) arrived();
          }, 500);
        }
        return original.apply(this, args);
      },
    }[name];
    Object.defineProperty(wrapped, "length", { value: original.length });
    Object.defineProperty(History.prototype, name, { value: wrapped, writable: true, configurable: true, enumerable: true });
  };
  wrap("back", () => -1);
  wrap("forward", () => 1);
  wrap("go", (args) => Math.trunc(Number(args[0])) || 0);
};

// A locator's steps, from its page. Two steps go into a frame, after which
// the steps that follow are applied inside it: "frame" (the page's frame
// whose address matches a glob, which must be the first step) and
// "frameLocator" (the frame an iframe element shows).
const stepFrom = (at, step) => {
  if ("frame" in step) {
    const frame = at.frame?.({ url: step.frame });
    if (!frame) throw Object.assign(new Error(`no frame of the page's has an address matching ${step.frame}`), { name: "NoFrame" });
    return frame;
  }
  if ("frameLocator" in step) return at.frameLocator(step.frameLocator);
  if ("css" in step) return at.locator(step.css);
  if ("nth" in step) return at.nth(step.nth);
  if ("hasText" in step) return at.filter({ hasText: step.hasText });
  // The whole text, exactly: Playwright's text matching against a regular
  // expression made of the text, escaped, from start to end.
  if ("hasTextExact" in step) return at.filter({ hasText: new RegExp(`^${escapeRegExp(step.hasTextExact)}$`) });
  if ("role" in step) return at.getByRole(step.role, step.name == null ? {} : { name: step.name, exact: true });
  if ("text" in step) return at.getByText(step.text);
  if ("label" in step) return at.getByLabel(step.label);
  throw new Error(`unknown locator step ${JSON.stringify(step)}`);
};

const locate = ({ page, steps }) => steps.reduce(stepFrom, pageOf(page));

const escapeRegExp = (text) => text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

// In the page, in this order:
//
// 1. A task of the page's at background priority: a browser event reaches
//    a wasm program as a task of its own, so its handler may not have
//    started when the action returns.
// 2. If a program in this document answers the probe, its answer until it
//    is idle, a background task between asks: the program says when it
//    has finished.
// 3. A move through the history the page asked for and the browser has
//    not made yet (probeScript counts them), waited for until the document
//    hears of it: a handler that called history.back() is idle at once,
//    and its popstate comes a moment later.
// 4. An animation frame, then a background task. What the browser
//    dispatches with the next frame (a dialog's close, after Escape or
//    close(); resize; a ResizeObserver's callback) is dispatched at the
//    start of the frame, before its requestAnimationFrame callbacks, so
//    after this frame it has been; the task after it is where a wasm
//    program's handler of that event has started. The frame comes after
//    the program is idle, not before: a program's handler that closes a
//    dialog runs a task after the click, and its close comes with the
//    frame after that. A document that is hidden draws no frames and is
//    not waited for; nor, from the second time on, is one whose last frame
//    did not come within 100 ms (a page that draws none, such as one under
//    --enable-begin-frame-control): the wait is cut short there, and a
//    frame that comes later says frames are drawn again.
// 5. The program asked again. Busy (the event's handler started work), it
//    is waited for and 3 to 5 are done again, since its work may have
//    caused another late event; idle, the page has settled.
//
// No clock is involved except the deadline and the frame's bound.
const inPageSettle = async ({ deadline, quiet, pause, frameBound }) => {
  const task = () =>
    new Promise((resolve) =>
      globalThis.scheduler?.postTask ? scheduler.postTask(resolve, { priority: "background" }) : setTimeout(resolve, 0),
    );
  const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
  const frames = (globalThis[Symbol.for("halogen.playwright.frames")] ??= { missing: false });
  const frame = () =>
    new Promise((resolve) => {
      if (typeof requestAnimationFrame !== "function" || document.visibilityState === "hidden") return resolve();
      let done = false;
      requestAnimationFrame(() => {
        frames.missing = false;
        if (!done) {
          done = true;
          resolve();
        }
      });
      if (frames.missing) {
        done = true;
        return resolve();
      }
      setTimeout(() => {
        if (!done) {
          done = true;
          frames.missing = true;
          resolve();
        }
      }, frameBound);
    });
  const moves = globalThis[Symbol.for("halogen.playwright.traversals")];
  const moving = () => moves?.pending.size > 0;
  const moved = () => new Promise((resolve) => moves.waiters.push(resolve));
  // A program gives the probe its answer when its first tree mounts, which
  // may be during any of the waits here (a page whose program starts in
  // the task this settle begins with): it is looked for each time.
  const probe = globalThis.__halogenProbe;
  const program = () => typeof probe?.activity === "function";
  const idle = async () => {
    for (;;) {
      const a = await probe.activity();
      if (a.running === 0 && (!quiet || a.waiting === 0)) return { activity: a };
      if (Date.now() > deadline) return { activity: a, timedOut: true };
      await (pause > 0 ? sleep(pause) : task());
    }
  };
  await task();
  let last = null;
  for (;;) {
    if (program()) {
      const reading = await idle();
      if (reading.timedOut) return { program: true, ...reading };
      last = reading.activity;
    }
    if (moving()) await moved();
    await frame();
    await task();
    if (!program()) {
      if (!moving()) return { program: false };
      continue;
    }
    const a = await probe.activity();
    if (a.running === 0 && (!quiet || a.waiting === 0) && !moving()) return { program: true, activity: a };
    if (Date.now() > deadline) return { program: true, activity: a ?? last, timedOut: true };
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
const unsettled = (page, ms) => () => `the JavaScript evaluated in ${page.url()} did not settle within ${ms}ms`;
const silent = (page, ms) => () =>
  `the page ${page.url()} did not answer within ${ms}ms: its thread is held, or its program stopped and left the probe's question open`;

// The pages whose context has JavaScript disabled: no program runs there,
// and no task or timer of the page's would ever fire, so they settle at
// once.
const scriptless = new WeakSet();

// The page, or one of its frames: each document has a probe and a program
// of its own, so an action in a frame settles the frame's (frameOf) and
// then the page's.
const settle = async (target, { quiet = false, timeout: ms = timeout } = {}) => {
  const page = typeof target.page === "function" ? target.page() : target;
  if (scriptless.has(page)) return null;
  const started = Date.now();
  let outcome;
  try {
    // The page gives up at the deadline by itself when it can; a second
    // later it is taken not to be answering at all.
    outcome = await answeredWithin(
      ms + 1000,
      target.evaluate(inPageSettle, { deadline: started + ms, quiet, pause: quiet ? 5 : 0, frameBound: 100 }),
      silent(target, ms),
    );
  } catch (error) {
    // The document went, or the page itself did (a sign-in window that
    // closes itself as the click that signs in is handled), or the frame
    // was taken out of its page.
    if (navigatedAway(error) || page.isClosed() || target.isDetached?.()) return null;
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

const activity = async (page, ms = timeout) => {
  try {
    return await answeredWithin(
      ms,
      page.evaluate(async () => {
        const probe = globalThis.__halogenProbe;
        return typeof probe?.activity === "function" ? await probe.activity() : null;
      }),
      silent(page, ms),
    );
  } catch (error) {
    if (navigatedAway(error)) return null;
    throw error;
  }
};

// A predicate polled in the page at its animation frames until it is
// truthy, up to the call's timeout. Playwright's own waitForFunction
// compiles the predicate in the page with eval, which a Content Security
// Policy without 'unsafe-eval' refuses (an uncaught EvalError, and a wait
// that never ends); here the loop and the predicate are one function
// compiled in node and sent as Playwright sends any function, through the
// DevTools protocol, which the page's policy does not govern. A navigation
// under the wait starts it again in the new document, as Playwright's does;
// what the predicate throws is the call's error.
const pollInPage = async (page, source, argument, ms, what) => {
  const deadline = Date.now() + ms;
  const poll = functionOf(
    `async ({ argument, deadline }) => {
      const predicate = (${source}
);
      const frame = () => new Promise((resolve) => requestAnimationFrame(resolve));
      for (;;) {
        // An asynchronous predicate is waited for, as Playwright's wait
        // does: a promise is truthy whatever it will say. What it says
        // after the deadline comes too late to count.
        const held = await predicate(argument);
        if (Date.now() > deadline) return false;
        if (held) return true;
        await frame();
      }
    }`,
  );
  for (;;) {
    let done;
    try {
      done = await answeredWithin(Math.max(0, deadline - Date.now()) + 1000, page.evaluate(poll, { argument, deadline }), silent(page, ms));
    } catch (error) {
      if (!navigatedAway(error)) throw error;
      done = false;
    }
    if (done) return null;
    if (Date.now() > deadline) throw timeoutError(`${what} was not truthy in ${page.url()} within ${ms}ms`);
  }
};

// Going back or forward in the page's history, by one entry: the page's
// own, or one of a frame's navigations, which Back undoes in that frame
// alone. Playwright's goBack waits for the main frame to navigate, and so
// never returns when the entry is a frame's; here the history is the
// DevTools protocol's (the joint history, frames' entries included, as
// Playwright's own goBack reads it), and the call waits for whichever frame
// navigates: for its load, when it loads a document, or for nothing more
// when the traversal stays in its document (pushState, a fragment), where
// the page has had its popstate or hashchange by then. Then it settles.
// No entry to go to is an error (NoHistoryEntry), not a call that does
// nothing: a test that goes back expects to go somewhere.
const traverse = (delta) => async (m) => {
  const page = pageOf(m.page);
  const ms = within(m);
  const session = await sessionOf(page);
  const { currentIndex, entries } = await session.send("Page.getNavigationHistory");
  const entry = entries[currentIndex + delta];
  const way = delta < 0 ? "back" : "forward";
  if (!entry) {
    throw Object.assign(new Error(`there is no entry to go ${way} to in the history of ${page.url()}`), { name: "NoHistoryEntry" });
  }
  let listener;
  // Armed before the traversal, so that its navigation cannot be missed.
  // The load state is asked as the navigation is reported, when Playwright
  // has forgotten the old document's load and not yet seen the new one's.
  const navigated = new Promise((resolve) => {
    listener = (frame) => resolve(frame.waitForLoadState("load", { timeout: ms }));
    page.on("framenavigated", listener);
  });
  try {
    await session.send("Page.navigateToHistoryEntry", { entryId: entry.id });
    await answeredWithin(ms, navigated, () => `going ${way} in ${page.url()}: no frame navigated within ${ms}ms`);
  } finally {
    page.off("framenavigated", listener);
  }
  await settle(page, { timeout: ms });
  return null;
};

const watch = (id, page) => {
  // With where it was written: the frame's document or script, which tells a
  // frame's message from the page's. Playwright gives an empty address when
  // it does not know, and a line from 0.
  page.on("console", (message) => {
    const at = message.location();
    send({
      event: "console",
      page: id,
      type: message.type(),
      text: message.text(),
      url: at?.url ? at.url : null,
      line: at?.url ? at.lineNumber + 1 : null,
    });
  });
  // Every frame of every WebSocket the page opens, both ways, as it crosses:
  // kept on the Haskell side as the console is. A binary frame's payload is
  // sent as base64.
  page.on("websocket", (socket) => {
    const frame = (sent) => (data) => {
      const binary = typeof data.payload !== "string";
      send({ event: "websocket", page: id, url: socket.url(), sent, binary, payload: binary ? data.payload.toString("base64") : data.payload });
    };
    socket.on("framesent", frame(true));
    socket.on("framereceived", frame(false));
  });
  page.on("pageerror", (error) => send({ event: "pageerror", page: id, message: String(error.stack ?? error.message) }));
  // Every dialog is answered at once, as the context's options say (so that
  // a page never waits on one), and reported. Playwright would dismiss it
  // by itself with no listener; this listener takes that over.
  page.on("dialog", (dialog) => {
    send({ event: "dialog", page: id, type: dialog.type(), message: dialog.message() });
    (page.context().halogenAcceptDialogs ? dialog.accept() : dialog.dismiss()).catch(() => {});
  });
  page.on("close", () => {
    pages.delete(id);
    dropRoutes((entry) => entry.page === id);
    send({ event: "closed", page: id });
  });
};

// One DevTools session per page, made on first use. The promise is kept, so
// that two calls at once make one session.
const sessions = new WeakMap();
const sessionOf = (page) => {
  let session = sessions.get(page);
  if (!session) {
    session = page.context().newCDPSession(page);
    sessions.set(page, session);
  }
  return session;
};

// Which document asked for a request. Playwright's request says which
// frame, and for a dedicated worker's the frame that started it, but not
// which of the frame's documents: frame.url() is the frame's address when
// the event is heard, which for a request reported late (Playwright reports
// a routed request once its route has it, up to a few hundred milliseconds
// after the browser sent it) is the next document's. The DevTools protocol
// says exactly: Network.requestWillBeSent carries the loader of the
// document that asked (loaderId, documentURL), and for a navigation request
// the loader of the document it loads. Its id is the document's identity: a
// new one for every document a frame loads, the same for a state pushed or
// a fragment. So each page has a session of the driver's with the Network
// domain on, and each request Playwright reports is matched with the
// protocol's report of it on the page's session: the same address and
// method, first come first matched, whichever of the two arrives first
// (both orders happen: the protocol's came first for 84% of a thousand
// requests measured, 69% with a route on the context).
//
// Until its match arrives, a request is the document's its frame has as
// the driver knows it (the page's main frame: the protocol's frameNavigated;
// another frame: the one tracked with its address), and the match amends it
// ("requestDocument"). A request the page's session never reports keeps
// that: a dedicated worker's (its frame's document when the request was
// heard), a request of a frame in another process, one made before the
// page's session existed (a popup's first). A service worker's request has
// no page.
const tracked = new WeakMap();
let nextRequest = 1;
// Each Playwright request's attribution, which the exchange reads when the
// request ends: {n, context, page, document, documentUrl}.
const attributions = new WeakMap();

const trackDocuments = async (page) => {
  const tracker = { frames: new Map(), main: null, twins: [], heard: [] };
  tracked.set(page, tracker);
  const session = await sessionOf(page);
  const known = (frame) => {
    tracker.frames.set(frame.id, { document: frame.loaderId, url: frame.url + (frame.urlFragment ?? "") });
    if (!frame.parentId) tracker.main = frame.id;
  };
  session.on("Page.frameNavigated", ({ frame }) => known(frame));
  session.on("Page.navigatedWithinDocument", ({ frameId, url }) => {
    const frame = tracker.frames.get(frameId);
    if (frame) frame.url = url;
  });
  session.on("Page.frameDetached", ({ frameId }) => tracker.frames.delete(frameId));
  session.on("Network.requestWillBeSent", (event) => reported(tracker, event));
  await Promise.all([session.send("Page.enable"), session.send("Network.enable")]);
  const { frameTree } = await session.send("Page.getFrameTree");
  const walk = (tree) => {
    if (!tracker.frames.has(tree.frame.id)) known(tree.frame);
    for (const child of tree.childFrames ?? []) walk(child);
  };
  walk(frameTree);
};

// The protocol's report of a request: the document that asked (a worker's
// script, which has no loader of its own, is its frame's document's).
const reported = (tracker, event) => {
  const { url, method } = event.request;
  if (url.startsWith("data:")) return;
  const frame = tracker.frames.get(event.frameId);
  const found = event.loaderId
    ? { document: event.loaderId, documentUrl: event.documentURL ?? frame?.url ?? null }
    : { document: frame?.document ?? null, documentUrl: frame?.url ?? null };
  const now = Date.now();
  tracker.heard = tracker.heard.filter((h) => now - h.at < 5000);
  const i = tracker.heard.findIndex((h) => h.url === url && h.method === method);
  if (i >= 0) {
    const [heard] = tracker.heard.splice(i, 1);
    const a = heard.attribution;
    if (a.document !== found.document || a.documentUrl !== found.documentUrl) {
      a.document = found.document;
      a.documentUrl = found.documentUrl;
      send({ event: "requestDocument", context: a.context, n: a.n, document: a.document, documentUrl: a.documentUrl });
    }
    return;
  }
  tracker.twins = tracker.twins.filter((t) => now - t.at < 10000);
  tracker.twins.push({ url, method, at: now, ...found });
};

// A request Playwright reports, attributed: matched with the protocol's
// report if it has come, else its frame's document as known now.
const attribute = (context, request) => {
  const a = { n: nextRequest++, context, page: null, document: null, documentUrl: null };
  attributions.set(request, a);
  let frame = null;
  try {
    frame = request.frame();
  } catch {
    // A service worker's request: no frame, no page.
  }
  const page = frame?.page();
  if (!page) return a;
  a.page = register(page).id;
  const tracker = tracked.get(page);
  const url = request.url();
  const method = request.method();
  const i = tracker.twins.findIndex((t) => t.url === url && t.method === method);
  if (i >= 0) {
    const [twin] = tracker.twins.splice(i, 1);
    a.document = twin.document;
    a.documentUrl = twin.documentUrl;
    return a;
  }
  const main = frame === page.mainFrame();
  const candidates = [...tracker.frames.entries()].filter(([id, f]) => (main ? id === tracker.main : id !== tracker.main && f.url === frame.url()));
  if (candidates.length === 1) {
    a.document = candidates[0][1].document;
    a.documentUrl = candidates[0][1].url;
  } else {
    a.documentUrl = frame.url() || null;
  }
  tracker.heard.push({ url, method, at: Date.now(), attribution: a });
  return a;
};

// The page's frames' documents now, the main frame's first.
const documentsOf = async (page) => {
  const session = await sessionOf(page);
  const { frameTree } = await session.send("Page.getFrameTree");
  const walk = (tree) => [
    { id: tree.frame.loaderId, url: tree.frame.url + (tree.frame.urlFragment ?? "") },
    ...(tree.childFrames ?? []).flatMap(walk),
  ];
  return walk(frameTree);
};

// Routes: the test's handlers for a page's or a context's requests. Each
// matching request is asked of Haskell, and waits for its answer (see the
// header). An entry is {id, kind, page, context, target, pattern,
// resourceTypes, methods, handler, asks, gone}. The functions a test exposes
// to its pages are entries of the same map, of the kind "exposed", so that
// they go with their page or context as routes do.
const routes = new Map();
const asks = new Map();
let nextAsk = 1;

// An ask that will get no answer from Haskell: the request goes on as if
// the route had not been there (to the next route that matches, or the
// network); an exposed function's call is rejected. On a page or context
// that has closed this fails, which is nothing to wait for.
const letGo = (ask) => {
  asks.delete(ask.id);
  ask.entry.asks.delete(ask.id);
  if (ask.entry.kind === "exposed") {
    ask.reject(new Error(`the test's function ${ask.entry.pattern} is gone`));
    return;
  }
  ask.route.fallback().catch(() => {});
  ask.resolve();
};

// Whether a route asks about a request: its resource types and methods, when
// it has them, are decided here, so that a request of another kind costs
// nothing but a fallback.
const wanted = (entry, request) =>
  (entry.resourceTypes == null || entry.resourceTypes.includes(request.resourceType())) &&
  (entry.methods == null || entry.methods.includes(request.method().toUpperCase()));

const routeHandler = (entry) => async (route) => {
  const request = route.request();
  if (!wanted(entry, request)) return route.fallback();
  // Every header the browser sends, cookies included: Playwright documents
  // headers() as leaving those out (Chromium gives them for a routed request
  // all the same). For a routed request they are known at once.
  const allHeaders = await request.allHeaders().catch(() => request.headers());
  return new Promise((resolve) => {
    const ask = { id: nextAsk++, entry, route, resolve };
    if (entry.gone) return letGo(ask);
    asks.set(ask.id, ask);
    entry.asks.add(ask.id);
    send({
      ask: ask.id,
      route: entry.id,
      request: {
        url: request.url(),
        method: request.method(),
        resourceType: request.resourceType(),
        headers: request.headers(),
        allHeaders,
        postData: request.postDataBuffer()?.toString("base64") ?? null,
      },
    });
  });
};

// A function the page calls, answered by Haskell: each call is an ask, and
// the page's promise settles with the answer.
const exposedHandler = (entry) => (_source, argument) =>
  new Promise((resolve, reject) => {
    const ask = { id: nextAsk++, entry, resolve, reject };
    if (entry.gone) return letGo(ask);
    asks.set(ask.id, ask);
    entry.asks.add(ask.id);
    send({ ask: ask.id, exposed: entry.id, argument: argument === undefined ? null : argument });
  });

// The headers of a response, in order, each name in lower case: a list, as
// a header may come more than once (set-cookie).
const headerList = (headers) => headers.map(({ name, value }) => [name.toLowerCase(), value]);

// Haskell's answer to an ask. One for an ask that has gone (its route was
// removed, its page closed) is ignored.
const answered = (message) => {
  const ask = asks.get(message.answer);
  if (!ask) return;
  asks.delete(ask.id);
  ask.entry.asks.delete(ask.id);
  if (ask.entry.kind === "exposed") {
    if (message.error != null) ask.reject(new Error(message.error));
    else ask.resolve(message.result);
    return;
  }
  const { route } = ask;
  let done;
  if (message.action === "fulfill") {
    done = route.fulfill({
      status: message.status ?? 200,
      headers: message.headers ?? undefined,
      contentType: message.contentType ?? undefined,
      body: Buffer.from(message.body ?? "", "base64"),
    });
  } else if (message.action === "abort") {
    done = route.abort(message.error ?? "failed");
  } else {
    done = route.fallback();
  }
  done
    .catch((error) => {
      // A page that closed meanwhile; anything else is worth a line.
      if (!/closed|Target page|has been closed/i.test(String(error?.message))) {
        process.stderr.write(`haskell-halogen-playwright: answering ${route.request().url()}: ${error?.message ?? error}\n`);
      }
    })
    .finally(ask.resolve);
};

// Take away the routes that satisfy a test (those of a page or a context
// that has closed), let their waiting requests go and tell Haskell, whose
// handlers for them are then stopped.
const dropRoutes = (which) => {
  const gone = [];
  for (const [id, entry] of routes) {
    if (!which(entry)) continue;
    routes.delete(id);
    entry.gone = true;
    for (const ask of [...entry.asks]) letGo(asks.get(ask));
    gone.push(id);
  }
  if (gone.length > 0) send({ event: "unrouted", routes: gone });
};

// Whether an action failed only because it closed its own page: a sign-in
// window's button, whose click posts to the opener and closes the window.
// Playwright performs the click and then waits for what the click set off,
// and when the page goes under that wait it complains that its target has
// closed, some of the time (a race between the click's acknowledgement and
// the page's going). The action was performed, which its log says, so that
// is no failure. A page that closes before the action is performed (while
// it waits for a disabled button, say) is one: the log has no such line.
const performedAsPageClosed = (page, error) => {
  const message = String(error?.message ?? "");
  return page.isClosed() && /Target page, context or browser has been closed/.test(message) && /performing \w+ action/.test(message);
};

// The frame a locator goes into, if it goes into one (the innermost): the
// document its element is in, whose program hears what is done to the
// element, and which settle asks as well as the page. Found without
// waiting: null for a locator of the page's own document, and for a frame
// that is not there (yet).
const frameOf = async ({ page, steps }) => {
  try {
    let at = pageOf(page);
    let inner = null;
    for (const step of steps) {
      at = stepFrom(at, step);
      if ("frame" in step || "frameLocator" in step) inner = at;
    }
    if (!inner) return null;
    // A frame found by its address is the frame itself; one an iframe
    // element shows is that element's content.
    if (typeof inner.evaluate === "function") return inner;
    const [element] = await inner.owner().elementHandles();
    return element ? await element.contentFrame() : null;
  } catch {
    return null;
  }
};

// The frame an action's locator goes into, then the page: both settled.
const settleAround = async (frame, page, options) => {
  if (frame) await settle(frame, options);
  await settle(page, options);
};

// An action on a locator, then settle: the frame the locator goes into, if
// it goes into one, and the page. The action is given Playwright's options
// with the call's timeout.
// The page is taken before the action, which may close it (a sign-in
// window's button): settling a page that has closed is nothing to do. The
// frame is looked for before the action, which may take it away, and
// again after it if it was not there yet.
const acting = (act) => async (m) => {
  const page = pageOf(m.locator.page);
  const target = locate(m.locator);
  const before = await frameOf(m.locator);
  try {
    await act(target, m, { timeout: within(m) });
  } catch (error) {
    if (!performedAsPageClosed(page, error)) throw error;
    return null;
  }
  await settleAround(before ?? (await frameOf(m.locator)), page, { timeout: within(m) });
  return null;
};
// An action on a page, then settle.
const pageActing = (act) => async (m) => {
  const page = pageOf(m.page);
  const result = await act(page, m, { timeout: within(m) });
  await settle(page, { timeout: within(m) });
  return result ?? null;
};
// A reading of a locator's element, which Playwright waits for up to the
// call's timeout.
const reading = (read) => async (m) => {
  const result = await read(locate(m.locator), m, { timeout: within(m) });
  return result === undefined ? null : result;
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
      permissions: o.permissions ?? [],
      isMobile: o.isMobile ?? false,
      hasTouch: o.hasTouch ?? false,
      reducedMotion: o.reducedMotion ? "reduce" : undefined,
    });
    context.setDefaultTimeout(timeout);
    context.setDefaultNavigationTimeout(timeout);
    await context.addInitScript(probeScript);
    context.halogenScriptless = o.javaScriptEnabled === false;
    context.halogenAcceptDialogs = o.acceptDialogs ?? false;
    const id = nextHandle++;
    contexts.set(id, context);
    context.on("close", () => dropRoutes((entry) => entry.context === id));
    // Every page as it appears, a popup too (see register).
    context.on("page", register);
    // Every request of the context's, as it is made: its pages' and their
    // workers', whose requests are the context's and not always a page's.
    // Each with the page and the document that asked (attribute).
    context.on("request", (request) => {
      const a = attribute(id, request);
      send({
        event: "request",
        context: id,
        n: a.n,
        url: request.url(),
        method: request.method(),
        resourceType: request.resourceType(),
        page: a.page,
        document: a.document,
        documentUrl: a.documentUrl,
      });
    });
    // A request that failed: refused, aborted by a route, cut off.
    context.on("requestfailed", (request) =>
      send({ event: "requestfailed", context: id, url: request.url(), method: request.method(), failure: request.failure()?.errorText ?? "" }),
    );
    if (o.recordExchanges != null) recordExchanges(context, id, o.recordExchanges);
    return id;
  },
  async addInitScript(m) {
    // After the probe's, so that a script of the test's own sees it.
    await contextOf(m.context).addInitScript({ content: m.script });
    return null;
  },
  async closeContext(m) {
    const context = contexts.get(m.context);
    contexts.delete(m.context);
    if (context) await context.close();
    return null;
  },
  async newPage(m) {
    const page = await contextOf(m.context).newPage();
    const { id, ready } = register(page);
    await ready;
    return id;
  },
  // The context's pages now, those its pages opened included, oldest first.
  async pages(m) {
    return contextOf(m.context)
      .pages()
      .map((page) => register(page).id);
  },
  async closePage(m) {
    const page = pages.get(m.page);
    if (page) await page.close();
    pages.delete(m.page);
    return null;
  },

  // An address that turns into a download (Playwright rejects with
  // "Download is starting") has gone where it was asked to: no error, and
  // nothing to settle, since the page stayed as it was.
  //
  // The status of the response the navigation ended with, null when there
  // was none (a navigation within the document, a download).
  async goto(m) {
    const page = pageOf(m.page);
    let response;
    try {
      response = await page.goto(m.url, { timeout: within(m), waitUntil: "load" });
    } catch (error) {
      if (/Download is starting/.test(String(error?.message))) return null;
      throw error;
    }
    await settle(page, { timeout: within(m) });
    return response ? response.status() : null;
  },
  reload: pageActing((page, _, o) => page.reload({ ...o, waitUntil: "load" }).then(() => null)),
  goBack: traverse(-1),
  goForward: traverse(1),
  async url(m) {
    return pageOf(m.page).url();
  },
  async documents(m) {
    return await documentsOf(pageOf(m.page));
  },
  async title(m) {
    return await pageOf(m.page).title();
  },
  // The page's dedicated workers alive now (Playwright drops one when it
  // ends).
  async workers(m) {
    return pageOf(m.page).workers().length;
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
    await settle(pageOf(m.page), { timeout: within(m) });
    return null;
  },
  async waitQuiet(m) {
    await settle(pageOf(m.page), { quiet: true, timeout: within(m) });
    return null;
  },
  async waitStarted(m) {
    await waitStarted(pageOf(m.page), within(m));
    return null;
  },
  async activity(m) {
    return await activity(pageOf(m.page), within(m));
  },

  click: acting((target, _, o) => target.click(o)),
  doubleClick: acting((target, _, o) => target.dblclick(o)),
  hover: acting((target, _, o) => target.hover(o)),
  fill: acting((target, m, o) => target.fill(m.text, o)),
  typeText: acting((target, m, o) => target.pressSequentially(m.text, o)),
  clear: acting((target, _, o) => target.clear(o)),
  pressOn: acting((target, m, o) => target.press(m.key, o)),
  check: acting((target, _, o) => target.check(o)),
  uncheck: acting((target, _, o) => target.uncheck(o)),
  selectOption: acting((target, m, o) => target.selectOption(m.values, o)),
  focus: acting((target, _, o) => target.focus(o)),
  blur: acting((target, _, o) => target.blur(o)),
  dragTo: acting((target, m, o) => target.dragTo(locate(m.target), o)),

  textContent: reading(async (target, _, o) => (await target.textContent(o)) ?? ""),
  innerText: reading((target, _, o) => target.innerText(o)),
  inputValue: reading((target, _, o) => target.inputValue(o)),
  getAttribute: reading((target, m, o) => target.getAttribute(m.name, o)),
  isChecked: reading((target, _, o) => target.isChecked(o)),
  isEnabled: reading((target, _, o) => target.isEnabled(o)),
  isDisabled: reading((target, _, o) => target.isDisabled(o)),
  // Every match's text, none not an error: this does not wait.
  allTextContents: reading((target) => target.allTextContents()),
  // These two answer for the page as it is, and do not wait.
  isVisible: reading((target) => target.isVisible()),
  count: reading((target) => target.count()),
  outerHTML: reading((target, _, o) => target.evaluate((element) => element.outerHTML, undefined, o)),
  waitFor: reading((target, m, o) => target.waitFor({ ...o, state: m.state }).then(() => null)),

  // JavaScript in the page, whose promise is awaited: up to the call's
  // timeout, since a promise that never settles would hold the call for
  // ever (Playwright's evaluate takes no timeout). The question lost is left
  // to end with its page, as the probe's is.
  async evaluate(m) {
    const page = pageOf(m.page);
    const result = await answeredWithin(within(m), page.evaluate(m.expression), unsettled(page, within(m)));
    return result === undefined ? null : result;
  },
  async evaluateWith(m) {
    // Made a function here, and sent to the page as its source, which is
    // how Playwright sends any function.
    const page = pageOf(m.page);
    const result = await answeredWithin(within(m), page.evaluate(functionOf(m.function), m.argument), unsettled(page, within(m)));
    return result === undefined ? null : result;
  },
  // The function called with every element the locator matches (none is
  // not an error) and the argument, in the page.
  async evaluateAll(m) {
    const page = pageOf(m.locator.page);
    const result = await answeredWithin(
      within(m),
      locate(m.locator).evaluateAll(functionOf(m.function), m.argument),
      unsettled(page, within(m)),
    );
    return result === undefined ? null : result;
  },
  // An expression polled in the page, at its animation frames, until it is
  // truthy (pollInPage). As Playwright's, an expression whose value is a
  // function is called, and that is polled.
  async waitForFunction(m) {
    const predicate = `() => { const value = (${m.expression}\n); return typeof value === "function" ? value() : value; }`;
    return await pollInPage(pageOf(m.page), predicate, null, within(m), `the expression ${m.expression}`);
  },
  // A function polled as waitForFunction's expression, with an argument.
  async waitForFunctionWith(m) {
    return await pollInPage(pageOf(m.page), m.function, m.argument, within(m), `the function ${m.function}`);
  },
  // A function called in the page with the locator's element (waited for)
  // and the argument.
  // Playwright's timeout is for finding the element; the deadline here is
  // for the whole call, the function's promise included.
  evaluateOn: reading(async (target, m, o) => {
    const page = pageOf(m.locator.page);
    const result = await answeredWithin(within(m), target.evaluate(functionOf(m.function), m.argument, o), unsettled(page, within(m)));
    return result === undefined ? null : result;
  }),
  // The page's address matching a glob (or equal to it), after which the
  // page has loaded, if it navigated, and settled.
  waitForURL: pageActing((page, m, o) => page.waitForURL(m.url, o).then(() => null)),
  // Every frame's address, the page's own first.
  async frames(m) {
    return pageOf(m.page)
      .frames()
      .map((frame) => frame.url());
  },

  // Keyboard, mouse and geometry.
  typeKeys: pageActing((page, m) => page.keyboard.type(m.text).then(() => null)),
  insertText: pageActing((page, m) => page.keyboard.insertText(m.text).then(() => null)),
  mouseMove: pageActing((page, m) => page.mouse.move(m.x, m.y).then(() => null)),
  mouseClick: pageActing((page, m) => page.mouse.click(m.x, m.y).then(() => null)),
  tap: acting((target, _, o) => target.tap(o)),
  boundingBox: reading((target, _, o) => target.boundingBox(o)),
  async viewportSize(m) {
    return pageOf(m.page).viewportSize();
  },

  // Routes (see the header).
  async route(m) {
    const target = m.page != null ? pageOf(m.page) : contextOf(m.context);
    const entry = {
      id: m.route,
      kind: "route",
      page: m.page ?? null,
      context: m.context,
      target,
      pattern: m.pattern,
      resourceTypes: m.resourceTypes ?? null,
      methods: m.methods?.map((method) => method.toUpperCase()) ?? null,
      asks: new Set(),
      gone: false,
    };
    entry.handler = routeHandler(entry);
    routes.set(entry.id, entry);
    try {
      await target.route(m.pattern, entry.handler);
    } catch (error) {
      routes.delete(entry.id);
      throw error;
    }
    return null;
  },
  // The requests waiting for an answer are let go on first, so no handler
  // of this route's is under way when Playwright removes it: "ignoreErrors"
  // then has nothing to ignore, and it never waits (a page whose other
  // requests are under way is left alone).
  async unroute(m) {
    const entry = routes.get(m.route);
    if (!entry) return null;
    routes.delete(entry.id);
    entry.gone = true;
    for (const ask of [...entry.asks]) letGo(asks.get(ask));
    await entry.target.unroute(entry.pattern, entry.handler, { behavior: "ignoreErrors" }).catch(() => {});
    return null;
  },
  // A routed request sent on to the server, while its ask waits: the
  // response, for the handler to look at or change before it answers. The
  // body is the one the page would read, decoded, so the headers that
  // described the bytes on the wire (their length, their encoding) are left
  // out: fulfilled with another body, they would be wrong. A redirect is
  // not followed here: it is the response, which the page follows itself
  // when the handler fulfils with it, so that its address is the one it
  // was sent to (Playwright's fetch would follow it, and the page would
  // show the last response at the first address). The response is
  // Playwright's to keep until its context closes unless it is disposed
  // of, so it is, once copied.
  async fetchRouted(m) {
    const ask = asks.get(m.ask);
    if (!ask?.route) {
      throw Object.assign(new Error(`the request ${m.ask} is no longer the route's to fetch: its route, its page or its context went`), { name: "ClosedError" });
    }
    const response = await ask.route.fetch({ timeout: within(m), maxRedirects: 0 });
    const hop = new Set(["content-length", "content-encoding", "transfer-encoding"]);
    try {
      return {
        status: response.status(),
        headers: headerList(response.headersArray()).filter(([name]) => !hop.has(name)),
        body: (await response.body()).toString("base64"),
      };
    } finally {
      await response.dispose().catch(() => {});
    }
  },

  // A function of the test's that the context's or a page's pages call as
  // window[name], each call an ask (see the header).
  async expose(m) {
    const target = m.page != null ? pageOf(m.page) : contextOf(m.context);
    const entry = { id: m.exposed, kind: "exposed", page: m.page ?? null, context: m.context, target, pattern: m.name, asks: new Set(), gone: false };
    routes.set(entry.id, entry);
    try {
      await target.exposeBinding(m.name, exposedHandler(entry));
    } catch (error) {
      routes.delete(entry.id);
      throw error;
    }
    return null;
  },

  // Popups: armed before the action that opens one, as downloads are, and
  // awaited after it: the page's handle, once it has loaded and settled.
  async expectPopup(m) {
    const id = nextHandle++;
    const waiting = pageOf(m.page).waitForEvent("popup", { timeout: within(m) });
    // A popup that came is kept until awaitPopup takes it (or forgetPopup,
    // when the action failed), however long the action goes on after it
    // opened. A wait that ended with its timeout is kept a minute, for
    // awaitPopup to say so, and its rejection is nobody's otherwise.
    waiting.then(
      () => {},
      () => setTimeout(() => popups.delete(id), 60000).unref(),
    );
    popups.set(id, waiting);
    return id;
  },
  async awaitPopup(m) {
    const waiting = popups.get(m.popup);
    popups.delete(m.popup);
    if (!waiting) throw new Error(`no popup ${m.popup} is expected`);
    const popup = await waiting;
    const { id, ready } = register(popup);
    await ready;
    await popup.waitForLoadState("load", { timeout: within(m) });
    await settle(popup, { timeout: within(m) });
    return id;
  },
  async forgetPopup(m) {
    popups.delete(m.popup);
    return null;
  },

  // An event of a target in the page: armed before the action, as popups
  // are, by a listener added in the page (on the locator's element, waited
  // for as an action waits, or on the window or the document), and awaited
  // after it, until the event has been dispatched, then settled. The
  // listener is added last, so the page's own listeners on the target have
  // run when it hears the event; the settle after it gives a program's
  // handler its task. The listener lives in the document: one that goes
  // (a navigation) takes it, and the wait fails, saying so, whether or not
  // the event came first. Every function is sent to the page through the
  // DevTools protocol, so a Content Security Policy does not stop it.
  async expectEvent(m) {
    const listen = (target, type) => {
      let heard;
      const entry = { fired: new Promise((resolve) => (heard = resolve)) };
      const listener = () => {
        target.removeEventListener(type, listener);
        heard(true);
      };
      entry.remove = () => target.removeEventListener(type, listener);
      target.addEventListener(type, listener);
      return entry;
    };
    const source = `(target, type) => (${listen})(target, type)`;
    let handle;
    let page;
    let frame = null;
    if (m.locator) {
      page = pageOf(m.locator.page);
      handle = await locate(m.locator).evaluateHandle(functionOf(source), m.eventType, { timeout: within(m) });
      // The document the element is in, whose program hears the event.
      frame = await frameOf(m.locator);
    } else {
      page = pageOf(m.page);
      handle = await page.evaluateHandle(
        functionOf(`({ on, type }) => (${listen})(on === "document" ? document : window, type)`),
        { on: m.on, type: m.eventType },
      );
    }
    const id = nextHandle++;
    events.set(id, { handle, page, frame, type: m.eventType, what: m.what });
    return id;
  },
  async awaitEvent(m) {
    const armed = events.get(m.armed);
    events.delete(m.armed);
    if (!armed) throw new Error(`no event ${m.armed} is expected`);
    const { handle, page, frame, type, what } = armed;
    const ms = within(m);
    const late = () => `no ${type} event on ${what} of ${page.url()} within ${ms}ms`;
    let fired;
    try {
      fired = await answeredWithin(
        ms + 1000,
        handle.evaluate(
          (entry, ms) =>
            Promise.race([entry.fired, new Promise((resolve) => setTimeout(() => resolve(false), ms))]).then((fired) => {
              if (!fired) entry.remove();
              return fired;
            }),
          ms,
        ),
        late,
      );
    } catch (error) {
      if (navigatedAway(error) || /disposed|has been closed/i.test(String(error?.message))) {
        throw Object.assign(
          new Error(`the document listening for ${type} on ${what} went (the page navigated, or closed) before the event was heard; it may have come first`),
          { name: "NavigatedAway" },
        );
      }
      throw error;
    } finally {
      handle.dispose().catch(() => {});
    }
    if (!fired) throw timeoutError(late());
    await settleAround(frame, page, { timeout: ms });
    return null;
  },
  async forgetEvent(m) {
    const armed = events.get(m.armed);
    events.delete(m.armed);
    if (armed) {
      await armed.handle.evaluate((entry) => entry.remove()).catch(() => {});
      armed.handle.dispose().catch(() => {});
    }
    return null;
  },

  // A DevTools event of the page's session, sent to Haskell each time it
  // comes, from now on. Listening twice to one event is listening once.
  async listenCDP(m) {
    const page = pageOf(m.page);
    const session = await sessionOf(page);
    let heard = listening.get(page);
    if (!heard) listening.set(page, (heard = new Set()));
    if (!heard.has(m.cdpEvent)) {
      heard.add(m.cdpEvent);
      session.on(m.cdpEvent, (params) => send({ event: "cdp", page: m.page, name: m.cdpEvent, params: params ?? null }));
    }
    return null;
  },

  // Downloads: armed before the action that causes one, so that it cannot
  // be missed, and awaited after it.
  async expectDownload(m) {
    const id = nextHandle++;
    const waiting = pageOf(m.page).waitForEvent("download", { timeout: within(m) });
    // A download that started is kept until awaitDownload takes it (or
    // forgetDownload, when the action failed), however long the action
    // goes on after it. A wait nobody comes back for ends with its timeout
    // and is kept a minute; its rejection is nobody's.
    waiting.then(
      () => {},
      () => setTimeout(() => downloads.delete(id), 60000).unref(),
    );
    downloads.set(id, waiting);
    return id;
  },
  async awaitDownload(m) {
    const waiting = downloads.get(m.download);
    downloads.delete(m.download);
    if (!waiting) throw new Error(`no download ${m.download} is expected`);
    const download = await waiting;
    // The whole file, saved under the downloads' directory.
    const path = await answeredWithin(within(m), download.path(), () => `the download of ${download.url()} did not end within ${within(m)}ms`);
    return { suggestedFilename: download.suggestedFilename(), url: download.url(), path };
  },
  async forgetDownload(m) {
    downloads.delete(m.download);
    return null;
  },
  setInputFiles: acting((target, m, o) =>
    target.setInputFiles(
      m.files.map((f) => ({ name: f.name, mimeType: f.mimeType, buffer: Buffer.from(f.bytes, "base64") })),
      o,
    ),
  ),

  // Cookies and the context's state.
  async cookies(m) {
    return await contextOf(m.context).cookies();
  },
  async addCookies(m) {
    // Playwright refuses a field given as null: what is not given is left out.
    const given = (cookie) => Object.fromEntries(Object.entries(cookie).filter(([, v]) => v !== null));
    await contextOf(m.context).addCookies(m.cookies.map(given));
    return null;
  },
  async clearCookies(m) {
    await contextOf(m.context).clearCookies();
    return null;
  },
  // Offline or online, then every page of the context settled: the pages
  // hear "offline" or "online", and a program may act on it.
  async setOffline(m) {
    const context = contextOf(m.context);
    await context.setOffline(m.offline);
    await Promise.all(
      context.pages().map((page) =>
        settle(page, { timeout: within(m) }).catch((error) => {
          if (!page.isClosed()) throw error;
        }),
      ),
    );
    return null;
  },
  // A media query's change is reported at a later rendering of the page's,
  // to every list of the query's in the order they were made. So a list of
  // the driver's, made just before, hears it after the program's have, and
  // the page is settled after that: a program listening to a matchMedia
  // has heard the change by then. Nothing is waited for when the query
  // already matches what is asked.
  emulateMedia: pageActing(async (page, m) => {
    const query = m.media != null ? "print" : "(prefers-color-scheme: dark)";
    const wanted = m.media != null ? m.media === "print" : m.colorScheme === "dark";
    const changing =
      !scriptless.has(page) &&
      (await page.evaluate(
        ([query, wanted]) => {
          const list = matchMedia(query);
          if (list.matches === wanted) return false;
          globalThis.__halogenMediaChange = new Promise((resolve) => list.addEventListener("change", () => setTimeout(resolve, 0), { once: true }));
          return true;
        },
        [query, wanted],
      ));
    await page.emulateMedia({ media: m.media ?? undefined, colorScheme: m.colorScheme ?? undefined });
    if (changing) {
      try {
        await answeredWithin(within(m), page.evaluate(() => globalThis.__halogenMediaChange), silent(page, within(m)));
      } catch (error) {
        if (!navigatedAway(error)) throw error;
      }
    }
  }),
  // The page printed, its pages the size its CSS asks for, as the scripts
  // print a deck of slides.
  async pdf(m) {
    const page = pageOf(m.page);
    const pdf = await answeredWithin(within(m), page.pdf({ preferCSSPageSize: true }), () => `the page ${page.url()} was not printed within ${within(m)}ms`);
    return pdf.toString("base64");
  },

  // The Chrome DevTools Protocol: a method of the page's session and its
  // parameters, the result.
  async cdp(m) {
    const page = pageOf(m.page);
    const session = await sessionOf(page);
    return (
      (await answeredWithin(within(m), session.send(m.cdpMethod, m.params ?? {}), () => `${m.cdpMethod} was not answered within ${within(m)}ms`)) ?? null
    );
  },

  // The whole page, or only what the viewport shows: a PNG, unless the
  // path's extension says JPEG.
  async screenshot(m) {
    await pageOf(m.page).screenshot({ path: m.path, fullPage: m.fullPage ?? true, timeout: within(m) });
    return null;
  },
};

// A function's source, made a function here: Playwright sends a function to
// the page as its source.
const functionOf = (source) => new Function(`return (${source});`)();

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
  else if ("answer" in message) answered(message);
  else process.stderr.write(`haskell-halogen-playwright: not a call or an answer: ${line}\n`);
});
input.on("close", close);
// Chromium closing by itself (it crashed, say) ends the session too.
browser.on("disconnected", () => {
  if (!closing) fatal("Chromium went away");
});

send({ event: "ready" });
