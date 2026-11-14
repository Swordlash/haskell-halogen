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
//   {"event": "request", "context": 1, "url": "...", "method": "GET", "resourceType": "script"}
//   {"event": "requestfailed", "context": 1, "url": "...", "method": "GET", "failure": "net::ERR_FAILED"}
//   {"event": "exchange", "context": 1, "url": "...", "method": "POST", "resourceType": "fetch",
//    "requestHeaders": [["cookie", "..."], ...], "postData": "<base64>" | null,
//    "status": 200 | null, "responseHeaders": [[name, value], ...],
//    "responseBody": "<base64>" | null, "failure": "net::..." | null}
//                                            a request of a context that records
//                                            its exchanges, once it has ended
//   {"event": "unrouted", "routes": [3, 4]}  these routes went with their page
//                                            or context; no ask of theirs is
//                                            waiting for an answer any more
// node to Haskell, a question, which Haskell answers once:
//   {"ask": 12, "route": 3, "request": {"url": "...", "method": "GET", "resourceType": "fetch",
//                                       "headers": {...}, "allHeaders": {...}, "postData": "<base64>" | null}}
// Haskell to node, its answer:
//   {"answer": 12, "action": "continue"}
//   {"answer": 12, "action": "fulfill", "status": 200, "headers": {...}, "contentType": "..." | null, "body": "<base64>"}
//   {"answer": 12, "action": "abort", "error": "connectionrefused"}
// While a route's ask waits for its answer, Haskell may call "fetchRouted"
// with the ask's number: the request is sent to the server, and the reply is
// the response (status, headers, body), which the handler then looks at or
// changes before it answers "fulfill" with it.
// Messages are told apart by their keys: "id" (a call, or its reply),
// "event", "ask" and "answer". Bytes cross as base64.
//
// A route's id is chosen by Haskell, which registers its handler before it
// calls "route", so that no ask can arrive for a route Haskell does not
// know. A route may carry "resourceTypes" and "methods": a request of
// another kind is passed on here without asking Haskell. An ask waits here
// until its answer comes, or until its route goes (unrouted by Haskell, or
// with its page or context), whichever is first: then the request is let go
// on as if the route had not been there, and an answer that comes later is
// ignored. So neither side is ever left waiting for the other.
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
// The downloads being waited for, by the number expectDownload gave.
const downloads = new Map();

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

// Every page of every context gets the probe before any of its scripts
// runs, so that a Halogen program in it answers (Halogen.Activity).
const probeScript = () => {
  if (globalThis.__halogenProbe === undefined) globalThis.__halogenProbe = {};
};

// A locator's steps, from its page. Two steps go into a frame, after which
// the steps that follow are applied inside it: "frame" (the page's frame
// whose address matches a glob, which must be the first step) and
// "frameLocator" (the frame an iframe element shows).
const locate = ({ page, steps }) =>
  steps.reduce((at, step) => {
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
  }, pageOf(page));

const escapeRegExp = (text) => text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

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
const unsettled = (page, ms) => () => `the JavaScript evaluated in ${page.url()} did not settle within ${ms}ms`;
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

// Routes: the test's handlers for a page's or a context's requests. Each
// matching request is asked of Haskell, and waits for its answer (see the
// header). An entry is {id, page, context, target, pattern, resourceTypes,
// methods, handler, asks, gone}.
const routes = new Map();
const asks = new Map();
let nextAsk = 1;

// An ask that will get no answer from Haskell: the request goes on as if
// the route had not been there (to the next route that matches, or the
// network). On a page or context that has closed this fails, which is
// nothing to wait for.
const letGo = (ask) => {
  asks.delete(ask.id);
  ask.entry.asks.delete(ask.id);
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

// An action on a locator, then settle. The action is given Playwright's
// options with the call's timeout.
const acting = (act) => async (m) => {
  const target = locate(m.locator);
  await act(target, m, { timeout: within(m) });
  await settle(pageOf(m.locator.page), { timeout: within(m) });
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
    // Every request of the context's, as it is made: its pages' and their
    // workers', whose requests are the context's and not always a page's.
    context.on("request", (request) =>
      send({ event: "request", context: id, url: request.url(), method: request.method(), resourceType: request.resourceType() }),
    );
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
    const context = contextOf(m.context);
    const page = await context.newPage();
    if (context.halogenScriptless) scriptless.add(page);
    const id = nextHandle++;
    pages.set(id, page);
    watch(id, page);
    // The options' processor throttle, on every page as it is made, before
    // it loads anything.
    if (options.throttle) await (await sessionOf(page)).send("Emulation.setCPUThrottlingRate", { rate: options.throttle });
    return id;
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
  goBack: pageActing((page, _, o) => page.goBack({ ...o, waitUntil: "load" }).then(() => null)),
  goForward: pageActing((page, _, o) => page.goForward({ ...o, waitUntil: "load" }).then(() => null)),
  async url(m) {
    return pageOf(m.page).url();
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
  // truthy.
  async waitForFunction(m) {
    await pageOf(m.page).waitForFunction(m.expression, undefined, { timeout: within(m) });
    return null;
  },
  // An expression's function, polled as waitForFunction's, with an
  // argument.
  async waitForFunctionWith(m) {
    await pageOf(m.page).waitForFunction(functionOf(m.function), m.argument, { timeout: within(m) });
    return null;
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
  // out: fulfilled with another body, they would be wrong.
  async fetchRouted(m) {
    const ask = asks.get(m.ask);
    if (!ask?.route) {
      throw Object.assign(new Error(`the request ${m.ask} is no longer the route's to fetch: its route, its page or its context went`), { name: "ClosedError" });
    }
    const response = await ask.route.fetch({ timeout: within(m) });
    const hop = new Set(["content-length", "content-encoding", "transfer-encoding"]);
    return {
      status: response.status(),
      headers: headerList(response.headersArray()).filter(([name]) => !hop.has(name)),
      body: (await response.body()).toString("base64"),
    };
  },


  // Downloads: armed before the action that causes one, so that it cannot
  // be missed, and awaited after it.
  async expectDownload(m) {
    const id = nextHandle++;
    const waiting = pageOf(m.page).waitForEvent("download", { timeout: within(m) });
    // A wait nobody comes back for (the action failed) ends with its
    // timeout; its rejection is nobody's.
    waiting.catch(() => {}).finally(() => setTimeout(() => downloads.delete(id), 60000).unref());
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
