// A stand-in for Tauri v2's global API (window.__TAURI__), with the parts
// Halogen.Tauri calls, for the browser suite: there is no Tauri in the
// runner's Chromium, and no Rust side to answer. The runner loads every .js
// file in test/web into the page before the suite starts, so the API is
// there before anything asks for it.
//
// core.invoke(command, args) answers from a table of commands:
//
//   echo         resolves with its arguments
//   fail         rejects with args.message, a string, as a Rust command
//                returning Err(String) does
//   fail_object  rejects with { kind: "NotFound", path: args.path }, as a
//                command whose error serde makes an object of
//   held         resolves with its arguments once the test releases
//                args.name
//   nothing      resolves with null, as a command returning () does
//   number       resolves with 42
//
// and rejects any other command with the string Tauri itself gives. Like
// Tauri's, its promises settle later, never during the call.
//
// event.listen(name, handler) and window.getCurrentWindow().listen(name,
// handler) put a listener on a bus, listening anywhere or on the window
// labelled "main"; each gives, later, the promise of a function that takes
// it off. event.emit and event.emitTo, and the test's fire, deliver
// { event, id, payload } to the event's listeners that an emit to that
// target reaches (to any target: all of them; to a label: those listening
// anywhere and those on that window), asynchronously, as Tauri's IPC does.
// window.getCurrentWindow().destroy() notes that it was called.
//
// The suite's bindings to the test side are halogenTauriTest (see
// test/jsbits/double.js): fire, live listeners, unlistens, the emits it
// saw, releasing a held command, hiding the API to be outside Tauri, and
// reset. What it notes lives in the page, which browser GHCi
// (toolchain/dev-test.sh) keeps from one :main to the next, so the suite
// resets it as it starts: every listener is dropped, every note forgotten,
// and a command still held from the run before is answered first, to
// nobody.
(() => {
  const later = (f) => setTimeout(f, 0);
  let listeners = [];
  let nextId = 1;
  let unlistens = 0;
  let emitted = [];
  let destroyed = 0;
  let held = new Map();
  let released = new Set();

  const deliver = (target, event, payload) => {
    const id = nextId++;
    const reached = (l) => l.event === event && (target === null || l.window === null || l.window === target);
    for (const listener of listeners.filter(reached)) {
      later(() => {
        if (listeners.includes(listener)) listener.handler({ event, id, payload });
      });
    }
  };

  const listen = (window) => (event, handler) => {
    const listener = { event, window, handler };
    return new Promise((resolve) =>
      later(() => {
        listeners.push(listener);
        resolve(() => {
          unlistens++;
          listeners = listeners.filter((l) => l !== listener);
        });
      }),
    );
  };

  const commands = {
    echo: (args) => Promise.resolve(args),
    fail: (args) => Promise.reject(args.message),
    fail_object: (args) => Promise.reject({ kind: "NotFound", path: args.path }),
    held: (args) =>
      new Promise((resolve) => {
        if (released.has(args.name)) return resolve(args);
        held.set(args.name, () => resolve(args));
      }),
    nothing: () => Promise.resolve(null),
    number: () => Promise.resolve(42),
  };

  const currentWindow = {
    label: "main",
    listen: listen("main"),
    destroy: () => {
      destroyed++;
      return new Promise((resolve) => later(resolve));
    },
  };

  const api = {
    core: {
      invoke: (command, args = {}) =>
        new Promise((resolve, reject) =>
          later(() => {
            const run = commands[command];
            if (run === undefined) reject("Command " + command + " not found");
            else run(structuredClone(args)).then(resolve, reject);
          }),
        ),
    },
    event: {
      listen: (event, handler) => listen(null)(event, handler),
      emit: (event, payload) => {
        emitted.push({ target: null, event, payload: payload ?? null });
        deliver(null, event, payload);
        return Promise.resolve();
      },
      emitTo: (target, event, payload) => {
        emitted.push({ target, event, payload: payload ?? null });
        deliver(target, event, payload);
        return Promise.resolve();
      },
    },
    window: { getCurrentWindow: () => currentWindow },
  };

  globalThis.__TAURI__ = api;

  globalThis.halogenTauriTest = {
    fire: (target, event, payload) => deliver(target, event, payload),
    listening: (event) => listeners.filter((l) => event === null || l.event === event).length,
    unlistens: () => unlistens,
    emitted: () => emitted,
    destroyed: () => destroyed,
    release: (name) => {
      released.add(name);
      const go = held.get(name);
      held.delete(name);
      if (go !== undefined) go();
    },
    outside: (hidden) => {
      if (hidden) delete globalThis.__TAURI__;
      else globalThis.__TAURI__ = api;
    },
    reset: () => {
      for (const go of held.values()) go();
      held = new Map();
      released = new Set();
      listeners = [];
      unlistens = 0;
      emitted = [];
      destroyed = 0;
      globalThis.__TAURI__ = api;
    },
  };
})();
