// Shared libraries loaded into the running program, for Halogen.Lazy.Dyld.
// The GHC JavaScript backend links this file and calls these by name; the
// wasm backend embeds it into its JSFFI module (Halogen.JSBits.browserJS).
//
// Only a program GHC's dynamic linker (dyld.mjs) loaded can link a library
// into itself: one linked into a single .wasm has no linker left to call.
// The page that started the program with dyld says so by setting
//
//   globalThis.halogenDyld = { dyld, rootfs, shim }
//
// dyld: what dyld.mjs's main(...) resolved with; rootfs: the
// browser_wasi_shim PreopenDirectory dyld's DyLDBrowserHost reads its
// libraries from; shim: that browser_wasi_shim module (its File and
// Directory classes, which dyld checks the file system's entries against).
// The tutorial's page (examples/learn/js/playground.js) and learn/'s set it.
//
// What this file keeps (which libraries it has linked, and the one load
// under way) is kept on that object rather than in this file's scope: the
// wasm backend evaluates this file once per splice and per program, and a
// library is linked once per dyld, however many programs ask for it.
//
// No nullish assignment here: the wasm backend embeds the file in a C
// string, where its two question marks and the equals sign are a trigraph.
// Nor template literals, which no other package's jsbits use either.

function halogen_lazy_available() {
  const host = globalThis.halogenDyld;
  return host != null && host.dyld != null && host.rootfs != null && host.shim != null;
}

// The library's bytes at this absolute path of the file system dyld reads,
// with the directories on the way made as needed.
function halogen_lazy_write(host, path, bytes) {
  const parts = path.split("/").filter((part) => part !== "");
  const name = parts.pop();
  let dir = host.rootfs.dir;
  for (const part of parts) {
    let next = dir.contents.get(part);
    if (!next) {
      next = new host.shim.Directory([]);
      dir.contents.set(part, next);
    }
    dir = next;
  }
  dir.contents.set(name, new host.shim.File(bytes));
}

function halogen_lazy_error(name, message) {
  const error = new Error(message);
  error.name = name;
  return error;
}

// Fetch the library at url, put it at path and have dyld link it (and
// whatever it needs that is not linked yet, from dyld's search directories),
// once per path: a library is linked for good, and dyld knows a library by
// its file name, so linking it again would do nothing. A fetch that fails is
// forgotten, for the next load to try again. A link that fails is not: dyld
// marks a library's name as loaded before it links it, and has no way to
// take that back, so a second loadDLLs of it would do nothing and its entry
// would be missing. Its failure is kept and given again to every later load
// of the path, until the page is reloaded. Loads go one after the other, as
// dyld links one library at a time.
function halogen_lazy_link(host, url, path) {
  if (!host.halogenLazy) host.halogenLazy = { linked: new Map(), queue: Promise.resolve() };
  const state = host.halogenLazy;
  let linked = state.linked.get(path);
  if (!linked) {
    linked = state.queue.then(async () => {
      const response = await fetch(url);
      if (!response.ok) throw halogen_lazy_error("LinkError", url + ": " + response.status + " " + response.statusText);
      halogen_lazy_write(host, path, new Uint8Array(await response.arrayBuffer()));
      try {
        await host.dyld.loadDLLs(path);
      } catch (error) {
        const message = error !== null && typeof error === "object" ? String(error.message) : String(error);
        throw halogen_lazy_error("LinkError", path + " did not link, and cannot be linked again before the page is reloaded: " + message);
      }
    });
    state.queue = linked.catch(() => {});
    state.linked.set(path, linked);
    linked.catch((error) => {
      if (!String(error.message).includes("cannot be linked again")) state.linked.delete(path);
    });
  }
  return linked;
}

// Link the library and call its entry, an exported function of no arguments
// that resolves with a StablePtr (a number) to a Dynamic holding what the
// library offers. dyld keeps every library's exports in one table, so an
// entry's name must be unique among every library the program links.
async function halogen_lazy_load(url, path, entry) {
  if (!halogen_lazy_available()) {
    throw halogen_lazy_error("NotAvailableError", "the page started this program without dyld (globalThis.halogenDyld)");
  }
  const host = globalThis.halogenDyld;
  await halogen_lazy_link(host, url, path);
  const exported = host.dyld.exportFuncs[entry];
  if (typeof exported !== "function") {
    throw halogen_lazy_error("LinkError", path + " exports no function " + entry);
  }
  const pointer = await exported();
  if (typeof pointer !== "number" || pointer === 0) {
    throw halogen_lazy_error("LinkError", entry + " gave " + String(pointer) + ", not a StablePtr");
  }
  return pointer;
}

function halogen_lazy_number(value) {
  return value;
}

// A load whose program stopped waiting for it (the thread that asked was
// cancelled): the StablePtr the entry gives is freed as it arrives, through
// the runtime dyld exports, as the program would have freed it. A load that
// fails has nothing to free.
function halogen_lazy_abandon(promise) {
  Promise.resolve(promise).then(
    (pointer) => {
      const host = globalThis.halogenDyld;
      if (host && host.dyld && typeof host.dyld.exportFuncs.rts_freeStablePtr === "function") {
        host.dyld.exportFuncs.rts_freeStablePtr(pointer);
      }
    },
    () => {},
  );
}
