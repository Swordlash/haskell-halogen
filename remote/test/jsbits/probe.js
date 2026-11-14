// The page's side of the activity probe (Halogen.Activity), as a test
// driving the page from outside would use it: set it up, ask it, and count
// who answers it.
function remote_probe_set() {
  if (globalThis.__halogenProbe === undefined) globalThis.__halogenProbe = {};
}

// "trees running waiting", or a rejection if nothing has answered.
async function remote_probe_ask() {
  const probe = globalThis.__halogenProbe;
  if (typeof probe?.activity !== "function") throw new Error("nothing has answered the probe");
  const a = await probe.activity();
  return `${a.trees} ${a.running} ${a.waiting}`;
}

// How many askers the page's probe has: one per program, and one per
// client of a remote server.
function remote_probe_askers() {
  const list = globalThis.__halogenProbe?.[Symbol.for("halogen.activity.programs")];
  return Array.isArray(list) ? list.length : 0;
}

// The id of the element that has the focus, empty when it has none.
function remote_active_id() {
  const active = document.activeElement;
  return active && typeof active.id === "string" ? active.id : "";
}

// An item of the page's local storage, empty for none.
function remote_storage_get(key) {
  const value = localStorage.getItem(key);
  return value === null ? "" : value;
}

function remote_storage_remove(key) {
  localStorage.removeItem(key);
}

// The page's pathname, query and fragment.
function remote_address() {
  return location.pathname + location.search + location.hash;
}

// The page's address without its fragment, in place of the current entry:
// where a hash router starts at its root.
function remote_drop_fragment() {
  history.replaceState(null, "", location.pathname + location.search);
}

// Back, as the browser's button: once the page has dispatched the
// popstate.
function remote_back() {
  return new Promise((done) => {
    addEventListener("popstate", () => done(""), { once: true });
    history.back();
  });
}
