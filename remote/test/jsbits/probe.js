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
